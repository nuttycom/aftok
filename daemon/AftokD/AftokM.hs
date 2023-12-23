{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module AftokD.AftokM where

import Aftok.Billing
  ( Billable',
    ContactChannel (..),
    Subscription',
    contactChannel,
    customer,
    paymentRequestEmailTemplate,
    project,
  )
import qualified Aftok.Billing as B
import qualified Aftok.Config as AC
import Aftok.Currency.Bitcoin (Satoshi, _Satoshi)
import qualified Aftok.Currency.Bitcoin.Payments as Bitcoin
import qualified Aftok.Database as DB
import Aftok.Database.PostgreSQL (QDBM (..))
import qualified Aftok.Payments as P
import qualified Aftok.Payments.Bitcoin as Bitcoin
import qualified Aftok.Payments.Types as P
import Aftok.Project
  ( Project,
    projectName,
  )
import Aftok.Types
  ( ProjectId (..),
    User,
    UserId,
    _Email,
  )
import qualified AftokD as D
import Control.Error.Util (exceptT, maybeT)
import Control.Lens
  ( Iso',
    from,
    iso,
    makeClassyPrisms,
    makeLenses,
    over,
    set,
    to,
    traverseOf,
    (.~),
    (^.),
  )
import Control.Monad.Except
  ( MonadError,
    throwError,
  )
import Control.Monad.Trans.Except (withExceptT)
import Control.Monad.Trans.Reader (mapReaderT)
import Crypto.Random.Types (MonadRandom (..))
import qualified Data.Text as T
import Data.Thyme.Clock as C
import Database.PostgreSQL.Simple
  ( Connection,
    connect,
  )
import Filesystem.Path.CurrentOS (encodeString)
import qualified Network.Mail.Mime as Mime
import qualified Network.Mail.SMTP as SMTP
import Network.URI (URI)
import Text.StringTemplate
  ( directoryGroup,
    getStringTemplate,
    newSTMP,
    render,
    setManyAttrib,
  )

data AftokDErr
  = ConfigError Text
  | DBErr DB.DBError
  | PaymentErr P.PaymentError
  | MailGenError

makeClassyPrisms ''AftokDErr

-- instance P.AsPaymentError AftokDErr where
--   _PaymentError = _PaymentErr . P._PaymentError
--   _Overdue = _PaymentErr . P._Overdue
--   _SigningError = _PaymentErr . P._SigningError

data AftokMEnv = AftokMEnv
  { _dcfg :: !D.Config,
    _conn :: !Connection,
    _pcfg :: !(P.PaymentsConfig AftokM)
  }

-- instance P.HasPaymentsConfig AftokMEnv where
--   networkMode = pcfg . P.networkMode
--   signingKey = pcfg . P.signingKey
--   pkiData = pcfg . P.pkiData
--   paymentsConfig = pcfg

newtype AftokM a = AftokM {runAftokM :: ReaderT AftokMEnv (ExceptT AftokDErr IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadError AftokDErr, MonadReader AftokMEnv)

makeLenses ''AftokMEnv

instance MonadRandom AftokM where
  getRandomBytes = liftIO . getRandomBytes

instance DB.MonadDB AftokM where
  liftdb = liftQDBM . DB.liftdb

liftQDBM :: QDBM a -> AftokM a
liftQDBM (QDBM r) = do
  let f a = (a ^. dcfg . D.billingConfig . AC.bitcoinConfig . AC.networkMode, a ^. conn)
  AftokM . mapReaderT (withExceptT DBErr) . withReaderT f $ r

createAllPaymentRequests :: D.Config -> IO ()
createAllPaymentRequests cfg = do
  conn' <- connect $ cfg ^. D.dbConfig
  pcfg' <- AC.toPaymentsConfig $ cfg ^. D.billingConfig
  let env = AftokMEnv cfg conn' pcfg'
  void
    . runExceptT
    $ (runReaderT . runAftokM) createProjectsPaymentRequests
    $ env

createProjectsPaymentRequests :: AftokM ()
createProjectsPaymentRequests = do
  projects <- liftQDBM $ DB.listProjects
  traverse_ createProjectSubscriptionPaymentRequests projects

createProjectSubscriptionPaymentRequests :: ProjectId -> AftokM ()
createProjectSubscriptionPaymentRequests pid = do
  now <- liftIO C.getCurrentTime
  pcfg' <- asks _pcfg
  subscribers <- liftQDBM $ DB.findSubscribers pid
  subscriptions <- join <$> traverse (DB.findSubscriptions pid) subscribers
  requests <-
    fmap join
      . exceptT (throwError . PaymentErr) pure
      $ traverse (\s -> fmap (snd s,) <$> P.createSubscriptionPaymentRequests pcfg' now s) subscriptions
  traverse_ sendPaymentRequestEmail requests

_Compose :: Iso' (f (g a)) (Compose f g a)
_Compose = iso Compose getCompose

-- | TODO: Currently will only send email for bip70 requests
sendPaymentRequestEmail :: (B.Subscription, (P.PaymentRequestId, P.SomePaymentRequestDetail)) -> AftokM ()
sendPaymentRequestEmail (sub, (_, P.SomePaymentRequest req)) = do
  cfg <- ask
  pcfg' <- liftIO $ AC.toPaymentsConfig @AftokM (cfg ^. dcfg . D.billingConfig)
  let AC.SmtpConfig {..} = cfg ^. (dcfg . D.smtpConfig)
      preqCfg = cfg ^. (dcfg . D.paymentRequestConfig)
      req' = over P.billable (\b -> Compose $ sub & B.billable .~ b) req
  req'' <- enrichWithUser req'
  req''' <- enrichWithProject req''
  case req''' ^. P.nativeRequest of
    P.Bip70Request nreq -> do
      let bip70URIGen = Bitcoin.uriGen (pcfg' ^. P.bitcoinBillingOps)
      bip70URL <- bip70URIGen (nreq ^. Bitcoin.paymentRequestKey)
      mail <- traverse (buildBip70PaymentRequestEmail preqCfg req''') bip70URL
      let mailer =
            maybe
              (SMTP.sendMailWithLogin _smtpHost)
              (SMTP.sendMailWithLogin' _smtpHost)
              _smtpPort
      case mail of
        Just email -> liftIO $ mailer _smtpUser _smtpPass email
        Nothing -> throwError MailGenError
    P.Zip321Request _ -> pure ()

enrichWithUser ::
  P.PaymentRequest' (Compose (Subscription' UserId) (Billable' p u)) a ->
  AftokM (P.PaymentRequest' (Compose (Subscription' User) (Billable' p u)) a)
enrichWithUser req = do
  let sub = req ^. P.billable . from _Compose
  sub' <-
    maybeT (throwError $ DBErr DB.SubjectNotFound) pure $
      traverseOf customer DB.findUser sub
  pure (set P.billable (Compose sub') req)

enrichWithProject ::
  P.PaymentRequest' (Compose (Subscription' u) (Billable' ProjectId u')) a ->
  AftokM (P.PaymentRequest' (Compose (Subscription' u) (Billable' Project u')) a)
enrichWithProject req = do
  let sub = req ^. P.billable . from _Compose
  sub' <-
    maybeT (throwError $ DBErr DB.SubjectNotFound) pure $
      traverseOf (B.billable . project) DB.findProject sub
  pure (set P.billable (Compose sub') req)

buildBip70PaymentRequestEmail ::
  (MonadIO m, MonadError AftokDErr m) =>
  D.PaymentRequestConfig ->
  P.PaymentRequest' (Compose (Subscription' User) (Billable' Project UserId)) Satoshi ->
  URI ->
  m Mime.Mail
buildBip70PaymentRequestEmail cfg req paymentUrl = do
  templates <- liftIO . directoryGroup $ encodeString (cfg ^. D.templatePath)
  let billTemplate =
        (newSTMP . T.unpack)
          <$> (req ^. P.billable . to getCompose . B.billable . paymentRequestEmailTemplate)
      defaultTemplate = getStringTemplate "payment_request" templates
  case billTemplate <|> defaultTemplate of
    Nothing ->
      throwError $ ConfigError "Could not find template for invitation email"
    Just template -> do
      toEmail <- case req ^. (P.billable . to getCompose . contactChannel) of
        EmailChannel email -> pure email
      -- TODO: other channels
      let fromEmail = cfg ^. D.billingFromEmail
          pname = req ^. P.billable . to getCompose . B.billable . B.project . projectName
          total = req ^. P.billable . to getCompose . B.billable . B.amount
          setAttrs =
            setManyAttrib
              [ ("from_email", fromEmail ^. _Email),
                ("project_name", pname),
                ("to_email", toEmail ^. _Email),
                ("amount_due", show $ total ^. _Satoshi),
                ("payment_url", show paymentUrl)
              ]
          fromAddr = Mime.Address Nothing ("billing@aftok.com")
          toAddr = Mime.Address Nothing (toEmail ^. _Email)
          subject = "Payment is due for your " <> pname <> " subscription!"
          body = Mime.plainPart . render $ setAttrs template
      pure $ SMTP.simpleMail fromAddr [toAddr] [] [] subject [body]
