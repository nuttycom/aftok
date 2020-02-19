{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module AftokD.AftokM where

import ClassyPrelude

import Control.Error.Util (maybeT)
import Control.Lens ((^.), makeLenses, makeClassyPrisms, traverseOf, to)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Except (ExceptT, withExceptT, runExceptT)
import Control.Monad.Trans.Reader (mapReaderT, withReaderT)

import           Crypto.Random.Types     (MonadRandom(..))

import Database.PostgreSQL.Simple        (Connection, connect)
import           Data.Thyme.Clock     as C
import Data.Thyme.Time  as T
import qualified Network.Mail.Mime          as Mime
import qualified Network.Mail.SMTP          as SMTP
import Network.URI (URI, parseURI)
import Network.Haskoin.Address (Address)
import Text.StringTemplate (directoryGroup, newSTMP, getStringTemplate, setManyAttrib, render)
import Filesystem.Path.CurrentOS (encodeString)

import Network.Bippy.Types (Satoshi)

import Aftok.Types            (User, UserId, ProjectId(..), userEmail, _Email)
import Aftok.Currency.Bitcoin (NetworkId, satoshi)
import qualified Aftok.Config as AC
import Aftok.Billables  (Billable, Billable', Subscription', customer, name, billable, project, paymentRequestEmailTemplate, paymentRequestMemoTemplate)
import qualified Aftok.Database as DB
import           Aftok.Database.PostgreSQL (QDBM(..))
import qualified Aftok.Payments as P
import           Aftok.Payments.Types (PaymentKey(..), subscription, paymentRequestTotal, paymentKey)
import           Aftok.Project (Project, projectName)
import qualified AftokD as D

data AftokDErr
  = ConfigError Text
  | DBErr DB.DBError
  | PaymentErr P.PaymentError
makeClassyPrisms ''AftokDErr

instance P.AsPaymentError AftokDErr where
  _PaymentError = _PaymentErr . P._PaymentError
  _Overdue = _PaymentErr . P._Overdue
  _SigningError = _PaymentErr . P._SigningError

data AftokMEnv = AftokMEnv
  { _dcfg :: !D.Config
  , _conn :: !Connection
  , _pcfg :: !P.PaymentsConfig
  }
makeLenses ''AftokMEnv

instance P.HasPaymentsConfig AftokMEnv where
  networkMode = pcfg . P.networkMode
  signingKey = pcfg . P.signingKey
  pkiData = pcfg . P.pkiData
  paymentsConfig = pcfg

newtype AftokM a = AftokM { runAftokM :: ReaderT AftokMEnv (ExceptT AftokDErr IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError AftokDErr, MonadReader AftokMEnv)

instance MonadRandom AftokM where
  getRandomBytes = liftIO . getRandomBytes

instance DB.MonadDB AftokM where
  liftdb = liftQDBM . DB.liftdb

liftQDBM :: QDBM a -> AftokM a
liftQDBM (QDBM r) = do
  let f a = (a ^. dcfg . D.billingConfig . AC.networkMode, a ^. conn)
  AftokM . mapReaderT (withExceptT DBErr) . withReaderT f $ r

createAllPaymentRequests :: D.Config -> IO ()
createAllPaymentRequests cfg = do
  conn' <- connect $ cfg ^. D.dbConfig
  pcfg' <- AC.toPaymentsConfig $ cfg ^. D.billingConfig
  let env = AftokMEnv cfg conn' pcfg'
  void . runExceptT $ (runReaderT . runAftokM) createProjectsPaymentRequests $ env

createProjectsPaymentRequests :: AftokM ()
createProjectsPaymentRequests = do
  projects <- liftQDBM $ DB.listProjects
  traverse_ createProjectPaymentRequests projects

createProjectPaymentRequests :: ProjectId -> AftokM ()
createProjectPaymentRequests pid = do
    now <- liftIO C.getCurrentTime
    let ops = P.BillingOps memoGen (fmap Just . paymentURL) payloadGen
    subscribers <- liftQDBM $ DB.findSubscribers pid
    requests <- traverse (\uid -> P.createPaymentRequests ops now uid pid) $ subscribers
    traverse_ sendPaymentRequestEmail (join requests)

sendPaymentRequestEmail :: P.PaymentRequestId -> AftokM ()
sendPaymentRequestEmail reqId = do
  cfg <- ask
  let AC.SmtpConfig{..} = cfg ^. (dcfg . D.smtpConfig)
      preqCfg = cfg ^. (dcfg . D.paymentRequestConfig)
      reqMay = do
        preq <- DB.findPaymentRequestId reqId
        preq' <- traverseOf P.subscription DB.findSubscriptionBillable preq
        preq'' <- traverseOf (P.subscription . customer) DB.findUser preq'
        traverseOf (P.subscription . billable . project) DB.findProject preq''
  req <- maybeT (throwError $ DBErr DB.SubjectNotFound) pure reqMay
  bip70URL <- paymentURL (req ^. paymentKey)
  mail <- buildPaymentRequestEmail preqCfg req bip70URL
  let mailer = maybe (SMTP.sendMailWithLogin _smtpHost) (SMTP.sendMailWithLogin' _smtpHost) _smtpPort
  liftIO $ mailer _smtpUser _smtpPass mail

buildPaymentRequestEmail :: (MonadIO m, MonadError AftokDErr m)
                         => D.PaymentRequestConfig
                         -> P.PaymentRequest' (Subscription' (User (NetworkId, Address)) (Billable' Project UserId Satoshi))
                         -> URI
                         -> m Mime.Mail
buildPaymentRequestEmail cfg req paymentUrl = do
  templates <- liftIO . directoryGroup $ encodeString (cfg ^. D.templatePath)
  let billTemplate = (newSTMP . unpack) <$> req ^. (subscription . billable . paymentRequestEmailTemplate)
      defaultTemplate = getStringTemplate "payment_request" templates
  case billTemplate <|> defaultTemplate of
    Nothing -> throwError $ ConfigError "Could not find template for invitation email"
    Just template ->
      let fromEmail = cfg ^. D.billingFromEmail
          toEmail = req ^. (subscription . customer . userEmail)
          pname = req ^. (subscription . billable . project . projectName)
          total = req ^. (P.paymentRequest . to paymentRequestTotal)
          setAttrs = setManyAttrib
            [ ("from_email", fromEmail ^. _Email)
            , ("project_name", pname)
            , ("to_email", toEmail ^. _Email)
            , ("amount_due", tshow $ total ^. satoshi)
            , ("payment_url", tshow paymentUrl)
            ]
          fromAddr = Mime.Address Nothing ("billing@aftok.com")
          toAddr   = Mime.Address Nothing (toEmail ^. _Email)
          subject  = "Payment is due for your "<>pname<>" subscription!"
          body     = SMTP.plainTextPart . render $ setAttrs template
      in  pure $ SMTP.simpleMail fromAddr [toAddr] [] [] subject [body]

memoGen :: Subscription' UserId Billable
        -> T.Day
        -> C.UTCTime
        -> AftokM (Maybe Text)
memoGen sub billingDate requestTime = do
  req <- traverseOf (billable . project) DB.findProjectOrError sub
  let template = (newSTMP . unpack) <$> (sub ^. (billable . paymentRequestMemoTemplate))
      setAttrs = setManyAttrib
        [ ("project_name", req ^. (billable . project . projectName))
        , ("subscription", req ^. (billable . name))
        , ("billing_date", tshow billingDate)
        , ("issue_time", tshow requestTime)
        ]
  pure $ fmap (render . setAttrs) template

-- The same URL is used for retrieving a BIP-70 payment request and for submitting
-- the response.
paymentURL :: PaymentKey -> AftokM URI
paymentURL (PaymentKey k) = do
  env <- ask
  let hostname = env ^. (dcfg . D.paymentRequestConfig . D.aftokHost)
      paymentRequestPath = "https://" <> hostname <> "/pay/" <> k
  maybe
    (throwError . ConfigError $ "Could not parse path " <> paymentRequestPath <> " to a valid URI")
    pure
    (parseURI $ show paymentRequestPath)

payloadGen :: Monad m => Subscription' UserId Billable -> T.Day -> C.UTCTime -> m (Maybe ByteString)
payloadGen _ _ _ = pure Nothing
