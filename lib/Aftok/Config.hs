{-# LANGUAGE TemplateHaskell #-}

module Aftok.Config where

import qualified Aftok.Billing as B
import Aftok.Currency.Bitcoin (NetworkMode, Satoshi (..))
import Aftok.Currency.Zcash (Zatoshi (..))
import Aftok.Currency.Zcash.Types (Memo (..))
import Aftok.Database (MonadDB, findProjectOrError)
import Aftok.Payments (PaymentsConfig (..))
import qualified Aftok.Payments.Bitcoin as Bitcoin
import Aftok.Payments.Common (PaymentKey (..))
import qualified Aftok.Payments.Zcash as Zcash
import Aftok.Project (projectName)
import Aftok.Types (AccountId)
import qualified Bippy.Types as BT
import Control.Lens
  ( makeLenses,
    traverseOf,
    (^.),
  )
import Crypto.Random.Types
  ( MonadRandom,
    getRandomBytes,
  )
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Thyme.Clock (UTCTime)
import Data.Thyme.Time (Day)
import Data.X509
import Data.X509.File
  ( readKeyFile,
    readSignedObject,
  )
import Database.PostgreSQL.Simple (ConnectInfo (..))
import Filesystem.Path.CurrentOS
  ( encodeString,
    fromText,
  )
import qualified Filesystem.Path.CurrentOS as P
import Haskoin.Address (encodeBase58Check)
import qualified Network.Mail.SMTP as SMTP
import qualified Network.Socket as NS
import Network.URI (URI, parseURI)
import Safe (headMay)
import Text.StringTemplate
  ( newSTMP,
    render,
    setManyAttrib,
  )

readConnectInfo :: C.Config -> IO ConnectInfo
readConnectInfo cfg =
  ConnectInfo
    <$> C.require cfg "host"
    <*> C.require cfg "port"
    <*> C.require cfg "user"
    <*> C.require cfg "password"
    <*> C.require cfg "database"

data SmtpConfig = SmtpConfig
  { _smtpHost :: NS.HostName,
    _smtpPort :: Maybe NS.PortNumber,
    _smtpUser :: SMTP.UserName,
    _smtpPass :: SMTP.Password
  }

makeLenses ''SmtpConfig

readSmtpConfig :: C.Config -> IO SmtpConfig
readSmtpConfig cfg =
  SmtpConfig
    <$> C.require cfg "smtpHost"
    <*> ((fmap . fmap) fromInteger $ C.lookup cfg "smtpPort")
    <*> C.require cfg "smtpUser"
    <*> C.require cfg "smtpKey"

data BitcoinConfig = BitcoinConfig
  { _networkMode :: NetworkMode,
    _signingKeyFile :: P.FilePath,
    _certsFile :: P.FilePath,
    _exchangeRateServiceURI :: String,
    _minPayment :: Satoshi,
    _bip70Host :: NS.HostName
  }

makeLenses ''BitcoinConfig

data BillingConfig = BillingConfig
  { _bitcoinConfig :: BitcoinConfig,
    _zcashConfig :: Zcash.PaymentsConfig
  }

makeLenses ''BillingConfig

readBillingConfig :: C.Config -> IO BillingConfig
readBillingConfig cfg =
  BillingConfig
    <$> (readBitcoinConfig $ C.subconfig "bitcoin" cfg)
    <*> (readZcashPaymentsConfig $ C.subconfig "zcash" cfg)

readBitcoinConfig :: C.Config -> IO BitcoinConfig
readBitcoinConfig cfg =
  BitcoinConfig
    <$> C.require cfg "networkMode"
    <*> (fromText <$> C.require cfg "signingKeyFile")
    <*> (fromText <$> C.require cfg "certsFile")
    <*> C.require cfg "exchangeRateServiceURI"
    <*> (Satoshi <$> C.lookupDefault 100 cfg "minPayment")
    <*> C.require cfg "bip70Host"

readZcashPaymentsConfig :: C.Config -> IO Zcash.PaymentsConfig
readZcashPaymentsConfig cfg =
  Zcash.PaymentsConfig
    <$> (Zatoshi <$> C.require cfg "minPayment")

toBitcoinPaymentsConfig :: BitcoinConfig -> IO Bitcoin.PaymentsConfig
toBitcoinPaymentsConfig c = do
  privKeys <- readKeyFile . encodeString $ c ^. signingKeyFile
  pkiEntries <- readSignedObject . encodeString $ c ^. certsFile
  privKey <- case headMay privKeys of
    Just (PrivKeyRSA k) -> pure k
    Just _ ->
      fail $
        "Only RSA keys are currently supported for payment request signing."
    Nothing ->
      fail $
        "No keys found in private key file "
          <> encodeString
            (c ^. signingKeyFile)
  let pkiData = BT.X509SHA256 . CertificateChain $ pkiEntries
  pure $ Bitcoin.PaymentsConfig (c ^. networkMode) privKey pkiData (_minPayment c)

toPaymentsConfig :: (MonadRandom m, MonadDB m) => BillingConfig -> IO (PaymentsConfig m)
toPaymentsConfig cfg = do
  btcCfg <- toBitcoinPaymentsConfig (cfg ^. bitcoinConfig)
  let btcOps = Bitcoin.BillingOps _btcMemoGen (_uriGen $ cfg ^. bitcoinConfig . bip70Host) _payloadGen
  pure $
    PaymentsConfig
      { _bitcoinBillingOps = btcOps,
        _bitcoinPaymentsConfig = btcCfg,
        _zcashBillingOps = _zcashMemoGen,
        _zcashPaymentsConfig = cfg ^. zcashConfig
      }

_btcMemoGen ::
  MonadDB m =>
  B.Billable Satoshi ->
  Day ->
  UTCTime ->
  m (Maybe Text)
_btcMemoGen bill billingDate requestTime = do
  req <- traverseOf B.project findProjectOrError bill
  let template =
        (newSTMP . toString)
          <$> (bill ^. B.paymentRequestMemoTemplate)
      setAttrs =
        setManyAttrib
          [ ("project_name", req ^. B.project . projectName),
            ("subscription", req ^. B.name),
            ("billing_date", show billingDate),
            ("issue_time", show requestTime)
          ]
  pure $ fmap (render . setAttrs) template

_zcashMemoGen ::
  (MonadRandom m, MonadDB m) =>
  B.Billable Zatoshi ->
  Day ->
  UTCTime ->
  AccountId ->
  m (Maybe Memo)
_zcashMemoGen _ _ _ _ = do
  pkey <- encodeBase58Check <$> getRandomBytes 32
  -- for now
  pure $ Just (Memo $ encodeUtf8 pkey)

_payloadGen ::
  Monad m =>
  B.Billable Satoshi ->
  Day ->
  UTCTime ->
  m (Maybe ByteString)
_payloadGen _ _ _ = pure Nothing

-- The same URL is used for retrieving a BIP-70 payment request and for submitting
-- the response.
_uriGen ::
  Monad m =>
  NS.HostName ->
  PaymentKey ->
  m (Maybe URI)
_uriGen hostname (PaymentKey k) =
  let paymentRequestPath = "https://" <> fromString hostname <> "/pay/" <> k
   in pure . parseURI $ show paymentRequestPath
