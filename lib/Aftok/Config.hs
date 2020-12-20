{-# LANGUAGE TemplateHaskell #-}

module Aftok.Config where

import Aftok.Currency.Bitcoin (NetworkMode)
import Aftok.Payments.Bitcoin (PaymentsConfig (..))
import qualified Bippy.Types as BT
import Control.Lens
  ( (^.),
    makeClassy,
  )
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
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
import qualified Network.Mail.SMTP as SMTP
import qualified Network.Socket as NS
import Safe (headMay)

data SmtpConfig
  = SmtpConfig
      { _smtpHost :: NS.HostName,
        _smtpPort :: Maybe NS.PortNumber,
        _smtpUser :: SMTP.UserName,
        _smtpPass :: SMTP.Password
      }

makeClassy ''SmtpConfig

data BillingConfig
  = BillingConfig
      { _networkMode :: NetworkMode,
        _signingKeyFile :: P.FilePath,
        _certsFile :: P.FilePath,
        _exchangeRateServiceURI :: String
      }

makeClassy ''BillingConfig

readSmtpConfig :: C.Config -> IO SmtpConfig
readSmtpConfig cfg =
  SmtpConfig
    <$> C.require cfg "smtpHost"
    <*> ((fmap . fmap) fromInteger $ C.lookup cfg "smtpPort")
    <*> C.require cfg "smtpUser"
    <*> C.require cfg "smtpKey"

readBillingConfig :: C.Config -> IO BillingConfig
readBillingConfig cfg =
  BillingConfig
    <$> C.require cfg "networkMode"
    <*> (fromText <$> C.require cfg "signingKeyFile")
    <*> (fromText <$> C.require cfg "certsFile")
    <*> C.require cfg "exchangeRateServiceURI"

readConnectInfo :: C.Config -> IO ConnectInfo
readConnectInfo cfg =
  ConnectInfo
    <$> C.require cfg "host"
    <*> C.require cfg "port"
    <*> C.require cfg "user"
    <*> C.require cfg "password"
    <*> C.require cfg "database"

toPaymentsConfig :: BillingConfig -> IO PaymentsConfig
toPaymentsConfig c = do
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
  pure $ PaymentsConfig (c ^. networkMode) privKey pkiData mempty
