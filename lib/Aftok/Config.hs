{-# LANGUAGE TemplateHaskell #-}

module Aftok.Config where

import           ClassyPrelude hiding (FilePath)

import Control.Lens (makeClassy, (^.))
import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as CT
import           Data.X509
import           Data.X509.File                (readKeyFile, readSignedObject)
import Database.PostgreSQL.Simple              (ConnectInfo(..))
import Filesystem.Path.CurrentOS (FilePath, fromText, encodeString)

import qualified Network.Bippy.Types           as BT
import qualified Network.Mail.SMTP             as SMTP
import qualified Network.Socket                as NS

import Aftok.Payments (PaymentsConfig(..))

data SmtpConfig = SmtpConfig
  { _smtpHost :: NS.HostName
  , _smtpPort :: Maybe NS.PortNumber
  , _smtpUser :: SMTP.UserName
  , _smtpPass :: SMTP.Password
  }
makeClassy ''SmtpConfig

data BillingConfig = BillingConfig
  { _network                :: BT.Network
  , _signingKeyFile         :: FilePath
  , _certsFile              :: FilePath
  , _exchangeRateServiceURI :: String
  } 
makeClassy ''BillingConfig

readSmtpConfig :: CT.Config -> IO SmtpConfig
readSmtpConfig cfg =
  SmtpConfig <$> C.require cfg "smtpHost"
             <*> ((fmap . fmap) fromInteger $ C.lookup cfg "smtpPort")
             <*> C.require cfg "smtpUser"
             <*> C.require cfg "smtpKey"

readBillingConfig :: CT.Config -> IO BillingConfig
readBillingConfig cfg =
  BillingConfig <$> (parseNetwork <$> C.require cfg "network")
                <*> (fromText <$> C.require cfg "signingKeyFile")
                <*> (fromText <$> C.require cfg "certsFile")
                <*> C.require cfg "exchangeRateServiceURI"
  where parseNetwork :: String -> BT.Network
        parseNetwork "main" = BT.MainNet
        parseNetwork _      = BT.TestNet

readConnectInfo :: CT.Config -> IO ConnectInfo
readConnectInfo cfg =
  ConnectInfo <$> C.require cfg "host"
              <*> C.require cfg "port"
              <*> C.require cfg "user"
              <*> C.require cfg "password"
              <*> C.require cfg "database"

toPaymentsConfig :: BillingConfig -> IO PaymentsConfig
toPaymentsConfig c = do
  privKeys   <- readKeyFile . encodeString $ c ^. signingKeyFile
  pkiEntries <- readSignedObject . encodeString $ c ^. certsFile
  privKey <- case headMay privKeys of
    Just (PrivKeyRSA k) -> pure k
    Just (PrivKeyDSA _) -> fail "DSA keys not supported for payment request signing."
    Nothing             -> fail $ "No keys found in private key file " <> encodeString (c ^. signingKeyFile)
  let pkiData = BT.X509SHA256 . CertificateChain $ pkiEntries
  pure $ PaymentsConfig (c ^. network) privKey pkiData
