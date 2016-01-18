{-# LANGUAGE TypeApplications #-}
module Aftok.QConfig where

import           ClassyPrelude

import qualified Data.ByteString.Char8         as C
import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as CT
import           Data.X509
import           Data.X509.File                (readKeyFile, readSignedObject)
import qualified Network.Bippy.Types           as BT
import qualified Network.Mail.SMTP             as SMTP
import qualified Network.Socket                as NS
import           System.Environment
import           System.IO                     (FilePath)

import           Snap.Core
import qualified Snap.Http.Server.Config       as SC
import           Snap.Snaplet.PostgresqlSimple

import qualified Aftok.Payments                as AP

data QConfig = QConfig
  { hostname      :: ByteString
  , port          :: Int
  , authSiteKey   :: System.IO.FilePath
  , cookieTimeout :: Maybe Int
  , pgsConfig     :: PGSConfig
  , smtpConfig    :: SmtpConfig
  , billingConfig :: BillingConfig
  , templatePath  :: System.IO.FilePath
  , staticAssetPath :: System.IO.FilePath
  }

data SmtpConfig = SmtpConfig
  { smtpHost :: NS.HostName
  , smtpPort :: Maybe NS.PortNumber
  , smtpUser :: SMTP.UserName
  , smtpPass :: SMTP.Password
  }

data BillingConfig = BillingConfig
  { network                :: BT.Network
  , signingKeyFile         :: System.IO.FilePath
  , certsFile              :: System.IO.FilePath
  , exchangeRateServiceURI :: String
  }

loadQConfig :: System.IO.FilePath -> IO QConfig
loadQConfig cfgFile = do
  env <- getEnvironment
  cfg <- C.load [C.Required cfgFile]
  let dbEnvCfg = pgsDefaultConfig . C.pack <$> lookup "DATABASE_URL" env
  readQConfig cfg dbEnvCfg

readQConfig :: CT.Config -> Maybe PGSConfig -> IO QConfig
readQConfig cfg pc =
  QConfig <$> C.lookupDefault "localhost" cfg "hostname"
          <*> C.lookupDefault 8000 cfg "port"
          <*> C.require cfg "siteKey"
          <*> C.lookup cfg "cookieTimeout"
          <*> maybe (mkPGSConfig $ C.subconfig "db" cfg) pure pc
          <*> readSmtpConfig cfg
          <*> (readBillingConfig $ C.subconfig "billing" cfg)
          <*> C.lookupDefault "/opt/aftok/server/templates/" cfg "templatePath"
          <*> C.lookupDefault "/opt/aftok/server/static/" cfg "staticAssetPath"

readSmtpConfig :: CT.Config -> IO SmtpConfig
readSmtpConfig cfg =
  SmtpConfig <$> C.require cfg "smtpHost"
             <*> ((fmap . fmap) fromInteger $ C.lookup cfg "smtpPort")
             <*> C.require cfg "smtpUser"
             <*> C.require cfg "smtpKey"

readBillingConfig :: CT.Config -> IO BillingConfig
readBillingConfig cfg =
  BillingConfig <$> (parseNetwork <$> C.require cfg "network")
                <*> C.require cfg "signingKeyFile"
                <*> C.require cfg "certsFile"
                <*> C.require cfg "exchangeRateServiceURI"
  where parseNetwork :: String -> BT.Network
        parseNetwork "main" = BT.MainNet
        parseNetwork _      = BT.TestNet


baseSnapConfig :: QConfig -> SC.Config m a -> SC.Config m a
baseSnapConfig qc =
  SC.setHostname (hostname qc) .
  SC.setPort (port qc)

-- configuration specific to Snap, commandLineConfig arguments override
-- config file.
snapConfig :: QConfig -> IO (SC.Config Snap a)
snapConfig qc = SC.commandLineConfig $ baseSnapConfig qc SC.emptyConfig

toBillingConfig :: BillingConfig -> IO AP.BillingConfig
toBillingConfig c = do
  privKeys   <- readKeyFile (signingKeyFile c)
  pkiEntries <- readSignedObject (certsFile c)
  privKey <- case headMay privKeys of
    Just (PrivKeyRSA k) -> pure k
    Just (PrivKeyDSA _) -> fail "DSA keys not supported for payment request signing."
    Nothing             -> fail $ "No keys found in private key file " <> signingKeyFile c
  let pkiData = BT.X509SHA256 . CertificateChain $ pkiEntries
  pure $ AP.BillingConfig (network c) privKey pkiData
