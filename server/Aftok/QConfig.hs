{-# LANGUAGE TypeApplications #-}
module Aftok.QConfig where

import           ClassyPrelude

import qualified Data.ByteString.Char8         as C
import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as CT
import qualified Network.Bippy.Types           as B
import qualified Network.Mail.SMTP             as SMTP
import qualified Network.Socket                as NS
import           System.Environment
import           System.IO                     (FilePath)

import           Snap.Core
import qualified Snap.Http.Server.Config       as SC
import           Snap.Snaplet.PostgresqlSimple

data QConfig = QConfig
  { hostname      :: ByteString
  , port          :: Int
  , authSiteKey   :: System.IO.FilePath
  , cookieTimeout :: Maybe Int
  , pgsConfig     :: PGSConfig
  , smtpConfig    :: SmtpConfig
  , btcConfig     :: BtcConfig
  , templatePath  :: System.IO.FilePath
  }

data SmtpConfig = SmtpConfig
  { smtpHost :: NS.HostName
  , smtpPort :: Maybe NS.PortNumber
  , smtpUser :: SMTP.UserName
  , smtpPass :: SMTP.Password
  }

data BtcConfig = BtcConfig 
  { btcNetwork :: B.Network }

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
          <*> readBtcConfig cfg
          <*> C.lookupDefault "/opt/aftok/server/templates/" cfg "templatePath"

readSmtpConfig :: CT.Config -> IO SmtpConfig
readSmtpConfig cfg =
  SmtpConfig <$> C.require cfg "smtpHost"
             <*> ((fmap . fmap) fromInteger $ C.lookup cfg "smtpPort")
             <*> C.require cfg "smtpUser"
             <*> C.require cfg "smtpKey"

readBtcConfig :: CT.Config -> IO BtcConfig
readBtcConfig cfg = 
  let parseNetwork :: String -> B.Network
      parseNetwork "main" = B.MainNet
      parseNetwork _ = B.TestNet
  in  (BtcConfig . parseNetwork) <$> C.require cfg "network"

baseSnapConfig :: QConfig -> SC.Config m a -> SC.Config m a
baseSnapConfig qc =
  SC.setHostname (hostname qc) .
  SC.setPort (port qc)

-- configuration specific to Snap, commandLineConfig arguments override
-- config file.
snapConfig :: QConfig -> IO (SC.Config Snap a)
snapConfig qc = SC.commandLineConfig $ baseSnapConfig qc SC.emptyConfig


