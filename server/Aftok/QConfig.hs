{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Aftok.QConfig where

import           ClassyPrelude hiding (FilePath)

import Control.Lens (makeLenses, (^.))
import qualified Data.ByteString.Char8         as C8
import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as CT
import           System.Directory              (doesFileExist, doesPathExist, listDirectory)
import           System.Environment            (getEnvironment)
import Filesystem.Path.CurrentOS (FilePath, fromText, encodeString, parent)

import           Snap.Core
import qualified Snap.Http.Server.Config       as SC
import           Snap.Snaplet.PostgresqlSimple

import           Aftok.Config

data QConfig = QConfig
  { _hostname      :: ByteString
  , _port          :: Int
  , _authSiteKey   :: FilePath
  , _cookieTimeout :: Maybe Int
  , _pgsConfig     :: PGSConfig
  , _smtpConfig    :: SmtpConfig
  , _billingConfig :: BillingConfig
  , _templatePath  :: FilePath
  , _staticAssetPath :: FilePath
  }
makeLenses ''QConfig

loadQConfig :: FilePath -> IO QConfig
loadQConfig cfgFile = do
  env <- getEnvironment
  cfgExists <- doesFileExist $ encodeString cfgFile
  pathExists <- doesPathExist $ encodeString cfgFile
  files <- listDirectory (encodeString $ parent cfgFile)
  putStrLn $ "Loading config from: " <> (pack . encodeString $ cfgFile)
          <> "; file exists = " <> (pack . show $ cfgExists)
          <> "; path exists = " <> (pack . show $ pathExists)
          <> "; parent dir = " <> (pack . encodeString $ parent cfgFile)
          <> "; dir contents = " <> (pack . show $ files)

  cfg <- C.load [C.Required $ encodeString cfgFile]
  let dbEnvCfg = pgsDefaultConfig . C8.pack <$> lookup "DATABASE_URL" env
  conf <- readQConfig cfg dbEnvCfg
  putStrLn $ "Config loaded successfully."
  pure conf

readQConfig :: CT.Config -> Maybe PGSConfig -> IO QConfig
readQConfig cfg pc =
  QConfig <$> C.lookupDefault "localhost" cfg "hostname"
          <*> C.lookupDefault 8000 cfg "port"
          <*> (fromText <$> C.require cfg "siteKey")
          <*> C.lookup cfg "cookieTimeout"
          <*> maybe (mkPGSConfig $ C.subconfig "db" cfg) pure pc
          <*> readSmtpConfig cfg
          <*> (readBillingConfig $ C.subconfig "billing" cfg)
          <*> (fromText <$> C.lookupDefault "/opt/aftok/server/templates/" cfg "templatePath")
          <*> (fromText <$> C.lookupDefault "/opt/aftok/server/static/" cfg "staticAssetPath")

baseSnapConfig :: QConfig -> SC.Config m a -> SC.Config m a
baseSnapConfig qc =
  SC.setHostname (qc ^. hostname) .
  SC.setPort (qc ^. port)

-- configuration specific to Snap, commandLineConfig arguments override
-- config file.
snapConfig :: QConfig -> IO (SC.Config Snap a)
snapConfig qc = SC.commandLineConfig $ baseSnapConfig qc SC.emptyConfig

