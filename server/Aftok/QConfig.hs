{-# LANGUAGE TypeApplications #-}
module Aftok.QConfig where

import           ClassyPrelude hiding (FilePath)

import qualified Data.ByteString.Char8         as C8
import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as CT
import           System.Environment            (getEnvironment)
import Filesystem.Path.CurrentOS (FilePath, fromText, encodeString)

import           Snap.Core
import qualified Snap.Http.Server.Config       as SC
import           Snap.Snaplet.PostgresqlSimple

import           Aftok.Config

data QConfig = QConfig
  { hostname      :: ByteString
  , port          :: Int
  , authSiteKey   :: FilePath
  , cookieTimeout :: Maybe Int
  , pgsConfig     :: PGSConfig
  , smtpConfig    :: SmtpConfig
  , billingConfig :: BillingConfig
  , templatePath  :: FilePath
  , staticAssetPath :: FilePath
  }

loadQConfig :: FilePath -> IO QConfig
loadQConfig cfgFile = do
  env <- getEnvironment
  cfg <- C.load [C.Required $ encodeString cfgFile]
  let dbEnvCfg = pgsDefaultConfig . C8.pack <$> lookup "DATABASE_URL" env
  readQConfig cfg dbEnvCfg

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
  SC.setHostname (hostname qc) .
  SC.setPort (port qc)

-- configuration specific to Snap, commandLineConfig arguments override
-- config file.
snapConfig :: QConfig -> IO (SC.Config Snap a)
snapConfig qc = SC.commandLineConfig $ baseSnapConfig qc SC.emptyConfig

