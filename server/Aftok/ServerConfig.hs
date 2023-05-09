{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Aftok.ServerConfig where

import Aftok.Config
import Aftok.Currency.Zcash (ZcashConfig (..), ZcashdConfig (..))
import Aftok.Snaplet.Users (CaptchaConfig (..))
import Control.Lens
  ( makeLenses,
    (^.),
  )
import qualified Data.ByteString.Char8 as C8
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.List as L
import Filesystem.Path.CurrentOS
  ( encodeString,
    fromText,
  )
import qualified Filesystem.Path.CurrentOS as P
import Lrzhs.Types (Network (..))
import Snap.Core
import qualified Snap.Http.Server.Config as SC
import Snap.Snaplet.PostgresqlSimple
import System.Environment (getEnvironment)

data ServerConfig = ServerConfig
  { _hostname :: C8.ByteString,
    _port :: Int,
    _authSiteKey :: P.FilePath,
    _cookieTimeout :: Maybe Int,
    _pgsConfig :: PGSConfig,
    _smtpConfig :: SmtpConfig,
    _billingConfig :: BillingConfig,
    _templatePath :: P.FilePath,
    _staticAssetPath :: P.FilePath,
    _recaptchaSecret :: CaptchaConfig,
    _zcashConfig :: ZcashConfig
  }

makeLenses ''ServerConfig

loadServerConfig :: P.FilePath -> IO ServerConfig
loadServerConfig cfgFile = do
  env <- getEnvironment
  putStrLn $ "Loading config from file " <> show cfgFile
  cfg <- C.load [C.Required $ encodeString cfgFile]
  let dbEnvCfg = pgsDefaultConfig . C8.pack <$> L.lookup "DATABASE_URL" env
  readServerConfig cfg dbEnvCfg

readServerConfig :: CT.Config -> Maybe PGSConfig -> IO ServerConfig
readServerConfig cfg pc =
  ServerConfig
    <$> C.lookupDefault "localhost" cfg "hostname"
    <*> C.lookupDefault 8000 cfg "port"
    <*> (fromText <$> C.require cfg "siteKey")
    <*> C.lookup cfg "cookieTimeout"
    <*> maybe (mkPGSConfig $ C.subconfig "db" cfg) pure pc
    <*> readSmtpConfig cfg
    <*> (readBillingConfig $ C.subconfig "billing" cfg)
    <*> ( fromText
            <$> C.lookupDefault
              "/opt/aftok/server/templates/"
              cfg
              "templatePath"
        )
    <*> ( fromText
            <$> C.lookupDefault
              "/opt/aftok/server/static/"
              cfg
              "staticAssetPath"
        )
    <*> (CaptchaConfig <$> C.require cfg "recaptchaSecret")
    <*> (readZcashConfig $ C.subconfig "zcash" cfg)

instance CT.Configured Network where
  convert = \case
    CT.String "mainnet" -> Just Mainnet
    CT.String "testnet" -> Just Testnet
    _ -> Nothing

readZcashConfig :: CT.Config -> IO ZcashConfig
readZcashConfig cfg =
  ZcashConfig
    <$> (C.require cfg "network")
    <*> (readZcashdConfig $ C.subconfig "zcashd" cfg)

readZcashdConfig :: CT.Config -> IO (Maybe ZcashdConfig)
readZcashdConfig cfg =
  getCompose $
    ZcashdConfig
      <$> Compose (C.lookup cfg "rpcHost")
      <*> Compose (C.lookup cfg "rpcPort")
      <*> Compose (C.lookup cfg "rpcUser")
      <*> Compose (C.lookup cfg "rpcPassword")

baseSnapConfig :: ServerConfig -> SC.Config m a -> SC.Config m a
baseSnapConfig qc = SC.setHostname (qc ^. hostname) . SC.setPort (qc ^. port)

-- configuration specific to Snap, commandLineConfig arguments override
-- config file.
snapConfig :: ServerConfig -> IO (SC.Config Snap a)
snapConfig qc = SC.commandLineConfig $ baseSnapConfig qc SC.emptyConfig
