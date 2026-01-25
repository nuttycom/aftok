{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Aftok.ServerConfig
  ( -- * Server Configuration
    ServerConfig (..),
    loadServerConfig,
    readServerConfig,

    -- * Database Configuration
    DbConfig (..),
    mkDbConfig,
    dbConfigFromUrl,

    -- * Captcha Configuration
    CaptchaConfig (..),

    -- * Zcash Configuration
    readZcashConfig,

    -- * Lenses
    hostname,
    port,
    cookieTimeout,
    secureCookies,
    dbConfig,
    dbConnStr,
    smtpConfig,
    billingConfig,
    templatePath,
    staticAssetPath,
    recaptchaSecret,
    zcashConfig,
  )
where

import Aftok.Config (BillingConfig, SmtpConfig, readBillingConfig, readSmtpConfig)
import Aftok.Currency.Zcash (ZcashConfig (..))
import Control.Lens
  ( makeLenses,
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
import System.Environment (getEnvironment)

-- | Captcha configuration for reCAPTCHA
newtype CaptchaConfig = CaptchaConfig
  { secretKey :: Text
  }

-- | Database configuration
data DbConfig = DbConfig
  { _dbConnStr :: ByteString
  }

makeLenses ''DbConfig

-- | Build a connection string from individual config values
mkDbConfig :: CT.Config -> IO DbConfig
mkDbConfig cfg = do
  host <- C.lookupDefault "localhost" cfg "host"
  dbPort <- C.lookupDefault (5432 :: Int) cfg "port"
  user <- C.require cfg "user"
  password <- C.require cfg "pass"
  db <- C.require cfg "db"
  let connStr =
        C8.pack $
          "host=" <> host
            <> " port=" <> show dbPort
            <> " user=" <> user
            <> " password=" <> password
            <> " dbname=" <> db
  pure $ DbConfig connStr

-- | Create DbConfig from a DATABASE_URL environment variable
dbConfigFromUrl :: ByteString -> DbConfig
dbConfigFromUrl = DbConfig

data ServerConfig = ServerConfig
  { _hostname :: C8.ByteString,
    _port :: Int,
    _cookieTimeout :: Maybe Int,
    _secureCookies :: Bool,
    _dbConfig :: DbConfig,
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
  let dbEnvCfg = dbConfigFromUrl . C8.pack <$> L.lookup "DATABASE_URL" env
  readServerConfig cfg dbEnvCfg

readServerConfig :: CT.Config -> Maybe DbConfig -> IO ServerConfig
readServerConfig cfg pc =
  ServerConfig
    <$> C.lookupDefault "localhost" cfg "hostname"
    <*> C.lookupDefault 8000 cfg "port"
    <*> C.lookup cfg "cookieTimeout"
    <*> C.lookupDefault True cfg "secureCookies"
    <*> maybe (mkDbConfig $ C.subconfig "db" cfg) pure pc
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
