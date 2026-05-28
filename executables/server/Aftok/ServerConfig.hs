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
    captchaSiteKey,
    captchaSecretKey,

    -- * GitHub OAuth Configuration
    GitHubOAuthConfig (..),
    ghOAuthClientId,
    ghOAuthClientSecret,

    -- * Zcash Configuration
    readZcashConfig,

    -- * Lenses
    hostname,
    port,
    cookieTimeout,
    secureCookies,
    corsAllowedOrigins,
    dbConfig,
    dbConnStr,
    smtpConfig,
    billingConfig,
    migrationsPath,
    templatePath,
    staticAssetPath,
    recaptchaSecret,
    zcashConfig,
    gitHubOAuthConfig,
    externalPort,
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
data CaptchaConfig = CaptchaConfig
  { -- | Public site key (sent to client)
    _captchaSiteKey :: Text,
    -- | Secret key (server-side verification)
    _captchaSecretKey :: Text
  }

makeLenses ''CaptchaConfig

-- | GitHub OAuth application credentials
data GitHubOAuthConfig = GitHubOAuthConfig
  { _ghOAuthClientId :: Text,
    _ghOAuthClientSecret :: Text
  }

makeLenses ''GitHubOAuthConfig

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
          "host="
            <> host
            <> " port="
            <> show dbPort
            <> " user="
            <> user
            <> " password="
            <> password
            <> " dbname="
            <> db
  pure $ DbConfig connStr

-- | Create DbConfig from a DATABASE_URL environment variable
dbConfigFromUrl :: ByteString -> DbConfig
dbConfigFromUrl = DbConfig

data ServerConfig = ServerConfig
  { _hostname :: C8.ByteString,
    _port :: Int,
    _cookieTimeout :: Maybe Int,
    _secureCookies :: Bool,
    _corsAllowedOrigins :: [Text],
    _dbConfig :: DbConfig,
    _smtpConfig :: SmtpConfig,
    _billingConfig :: BillingConfig,
    _migrationsPath :: FilePath,
    _templatePath :: P.FilePath,
    _staticAssetPath :: P.FilePath,
    _recaptchaSecret :: CaptchaConfig,
    _zcashConfig :: ZcashConfig,
    _gitHubOAuthConfig :: Maybe GitHubOAuthConfig,
    _externalPort :: Maybe Int
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
    <*> C.lookupDefault [] cfg "corsAllowedOrigins"
    <*> maybe (mkDbConfig $ C.subconfig "db" cfg) pure pc
    <*> readSmtpConfig cfg
    <*> (readBillingConfig $ C.subconfig "billing" cfg)
    <*> C.lookupDefault "/opt/aftok/migrations" cfg "migrationsPath"
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
    <*> (CaptchaConfig <$> C.require cfg "recaptchaSiteKey" <*> C.require cfg "recaptchaSecret")
    <*> (readZcashConfig $ C.subconfig "zcash" cfg)
    <*> readGitHubOAuthConfig (C.subconfig "github" cfg)
    <*> C.lookup cfg "externalPort"

instance CT.Configured Network where
  convert = \case
    CT.String "mainnet" -> Just Mainnet
    CT.String "testnet" -> Just Testnet
    _ -> Nothing

readZcashConfig :: CT.Config -> IO ZcashConfig
readZcashConfig cfg =
  ZcashConfig
    <$> (C.require cfg "network")

readGitHubOAuthConfig :: CT.Config -> IO (Maybe GitHubOAuthConfig)
readGitHubOAuthConfig cfg = do
  mClientId <- C.lookup cfg "oauthClientId"
  mClientSecret <- C.lookup cfg "oauthClientSecret"
  pure $ GitHubOAuthConfig <$> mClientId <*> mClientSecret
