{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aftok.Config (SmtpConfig (..))
import qualified Aftok.Config as C
import Aftok.Currency.Zcash (zcashNetwork)
import qualified Aftok.Currency.Zcash as Zcash
import Aftok.Database.PostgreSQL (QDBM)
import Aftok.Servant.PasswordReset (PasswordResetOps (..))
import Aftok.Servant.Server (aftokApp, mkAppEnv)
import Aftok.Servant.Users
  ( AddressInvalid (..),
    RegisterOps (..),
  )
import Aftok.ServerConfig
  ( ServerConfig,
    billingConfig,
    corsAllowedOrigins,
    dbConfig,
    dbConnStr,
    hostname,
    loadServerConfig,
    migrationsPath,
    port,
    recaptchaSecret,
    secureCookies,
    smtpConfig,
    staticAssetPath,
    templatePath,
    zcashConfig,
  )
import Aftok.Types (Email (..), _Email)
import Control.Lens ((^.))
import Control.Monad.Trans.Except (except, withExceptT)
import Data.List (lookup)
import Data.Pool (defaultPoolConfig, newPool, withResource)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import qualified Database.PostgreSQL.Simple as PG
import Database.Schema.Migrations (ensureBootstrappedBackend, migrationsToApply, missingMigrations)
import Database.Schema.Migrations.Backend (Backend, applyMigration, commitBackend)
import qualified Database.Schema.Migrations.Backend.PostgreSQL as PGM
import Database.Schema.Migrations.Filesystem (FilesystemStoreSettings (..), filesystemStore)
import Database.Schema.Migrations.Store (MapValidationError, StoreData, loadMigrations, storeLookup)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import qualified Filesystem.Path.CurrentOS as P
import Lrzhs (isValidShieldedAddress)
import Network.Mail.Mime (Mail, plainPart)
import qualified Network.Mail.Mime as Mime
import qualified Network.Mail.SMTP as SMTP
import Network.Wai (requestHeaders)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (..),
    cors,
    corsExposedHeaders,
    corsIgnoreFailures,
    corsMaxAge,
    corsMethods,
    corsOrigins,
    corsRequestHeaders,
    corsRequireOrigin,
    corsVaryOrigin,
    simpleHeaders,
    simpleMethods,
  )
import Network.Wai.Middleware.RequestLogger
  ( Destination (..),
    IPAddrSource (..),
    OutputFormat (..),
    defaultRequestLoggerSettings,
    destination,
    mkRequestLogger,
    outputFormat,
  )
import Options.Applicative
  ( Parser,
    execParser,
    header,
    help,
    helper,
    info,
    long,
    short,
    strOption,
  )
import Servant.Auth.Server (generateKey)
import Text.StringTemplate
  ( directoryGroup,
    getStringTemplate,
    render,
    setAttribute,
  )

data CmdArgs = CmdArgs {cfgFile :: String}

args :: Parser CmdArgs
args = CmdArgs <$> strOption (long "conf" <> short 'c' <> help "Configuration file")

data MigrationError
  = MigrationNotFound Text
  | MigrationInvalid [MapValidationError]
  deriving (Show)

runMigrations :: FilePath -> PG.Connection -> ExceptT MigrationError IO ()
runMigrations path conn = do
  let store = filesystemStore $ FSStore {storePath = path}
      backend = PGM.backend conn

  storeData <- withExceptT MigrationInvalid . ExceptT $ loadMigrations store
  lift $ PG.begin conn
  lift $ ensureBootstrappedBackend backend
  lift $ commitBackend backend
  lift $ PG.begin conn
  migrationNames <- lift $ missingMigrations backend storeData
  traverse_ (applyOneMigration storeData backend) migrationNames
  lift $ commitBackend backend
  where
    applyOneMigration :: StoreData -> Backend -> Text -> ExceptT MigrationError IO ()
    applyOneMigration storeData backend migrationName = do
      m <- except . maybe (Left $ MigrationNotFound migrationName) Right $ storeLookup storeData migrationName
      toApply <- lift $ migrationsToApply storeData backend m
      lift $ traverse_ (applyMigration backend) toApply

main :: IO ()
main = do
  opts <- execParser $ info (args <**> helper) (header "The Aftok collaboration server")
  cfg <- loadServerConfig . decodeString $ cfgFile opts

  -- Create database connection pool
  let connStr = cfg ^. dbConfig . dbConnStr
  pool <- newPool $ defaultPoolConfig (connectPostgreSQL connStr) close 60 10

  -- Run database migrations before starting the server
  putStrLn $ "Running database migrations from " <> (cfg ^. migrationsPath)
  migrationResult <- withResource pool (runExceptT . runMigrations (cfg ^. migrationsPath))
  case migrationResult of
    Left err -> do
      putStrLn $ "Database migration failed: " <> show err
      exitFailure
    Right () ->
      putStrLn "Database migrations completed successfully."

  -- Create payments configuration
  paymentsConfig <- C.toPaymentsConfig @QDBM (cfg ^. billingConfig)

  -- Generate JWT key (in production, this should be loaded from a file)
  jwk <- generateKey

  -- Set up configuration
  let nmode = cfg ^. billingConfig . C.bitcoinConfig . C.networkMode
      btcCfg = cfg ^. billingConfig . C.bitcoinConfig
      rops = registerOps cfg
      captchaCfg = cfg ^. recaptchaSecret
      pwResetOps = passwordResetOps cfg
      staticDir = encodeString $ cfg ^. staticAssetPath

  -- Create application environment
  let env = mkAppEnv nmode pool cfg jwk

  -- Create request logger (Apache format - doesn't log request bodies)
  requestLogger <-
    mkRequestLogger
      defaultRequestLoggerSettings
        { outputFormat = Apache FromSocket,
          destination = Handle stdout
        }

  -- Create WAI application with CORS that supports credentials
  -- corsOrigins = Just (origins, allowCredentials) - the Bool enables Access-Control-Allow-Credentials
  -- Only allow origins from the configured allowlist
  let allowedOrigins = cfg ^. corsAllowedOrigins
      corsPolicy = cors $ \req ->
        let maybeOrigin = lookup "Origin" (requestHeaders req)
            originText = decodeUtf8 <$> maybeOrigin
            isAllowed = maybe False (`elem` allowedOrigins) originText
         in Just
              CorsResourcePolicy
                { corsOrigins =
                    if isAllowed
                      then fmap (\origin -> ([origin], True)) maybeOrigin
                      else Nothing, -- Deny credentials for unknown origins
                  corsMethods = simpleMethods <> ["PUT", "DELETE", "PATCH"],
                  corsRequestHeaders = simpleHeaders <> ["Content-Type", "X-XSRF-TOKEN", "Authorization"],
                  corsExposedHeaders = Just ["Set-Cookie"],
                  corsMaxAge = Just 86400, -- Cache preflight for 24 hours
                  corsVaryOrigin = True, -- Important: vary response by Origin header
                  corsRequireOrigin = False,
                  corsIgnoreFailures = False
                }
      app = requestLogger $ corsPolicy $ aftokApp env btcCfg paymentsConfig rops captchaCfg pwResetOps staticDir

  -- Run server
  let serverPort = cfg ^. port
  putStrLn $ "Starting Aftok server on port " <> show serverPort
  run serverPort app

-- | Operations needed for user registration
registerOps :: ServerConfig -> RegisterOps IO
registerOps cfg =
  RegisterOps
    { validateZAddr = \zaddr ->
        isValidShieldedAddress (zcashNetwork $ cfg ^. zcashConfig) zaddr <&> \valid ->
          if valid
            then Right (Zcash.Address zaddr)
            else Left AddressInvalid,
      sendConfirmationEmail = const $ pure ()
    }

-- | Operations needed for password reset
passwordResetOps :: ServerConfig -> PasswordResetOps IO
passwordResetOps cfg =
  PasswordResetOps
    { sendPasswordResetEmail = \email uname resetUrl expiryHours ->
        sendPasswordResetEmailImpl cfg email uname resetUrl expiryHours,
      generateResetUrl = \token ->
        let host = decodeUtf8 $ cfg ^. hostname
            scheme = if cfg ^. secureCookies then "https://" else "http://"
         in scheme <> host <> "/app/reset-confirm/" <> token
    }

-- | Send password reset email implementation
sendPasswordResetEmailImpl ::
  ServerConfig ->
  Email ->
  Text ->
  Text ->
  Text ->
  IO ()
sendPasswordResetEmailImpl cfg toEmail uname resetUrl expiryHours =
  let SmtpConfig {..} = cfg ^. smtpConfig
      -- Only allow unauthenticated SMTP in development mode (secureCookies = false)
      -- and when credentials are empty
      isDevelopment = not (cfg ^. secureCookies)
      useUnauthenticated = isDevelopment && (null _smtpUser || null _smtpPass)
      sendEmail mail =
        if useUnauthenticated
          then case _smtpPort of
            Nothing -> SMTP.sendMail _smtpHost mail
            Just smtpPort -> SMTP.sendMail' _smtpHost smtpPort mail
          else case _smtpPort of
            Nothing -> SMTP.sendMailWithLogin _smtpHost _smtpUser _smtpPass mail
            Just smtpPort -> SMTP.sendMailWithLogin' _smtpHost smtpPort _smtpUser _smtpPass mail
   in buildPasswordResetEmail (cfg ^. templatePath) toEmail uname resetUrl expiryHours
        >>= sendEmail

-- | Build password reset email
buildPasswordResetEmail ::
  P.FilePath ->
  Email ->
  Text ->
  Text ->
  Text ->
  IO Mail
buildPasswordResetEmail tpath toEmail uname resetUrl expiryHours = do
  templates <- directoryGroup $ encodeString tpath
  case getStringTemplate "password_reset_email" templates of
    Nothing -> fail "Could not find template for password reset email"
    Just template ->
      let setAttrs =
            setAttribute "username" uname
              . setAttribute "reset_url" resetUrl
              . setAttribute "expiry_hours" expiryHours
          fromAddr = Mime.Address Nothing "noreply@aftok.com"
          toAddr = Mime.Address Nothing (toEmail ^. _Email)
          subject = "Password Reset Request - Aftok"
          body = plainPart . render $ setAttrs template
       in pure $ SMTP.simpleMail fromAddr [toAddr] [] [] subject [body]
