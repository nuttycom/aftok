{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Aftok.Config as C
import Aftok.Currency.Zcash (zcashNetwork)
import qualified Aftok.Currency.Zcash as Zcash
import Aftok.Database.PostgreSQL (QDBM)
import Aftok.ServerConfig
  ( ServerConfig,
    billingConfig,
    dbConfig,
    dbConnStr,
    loadServerConfig,
    port,
    recaptchaSecret,
    staticAssetPath,
    zcashConfig,
  )
import Aftok.Servant.Server (aftokApp, mkAppEnv)
import Aftok.Servant.Users
  ( AddressInvalid (..),
    RegisterOps (..),
  )
import Control.Lens ((^.))
import Data.Pool (defaultPoolConfig, newPool)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import Lrzhs (isValidSaplingAddress)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
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

data CmdArgs = CmdArgs {cfgFile :: String}

args :: Parser CmdArgs
args = CmdArgs <$> strOption (long "conf" <> short 'c' <> help "Configuration file")

main :: IO ()
main = do
  opts <- execParser $ info (args <**> helper) (header "The Aftok collaboration server")
  cfg <- loadServerConfig . decodeString $ cfgFile opts

  -- Create database connection pool
  let connStr = cfg ^. dbConfig . dbConnStr
  pool <- newPool $ defaultPoolConfig (connectPostgreSQL connStr) close 60 10

  -- Create payments configuration
  paymentsConfig <- C.toPaymentsConfig @QDBM (cfg ^. billingConfig)

  -- Generate JWT key (in production, this should be loaded from a file)
  jwk <- generateKey

  -- Set up configuration
  let nmode = cfg ^. billingConfig . C.bitcoinConfig . C.networkMode
      btcCfg = cfg ^. billingConfig . C.bitcoinConfig
      rops = registerOps cfg
      captchaCfg = cfg ^. recaptchaSecret
      staticDir = encodeString $ cfg ^. staticAssetPath

  -- Create application environment
  let env = mkAppEnv nmode pool cfg jwk

  -- Create WAI application
  let app = logStdoutDev $ simpleCors $ aftokApp env btcCfg paymentsConfig rops captchaCfg staticDir

  -- Run server
  let serverPort = cfg ^. port
  putStrLn $ "Starting Aftok server on port " <> show serverPort
  run serverPort app

-- | Operations needed for user registration
registerOps :: ServerConfig -> RegisterOps IO
registerOps cfg =
  RegisterOps
    { validateZAddr = \zaddr ->
        isValidSaplingAddress (zcashNetwork $ cfg ^. zcashConfig) zaddr <&> \valid ->
          if valid
            then Right (Zcash.Address zaddr)
            else Left AddressInvalid,
      sendConfirmationEmail = const $ pure ()
    }
