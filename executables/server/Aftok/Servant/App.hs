{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Aftok.Servant.App
  ( AppEnv (..),
    AppM (..),
    envNetworkMode,
    envDbPool,
    envConfig,
    envCookieSettings,
    envJWTSettings,
    envHttpManager,
    appToHandler,
    runDB,
    dbErrorToServerError,
  )
where

import Aftok.Currency.Bitcoin (NetworkMode)
import Aftok.Database (DBError (..))
import Aftok.Database.PostgreSQL (QDBM, runQDBM)
import Aftok.ServerConfig (ServerConfig)
import Control.Lens (makeLenses, (^.))
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (encode)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import qualified Network.HTTP.Client as HTTP
import Servant (Handler, ServerError, err403, err404, err409, err500, errBody)
import Servant.Auth.Server (CookieSettings, JWTSettings)

-- | Application environment (replaces Snap's App snaplet)
data AppEnv = AppEnv
  { _envNetworkMode :: !NetworkMode,
    _envDbPool :: !(Pool Connection),
    _envConfig :: !ServerConfig,
    _envCookieSettings :: !CookieSettings,
    _envJWTSettings :: !JWTSettings,
    _envHttpManager :: !HTTP.Manager
  }

makeLenses ''AppEnv

-- | Application monad stack
newtype AppM a = AppM {unAppM :: ReaderT AppEnv (ExceptT ServerError IO) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError ServerError,
      MonadReader AppEnv
    )

-- | Natural transformation from AppM to Servant's Handler
appToHandler :: AppEnv -> AppM a -> Handler a
appToHandler env (AppM action) = do
  result <- liftIO $ runExceptT $ runReaderT action env
  case result of
    Left err -> throwError err
    Right a -> pure a

-- | Run a database operation within AppM
runDB :: QDBM a -> AppM a
runDB qdbm = do
  env <- ask
  let nmode = env ^. envNetworkMode
      pool = env ^. envDbPool
  result <- liftIO $ withResource pool $ \conn ->
    runExceptT $ runQDBM nmode conn qdbm
  case result of
    Left dbErr -> throwError $ dbErrorToServerError dbErr
    Right a -> pure a

-- | Convert a database error to a Servant ServerError
dbErrorToServerError :: DBError -> ServerError
dbErrorToServerError = \case
  SubjectNotFound ->
    err404 {errBody = encode ("Resource not found" :: Text)}
  OpForbidden _ reason ->
    err403 {errBody = encode (show reason :: Text)}
  EventStorageFailed ->
    err500 {errBody = encode ("Failed to store event" :: Text)}
  DuplicateRecord msg ->
    err409 {errBody = encode (msg :: String)}
