{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Server
  ( runServer,
    readServerConf,
  )
where

import Aftok.Currency.Bitcoin (NetworkMode (..))
import Aftok.Database (DBError (..), KeyedLogEntry (..))
import Aftok.Database.Events (EventCaps, createEvent, hoistEventCaps)
import Aftok.Database.PostgreSQL.Events (pgEventCaps)
import Aftok.Database.PostgreSQL.Users (pgUserCaps)
import Aftok.Database.Users (UserCaps, findUserByName, hoistUserCaps)
import Aftok.Http.Json ()
import Aftok.Server.Auth
  ( AuthConfig (..),
    Login (..),
    RegisterRequest,
    UserJWT (..),
    userId,
  )
import Aftok.Server.Orphans ()
import Aftok.Time (Clock, getCurrentTime, hoistClock, systemClock)
import Aftok.TimeLog (LogEntry (..), LogEvent (..))
import Aftok.Types (CreditTo (..), ProjectId, UserId, passwordHash)
import Control.Error.Util (exceptT)
import Control.Lens ((^.))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.Except
  ( except,
    withExceptT,
  )
import Control.Monad.Trans.Reader
  ( mapReaderT,
  )
import Crypto.JOSE.JWK (JWK)
import Crypto.KDF.BCrypt (validatePassword)
import Crypto.Random.Types (MonadRandom)
import qualified Data.Configurator as CF
import Data.Configurator.Types (Config)
import Data.Pool
  ( createPool,
    withResource,
  )
import qualified Data.Text.Encoding as T
import qualified Data.Thyme.Clock as C
import qualified Database.PostgreSQL.Simple as PG
import Database.Schema.Migrations (ensureBootstrappedBackend, migrationsToApply, missingMigrations)
import Database.Schema.Migrations.Backend (Backend, applyMigration, commitBackend)
import qualified Database.Schema.Migrations.Backend.PostgreSQL as PGM
import Database.Schema.Migrations.Filesystem (FilesystemStoreSettings (..), filesystemStore)
import Database.Schema.Migrations.Store (MapValidationError, StoreData, loadMigrations, storeLookup)
import Network.Wai.Handler.Warp
import Servant
  ( Application,
    Capture,
    Context (EmptyContext, (:.)),
    Handler (..),
    HasServer,
    Header,
    Headers,
    JSON,
    NoContent (..),
    Post,
    ReqBody,
    ServerError,
    ServerT,
    StdMethod (POST),
    Verb,
    err401,
    err403,
    err404,
    err500,
    errBody,
    hoistServerWithContext,
    serveWithContext,
    throwError,
    (:<|>) (..),
    (:>),
  )
import Servant.Auth.Server
  ( Auth,
    AuthResult (Authenticated),
    Cookie,
    CookieSettings,
    JWT,
    JWTSettings,
    SetCookie,
    acceptLogin,
    defaultCookieSettings,
    defaultJWTSettings,
    generateKey,
  )
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn)

type LoginAPI =
  "register"
    :> ReqBody '[JSON] RegisterRequest
    :> Post '[JSON] UserId
    :<|> "login"
      :> ReqBody '[JSON] Login
      :> Verb
           'POST
           204
           '[JSON]
           ( Headers
               '[ Header "Set-Cookie" SetCookie,
                  Header "Set-Cookie" SetCookie
                ]
               NoContent
           )

type EventsAPI =
  "user"
    :> "projects"
    :> Capture "project_id" ProjectId
    :> "logStart"
    :> Post '[JSON] (ProjectId, UserId, KeyedLogEntry)

type API auths =
  LoginAPI
    :<|> (Auth auths UserJWT :> EventsAPI)

data ServerConf = ServerConf
  { postgresUri :: Text,
    port :: Maybe Int,
    authConfig :: AuthConfig,
    migrationsPath :: FilePath,
    networkMode :: NetworkMode
  }

readServerConf :: Config -> IO ServerConf
readServerConf c =
  ServerConf
    <$> CF.require c "db.uri"
    <*> CF.lookup c "server.port"
    <*> (AuthConfig <$> CF.require c "auth.hash_cost")
    <*> CF.require c "migrations.path"
    <*> CF.require c "network.mode"

readServerKey :: ServerConf -> IO JWK
readServerKey _ =
  -- FIXME, just generating a new key for now. We'll want to persist
  -- the key so that it survives across server restarts, otherwise
  -- everyone will get logged out whenever we start up.
  generateKey

hoistServerWithAuth ::
  HasServer api '[CookieSettings, JWTSettings] =>
  Proxy api ->
  (forall x. m x -> n x) ->
  ServerT api m ->
  ServerT api n
hoistServerWithAuth api =
  hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings])

data MigrationError
  = NotFound Text
  | Invalid [MapValidationError]
  deriving (Show)

runMigrations :: FilePath -> PG.Connection -> ExceptT MigrationError IO ()
runMigrations path conn = do
  let store = filesystemStore $ FSStore {storePath = path}
      backend = PGM.backend conn

  storeData <- withExceptT Invalid . ExceptT $ loadMigrations store
  lift $ PG.begin conn
  lift $ ensureBootstrappedBackend backend
  lift $ commitBackend backend
  lift $ PG.begin conn
  migrationNames <- lift $ missingMigrations backend storeData
  traverse_ (runMigration storeData backend) migrationNames
  lift $ commitBackend backend
  where
    runMigration :: StoreData -> Backend -> Text -> ExceptT MigrationError IO ()
    runMigration storeData backend migrationName = do
      m <- except . maybe (Left $ NotFound migrationName) Right $ storeLookup storeData migrationName
      toApply <- lift $ migrationsToApply storeData backend m
      lift $ traverse_ (applyMigration backend) toApply

runServer :: ServerConf -> IO ()
runServer conf = do
  pool <-
    createPool
      (PG.connectPostgreSQL $ encodeUtf8 (postgresUri conf))
      PG.close
      2 -- stripes
      60 -- unused connections are kept open for a minute
      10 -- max. 10 connections open per stripe
  jwk <- readServerKey conf

  let api = Proxy :: Proxy (API '[Cookie, JWT])
      jwtSettings = defaultJWTSettings jwk
      cookieSettings = defaultCookieSettings
      ctxt = cookieSettings :. jwtSettings :. EmptyContext
      nmode = networkMode conf

      toServerError :: ExceptT DBError IO a -> ExceptT ServerError IO a
      toServerError = withExceptT $ \case
        OpForbidden _ _ -> err403 {errBody = "Operation forbidden"}
        SubjectNotFound -> err404 {errBody = "The subject of the requested operation could not be found"}
        EventStorageFailed -> err500 {errBody = "The event submitted could not be saved to the log"}

      pgServer =
        server
          (authConfig conf)
          (hoistClock liftIO systemClock)
          (hoistUserCaps (mapReaderT toServerError) pgUserCaps)
          (hoistEventCaps (mapReaderT toServerError) pgEventCaps)
          cookieSettings
          jwtSettings

      runEffects :: ReaderT (NetworkMode, PG.Connection) (ExceptT ServerError IO) a -> Handler a
      runEffects r = Handler (withResource pool $ \c -> runReaderT r (nmode, c))

      app :: Application
      app =
        serveWithContext api ctxt $ hoistServerWithAuth api runEffects pgServer

  let exitMigrationFailed = \err -> do
        hPutStrLn stderr $ "database migration failed: " <> show err
        exitWith (ExitFailure 1)

  exceptT exitMigrationFailed (const $ pure ()) . withResource pool $ runMigrations (migrationsPath conf)
  run (fromMaybe 8000 (port conf)) app

server ::
  (MonadRandom m, MonadIO m, MonadError ServerError m) =>
  AuthConfig ->
  Clock m ->
  UserCaps m ->
  EventCaps m ->
  CookieSettings ->
  JWTSettings ->
  ServerT (API auths) m
server auc clock uc ec cs jwts =
  loginServer auc uc cs jwts :<|> eventsServer clock ec

loginServer ::
  (MonadRandom m, MonadIO m, MonadError ServerError m) =>
  AuthConfig ->
  UserCaps m ->
  CookieSettings ->
  JWTSettings ->
  ServerT LoginAPI m
loginServer auc uc cs jwts =
  registerHandler auc uc :<|> loginHandler uc cs jwts

eventsServer ::
  MonadError ServerError m =>
  Clock m ->
  EventCaps m ->
  AuthResult UserJWT ->
  ServerT EventsAPI m
eventsServer clock ec (Authenticated jwt) =
  logEventHandler clock ec StartWork (jwt ^. userId)
eventsServer _ _ _ =
  (\_ -> throwError err401)

loginHandler ::
  (MonadIO m, MonadError ServerError m) =>
  UserCaps m ->
  CookieSettings ->
  JWTSettings ->
  Login ->
  m
    ( Headers
        '[ Header "Set-Cookie" SetCookie,
           Header
             "Set-Cookie"
             SetCookie
         ]
        NoContent
    )
loginHandler uc cs jwts (Login uname pwd) = do
  user <- findUserByName uc uname
  let pwdBytes = T.encodeUtf8 pwd
  case user of
    Just (uid, u) | validatePassword pwdBytes (u ^. passwordHash) -> do
      mApplyCookies <- liftIO $ acceptLogin cs jwts (UserJWT uid)
      case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> pure $ applyCookies NoContent
    _ -> throwError err401

logEventHandler ::
  Monad m =>
  Clock m ->
  EventCaps m ->
  (C.UTCTime -> LogEvent) ->
  UserId ->
  ProjectId ->
  m (ProjectId, UserId, KeyedLogEntry)
logEventHandler clock ec ev uid pid = do
  now <- getCurrentTime clock
  let entry =
        LogEntry
          { _creditTo = CreditToUser uid,
            _event = ev now,
            _eventMeta = Nothing
          }
  eventId <- createEvent ec pid uid entry
  pure (pid, uid, KeyedLogEntry {_workId = eventId, _logEntry = entry})
