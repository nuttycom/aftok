{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE TemplateHaskell #-}

module Main where

import ClassyPrelude 

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Aeson as A
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Data.Map
import Database.PostgreSQL.Simple

import Quixotic
import Quixotic.Database
import Quixotic.Database.PostgreSQL
import Quixotic.Json
import Quixotic.TimeLog
import Quixotic.Users

import Snap.Core
import Snap.Http.Server
import qualified Snap.Http.Server.Config as SC
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import qualified Snap.Snaplet.Auth as AU
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession

data QConfig = QConfig
  { hostname :: ByteString
  , port :: Int
  , authSiteKey :: FilePath
  , cookieTimeout :: Maybe Int
  -- , sslCert :: FilePath
  -- , sslKey :: FilePath
  -- , dbName :: String
  } 

type PQDB = QDB (ReaderT Connection IO)

data App = App 
  { _qdb  :: Snaplet PQDB
  , _sess :: Snaplet SessionManager
  , _db   :: Snaplet Postgres
  , _auth :: Snaplet (AU.AuthManager App)
  }
makeLenses ''App

instance HasPostgres (Handler b App) where
    getPostgresState = with db get
    setLocalPostgresState s = local (set (db . snapletValue) s)

main :: IO ()
main = do
  cfg <- loadQConfig "quixotic.cfg"
  sconf <- snapConfig cfg
  --simpleHttpServe sconf $ runReaderT (site sqliteQDB) db
  serveSnaplet sconf $ appInit cfg

appInit :: QConfig -> SnapletInit App App
appInit QConfig{..} = makeSnaplet "quixotic" "Quixotic Time Tracker" Nothing $ do
  qdbs  <- nestSnaplet "qdb" qdb qdbpgSnapletInit
  sesss <- nestSnaplet "sessions" sess $ 
           initCookieSessionManager (fpToString authSiteKey) "quookie" cookieTimeout
  pgs   <- nestSnaplet "db" db pgsInit
  auths <- nestSnaplet "auth" auth $ initPostgresAuth sess pgs
  addRoutes [ ("logStart/:btcAddr", logWorkHandler StartWork)
            , ("logEnd/:btcAddr",   logWorkHandler StopWork)
--            , ("loggedIntervals/:btcAddr", loggedIntervalsHandler qdb)
--            , ("payouts", payoutsHandler qdb)
            ] 
  return $ App qdbs sesss pgs auths

loadQConfig :: FilePath -> IO QConfig
loadQConfig cfgFile = do 
  cfg <- C.load [C.Required (fpToString cfgFile)]
  parseQConfig cfg

parseQConfig :: CT.Config -> IO QConfig
parseQConfig cfg = 
  QConfig <$> C.lookupDefault "localhost" cfg "hostname"
          <*> C.lookupDefault 8000 cfg "port" 
          <*> (fmap fpFromText $ C.require cfg "siteKey")
          <*> C.lookup cfg "cookieTimeout" 
          -- <*> (fmap fpFromText $ C.require cfg "sslCert")
          -- <*> (fmap fpFromText $ C.require cfg "sslKey")
          -- <*> C.require cfg "db" 

baseSnapConfig :: MonadSnap m => QConfig -> SC.Config m a -> SC.Config m a
baseSnapConfig cfg = 
  SC.setHostname (hostname cfg) . 
  SC.setPort (port cfg) 
  --SC.setSSLPort (port cfg) .
  --SC.setSSLCert (fpToString $ sslCert cfg) .
  --SC.setSSLKey (fpToString $ sslKey cfg)

snapConfig :: QConfig -> IO (SC.Config Snap a)
snapConfig cfg = SC.commandLineConfig $ baseSnapConfig cfg emptyConfig

-- | FIXME, make configurable
depf :: DepF
depf = linearDepreciation (Months 6) (Months 60) 

qdbpgSnapletInit :: SnapletInit a PQDB
qdbpgSnapletInit = makeSnaplet "qdbpg" "QDB on Postgresql" Nothing $ do
  return postgresQDB

requireLogin :: Handler App App a -> Handler App App a
requireLogin = AU.requireUser auth (redirect "/login")

requireUserId :: (UserId -> Handler App App a) -> Handler App App a 
requireUserId hf = AU.requireUser auth (redirect "/login") $ do
  QDB{..} <- with qdb get
  authedUser <- with auth AU.currentUser
  qdbUser <- case UserName . AU.unUid <$> (AU.userId =<< authedUser) of 
    Nothing -> snapError 403 "User is authenticated, but session lacks user identifier"
    Just n  -> liftPG . runReaderT $ findUserByUserName n
  case qdbUser of
    Nothing -> snapError 403 "Unable to retrieve user record for authenticated user" 
    Just u -> hf (u ^. userId)

logWorkHandler :: EventType -> Handler App App ()
logWorkHandler evType = requireUserId $ \uid -> do 
  QDB{..} <- with qdb get
  addrBytes <- getParam "btcAddr"
  timestamp <- liftIO getCurrentTime
  let workEvent = WorkEvent evType timestamp
      storeEv addr = runReaderT . recordEvent uid $ LogEntry addr workEvent
  case fmap decodeUtf8 addrBytes >>= parseBtcAddr of
    Nothing -> snapError 400 $ "Unable to parse bitcoin address from " <> (tshow addrBytes)
    Just addr -> liftPG $ storeEv addr

loggedIntervalsHandler :: Handler App App ()
loggedIntervalsHandler = requireLogin $ do
  QDB{..} <- with qdb get
  widx <- liftPG $ runReaderT readWorkIndex
  modifyResponse $ addHeader "content-type" "application/json"
  writeLBS . A.encode $ mapKeys (^. address) widx

payoutsHandler :: Handler App App ()
payoutsHandler = requireLogin $ do 
  QDB{..} <- with qdb get
  ptime <- liftIO $ getCurrentTime
  widx <- liftPG $ runReaderT readWorkIndex
  modifyResponse $ addHeader "content-type" "application/json"
  writeLBS . A.encode . PayoutsResponse $ payouts depf ptime widx

snapError :: MonadSnap m => Int -> Text -> m a
snapError c t = do
  modifyResponse $ setResponseStatus c $ encodeUtf8 t
  writeText $ ((tshow c) <> " - " <> t)
  r <- getResponse
  finishWith r
