{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module Main where

import ClassyPrelude 

import Control.Lens
import Control.Lens.TH
import Control.Monad.Trans.Reader
import qualified Data.Aeson as A
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Data.Map
import Data.Pool
import Database.PostgreSQL.Simple

import Quixotic
import Quixotic.Database
import Quixotic.Database.PostgreSQL
import Quixotic.Json
import Quixotic.TimeLog

import Snap.Core
import Snap.Http.Server
import qualified Snap.Http.Server.Config as SC
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.PostgresqlSimple
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
  , _auth :: Snaplet (AuthManager App)
  }
makeLenses ''App

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

qdbpgSnapletInit :: SnapletInit a PQDB
qdbpgSnapletInit = makeSnaplet "qdbpg" "QDB on Postgresql" Nothing $ do
  return postgresQDB

logWorkHandler :: EventType -> Handler App App ()
logWorkHandler evType = do 
  QDB{..} <- with qdb mempty
  pg <- with db getPostgresState
  authedUser <- with auth currentUser
  qUid <- 
  addrBytes <- getParam "btcAddr"
  timestamp <- liftIO getCurrentTime
  let workEvent = WorkEvent evType timestamp
      btcAddr = fmap decodeUtf8 addrBytes >>= parseBtcAddr
      storeEv uid addr = runReaderT . recordEvent uid $ LogEntry addr workEvent
  maybe (snapError 400 "") (liftPG . storeEv) btcAddr

--loggedIntervalsHandler :: QDB IO a -> ReaderT a Snap ()
--loggedIntervalsHandler qdb = do
--  let QDB{..} = qdb
--  widx <- mapReaderT liftIO $ readWorkIndex
--  lift . modifyResponse $ addHeader "content-type" "application/json"
--  lift . writeLBS . A.encode $ mapKeys (^. address) widx
--
--payoutsHandler :: QDB IO a -> ReaderT a Snap ()
--payoutsHandler qdb = do 
--  let QDB{..} = qdb
--      dep = linearDepreciation (Months 6) (Months 60) 
--  ptime <- lift . liftIO $ getCurrentTime
--  widx <- mapReaderT liftIO $ readWorkIndex
--  lift . modifyResponse $ addHeader "content-type" "application/json"
--  lift . writeLBS . A.encode . PayoutsResponse $ payouts dep ptime widx

snapError :: Int -> Text -> Snap ()
snapError c t = do
  modifyResponse $ setResponseStatus c $ encodeUtf8 t
  writeText $ ((tshow c) <> " - " <> t)
  r <- getResponse
  finishWith r
