{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE TemplateHaskell #-}

module Main where

import ClassyPrelude 

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT

import Quixotic.TimeLog

import Quixotic.Snaplet
import Quixotic.Snaplet.Auth
import Quixotic.Snaplet.Users
import Quixotic.Snaplet.WorkLog
import Quixotic.Snaplet.Projects

import Snap.Core
import Snap.Http.Server
import qualified Snap.Http.Server.Config as SC
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
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

main :: IO ()
main = do
  cfg <- loadQConfig "quixotic.cfg"
  sconf <- snapConfig cfg
  --simpleHttpServe sconf $ runReaderT (site sqliteQDB) db
  serveSnaplet sconf $ appInit cfg

appInit :: QConfig -> SnapletInit App App
appInit QConfig{..} = makeSnaplet "quixotic" "Quixotic Time Tracker" Nothing $ do
  qms  <- nestSnaplet "qmodules" qm qdbpgSnapletInit
  sesss <- nestSnaplet "sessions" sess $ 
           initCookieSessionManager (fpToString authSiteKey) "quookie" cookieTimeout
  pgs   <- nestSnaplet "db" db pgsInit
  auths <- nestSnaplet "auth" auth $ initPostgresAuth sess pgs
  addRoutes [ ("login", requireLogin >> (redirect "/home")) 
            , ("register", method POST registerHandler)
            , ("projects", projectsHandler)
            , ("logStart/:projectId/:btcAddr", logWorkHandler StartWork)
            , ("logEnd/:projectId/:btcAddr",   logWorkHandler StopWork)
            , ("loggedIntervals/:projectId/:btcAddr", loggedIntervalsHandler)
            , ("projects/:projectId", method GET ok)
            , ("payouts/:projectId", method GET payoutsHandler)
            ] 
  return $ App qms sesss pgs auths

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
