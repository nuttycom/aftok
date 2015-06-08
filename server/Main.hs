module Main where

import ClassyPrelude 

import qualified Data.Aeson as A

import Aftok.TimeLog
import Aftok.Json

import Aftok.QConfig
import Aftok.Snaplet
import Aftok.Snaplet.Auth
import Aftok.Snaplet.Users
import Aftok.Snaplet.WorkLog
import Aftok.Snaplet.Projects

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.Session.Backends.CookieSession

main :: IO ()
main = do
  cfg <- loadQConfig "conf/aftok.cfg"
  sconf <- snapConfig cfg
  serveSnaplet sconf $ appInit cfg

appInit :: QConfig -> SnapletInit App App
appInit QConfig{..} = makeSnaplet "aftok" "Aftok Time Tracker" Nothing $ do
  qms   <- nestSnaplet "qmodules" qm qdbpgSnapletInit
  sesss <- nestSnaplet "sessions" sess $ 
           initCookieSessionManager authSiteKey "quookie" cookieTimeout
  pgs   <- nestSnaplet "db" db $ pgsInit' pgsConfig
  auths <- nestSnaplet "auth" auth $ initPostgresAuth sess pgs

  let loginRoute         = requireLogin >> redirect "/home"
      registerRoute      = void $ method POST registerHandler

      logEventRoute f    = serveJSON eventIdJSON . method POST $ logWorkHandler f
      logEntriesRoute    = serveJSON (fmap logEntryJSON) $ method GET logEntriesHandler
      logIntervalsRoute  = serveJSON workIndexJSON   $ method GET loggedIntervalsHandler
      amendEventRoute    = serveJSON amendmentIdJSON $ method PUT amendEventHandler

      projectCreateRoute = void $ method POST projectCreateHandler
      projectRoute       = serveJSON projectJSON $ method GET projectGetHandler
      listProjectsRoute  = serveJSON (fmap qdbProjectJSON) $ method GET projectListHandler

      payoutsRoute       = serveJSON payoutsJSON $ method GET payoutsHandler

  addRoutes [ ("login", loginRoute)   
            , ("register", registerRoute)
            , ("events/:eventId/amend", amendEventRoute)
            , ("projects/:projectId/logStart/:btcAddr", logEventRoute StartWork)
            , ("projects/:projectId/logEnd/:btcAddr",   logEventRoute StopWork) 
            , ("projects/:projectId/logEntries",        logEntriesRoute)
            , ("projects/:projectId/intervals",         logIntervalsRoute)
            , ("projects", projectCreateRoute)
            , ("projects", listProjectsRoute)
            , ("projects/:projectId", projectRoute)
            , ("projects/:projectId/payouts", payoutsRoute)
            ] 
  return $ App qms sesss pgs auths

serveJSON :: (MonadSnap m, A.ToJSON a) => (b -> a) -> m b -> m ()
serveJSON f ma = do
  modifyResponse $ addHeader "content-type" "application/json"
  writeLBS =<< (A.encode . f <$> ma)
