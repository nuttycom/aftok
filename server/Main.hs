module Main where

import ClassyPrelude 

import qualified Data.Aeson as A
import System.Environment

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
  cfgPath <- try $ getEnv "AFTOK_CFG" :: IO (Either IOError String)
  cfg <- loadQConfig $ either (const "conf/aftok.cfg") id cfgPath
  sconf <- snapConfig cfg
  serveSnaplet sconf $ appInit cfg

appInit :: QConfig -> SnapletInit App App
appInit cfg = makeSnaplet "aftok" "Aftok Time Tracker" Nothing $ do
  sesss <- nestSnaplet "sessions" sess $ 
           initCookieSessionManager (authSiteKey cfg) "quookie" (cookieTimeout cfg)
  pgs   <- nestSnaplet "db" db $ pgsInit' (pgsConfig cfg)
  auths <- nestSnaplet "auth" auth $ initPostgresAuth sess pgs

  let loginRoute         = requireLogin >> redirect "/home"
      registerRoute      = void $ method POST registerHandler
      acceptInviteRoute  = void $ method POST acceptInvitationHandler
      projectCreateRoute = void $ method POST projectCreateHandler
      listProjectsRoute  = serveJSON (fmap qdbProjectJSON) $ method GET projectListHandler

      projectRoute       = serveJSON projectJSON $ method GET projectGetHandler
      logEventRoute f    = serveJSON eventIdJSON . method POST $ logWorkHandler f
      logEntriesRoute    = serveJSON (fmap logEntryJSON) $ method GET logEntriesHandler
      logIntervalsRoute  = serveJSON workIndexJSON   $ method GET loggedIntervalsHandler
      payoutsRoute       = serveJSON payoutsJSON $ method GET payoutsHandler
      inviteRoute        = void . method POST $ projectInviteHandler cfg

      amendEventRoute    = serveJSON amendmentIdJSON $ method PUT amendEventHandler

  addRoutes [ ("login", loginRoute)   
            , ("register", registerRoute)
            , ("accept_invitation", acceptInviteRoute)
            , ("projects", projectCreateRoute)
            , ("projects", listProjectsRoute)
            , ("projects/:projectId",                   projectRoute)
            , ("projects/:projectId/logStart/:btcAddr", logEventRoute StartWork)
            , ("projects/:projectId/logEnd/:btcAddr",   logEventRoute StopWork) 
            , ("projects/:projectId/logEntries",        logEntriesRoute)
            , ("projects/:projectId/intervals",         logIntervalsRoute)
            , ("projects/:projectId/payouts",           payoutsRoute)
            , ("projects/:projectId/invite",            inviteRoute)
            , ("events/:eventId/amend", amendEventRoute)
            ] 
  return $ App sesss pgs auths

serveJSON :: (MonadSnap m, A.ToJSON a) => (b -> a) -> m b -> m ()
serveJSON f ma = do
  modifyResponse $ addHeader "content-type" "application/json"
  writeLBS =<< (A.encode . f <$> ma)
