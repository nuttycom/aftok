module Main where

import           ClassyPrelude

import qualified Data.Aeson                                  as A
import           Data.ProtocolBuffers                        (encodeMessage)
import           Data.Serialize.Put                          (runPutLazy)
import           System.Environment

import           Aftok.Json
import           Aftok.TimeLog

import           Aftok.QConfig
import           Aftok.Snaplet
import           Aftok.Snaplet.Auctions
import           Aftok.Snaplet.Auth
import           Aftok.Snaplet.Payments
import           Aftok.Snaplet.Projects
import           Aftok.Snaplet.Users
import           Aftok.Snaplet.WorkLog

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession

main :: IO ()
main = do
  cfgPath <- try $ getEnv "AFTOK_CFG" :: IO (Either IOError String)
  cfg <- loadQConfig $ either (const "conf/aftok.cfg") id cfgPath
  sconf <- snapConfig cfg
  serveSnaplet sconf $ appInit cfg

appInit :: QConfig -> SnapletInit App App
appInit cfg = makeSnaplet "aftok" "Aftok Time Tracker" Nothing $ do
  sesss <- nestSnaplet "sessions" sess $
           initCookieSessionManager (authSiteKey cfg) "quookie" (Just "aftok.com") (cookieTimeout cfg)
  pgs   <- nestSnaplet "db" db $ pgsInit' (pgsConfig cfg)
  auths <- nestSnaplet "auth" auth $ initPostgresAuth sess pgs

  let loginRoute          = requireLogin >> redirect "/home"
      registerRoute       = void $ method POST registerHandler
      acceptInviteRoute   = void $ method POST acceptInvitationHandler

      projectCreateRoute  = serveJSON projectIdJSON $ method POST projectCreateHandler
      listProjectsRoute   = serveJSON (fmap qdbProjectJSON) $ method GET projectListHandler

      projectRoute        = serveJSON projectJSON $ method GET projectGetHandler
      logWorkRoute f      = serveJSON eventIdJSON $ method POST (logWorkHandler f)
      logWorkBTCRoute f   = serveJSON eventIdJSON $ method POST (logWorkBTCHandler f)
      logEntriesRoute     = serveJSON (fmap logEntryJSON) $ method GET logEntriesHandler
      logIntervalsRoute   = serveJSON workIndexJSON $ method GET loggedIntervalsHandler
      payoutsRoute        = serveJSON payoutsJSON $ method GET payoutsHandler
      inviteRoute         = void . method POST $ projectInviteHandler cfg

      auctionCreateRoute  = serveJSON auctionIdJSON $ method POST auctionCreateHandler
      auctionRoute        = serveJSON auctionJSON $ method GET auctionGetHandler
      auctionBidRoute     = serveJSON bidIdJSON $ method POST auctionBidHandler

      payableRequestsRoute = serveJSON billDetailsJSON $ method GET listPayableRequestsHandler
      paymentRoute         = (writeLBS . runPutLazy . encodeMessage =<< method GET getPaymentRequestHandler)
                             <|> (void $ method POST paymentResponseHandler)

      amendEventRoute     = serveJSON amendmentIdJSON $ method PUT amendEventHandler

  addRoutes [ ("login",             loginRoute)
            , ("register",          registerRoute)
            , ("accept_invitation", acceptInviteRoute)

            , ("projects/:projectId/logStart/:btcAddr", logWorkBTCRoute StartWork)
            , ("projects/:projectId/logEnd/:btcAddr",   logWorkBTCRoute StopWork)
            , ("projects/:projectId/logStart",          logWorkRoute StartWork)
            , ("projects/:projectId/logEnd",            logWorkRoute StopWork)
            , ("projects/:projectId/auctions",          auctionCreateRoute)
            , ("projects/:projectId/logEntries",        logEntriesRoute)
            , ("projects/:projectId/intervals",         logIntervalsRoute)
            , ("projects/:projectId/payouts",           payoutsRoute)
            , ("projects/:projectId/invite",            inviteRoute)
            , ("projects/:projectId",                   projectRoute)
            , ("projects", projectCreateRoute)
            , ("projects", listProjectsRoute)

            , ("auctions/:auctionId",          auctionRoute)
            , ("auctions/:auctionId/bid",      auctionBidRoute)

            , ("subscriptions/:subscriptionId/payment_requests", payableRequestsRoute)
            , ("pay/:paymentRequestKey", paymentRoute)

            , ("events/:eventId/amend", amendEventRoute)
            ]
  return $ App sesss pgs auths

serveJSON :: (MonadSnap m, A.ToJSON a) => (b -> a) -> m b -> m ()
serveJSON f ma = do
  modifyResponse $ addHeader "content-type" "application/json"
  writeLBS =<< (A.encode . f <$> ma)
