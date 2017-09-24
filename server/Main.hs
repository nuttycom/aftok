module Main where

import           ClassyPrelude hiding (FilePath)

import qualified Data.Aeson                                  as A
import Data.Either.Combinators (fromRight)
import           Data.ProtocolBuffers                        (encodeMessage)
import           Data.Serialize.Put                          (runPutLazy)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import           System.Environment

import           Aftok.Json
import           Aftok.TimeLog

import           Aftok.QConfig
import           Aftok.Snaplet
import           Aftok.Snaplet.Auctions
import           Aftok.Snaplet.Billing
import           Aftok.Snaplet.Auth
import           Aftok.Snaplet.Payments
import           Aftok.Snaplet.Projects
import           Aftok.Snaplet.Users
import           Aftok.Snaplet.WorkLog

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe (serveDirectory)

main :: IO ()
main = do
  cfgPath <- try $ getEnv "AFTOK_CFG" :: IO (Either IOError String)
  cfg <- loadQConfig . decodeString $ fromRight "conf/aftok.cfg" cfgPath
  sconf <- snapConfig cfg
  serveSnaplet sconf $ appInit cfg

appInit :: QConfig -> SnapletInit App App
appInit cfg = makeSnaplet "aftok" "Aftok Time Tracker" Nothing $ do
  sesss <- nestSnaplet "sessions" sess $
           initCookieSessionManager (encodeString $ authSiteKey cfg) "quookie" (Just "aftok.com") (cookieTimeout cfg)
  pgs   <- nestSnaplet "db" db $ pgsInit' (pgsConfig cfg)
  auths <- nestSnaplet "auth" auth $ initPostgresAuth sess pgs

  let loginRoute          = method GET requireLogin >> redirect "/home"
      xhrLoginRoute      =  void $ method POST requireLogin
      registerRoute       = void $ method POST registerHandler

      inviteRoute         = void $ method POST (projectInviteHandler cfg)
      acceptInviteRoute   = void $ method POST acceptInvitationHandler

      projectCreateRoute  = serveJSON projectIdJSON $ method POST projectCreateHandler
      projectListRoute    = serveJSON (fmap qdbProjectJSON) $ method GET projectListHandler

      projectRoute        = serveJSON projectJSON $ method GET projectGetHandler
      logEntriesRoute     = serveJSON (fmap logEntryJSON) $ method GET logEntriesHandler
      logIntervalsRoute   = serveJSON workIndexJSON $ method GET loggedIntervalsHandler

      payoutsRoute        = serveJSON payoutsJSON $ method GET payoutsHandler

      logWorkRoute f      = serveJSON eventIdJSON $ method POST (logWorkHandler f)
      logWorkBTCRoute f   = serveJSON eventIdJSON $ method POST (logWorkBTCHandler f)
      amendEventRoute     = serveJSON amendmentIdJSON $ method PUT amendEventHandler

      auctionCreateRoute  = serveJSON auctionIdJSON $ method POST auctionCreateHandler
      auctionRoute        = serveJSON auctionJSON   $ method GET auctionGetHandler
      auctionBidRoute     = serveJSON bidIdJSON     $ method POST auctionBidHandler 

      billableCreateRoute = serveJSON billableIdJSON $ method POST billableCreateHandler
      billableListRoute   = serveJSON (fmap qdbBillableJSON) $ method GET billableListHandler
      subscribeRoute      = serveJSON subscriptionIdJSON $ method POST subscribeHandler

      payableRequestsRoute = serveJSON billDetailsJSON $ method GET listPayableRequestsHandler
      getPaymentRequestRoute = writeLBS . runPutLazy . encodeMessage =<< method GET getPaymentRequestHandler
      submitPaymentRoute     = serveJSON paymentIdJSON $ method POST (paymentResponseHandler $ billingConfig cfg)

  addRoutes [ ("static", serveDirectory . encodeString $ staticAssetPath cfg)

            , ("login",             loginRoute)   
            , ("login",             xhrLoginRoute)   
            , ("register",          registerRoute)
            , ("accept_invitation", acceptInviteRoute)

            , ("projects/:projectId/logStart/:btcAddr", logWorkBTCRoute StartWork)
            , ("projects/:projectId/logEnd/:btcAddr",   logWorkBTCRoute StopWork)
            , ("projects/:projectId/logStart",          logWorkRoute StartWork)
            , ("projects/:projectId/logEnd",            logWorkRoute StopWork)
            , ("projects/:projectId/logEntries",        logEntriesRoute)
            , ("projects/:projectId/intervals",         logIntervalsRoute)
            , ("projects/:projectId/auctions",          auctionCreateRoute) -- <|> auctionListRoute
            , ("projects/:projectId/billables",         billableCreateRoute <|> billableListRoute)
            , ("projects/:projectId/payouts",           payoutsRoute)
            , ("projects/:projectId/invite",            inviteRoute)
            , ("projects/:projectId",                   projectRoute)
            , ("projects",                              projectCreateRoute <|> projectListRoute)

            , ("auctions/:auctionId",          auctionRoute)
            , ("auctions/:auctionId/bid",      auctionBidRoute)

            , ("subscribe/:billableId",        subscribeRoute)
            , ("subscriptions/:subscriptionId/payment_requests", payableRequestsRoute)
            , ("pay/:paymentRequestKey", getPaymentRequestRoute <|> submitPaymentRoute)

            , ("events/:eventId/amend", amendEventRoute)
            ]
  return $ App sesss pgs auths

serveJSON :: (MonadSnap m, A.ToJSON a) => (b -> a) -> m b -> m ()
serveJSON f ma = do
  modifyResponse $ addHeader "content-type" "application/json"
  value <- ma
  writeLBS $ A.encode (f value)
