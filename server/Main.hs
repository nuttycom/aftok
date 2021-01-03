{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Aftok.Config as C
import Aftok.Currency.Bitcoin.Payments (_bip70Request)
import Aftok.Currency.Zcash (rpcValidateZAddr)
import Aftok.Json
import Aftok.QConfig as Q
import Aftok.Snaplet
import Aftok.Snaplet.Auctions
import Aftok.Snaplet.Auth
import Aftok.Snaplet.Billing
import Aftok.Snaplet.Payments
import Aftok.Snaplet.Projects
import Aftok.Snaplet.Users
import Aftok.Snaplet.WorkLog
import Aftok.TimeLog
import Control.Exception (try)
import Control.Lens
  ( (^.),
    to,
  )
import qualified Data.Aeson as A
import Data.ProtocolBuffers (encodeMessage)
import Data.Serialize.Put (runPutLazy)
import Filesystem.Path.CurrentOS
  ( decodeString,
    encodeString,
  )
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Snap.Core
import Snap.Snaplet
import qualified Snap.Snaplet.Auth as AU
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe (serveDirectory)
import System.Environment
import System.IO.Error (IOError)

main :: IO ()
main = do
  cfgPath <- try @IOError $ getEnv "AFTOK_CFG"
  cfg <- loadQConfig . decodeString $ fromRight "conf/aftok.cfg" cfgPath
  sconf <- snapConfig cfg
  serveSnaplet sconf $ appInit cfg

registerOps :: Manager -> QConfig -> RegisterOps IO
registerOps mgr cfg =
  RegisterOps
    { validateZAddr = rpcValidateZAddr mgr (_zcashdConfig cfg),
      sendConfirmationEmail = const $ pure ()
    }

appInit :: QConfig -> SnapletInit App App
appInit cfg = makeSnaplet "aftok" "Aftok Time Tracker" Nothing $ do
  mgr <- liftIO $ newManager defaultManagerSettings
  let cookieKey = cfg ^. authSiteKey . to encodeString
      rops = registerOps mgr cfg
  sesss <-
    nestSnaplet "sessions" sess $
      initCookieSessionManager
        cookieKey
        "quookie"
        Nothing
        (cfg ^. cookieTimeout)
  pgs <- nestSnaplet "db" db $ pgsInit' (cfg ^. pgsConfig)
  auths <- nestSnaplet "auth" auth $ initPostgresAuth sess pgs
  let nmode = cfg ^. billingConfig . C.networkMode
      loginRoute = method GET requireLogin >> redirect "/app"
      xhrLoginRoute = void $ method POST requireLoginXHR
      checkLoginRoute = void $ method GET requireUser
      logoutRoute = method GET (with auth AU.logout)
      checkZAddrRoute = void $ method GET (checkZAddrHandler rops)
      registerRoute = void $ method POST (registerHandler rops (cfg ^. recaptchaSecret))
      inviteRoute = void $ method POST (projectInviteHandler cfg)
      acceptInviteRoute = void $ method POST acceptInvitationHandler
      listContributorsRoute =
        serveJSON (fmap contributorJSON) $ method GET listContributorsHandler
      projectCreateRoute =
        serveJSON projectIdJSON $ method POST projectCreateHandler
      projectListRoute =
        serveJSON (fmap qdbProjectJSON) $ method GET projectListHandler
      projectRoute = serveJSON projectJSON $ method GET projectGetHandler
      projectWorkIndexRoute =
        serveJSON workIndexJSON $ method GET projectWorkIndex
      projectPayoutsRoute =
        serveJSON payoutsJSON $ method GET payoutsHandler
      logWorkRoute f =
        serveJSON keyedLogEntryJSON $ method POST (logWorkHandler f)
      amendEventRoute = serveJSON amendmentIdJSON $ method PUT amendEventHandler
      userEventsRoute =
        serveJSON (fmap logEntryJSON) $ method GET userEvents
      userWorkIndexRoute =
        serveJSON workIndexJSON $ method GET userWorkIndex
      auctionCreateRoute =
        serveJSON auctionIdJSON $ method POST auctionCreateHandler
      auctionListRoute =
        serveJSON (fmap auctionJSON) $ method GET auctionListHandler
      auctionRoute = serveJSON auctionJSON $ method GET auctionGetHandler
      auctionBidRoute = serveJSON bidIdJSON $ method POST auctionBidHandler
      billableCreateRoute =
        serveJSON billableIdJSON $ method POST billableCreateHandler
      billableListRoute =
        serveJSON (fmap qdbBillableJSON) $ method GET billableListHandler
      subscribeRoute =
        serveJSON subscriptionIdJSON $ method POST subscribeHandler
      -- payableRequestsRoute =
      --   serveJSON billDetailsJSON $ method GET listPayableRequestsHandler
      getBip70PaymentRequestRoute =
        writeLBS
          . runPutLazy
          . encodeMessage
          . _bip70Request
          . snd
          =<< method GET getBip70PaymentRequestHandler
      submitBip70PaymentRoute =
        serveJSON paymentIdJSON $
          method POST (bip70PaymentResponseHandler $ cfg ^. billingConfig)
  addRoutes
    [ ("static", serveDirectory . encodeString $ cfg ^. staticAssetPath),
      ("login", loginRoute), -- login.sh
      ("login", xhrLoginRoute), -- login_xhr.sh
      ("logout", logoutRoute), -- logout.sh
      ("login/check", checkLoginRoute), -- login.sh
      ("register", registerRoute), -- create_user.sh
      ("validate_zaddr", checkZAddrRoute), -- check_zaddr.sh
      ("accept_invitation", acceptInviteRoute),
      ("user/projects/:projectId/logStart", logWorkRoute StartWork), -- log_start.sh
      ("user/projects/:projectId/logEnd", logWorkRoute StopWork), -- log_end.sh
      ("user/projects/:projectId/events", userEventsRoute), -- list_user_events.sh
      ("user/projects/:projectId/workIndex", userWorkIndexRoute), -- list_user_intervals.sh
      ("projects/:projectId/workIndex", projectWorkIndexRoute), -- list_project_intervals.sh
      ("projects/:projectId/auctions", auctionCreateRoute), -- <|> auctionListRoute)
      ("projects/:projectId/billables", billableCreateRoute <|> billableListRoute), -- create_billable.sh / list_project_billables.sh
      ("projects/:projectId/payouts", projectPayoutsRoute), -- list_project_payouts.sh
      ("projects/:projectId/invite", inviteRoute), -- invite.sh
      ("projects/:projectId/contributors", listContributorsRoute), -- list_project_contributors.sh
      ("projects/:projectId", projectRoute), -- get_project.sh
      ("projects", projectCreateRoute <|> projectListRoute), --  create_project.sh, list_projects.sh
      ("auctions/:auctionId", auctionRoute),
      ("auctions/:auctionId/bid", auctionBidRoute),
      ("subscribe/:billableId", subscribeRoute),
      -- ("subscriptions/:subscriptionId/payment_requests", payableRequestsRoute),
      ("pay/btc/:paymentRequestKey", getBip70PaymentRequestRoute <|> submitBip70PaymentRoute),
      ("events/:eventId/amend", amendEventRoute)
    ]
  return $ App nmode sesss pgs auths

serveJSON :: (MonadSnap m, A.ToJSON a) => (b -> a) -> m b -> m ()
serveJSON f ma = do
  modifyResponse $ addHeader "content-type" "application/json"
  value <- ma
  writeLBS $ A.encode (f value)
