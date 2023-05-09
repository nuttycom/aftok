{-# LANGUAGE TypeApplications #-}

module Main where

import Aftok.Auction (_AuctionId)
import Aftok.Billing (_BillableId, _SubscriptionId)
import qualified Aftok.Config as C
import Aftok.Currency.Bitcoin.Payments (_bip70Request)
import Aftok.Currency.Zcash (zcashNetwork)
import qualified Aftok.Currency.Zcash as Zcash
import Aftok.Database.PostgreSQL (QDBM)
import Aftok.Json
import Aftok.Payments.Types (_PaymentId)
import Aftok.ServerConfig
import Aftok.Snaplet
import Aftok.Snaplet.Auctions
import Aftok.Snaplet.Auth
import Aftok.Snaplet.Billing
import Aftok.Snaplet.Json (idJSON)
import Aftok.Snaplet.Payments
import Aftok.Snaplet.Projects
import Aftok.Snaplet.Users
import Aftok.Snaplet.WorkLog
import Aftok.TimeLog
import Aftok.Types (_ProjectId)
import Control.Lens
  ( to,
    (^.),
  )
import qualified Data.Aeson as A
import Data.ProtocolBuffers (encodeMessage)
import Data.Serialize.Put (runPutLazy)
import Filesystem.Path.CurrentOS
  ( decodeString,
    encodeString,
  )
import Lrzhs (isValidSaplingAddress)
import Options.Applicative (Parser, execParser, header, help, helper, info, long, short, strOption)
import Snap.Core
import qualified Snap.Http.Server.Config as SC
import Snap.Snaplet
import qualified Snap.Snaplet.Auth as AU
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe (serveDirectory)

data CmdArgs = CmdArgs {cfgFile :: String}

args :: Parser CmdArgs
args = CmdArgs <$> strOption (long "conf" <> short 'c' <> help "Configuration file")

main :: IO ()
main = do
  opts <- execParser $ info (args <**> helper) (header "The Aftok collaboration server")
  cfg <- loadServerConfig . decodeString $ cfgFile opts
  let sconf = baseSnapConfig cfg SC.emptyConfig
  serveSnapletNoArgParsing sconf $ appInit cfg

registerOps :: ServerConfig -> RegisterOps IO
registerOps cfg =
  RegisterOps
    { validateZAddr = \zaddr ->
        isValidSaplingAddress (zcashNetwork . _zcashConfig $ cfg) zaddr <&> \valid ->
          if valid
            then Right (Zcash.Address zaddr)
            else Left (Zcash.ZAddrInvalid),
      sendConfirmationEmail = const $ pure ()
    }

appInit :: ServerConfig -> SnapletInit App App
appInit cfg = makeSnaplet "aftok" "Aftok Time Tracker" Nothing $ do
  paymentsConfig <- liftIO $ C.toPaymentsConfig @QDBM (cfg ^. billingConfig)
  let cookieKey = cfg ^. authSiteKey . to encodeString
      rops = registerOps cfg
  sesss <-
    nestSnaplet "sessions" sess $
      initCookieSessionManager
        cookieKey
        "quookie"
        Nothing
        (cfg ^. cookieTimeout)
  pgs <- nestSnaplet "db" db $ pgsInit' (cfg ^. pgsConfig)
  auths <- nestSnaplet "auth" auth $ initPostgresAuth sess pgs
  let nmode = cfg ^. billingConfig . C.bitcoinConfig . C.networkMode
      loginRoute = method GET requireLogin >> redirect "/app"
      xhrLoginRoute = void $ method POST requireLoginXHR
      checkLoginRoute = void $ method GET requireUser
      logoutRoute = method GET (with auth AU.logout)
      checkUsernameRoute = void $ method GET checkUsernameHandler
      checkZAddrRoute = void $ method GET (checkZAddrHandler rops)
      registerRoute = void $ method POST (registerHandler rops (cfg ^. recaptchaSecret))
      inviteRoute =
        serveJSON (projectInviteResponseJSON) $ method POST (projectInviteHandler cfg)
      acceptInviteRoute =
        void $ method POST acceptInvitationHandler
      projectDetailRoute =
        serveJSON (v1 . projectDetailJSON) $ method GET projectDetailGetHandler
      projectCreateRoute =
        serveJSON (idJSON "projectId" _ProjectId) $ method POST projectCreateHandler
      projectListRoute =
        serveJSON (fmap qdbProjectJSON) $ method GET projectListHandler
      projectRoute = serveJSON projectJSON $ method GET projectGetHandler
      projectWorkIndexRoute =
        serveJSON (workIndexJSON keyedLogEntryJSON) $ method GET projectWorkIndex
      projectPayoutsRoute =
        serveJSON payoutsJSON $ method GET payoutsHandler
      logWorkRoute f =
        serveJSON extendedLogEntryJSON $ method POST (logWorkHandler f)
      amendEventRoute = serveJSON amendEventResultJSON $ method PUT amendEventHandler
      userEventsRoute =
        serveJSON (fmap keyedLogEntryJSON) $ method GET userEvents
      userWorkIndexRoute =
        serveJSON (workIndexJSON keyedLogEntryJSON) $ method GET userWorkIndex
      auctionCreateRoute =
        serveJSON (idJSON "auctionId" _AuctionId) $ method POST auctionCreateHandler
      auctionListRoute =
        serveJSON (fmap auctionJSON) $ method GET auctionListHandler
      auctionRoute = serveJSON auctionJSON $ method GET auctionGetHandler
      auctionBidRoute = serveJSON bidIdJSON $ method POST auctionBidHandler
      -- Routes for billables
      billableCreateRoute =
        serveJSON (idJSON "billableId" _BillableId) $ method POST billableCreateHandler
      billableListRoute =
        serveJSON (fmap qdbBillableJSON) $ method GET billableListHandler
      subscribeRoute =
        serveJSON (idJSON "subscriptionId" _SubscriptionId) $ method POST subscribeHandler
      paymentRequestCreateRoute =
        serveJSON paymentRequestDetailJSON $ method POST (createPaymentRequestHandler paymentsConfig)
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
        serveJSON (idJSON "paymentId" _PaymentId) $
          method POST (bip70PaymentResponseHandler $ cfg ^. billingConfig . C.bitcoinConfig)
  addRoutes
    [ ("static", serveDirectory . encodeString $ cfg ^. staticAssetPath),
      ("login", loginRoute), -- login.sh
      ("login", xhrLoginRoute), -- login_xhr.sh
      ("logout", logoutRoute), -- logout.sh
      ("login/check", checkLoginRoute), -- login.sh
      ("register", registerRoute), -- create_user.sh
      ("validate_username", checkUsernameRoute), -- check_username.sh
      ("validate_zaddr", checkZAddrRoute), -- check_zaddr.sh
      ("accept_invitation", acceptInviteRoute),
      ("user/projects/:projectId/logStart", logWorkRoute StartWork), -- log_start.sh
      ("user/projects/:projectId/logEnd", logWorkRoute StopWork), -- log_end.sh
      ("user/projects/:projectId/events", userEventsRoute), -- list_user_events.sh
      ("user/projects/:projectId/workIndex", userWorkIndexRoute), -- list_user_intervals.sh
      ("projects/:projectId/workIndex", projectWorkIndexRoute), -- list_project_intervals.sh
      ("projects/:projectId/auctions", auctionCreateRoute <|> auctionListRoute),
      ("projects/:projectId/billables", billableCreateRoute <|> billableListRoute), -- create_billable.sh / list_project_billables.sh
      ("projects/:projectId/billables/:billableId/paymentRequests", paymentRequestCreateRoute), -- create_billable.sh / list_project_billables.sh
      ("projects/:projectId/payouts", projectPayoutsRoute), -- list_project_payouts.sh
      ("projects/:projectId/invite", inviteRoute), -- invite.sh
      ("projects/:projectId/detail", projectDetailRoute), -- list_project_contributors.sh
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
