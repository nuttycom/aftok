{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.Projects
  ( -- * API Types (re-exported from aftok-api)
    ProjectsAPI,
    ProtectedProjectsAPI,
    SingleProjectAPI,
    ProjectCreateRequest (..),
    ProjectCreateResponse (..),
    ProjectDetail (..),
    Contributor (..),
    ProjectInviteRequest (..),
    ProjectInviteResponse (..),
    CommsAddress (..),

    -- * Lenses (re-exported from aftok-api)
    cUserId,
    cHandle,
    cJoinedOn,
    cLoggedHours,
    cDepreciatedHours,
    cRevenueShare,
    pdProject,
    pdContributors,

    -- * Handlers
    protectedProjectsServer,
    singleProjectServer,

    -- * JSON helpers
    payoutsJSON,
  )
where

import Aftok.API.Projects
  ( CommsAddress (..),
    Contributor (..),
    ProjectCreateRequest (..),
    ProjectCreateResponse (..),
    ProjectDetail (..),
    ProjectDetailResponse (..),
    ProjectInviteRequest (..),
    ProjectInviteResponse (..),
    ProjectResponse (..),
    ProjectSummary (..),
    ProjectsAPI,
    ProtectedProjectsAPI,
    SingleProjectAPI,
    cDepreciatedHours,
    cHandle,
    cJoinedOn,
    cLoggedHours,
    cRevenueShare,
    cUserId,
    pdContributors,
    pdProject,
  )
import Aftok.Config (SmtpConfig (..))
import qualified Aftok.Currency.Zcash as Zcash
import qualified Aftok.Currency.Zcash.Zip321 as Zip321
import Aftok.Database
  ( createInvitation,
    createProject,
    findUserProject,
    findUserProjectDetail,
    findUserProjects,
    listProjectContributors,
    readWorkIndex,
  )
import Aftok.Database.PostgreSQL (QDBM)
import Aftok.Json (creditToJSON, obj)
import Aftok.Payments (PaymentsConfig)
import Aftok.Project
  ( InvitationCode,
    Project (..),
    ProjectName,
    depRules,
    projectName,
    renderInvCode,
  )
import Aftok.ServerConfig (ServerConfig)
import qualified Aftok.ServerConfig as QC
import Aftok.Servant.App (AppM, envConfig, runDB)
import Aftok.Servant.Auth (AuthenticatedUser (..))
import qualified Aftok.Servant.Auctions as Auctions
import qualified Aftok.Servant.Billing as Billing
import Aftok.API.WorkLog
  ( IntervalResponse (..),
    KeyedLogEntryResponse (..),
    WorkIndexEntry (..),
    WorkIndexResponse (..),
  )
import Aftok.Interval (Interval (..))
import Aftok.Database (KeyedLogEntry (..))
import Aftok.TimeLog (LogEntry (LogEntry))
import Aftok.TimeLog
  ( WorkIndex (..),
    WorkShare,
    WorkShares,
    creditToShares,
    payouts,
    toDepF,
    wsDepreciated,
    wsLogged,
    wsShare,
  )
import qualified Data.List.NonEmpty as L
import Aftok.Types
  ( CreditTo (..),
    DepreciationRules (..),
    Email (..),
    ProjectId,
    _Email,
    username,
  )
import Aftok.Util (fromMaybeT)
import Control.Lens ((^.))
import Control.Monad.Trans.Maybe (mapMaybeT)
import Data.Aeson
  ( ToJSON (..),
    Value (..),
    object,
    (.=),
  )
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.Thyme.Clock as C
import Filesystem.Path.CurrentOS (encodeString)
import qualified Filesystem.Path.CurrentOS as F
import Network.Mail.Mime (Mail, plainPart)
import qualified Network.Mail.Mime as Mime
import qualified Network.Mail.SMTP as SMTP
import Servant
import Servant.Auth.Server (AuthResult (..))
import Text.StringTemplate
  ( directoryGroup,
    getStringTemplate,
    render,
    setAttribute,
  )
import Time.Types (Hours (..))

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

-- | Protected projects server implementation
protectedProjectsServer ::
  PaymentsConfig QDBM ->
  AuthResult AuthenticatedUser ->
  ServerT ProtectedProjectsAPI AppM
protectedProjectsServer payCfg authResult =
  projectListHandler authResult
    :<|> projectCreateHandler authResult
    :<|> singleProjectServer payCfg authResult

-- | Single project server
singleProjectServer ::
  PaymentsConfig QDBM ->
  AuthResult AuthenticatedUser ->
  ProjectId ->
  ServerT SingleProjectAPI AppM
singleProjectServer payCfg authResult pid =
  projectGetHandler authResult pid
    :<|> projectDetailGetHandler authResult pid
    :<|> payoutsHandler authResult pid
    :<|> projectWorkIndexHandler authResult pid
    :<|> projectInviteHandler authResult pid
    :<|> Auctions.projectAuctionsServer authResult pid
    :<|> Billing.projectBillablesServer payCfg authResult pid

-- | List all projects for the authenticated user
projectListHandler :: AuthResult AuthenticatedUser -> AppM [ProjectSummary]
projectListHandler (Authenticated user) = do
  let uid = auUserId user
  projects <- runDB $ findUserProjects uid
  pure $ fmap (\(pid, p) -> ProjectSummary pid p) projects
projectListHandler _ =
  throwError err401 {errBody = "Authentication required"}

-- | Create a new project
projectCreateHandler :: AuthResult AuthenticatedUser -> ProjectCreateRequest -> AppM ProjectCreateResponse
projectCreateHandler (Authenticated user) req = do
  let uid = auUserId user
  t <- liftIO C.getCurrentTime
  pid <- runDB $ createProject $ Project (cpn req) t uid (DepreciationRules (cpdepf req) Nothing)
  pure $ ProjectCreateResponse pid
projectCreateHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Get a single project
projectGetHandler :: AuthResult AuthenticatedUser -> ProjectId -> AppM ProjectResponse
projectGetHandler (Authenticated user) pid = do
  let uid = auUserId user
  project <-
    fromMaybeT
      (throwError err404 {errBody = "Project not found"})
      (mapMaybeT runDB $ findUserProject uid pid)
  pure $ ProjectResponse project
projectGetHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Get project detail with contributors
projectDetailGetHandler :: AuthResult AuthenticatedUser -> ProjectId -> AppM ProjectDetailResponse
projectDetailGetHandler (Authenticated user) pid = do
  let uid = auUserId user
  project <-
    fromMaybeT
      (throwError err404 {errBody = "Project not found"})
      (mapMaybeT runDB $ findUserProject uid pid)
  widx <- runDB $ readWorkIndex pid uid
  contributors <- runDB $ listProjectContributors pid uid
  ptime <- liftIO C.getCurrentTime
  let p = payouts (toDepF $ project ^. depRules) ptime widx
      toContributorRecord uid' ws = do
        (userRec, joinedOn') <- findUserProjectDetail uid' pid
        pure $
          Contributor
            { _cUserId = uid',
              _cHandle = userRec ^. username,
              _cJoinedOn = joinedOn',
              _cLoggedHours = Hours . (`div` 3600) . round . C.toSeconds' $ ws ^. wsLogged,
              _cDepreciatedHours = Hours . (`div` 3600) . round . C.toSeconds' $ ws ^. wsDepreciated,
              _cRevenueShare = ws ^. wsShare
            }
      findContributorPayouts (uid', h, t) = do
        let userShares = M.lookup (CreditToUser uid') (p ^. creditToShares)
            zeroContrib = Contributor uid' h t (Hours 0) (Hours 0) 0
         in (uid',) <$> maybe (pure zeroContrib) (toContributorRecord uid') userShares
  contributorRecords <-
    fromMaybeT
      (throwError err500 {errBody = "No user record found for credited user."})
      . mapMaybeT runDB
      . fmap M.fromList
      $ traverse findContributorPayouts contributors
  let detail =
        ProjectDetail
          { _pdProject = project,
            _pdContributors = contributorRecords
          }
  pure $ ProjectDetailResponse detail
projectDetailGetHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Get work share payouts for a project
payoutsHandler :: AuthResult AuthenticatedUser -> ProjectId -> AppM Value
payoutsHandler (Authenticated user) pid = do
  let uid = auUserId user
  project <-
    fromMaybeT
      (throwError err404 {errBody = "Project not found"})
      (mapMaybeT runDB $ findUserProject uid pid)
  widx <- runDB $ readWorkIndex pid uid
  ptime <- liftIO C.getCurrentTime
  let ws = payouts (toDepF $ project ^. depRules) ptime widx
  pure $ Object $ payoutsJSON ws
payoutsHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Get the full project work index (all contributors)
projectWorkIndexHandler :: AuthResult AuthenticatedUser -> ProjectId -> AppM Value
projectWorkIndexHandler (Authenticated user) pid = do
  let uid = auUserId user
  widx <- runDB $ readWorkIndex pid uid
  pure $ toJSON $ toWorkIndexResponse widx
projectWorkIndexHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Convert a WorkIndex to a WorkIndexResponse
toWorkIndexResponse :: WorkIndex KeyedLogEntry -> WorkIndexResponse
toWorkIndexResponse (WorkIndex widx) =
  WorkIndexResponse
    { wirWorkIndex = fmap toEntry (M.assocs widx)
    }
  where
    toEntry (ct, ivals) =
      WorkIndexEntry
        { wieCreditTo = ct,
          wieIntervals = fmap toIntervalResponse (L.toList ivals)
        }
    toIntervalResponse (Interval s e) =
      IntervalResponse
        { irStart = toKeyedLogEntryResponse s,
          irEnd = toKeyedLogEntryResponse e
        }

-- | Convert a KeyedLogEntry to a KeyedLogEntryResponse
toKeyedLogEntryResponse :: KeyedLogEntry -> KeyedLogEntryResponse
toKeyedLogEntryResponse (KeyedLogEntry eid (LogEntry ct ev meta)) =
  KeyedLogEntryResponse
    { klrEventId = eid,
      klrCreditTo = ct,
      klrEvent = ev,
      klrEventMeta = meta
    }

-- | Send a project invitation
projectInviteHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  ProjectInviteRequest ->
  AppM ProjectInviteResponse
projectInviteHandler (Authenticated user) pid req = do
  let uid = auUserId user
  t <- liftIO C.getCurrentTime
  cfg <- asks (^. envConfig)
  let invite email =
        runDB $
          (,)
            <$> (runMaybeT $ findUserProject uid pid)
            <*> createInvitation pid uid email t
  case inviteBy req of
    EmailComms email -> do
      result <- invite (Email email)
      case result of
        (Nothing, _) ->
          throwError err404 {errBody = "Project not found"}
        (Just p, invCode) -> do
          liftIO $
            sendProjectInviteEmail
              cfg
              (p ^. projectName)
              (Email "noreply@aftok.com")
              (Email email)
              invCode
          pure (ProjectInviteResponse Nothing)
    ZcashComms zaddr -> do
      result <- invite (Email "")
      case result of
        (Nothing, _) ->
          throwError err404 {errBody = "Project not found"}
        (Just p, invCode) ->
          pure . ProjectInviteResponse . Just $
            Zip321.PaymentRequest . pure $
              Zip321.PaymentItem
                { Zip321._address = Zcash.Address zaddr,
                  Zip321._amount = Zcash.Zatoshi 1000,
                  Zip321._memo =
                    Just . Zcash.Memo . encodeUtf8 $
                      "Welcome to the "
                        <> (p ^. projectName)
                        <> " aftok, "
                        <> greetName req
                        <> "\n"
                        <> maybe "" (<> "\n") (pirMessage req)
                        <> "https://aftok.com/app/?invcode="
                        <> renderInvCode invCode
                        <> "&zaddr="
                        <> zaddr
                        <> "#signup",
                  Zip321._message = Nothing,
                  Zip321._label = Nothing,
                  Zip321._other = []
                }
projectInviteHandler _ _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Send project invitation email
sendProjectInviteEmail ::
  ServerConfig ->
  ProjectName ->
  Email ->
  Email ->
  InvitationCode ->
  IO ()
sendProjectInviteEmail cfg pn fromEmail toEmail invCode =
  let SmtpConfig {..} = cfg ^. QC.smtpConfig
      isDevelopment = not (cfg ^. QC.secureCookies)
      useUnauthenticated = isDevelopment && (null _smtpUser || null _smtpPass)
      sendEmail mail =
        if useUnauthenticated
          then case _smtpPort of
            Nothing -> SMTP.sendMail _smtpHost mail
            Just smtpPort -> SMTP.sendMail' _smtpHost smtpPort mail
          else case _smtpPort of
            Nothing -> SMTP.sendMailWithLogin _smtpHost _smtpUser _smtpPass mail
            Just smtpPort -> SMTP.sendMailWithLogin' _smtpHost smtpPort _smtpUser _smtpPass mail
   in buildProjectInviteEmail (cfg ^. QC.templatePath) pn fromEmail toEmail invCode
        >>= sendEmail

-- | Build project invitation email
buildProjectInviteEmail ::
  F.FilePath ->
  ProjectName ->
  Email ->
  Email ->
  InvitationCode ->
  IO Mail
buildProjectInviteEmail tpath pn fromEmail toEmail invCode = do
  templates <- directoryGroup $ encodeString tpath
  case getStringTemplate "invitation_email" templates of
    Nothing -> fail "Could not find template for invitation email"
    Just template ->
      let setAttrs =
            setAttribute "from_email" (fromEmail ^. _Email)
              . setAttribute "project_name" pn
              . setAttribute "to_email" (toEmail ^. _Email)
              . setAttribute "inv_code" (renderInvCode invCode)
          fromAddr = Mime.Address Nothing "invitations@aftok.com"
          toAddr = Mime.Address Nothing (toEmail ^. _Email)
          subject = "Welcome to the " <> pn <> " Aftok!"
          body = plainPart . render $ setAttrs template
       in pure $ SMTP.simpleMail fromAddr [toAddr] [] [] subject [body]

--------------------------------------------------------------------------------
-- JSON serializers
--------------------------------------------------------------------------------

payoutsJSON :: WorkShares -> A.Object
payoutsJSON ws =
  let payoutsRec :: (CreditTo, WorkShare Rational) -> Value
      payoutsRec (c, r) =
        object
          [ "creditTo" .= creditToJSON c,
            "payoutRatio" .= (r ^. wsShare),
            "payoutPercentage" .= (fromRational @Double (r ^. wsShare) * 100)
          ]
   in obj ["payouts" .= fmap payoutsRec (M.assocs (ws ^. creditToShares))]
