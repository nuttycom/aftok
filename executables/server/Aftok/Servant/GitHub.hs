{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.GitHub
  ( -- * Handlers
    gitHubWebhookServer,
    gitHubProjectServer,
    gitHubUserServer,
    gitHubOAuthCallbackHandler,
  )
where

import Aftok.API.GitHub
  ( GitHubOAuthInitResponse (..),
    GitHubProjectAPI,
    GitHubUserAPI,
    GitHubUsernameResponse (..),
    GitHubWebhookAPI,
    GitHubWebhookPayload (..),
    LinkRepoRequest (..),
    LinkRepoResponse (..),
    RepoLinkInfo (..),
  )
import Aftok.Database
  ( addUserToProject,
    claimWebhookDelivery,
    createEvent,
    createGitHubRepoLink,
    createUser,
    deleteGitHubRepoLink,
    finalizeWebhookDelivery,
    findGitHubRepoLink,
    findProjectGitHubRepoLinks,
    findUserByGitHubUsername,
    findUserProjects,
    getUserGitHubUsername,
    linkGitHubUsername,
    unlinkGitHubUsername,
  )
import Aftok.GitHub
  ( GitHubEventStatus (..),
    GitHubRepoLink (..),
    GitHubWebhookEvent (..),
    SkipReason (..),
    durationToNDT,
    normalizeGitHubUsername,
    parseTimeSpent,
    skipReasonText,
    verifySignature,
  )
import Aftok.Servant.App (AppM, envConfig, envHttpManager, envJWTSettings, runDB)
import Aftok.Servant.Auth (AuthenticatedUser (..))
import Aftok.ServerConfig (GitHubOAuthConfig, externalPort, ghOAuthClientId, ghOAuthClientSecret, gitHubOAuthConfig, hostname, secureCookies)
import Aftok.TimeLog
  ( CreditTo (..),
    EventId,
    LogEntry (..),
    LogEvent (..),
  )
import Aftok.Types
  ( Email (..),
    GitHubRepoLinkId (..),
    GitHubUsername (..),
    GitHubWebhookEventId,
    ProjectId,
    RecoverBy (..),
    User (..),
    UserId,
    UserName (..),
  )
import Control.Lens ((^.))
import Control.Monad.Except (catchError)
import Crypto.Random (getRandomBytes)
import Data.Aeson
  ( Value (..),
    (.=),
  )
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Thyme.Clock as C
import Data.Thyme.Time.Core (toThyme)
import qualified Data.Time as Time
import Data.UUID (UUID)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import qualified Network.HTTP.Types.URI as URI
import Servant
import Servant.Auth.Server (AuthResult (..), makeJWT, verifyJWT)

--------------------------------------------------------------------------------
-- Webhook handler
--------------------------------------------------------------------------------

-- | Webhook server (public, signature-verified).
--
-- The request body is taken as raw bytes. HMAC verification must compare
-- against exactly what GitHub signed; re-encoding parsed JSON yields a
-- different byte sequence (key order, spacing) and would reject every
-- legitimate delivery. The body is parsed to JSON only after signature
-- verification succeeds.
gitHubWebhookServer ::
  ServerT GitHubWebhookAPI AppM
gitHubWebhookServer mContentType mEventType mDeliveryId mSignature (GitHubWebhookPayload payloadBytes) = do
  -- Validate required headers
  eventType <- requireHeader mEventType "Missing X-GitHub-Event header"
  deliveryId <- requireHeader mDeliveryId "Missing X-GitHub-Delivery header"
  signature <- requireHeader mSignature "Missing X-Hub-Signature-256 header"

  -- Enforce JSON content-type. GitHub allows webhook config to specify
  -- application/x-www-form-urlencoded, which would interfere with both
  -- HMAC verification and our JSON parsing; reject anything else.
  case mContentType of
    Nothing -> throwError err400 {errBody = "Missing Content-Type header"}
    Just ct
      | T.toLower (T.takeWhile (/= ';') (T.strip ct)) == "application/json" -> pure ()
      | otherwise ->
          throwError
            err400
              { errBody = "Unsupported Content-Type; webhook must be application/json"
              }

  if eventType /= "pull_request"
    then pure NoContent
    else processWebhook eventType deliveryId signature payloadBytes
  where
    requireHeader Nothing msg = throwError err400 {errBody = msg}
    requireHeader (Just v) _ = pure v

-- | Internal webhook processing (after header validation).
processWebhook :: Text -> Text -> Text -> ByteString -> AppM NoContent
processWebhook eventType deliveryId signature payloadBytes = do
  -- Parse the body. Use the same bytes that we'll HMAC-verify against.
  payload <- case A.eitherDecodeStrict payloadBytes of
    Right v -> pure v
    Left _ -> throwError err400 {errBody = "Malformed JSON body"}

  -- Extract repository info from payload (needed to find the secret).
  let mRepoInfo = do
        obj <- case payload of
          Object o -> Just o
          _ -> Nothing
        repo <-
          KM.lookup "repository" obj >>= \case
            Object r -> Just r
            _ -> Nothing
        ownerObj <-
          KM.lookup "owner" repo >>= \case
            Object o -> Just o
            _ -> Nothing
        ownerLogin <-
          KM.lookup "login" ownerObj >>= \case
            String s -> Just s
            _ -> Nothing
        repoName <-
          KM.lookup "name" repo >>= \case
            String s -> Just s
            _ -> Nothing
        pure (ownerLogin, repoName)

  (owner, repo) <- maybe (throwError err400 {errBody = "Invalid payload structure"}) pure mRepoInfo

  -- Find the repo link to verify signature
  mRepoLink <- runDB $ runMaybeT $ findGitHubRepoLink owner repo
  (repoLinkId, repoLink) <- maybe (throwError err404 {errBody = "Repository not linked to any project"}) pure mRepoLink

  -- Verify HMAC signature against raw bytes using constant-time compare.
  let secret = _repoLinkWebhookSecret repoLink
  unless (verifySignature secret signature payloadBytes) $
    throwError err401 {errBody = "Invalid signature"}

  -- Atomically claim the delivery id. If another concurrent delivery has
  -- already claimed it, skip processing. (The single-row INSERT ... ON
  -- CONFLICT serves both as idempotency and as a race guard.)
  now <- liftIO $ toThyme <$> Time.getCurrentTime
  mClaim <- runDB $ claimWebhookDelivery repoLinkId deliveryId eventType now
  case mClaim of
    Nothing -> pure NoContent
    Just rowId -> processWebhookEvent repoLink rowId deliveryId payload now

-- | Process a verified, claimed webhook event. On exit, the claim row is
-- updated to a terminal status via 'finalizeWebhookDelivery'.
processWebhookEvent ::
  GitHubRepoLink ->
  GitHubWebhookEventId ->
  Text ->
  Value ->
  C.UTCTime ->
  AppM NoContent
processWebhookEvent repoLink rowId deliveryId payload now = do
  -- Extract PR info from payload
  let extractPRInfo = do
        obj <- case payload of
          Object o -> Just o
          _ -> Nothing
        action <-
          KM.lookup "action" obj >>= \case
            String s -> Just s
            _ -> Nothing
        pr <-
          KM.lookup "pull_request" obj >>= \case
            Object p -> Just p
            _ -> Nothing
        merged <-
          KM.lookup "merged" pr >>= \case
            Bool b -> Just b
            _ -> Nothing
        prNumber <-
          KM.lookup "number" pr >>= \case
            Number n -> Just (round n :: Int)
            _ -> Nothing
        prBody <-
          KM.lookup "body" pr >>= \case
            String s -> Just s
            Null -> Just ""
            _ -> Nothing
        mergedAt <-
          KM.lookup "merged_at" pr >>= \case
            String s -> parseISO8601 s
            _ -> Nothing
        prUrl <-
          KM.lookup "html_url" pr >>= \case
            String s -> Just s
            _ -> Nothing
        prTitle <-
          KM.lookup "title" pr >>= \case
            String s -> Just s
            _ -> Nothing

        -- Get merged_by info
        mergedBy <-
          KM.lookup "merged_by" pr >>= \case
            Object m -> Just m
            _ -> Nothing
        mergerLogin <-
          KM.lookup "login" mergedBy >>= \case
            String s -> Just s
            _ -> Nothing

        -- Get author info from the user object
        userObj <-
          KM.lookup "user" pr >>= \case
            Object u -> Just u
            _ -> Nothing
        authorLogin <-
          KM.lookup "login" userObj >>= \case
            String s -> Just s
            _ -> Nothing

        pure (action, merged, prNumber, prBody, mergedAt, prUrl, prTitle, mergerLogin, authorLogin)

  case extractPRInfo of
    Nothing ->
      -- Invalid payload, record and skip
      skipEvent Nothing Nothing Nothing SRInvalidPayload
    Just (action, merged, prNumber, prBody, mergedAt, prUrl, prTitle, mergerLogin, authorLogin) ->
      -- Only process closed PRs that were merged
      if not (action == "closed" && merged)
        then
          skipEvent
            (Just prNumber)
            (Just (GitHubUsername (normalizeGitHubUsername authorLogin)))
            Nothing
            (SRPRNotMerged action merged)
        else case parseTimeSpent prBody of
          Nothing ->
            skipEvent
              (Just prNumber)
              (Just (GitHubUsername (normalizeGitHubUsername authorLogin)))
              Nothing
              SRNoTimeSpent
          Just duration -> do
            let projectId = _repoLinkProjectId repoLink
                timeSpent = durationToNDT duration
                ghAuthor = GitHubUsername (normalizeGitHubUsername authorLogin)
                ghMerger = GitHubUsername (normalizeGitHubUsername mergerLogin)

            mMerger <- runDB $ runMaybeT $ findUserByGitHubUsername ghMerger
            mAuthor <- runDB $ runMaybeT $ findUserByGitHubUsername ghAuthor

            case mMerger of
              Nothing ->
                case mAuthor of
                  Just (authorUid, _) ->
                    creditWork
                      projectId
                      authorUid
                      ghAuthor
                      timeSpent
                      mergedAt
                      prUrl
                      prTitle
                      prNumber
                      False
                  Nothing ->
                    skipEvent
                      (Just prNumber)
                      (Just ghAuthor)
                      (Just timeSpent)
                      SRUnknownAuthor
              Just (mergerUid, _) ->
                case mAuthor of
                  Just (authorUid, _) ->
                    creditWork
                      projectId
                      authorUid
                      ghAuthor
                      timeSpent
                      mergedAt
                      prUrl
                      prTitle
                      prNumber
                      False
                  Nothing -> do
                    -- TODO(review): auto-creating a provisional user the
                    -- first time a merger credits an unknown GitHub author
                    -- silently expands the project membership without
                    -- any reconciliation flow. This is intentional for
                    -- the prototype but should grow an explicit
                    -- invitation/claim path before production use.
                    let provisionalUser =
                          User
                            { _username = UserName $ "github_" <> authorLogin,
                              _userAccountRecovery =
                                RecoverByEmail
                                  (Email $ authorLogin <> "@github.placeholder")
                            }
                    authorUid <- runDB $ createUser provisionalUser
                    runDB $ linkGitHubUsername authorUid ghAuthor
                    runDB $ addUserToProject projectId mergerUid authorUid
                    creditWork
                      projectId
                      authorUid
                      ghAuthor
                      timeSpent
                      mergedAt
                      prUrl
                      prTitle
                      prNumber
                      True
  where
    finalize ev = do
      runDB $ finalizeWebhookDelivery rowId ev
      pure NoContent

    skipEvent ::
      Maybe Int ->
      Maybe GitHubUsername ->
      Maybe C.NominalDiffTime ->
      SkipReason ->
      AppM NoContent
    skipEvent mPRNum mAuthor mTimeSpent reason = do
      let ev =
            GitHubWebhookEvent
              { _webhookEventDeliveryId = deliveryId,
                _webhookEventType = "pull_request",
                _webhookEventPRNumber = mPRNum,
                _webhookEventAuthorLogin = mAuthor,
                _webhookEventTimeSpent = mTimeSpent,
                _webhookEventStatus = EventSkipped,
                _webhookEventErrorMessage = Just (skipReasonText reason),
                _webhookEventWorkEventId = Nothing,
                _webhookEventUserId = Nothing,
                _webhookEventUserCreated = False,
                _webhookEventReceivedAt = now,
                _webhookEventProcessedAt = Just now
              }
      finalize ev

    creditWork projectId authorUid ghAuthor timeSpent mergedAt prUrl prTitle prNumber userCreated = do
      workEventId <-
        createWorkEvents
          projectId
          authorUid
          timeSpent
          mergedAt
          prUrl
          prTitle
          prNumber
      let ev =
            GitHubWebhookEvent
              { _webhookEventDeliveryId = deliveryId,
                _webhookEventType = "pull_request",
                _webhookEventPRNumber = Just prNumber,
                _webhookEventAuthorLogin = Just ghAuthor,
                _webhookEventTimeSpent = Just timeSpent,
                _webhookEventStatus = EventProcessed,
                _webhookEventErrorMessage = Nothing,
                _webhookEventWorkEventId = Just workEventId,
                _webhookEventUserId = Just authorUid,
                _webhookEventUserCreated = userCreated,
                _webhookEventReceivedAt = now,
                _webhookEventProcessedAt = Just now
              }
      finalize ev

-- | Create StartWork and StopWork events for credited work, returning the
-- StartWork EventId as the audit reference stored on the webhook row.
createWorkEvents ::
  ProjectId ->
  UserId ->
  C.NominalDiffTime ->
  C.UTCTime ->
  Text ->
  Text ->
  Int ->
  AppM EventId
createWorkEvents projectId userId duration mergedAt prUrl prTitle prNumber = do
  let endTime = mergedAt
      startTime = C.addUTCTime (negate duration) endTime
      metadata =
        Just $
          A.object
            [ "source" .= ("github_pr" :: Text),
              "pr_number" .= prNumber,
              "pr_title" .= prTitle,
              "pr_url" .= prUrl
            ]
      startEntry =
        LogEntry
          { _creditTo = CreditToUser userId,
            _event = StartWork startTime,
            _eventMeta = metadata
          }
      stopEntry =
        LogEntry
          { _creditTo = CreditToUser userId,
            _event = StopWork endTime,
            _eventMeta = metadata
          }
  startId <- runDB $ createEvent projectId userId startEntry
  _ <- runDB $ createEvent projectId userId stopEntry
  pure startId

-- | Parse ISO8601 timestamp
parseISO8601 :: Text -> Maybe C.UTCTime
parseISO8601 t =
  toThyme <$> Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (T.unpack t)

--------------------------------------------------------------------------------
-- Project-level GitHub management
--------------------------------------------------------------------------------

gitHubProjectServer ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  ServerT GitHubProjectAPI AppM
gitHubProjectServer authResult pid =
  listRepoLinksHandler authResult pid
    :<|> linkRepoHandler authResult pid
    :<|> unlinkRepoHandler authResult pid

checkMembership :: UserId -> ProjectId -> AppM ()
checkMembership uid pid = do
  projects <- runDB $ findUserProjects uid
  unless (any (\(p, _) -> p == pid) projects) $
    throwError err403 {errBody = "User is not a member of this project"}

listRepoLinksHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  AppM [RepoLinkInfo]
listRepoLinksHandler (Authenticated user) pid = do
  let uid = auUserId user
  checkMembership uid pid
  links <- runDB $ findProjectGitHubRepoLinks pid
  pure $
    fmap
      ( \(rLinkId, link) ->
          RepoLinkInfo
            { rliId = rLinkId,
              rliOwner = _repoLinkOwner link,
              rliRepo = _repoLinkRepo link,
              rliCreatedAt = _repoLinkCreatedAt link
            }
      )
      links
listRepoLinksHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Link a repo to a project.
--
-- TODO(review): the generated 'webhook_secret' is currently stored in
-- plaintext in the 'github_repo_links' table. Encryption-at-rest would
-- require introducing a server master key; leaving as a follow-up.
linkRepoHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  LinkRepoRequest ->
  AppM LinkRepoResponse
linkRepoHandler (Authenticated user) pid req = do
  let uid = auUserId user
  checkMembership uid pid
  now <- liftIO $ toThyme <$> Time.getCurrentTime

  -- Generate a secure random webhook secret
  secretBytes <- liftIO $ getRandomBytes 32
  let secret = T.decodeUtf8 $ B16.encode (secretBytes :: ByteString)

  let link =
        GitHubRepoLink
          { _repoLinkProjectId = pid,
            _repoLinkOwner = lrrOwner req,
            _repoLinkRepo = lrrRepo req,
            _repoLinkWebhookSecret = secret,
            _repoLinkCreatedBy = uid,
            _repoLinkCreatedAt = now
          }

  newLinkId <- runDB $ createGitHubRepoLink link
  pure $
    LinkRepoResponse
      { lrLinkId = newLinkId,
        lrWebhookSecret = secret
      }
linkRepoHandler _ _ _ =
  throwError err401 {errBody = "Authentication required"}

unlinkRepoHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  UUID ->
  AppM NoContent
unlinkRepoHandler (Authenticated user) pid linkUUID = do
  let uid = auUserId user
      rLinkId = GitHubRepoLinkId linkUUID
  checkMembership uid pid
  runDB $ deleteGitHubRepoLink rLinkId
  pure NoContent
unlinkRepoHandler _ _ _ =
  throwError err401 {errBody = "Authentication required"}

--------------------------------------------------------------------------------
-- User-level GitHub username linking (OAuth-verified)
--------------------------------------------------------------------------------

gitHubUserServer ::
  AuthResult AuthenticatedUser ->
  ServerT GitHubUserAPI AppM
gitHubUserServer authResult =
  getGitHubUsernameHandler authResult
    :<|> linkGitHubOAuthHandler authResult
    :<|> unlinkGitHubUsernameHandler authResult

getGitHubUsernameHandler ::
  AuthResult AuthenticatedUser ->
  AppM GitHubUsernameResponse
getGitHubUsernameHandler (Authenticated user) = do
  let uid = auUserId user
  mUsername <- runDB $ getUserGitHubUsername uid
  pure $ GitHubUsernameResponse $ fmap (\(GitHubUsername u) -> u) mUsername
getGitHubUsernameHandler _ =
  throwError err401 {errBody = "Authentication required"}

linkGitHubOAuthHandler ::
  AuthResult AuthenticatedUser ->
  AppM GitHubOAuthInitResponse
linkGitHubOAuthHandler (Authenticated user) = do
  env <- ask
  let cfg = env ^. envConfig
      jwtSettings = env ^. envJWTSettings
  ghCfg <- case cfg ^. gitHubOAuthConfig of
    Nothing ->
      throwError err501 {errBody = "GitHub OAuth is not configured"}
    Just c -> pure c
  now <- liftIO Time.getCurrentTime
  let expiry = Time.addUTCTime 600 now -- 10-minute expiry
  ejwt <- liftIO $ makeJWT user jwtSettings (Just expiry)
  stateToken <- case ejwt of
    Left _ -> throwError err500 {errBody = "Failed to generate OAuth state token"}
    Right jwt -> pure $ decodeUtf8 (LBS.toStrict jwt)
  let scheme = if cfg ^. secureCookies then "https://" else "http://"
      host = decodeUtf8 (cfg ^. hostname)
      portSuffix = maybe "" (\p -> ":" <> show p) (cfg ^. externalPort)
      redirectUri = scheme <> host <> portSuffix <> "/api/user/github/callback"
      clientId = ghCfg ^. ghOAuthClientId
      authUrl =
        "https://github.com/login/oauth/authorize"
          <> "?client_id="
          <> clientId
          <> "&redirect_uri="
          <> redirectUri
          <> "&scope=read:user"
          <> "&state="
          <> stateToken
  pure $ GitHubOAuthInitResponse authUrl
linkGitHubOAuthHandler _ =
  throwError err401 {errBody = "Authentication required"}

gitHubOAuthCallbackHandler ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  AppM (Headers '[Header "Location" Text] NoContent)
gitHubOAuthCallbackHandler _mCode _mState (Just ghError) =
  redirectTo $ "/app/settings?github=error&reason=" <> ghError
gitHubOAuthCallbackHandler (Just code) (Just stateJwt) _mError = do
  env <- ask
  let cfg = env ^. envConfig
      jwtSettings = env ^. envJWTSettings
      mgr = env ^. envHttpManager
  case cfg ^. gitHubOAuthConfig of
    Nothing ->
      redirectTo "/app/settings?github=error&reason=not_configured"
    Just ghCfg -> do
      mUser <- liftIO $ verifyJWT jwtSettings (encodeUtf8 stateJwt)
      case (mUser :: Maybe AuthenticatedUser) of
        Nothing ->
          redirectTo "/app/settings?github=error&reason=invalid_state"
        Just user -> do
          let uid = auUserId user
          mToken <- liftIO $ exchangeCodeForToken mgr ghCfg code
          case mToken of
            Nothing ->
              redirectTo "/app/settings?github=error&reason=token_exchange_failed"
            Just token -> do
              mLogin <- liftIO $ fetchGitHubLogin mgr token
              case mLogin of
                Nothing ->
                  redirectTo "/app/settings?github=error&reason=github_api_failed"
                Just login ->
                  linkAndRedirect uid login
gitHubOAuthCallbackHandler _ _ _ =
  redirectTo "/app/settings?github=error&reason=missing_params"

-- | Store the OAuth-verified GitHub username. 'linkGitHubUsername' raises
-- 'DuplicateRecord' (-> 409) on the unique-constraint violation, which we
-- translate to an 'already_linked' redirect query parameter.
linkAndRedirect :: UserId -> Text -> AppM (Headers '[Header "Location" Text] NoContent)
linkAndRedirect uid login =
  ( runDB (linkGitHubUsername uid (GitHubUsername (normalizeGitHubUsername login)))
      >> redirectTo "/app/settings?github=linked"
  )
    `catchError` \err ->
      if errHTTPCode err == 409
        then redirectTo "/app/settings?github=error&reason=already_linked"
        else redirectTo "/app/settings?github=error&reason=db_error"

redirectTo :: Text -> AppM (Headers '[Header "Location" Text] NoContent)
redirectTo url = pure $ addHeader url NoContent

-- | Exchange an authorization code for a GitHub access token. The request
-- body is application/x-www-form-urlencoded; every value is URL-encoded
-- so that any URL-special characters in the secret or the code don't
-- corrupt the request.
exchangeCodeForToken :: HTTP.Manager -> GitHubOAuthConfig -> Text -> IO (Maybe Text)
exchangeCodeForToken mgr ghCfg code = do
  let enc = URI.urlEncode True . T.encodeUtf8
      body =
        BS.concat
          [ "client_id=",
            enc (ghCfg ^. ghOAuthClientId),
            "&client_secret=",
            enc (ghCfg ^. ghOAuthClientSecret),
            "&code=",
            enc code
          ]
  initReq <- HTTP.parseRequest "https://github.com/login/oauth/access_token"
  let req =
        initReq
          { HTTP.method = "POST",
            HTTP.requestBody = HTTP.RequestBodyBS body,
            HTTP.requestHeaders =
              [ ("Accept", "application/json"),
                ("Content-Type", "application/x-www-form-urlencoded")
              ]
          }
  resp <- HTTP.httpLbs req mgr
  case A.decode (HTTP.responseBody resp) of
    Just (Object obj) ->
      case KM.lookup "access_token" obj of
        Just (String token) -> pure (Just token)
        _ -> pure Nothing
    _ -> pure Nothing

-- | Fetch the authenticated GitHub user's login name.
fetchGitHubLogin :: HTTP.Manager -> Text -> IO (Maybe Text)
fetchGitHubLogin mgr token = do
  initReq <- HTTP.parseRequest "https://api.github.com/user"
  let req =
        initReq
          { HTTP.requestHeaders =
              [ ("Authorization", "Bearer " <> T.encodeUtf8 token),
                ("Accept", "application/json"),
                ("User-Agent", "aftok-server")
              ]
          }
  resp <- HTTP.httpLbs req mgr
  if statusCode (HTTP.responseStatus resp) /= 200
    then pure Nothing
    else case A.decode (HTTP.responseBody resp) of
      Just (Object obj) ->
        case KM.lookup "login" obj of
          Just (String login) -> pure (Just login)
          _ -> pure Nothing
      _ -> pure Nothing

unlinkGitHubUsernameHandler ::
  AuthResult AuthenticatedUser ->
  AppM NoContent
unlinkGitHubUsernameHandler (Authenticated user) = do
  let uid = auUserId user
  runDB $ unlinkGitHubUsername uid
  pure NoContent
unlinkGitHubUsernameHandler _ =
  throwError err401 {errBody = "Authentication required"}
