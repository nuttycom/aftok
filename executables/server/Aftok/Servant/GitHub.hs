{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.GitHub
  ( -- * Handlers
    gitHubWebhookServer,
    gitHubProjectServer,
    gitHubUserServer,
  )
where

import Aftok.API.GitHub
  ( GitHubProjectAPI,
    GitHubUserAPI,
    GitHubUsernameResponse (..),
    GitHubWebhookAPI,
    LinkGitHubUsernameRequest (..),
    LinkRepoRequest (..),
    LinkRepoResponse (..),
    RepoLinkInfo (..),
  )
import Aftok.Database
  ( addUserToProject,
    createEvent,
    createGitHubRepoLink,
    createUser,
    deleteGitHubRepoLink,
    findGitHubRepoLink,
    findProjectGitHubRepoLinks,
    findUserByGitHubUsername,
    findUserProjects,
    getUserGitHubUsername,
    isDeliveryProcessed,
    linkGitHubUsername,
    recordWebhookEvent,
    unlinkGitHubUsername,
  )
import Aftok.GitHub
  ( GitHubEventStatus (..),
    GitHubRepoLink (..),
    GitHubWebhookEvent (..),
    durationToNDT,
    parseTimeSpent,
  )
import Aftok.Servant.App (AppM, runDB)
import Aftok.Servant.Auth (AuthenticatedUser (..))
import Aftok.TimeLog
  ( CreditTo (..),
    LogEntry (..),
    LogEvent (..),
  )
import Aftok.Types
  ( Email (..),
    GitHubRepoLinkId (..),
    GitHubUsername (..),
    ProjectId,
    RecoverBy (..),
    User (..),
    UserId,
    UserName (..),
  )
import Crypto.Hash (SHA256 (..))
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.Aeson
  ( Value (..),
    (.=),
  )
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Thyme.Clock as C
import Data.Thyme.Time.Core (toThyme)
import qualified Data.Time as Time
import Data.UUID (UUID)
import Crypto.Random (getRandomBytes)
import Servant
import Servant.Auth.Server (AuthResult (..))

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

-- | Webhook server (public, signature-verified)
gitHubWebhookServer ::
  ServerT GitHubWebhookAPI AppM
gitHubWebhookServer mEventType mDeliveryId mSignature payload = do
  -- Validate required headers
  eventType <- maybe (throwError err400 {errBody = "Missing X-GitHub-Event header"}) pure mEventType
  deliveryId <- maybe (throwError err400 {errBody = "Missing X-GitHub-Delivery header"}) pure mDeliveryId
  signature <- maybe (throwError err400 {errBody = "Missing X-Hub-Signature-256 header"}) pure mSignature

  -- Check idempotency - skip if already processed
  alreadyProcessed <- runDB $ isDeliveryProcessed deliveryId
  if alreadyProcessed || eventType /= "pull_request"
    then pure NoContent
    else processWebhook eventType deliveryId signature payload

-- | Internal webhook processing (after basic validation)
processWebhook :: Text -> Text -> Text -> Value -> AppM NoContent
processWebhook eventType deliveryId signature payload = do
  -- Extract repository info from payload
  let mRepoInfo = do
        obj <- case payload of
          Object o -> Just o
          _ -> Nothing
        repo <- KM.lookup "repository" obj >>= \case
          Object r -> Just r
          _ -> Nothing
        ownerObj <- KM.lookup "owner" repo >>= \case
          Object o -> Just o
          _ -> Nothing
        ownerLogin <- KM.lookup "login" ownerObj >>= \case
          String s -> Just s
          _ -> Nothing
        repoName <- KM.lookup "name" repo >>= \case
          String s -> Just s
          _ -> Nothing
        pure (ownerLogin, repoName)

  (owner, repo) <- maybe (throwError err400 {errBody = "Invalid payload structure"}) pure mRepoInfo

  -- Find the repo link to verify signature
  mRepoLink <- runDB $ runMaybeT $ findGitHubRepoLink owner repo
  (repoLinkId, repoLink) <- maybe (throwError err404 {errBody = "Repository not linked to any project"}) pure mRepoLink

  -- Verify HMAC signature
  let secret = _repoLinkWebhookSecret repoLink
      payloadBytes = toStrict $ A.encode payload
  unless (verifySignature secret signature payloadBytes) $
    throwError err401 {errBody = "Invalid signature"}

  -- Process the webhook event
  processWebhookEvent repoLinkId repoLink eventType deliveryId payload

-- | Process a verified webhook event
processWebhookEvent ::
  GitHubRepoLinkId ->
  GitHubRepoLink ->
  Text ->
  Text ->
  Value ->
  AppM NoContent
processWebhookEvent repoLinkId repoLink _eventType deliveryId payload = do
  now <- liftIO $ toThyme <$> Time.getCurrentTime

  -- Extract PR info from payload
  let extractPRInfo = do
        obj <- case payload of
          Object o -> Just o
          _ -> Nothing
        action <- KM.lookup "action" obj >>= \case
          String s -> Just s
          _ -> Nothing
        pr <- KM.lookup "pull_request" obj >>= \case
          Object p -> Just p
          _ -> Nothing
        merged <- KM.lookup "merged" pr >>= \case
          Bool b -> Just b
          _ -> Nothing
        prNumber <- KM.lookup "number" pr >>= \case
          Number n -> Just (round n :: Int)
          _ -> Nothing
        prBody <- KM.lookup "body" pr >>= \case
          String s -> Just s
          Null -> Just ""
          _ -> Nothing
        mergedAt <- KM.lookup "merged_at" pr >>= \case
          String s -> parseISO8601 s
          _ -> Nothing
        prUrl <- KM.lookup "html_url" pr >>= \case
          String s -> Just s
          _ -> Nothing
        prTitle <- KM.lookup "title" pr >>= \case
          String s -> Just s
          _ -> Nothing

        -- Get merged_by info
        mergedBy <- KM.lookup "merged_by" pr >>= \case
          Object m -> Just m
          _ -> Nothing
        mergerLogin <- KM.lookup "login" mergedBy >>= \case
          String s -> Just s
          _ -> Nothing

        -- Get author info from the user object
        userObj <- KM.lookup "user" pr >>= \case
          Object u -> Just u
          _ -> Nothing
        authorLogin <- KM.lookup "login" userObj >>= \case
          String s -> Just s
          _ -> Nothing

        pure (action, merged, prNumber, prBody, mergedAt, prUrl, prTitle, mergerLogin, authorLogin)

  case extractPRInfo of
    Nothing -> do
      -- Invalid payload, record and skip
      let ev =
            GitHubWebhookEvent
              { _webhookEventDeliveryId = deliveryId,
                _webhookEventType = "pull_request",
                _webhookEventPRNumber = Nothing,
                _webhookEventAuthorEmail = Nothing,
                _webhookEventAuthorLogin = Nothing,
                _webhookEventTimeSpent = Nothing,
                _webhookEventStatus = EventSkipped,
                _webhookEventErrorMessage = Just "Could not parse PR info from payload",
                _webhookEventWorkEventId = Nothing,
                _webhookEventUserId = Nothing,
                _webhookEventUserCreated = False,
                _webhookEventReceivedAt = now,
                _webhookEventProcessedAt = Just now
              }
      runDB $ void $ recordWebhookEvent repoLinkId ev
      pure NoContent
    Just (action, merged, prNumber, prBody, mergedAt, prUrl, prTitle, mergerLogin, authorLogin) ->
      -- Only process closed PRs that were merged
      if not (action == "closed" && merged)
        then do
          let ev =
                GitHubWebhookEvent
                  { _webhookEventDeliveryId = deliveryId,
                    _webhookEventType = "pull_request",
                    _webhookEventPRNumber = Just prNumber,
                    _webhookEventAuthorEmail = Nothing,
                    _webhookEventAuthorLogin = Just (GitHubUsername authorLogin),
                    _webhookEventTimeSpent = Nothing,
                    _webhookEventStatus = EventSkipped,
                    _webhookEventErrorMessage = Just $ "PR action=" <> action <> ", merged=" <> show merged,
                    _webhookEventWorkEventId = Nothing,
                    _webhookEventUserId = Nothing,
                    _webhookEventUserCreated = False,
                    _webhookEventReceivedAt = now,
                    _webhookEventProcessedAt = Just now
                  }
          runDB $ void $ recordWebhookEvent repoLinkId ev
          pure NoContent
        else
          -- Parse time spent from PR body
          case parseTimeSpent prBody of
        Nothing -> do
          let ev =
                GitHubWebhookEvent
                  { _webhookEventDeliveryId = deliveryId,
                    _webhookEventType = "pull_request",
                    _webhookEventPRNumber = Just prNumber,
                    _webhookEventAuthorEmail = Nothing,
                    _webhookEventAuthorLogin = Just (GitHubUsername authorLogin),
                    _webhookEventTimeSpent = Nothing,
                    _webhookEventStatus = EventSkipped,
                    _webhookEventErrorMessage = Just "No 'Time Spent:' found in PR body",
                    _webhookEventWorkEventId = Nothing,
                    _webhookEventUserId = Nothing,
                    _webhookEventUserCreated = False,
                    _webhookEventReceivedAt = now,
                    _webhookEventProcessedAt = Just now
                  }
          runDB $ void $ recordWebhookEvent repoLinkId ev
          pure NoContent
        Just duration -> do
          let projectId = _repoLinkProjectId repoLink
              timeSpent = durationToNDT duration

          -- Look up merger by GitHub username
          mMerger <- runDB $ runMaybeT $ findUserByGitHubUsername (GitHubUsername mergerLogin)

          -- Look up author by GitHub username
          mAuthor <- runDB $ runMaybeT $ findUserByGitHubUsername (GitHubUsername authorLogin)

          -- Determine if merger is project member and process accordingly
          case mMerger of
            Nothing -> do
              -- Merger not linked to any account - check if author is a member
              case mAuthor of
                Just (authorUid, _) -> do
                  -- Author is known, credit them
                  createWorkEvents projectId authorUid timeSpent mergedAt prUrl prTitle prNumber now
                  let ev =
                        GitHubWebhookEvent
                          { _webhookEventDeliveryId = deliveryId,
                            _webhookEventType = "pull_request",
                            _webhookEventPRNumber = Just prNumber,
                            _webhookEventAuthorEmail = Nothing,
                            _webhookEventAuthorLogin = Just (GitHubUsername authorLogin),
                            _webhookEventTimeSpent = Just timeSpent,
                            _webhookEventStatus = EventProcessed,
                            _webhookEventErrorMessage = Nothing,
                            _webhookEventWorkEventId = Nothing,
                            _webhookEventUserId = Just authorUid,
                            _webhookEventUserCreated = False,
                            _webhookEventReceivedAt = now,
                            _webhookEventProcessedAt = Just now
                          }
                  runDB $ void $ recordWebhookEvent repoLinkId ev
                  pure NoContent
                Nothing -> do
                  -- Neither merger nor author is known - skip
                  let ev =
                        GitHubWebhookEvent
                          { _webhookEventDeliveryId = deliveryId,
                            _webhookEventType = "pull_request",
                            _webhookEventPRNumber = Just prNumber,
                            _webhookEventAuthorEmail = Nothing,
                            _webhookEventAuthorLogin = Just (GitHubUsername authorLogin),
                            _webhookEventTimeSpent = Just timeSpent,
                            _webhookEventStatus = EventSkipped,
                            _webhookEventErrorMessage = Just "Merger not project member and author not found",
                            _webhookEventWorkEventId = Nothing,
                            _webhookEventUserId = Nothing,
                            _webhookEventUserCreated = False,
                            _webhookEventReceivedAt = now,
                            _webhookEventProcessedAt = Just now
                          }
                  runDB $ void $ recordWebhookEvent repoLinkId ev
                  pure NoContent
            Just (mergerUid, _) -> do
              -- Merger is known, check if they're a project member
              -- If author is known, credit them; otherwise create provisional user
              case mAuthor of
                Just (authorUid, _) -> do
                  -- Author is known, credit them
                  createWorkEvents projectId authorUid timeSpent mergedAt prUrl prTitle prNumber now
                  let ev =
                        GitHubWebhookEvent
                          { _webhookEventDeliveryId = deliveryId,
                            _webhookEventType = "pull_request",
                            _webhookEventPRNumber = Just prNumber,
                            _webhookEventAuthorEmail = Nothing,
                            _webhookEventAuthorLogin = Just (GitHubUsername authorLogin),
                            _webhookEventTimeSpent = Just timeSpent,
                            _webhookEventStatus = EventProcessed,
                            _webhookEventErrorMessage = Nothing,
                            _webhookEventWorkEventId = Nothing,
                            _webhookEventUserId = Just authorUid,
                            _webhookEventUserCreated = False,
                            _webhookEventReceivedAt = now,
                            _webhookEventProcessedAt = Just now
                          }
                  runDB $ void $ recordWebhookEvent repoLinkId ev
                  pure NoContent
                Nothing -> do
                  -- Author not found - create provisional user
                  let provisionalUser =
                        User
                          { _username = UserName $ "github_" <> authorLogin,
                            _userAccountRecovery = RecoverByEmail (Email $ authorLogin <> "@github.placeholder")
                          }
                  authorUid <- runDB $ createUser provisionalUser
                  -- Link their GitHub username
                  runDB $ linkGitHubUsername authorUid (GitHubUsername authorLogin)
                  -- Add them to the project (merger must be a member)
                  runDB $ addUserToProject projectId mergerUid authorUid
                  -- Credit the work
                  createWorkEvents projectId authorUid timeSpent mergedAt prUrl prTitle prNumber now
                  let ev =
                        GitHubWebhookEvent
                          { _webhookEventDeliveryId = deliveryId,
                            _webhookEventType = "pull_request",
                            _webhookEventPRNumber = Just prNumber,
                            _webhookEventAuthorEmail = Nothing,
                            _webhookEventAuthorLogin = Just (GitHubUsername authorLogin),
                            _webhookEventTimeSpent = Just timeSpent,
                            _webhookEventStatus = EventProcessed,
                            _webhookEventErrorMessage = Nothing,
                            _webhookEventWorkEventId = Nothing,
                            _webhookEventUserId = Just authorUid,
                            _webhookEventUserCreated = True,
                            _webhookEventReceivedAt = now,
                            _webhookEventProcessedAt = Just now
                          }
                  runDB $ void $ recordWebhookEvent repoLinkId ev
                  pure NoContent

-- | Create StartWork and StopWork events for credited work
createWorkEvents ::
  ProjectId ->
  UserId ->
  C.NominalDiffTime ->
  C.UTCTime ->
  Text ->
  Text ->
  Int ->
  C.UTCTime ->
  AppM ()
createWorkEvents projectId userId duration mergedAt prUrl prTitle prNumber _now = do
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
  -- Create both events
  _ <- runDB $ createEvent projectId userId startEntry
  _ <- runDB $ createEvent projectId userId stopEntry
  pure ()

-- | Verify HMAC-SHA256 signature
verifySignature :: Text -> Text -> ByteString -> Bool
verifySignature secret signature payload =
  let secretBytes = T.encodeUtf8 secret
      expectedSig = hmac secretBytes payload :: HMAC SHA256
      expectedHex = "sha256=" <> T.decodeUtf8 (B16.encode $ BA.convert expectedSig)
   in signature == expectedHex

-- | Parse ISO8601 timestamp
parseISO8601 :: Text -> Maybe C.UTCTime
parseISO8601 t =
  toThyme <$> Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (T.unpack t)

-- | Project-level GitHub management server (protected)
gitHubProjectServer ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  ServerT GitHubProjectAPI AppM
gitHubProjectServer authResult pid =
  listRepoLinksHandler authResult pid
    :<|> linkRepoHandler authResult pid
    :<|> unlinkRepoHandler authResult pid

-- | Check if user is a member of the project
checkMembership :: UserId -> ProjectId -> AppM ()
checkMembership uid pid = do
  projects <- runDB $ findUserProjects uid
  unless (any (\(p, _) -> p == pid) projects) $
    throwError err403 {errBody = "User is not a member of this project"}

-- | List all repos linked to a project
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

-- | Link a repo to a project
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

-- | Unlink a repo from a project
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

-- | User-level GitHub username linking server (protected)
gitHubUserServer ::
  AuthResult AuthenticatedUser ->
  ServerT GitHubUserAPI AppM
gitHubUserServer authResult =
  getGitHubUsernameHandler authResult
    :<|> linkGitHubUsernameHandler authResult
    :<|> unlinkGitHubUsernameHandler authResult

-- | Get the user's linked GitHub username
getGitHubUsernameHandler ::
  AuthResult AuthenticatedUser ->
  AppM GitHubUsernameResponse
getGitHubUsernameHandler (Authenticated user) = do
  let uid = auUserId user
  mUsername <- runDB $ getUserGitHubUsername uid
  pure $ GitHubUsernameResponse $ fmap (\(GitHubUsername u) -> u) mUsername
getGitHubUsernameHandler _ =
  throwError err401 {errBody = "Authentication required"}

-- | Link a GitHub username to the user's account
linkGitHubUsernameHandler ::
  AuthResult AuthenticatedUser ->
  LinkGitHubUsernameRequest ->
  AppM NoContent
linkGitHubUsernameHandler (Authenticated user) req = do
  let uid = auUserId user
      ghUsername = GitHubUsername (lgurUsername req)
  runDB $ linkGitHubUsername uid ghUsername
  pure NoContent
linkGitHubUsernameHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Unlink the GitHub username from the user's account
unlinkGitHubUsernameHandler ::
  AuthResult AuthenticatedUser ->
  AppM NoContent
unlinkGitHubUsernameHandler (Authenticated user) = do
  let uid = auUserId user
  runDB $ unlinkGitHubUsername uid
  pure NoContent
unlinkGitHubUsernameHandler _ =
  throwError err401 {errBody = "Authentication required"}
