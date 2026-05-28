{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Aftok.Database.PostgreSQL.GitHub
  ( -- * Repo link operations
    createGitHubRepoLink,
    findGitHubRepoLink,
    findProjectGitHubRepoLinks,
    deleteGitHubRepoLink,

    -- * Webhook event tracking
    recordWebhookEvent,
    isDeliveryProcessed,
    claimWebhookDelivery,
    finalizeWebhookDelivery,
  )
where

import Aftok.Database.PostgreSQL.Types
  ( DBM,
    idParser,
    pexec,
    pinsert,
    pquery,
    utcParser,
  )
import Aftok.GitHub
  ( GitHubEventStatus (..),
    GitHubRepoLink (..),
    GitHubWebhookEvent (..),
    repoLinkCreatedBy,
    repoLinkOwner,
    repoLinkProjectId,
    repoLinkRepo,
    repoLinkWebhookSecret,
  )
import Aftok.TimeLog (EventId (..))
import Aftok.Types
  ( GitHubRepoLinkId (..),
    GitHubUsername (..),
    GitHubWebhookEventId (..),
    ProjectId (..),
    UserId (..),
    _ProjectId,
    _UserId,
  )
import Control.Lens ((^.))
import qualified Data.Thyme.Clock as C
import Data.Thyme.Time.Core (fromThyme)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Safe (headMay)

-- | Create a new GitHub repo link
createGitHubRepoLink :: GitHubRepoLink -> DBM GitHubRepoLinkId
createGitHubRepoLink link =
  pinsert
    GitHubRepoLinkId
    [sql| INSERT INTO github_repo_links (project_id, github_owner, github_repo, webhook_secret, created_by)
          VALUES (?, ?, ?, ?, ?) RETURNING id |]
    ( link ^. repoLinkProjectId . _ProjectId,
      link ^. repoLinkOwner,
      link ^. repoLinkRepo,
      link ^. repoLinkWebhookSecret,
      link ^. repoLinkCreatedBy . _UserId
    )

-- | Find a GitHub repo link by owner/repo
findGitHubRepoLink :: Text -> Text -> DBM (Maybe (GitHubRepoLinkId, GitHubRepoLink))
findGitHubRepoLink owner repo =
  headMay
    <$> pquery
      repoLinkParser
      [sql| SELECT id, project_id, github_owner, github_repo, webhook_secret, created_by, created_at
            FROM github_repo_links
            WHERE github_owner = ? AND github_repo = ? |]
      (owner, repo)

-- | Find all GitHub repo links for a project
findProjectGitHubRepoLinks :: ProjectId -> DBM [(GitHubRepoLinkId, GitHubRepoLink)]
findProjectGitHubRepoLinks (ProjectId pid) =
  pquery
    repoLinkParser
    [sql| SELECT id, project_id, github_owner, github_repo, webhook_secret, created_by, created_at
          FROM github_repo_links
          WHERE project_id = ? |]
    (Only pid)

-- | Delete a GitHub repo link
deleteGitHubRepoLink :: GitHubRepoLinkId -> DBM ()
deleteGitHubRepoLink (GitHubRepoLinkId linkId) =
  void $
    pexec
      [sql| DELETE FROM github_repo_links WHERE id = ? |]
      (Only linkId)

-- | Parser for GitHubRepoLink rows
repoLinkParser :: RowParser (GitHubRepoLinkId, GitHubRepoLink)
repoLinkParser = do
  linkId <- idParser GitHubRepoLinkId
  projectId <- idParser ProjectId
  owner <- field
  repo <- field
  secret <- field
  createdBy <- idParser UserId
  createdAt <- utcParser
  pure
    ( linkId,
      GitHubRepoLink
        { _repoLinkProjectId = projectId,
          _repoLinkOwner = owner,
          _repoLinkRepo = repo,
          _repoLinkWebhookSecret = secret,
          _repoLinkCreatedBy = createdBy,
          _repoLinkCreatedAt = createdAt
        }
    )

-- | Record a webhook event for auditing/idempotency.
-- This is used by the legacy non-claim path (no concurrent-delivery race
-- protection). New code should use 'claimWebhookDelivery' followed by
-- 'finalizeWebhookDelivery'.
recordWebhookEvent :: GitHubRepoLinkId -> GitHubWebhookEvent -> DBM GitHubWebhookEventId
recordWebhookEvent (GitHubRepoLinkId linkId) ev =
  pinsert
    GitHubWebhookEventId
    [sql| INSERT INTO github_webhook_events
          (repo_link_id, github_delivery_id, event_type, pr_number,
           pr_author_github_login, time_spent_parsed, status, error_message,
           work_event_id, user_id, user_created, received_at, processed_at)
          VALUES (?, ?, ?, ?, ?, ?, ?::github_event_status, ?, ?, ?, ?, ?, ?)
          RETURNING id |]
    ( linkId,
      _webhookEventDeliveryId ev,
      _webhookEventType ev,
      _webhookEventPRNumber ev,
      fmap (\(GitHubUsername u) -> u) (_webhookEventAuthorLogin ev),
      fmap (round . C.toSeconds' :: C.NominalDiffTime -> Int64) (_webhookEventTimeSpent ev),
      statusToText (_webhookEventStatus ev),
      _webhookEventErrorMessage ev,
      fmap (\(EventId eid) -> eid) (_webhookEventWorkEventId ev),
      fmap (\(UserId uid) -> uid) (_webhookEventUserId ev),
      _webhookEventUserCreated ev,
      fromThyme $ _webhookEventReceivedAt ev,
      fmap fromThyme (_webhookEventProcessedAt ev)
    )

-- | Atomically claim a webhook delivery for processing. Inserts a row in
-- the 'processing' state and returns the row id on first claim; returns
-- 'Nothing' if some other concurrent caller already inserted the row for
-- the same delivery id. This is the idempotency guard against duplicate
-- deliveries racing to create work events.
claimWebhookDelivery ::
  GitHubRepoLinkId ->
  -- | delivery id (GitHub's X-GitHub-Delivery)
  Text ->
  -- | event type (X-GitHub-Event)
  Text ->
  -- | received-at
  C.UTCTime ->
  DBM (Maybe GitHubWebhookEventId)
claimWebhookDelivery (GitHubRepoLinkId linkId) deliveryId eventType receivedAt = do
  rows <-
    pquery
      (idParser GitHubWebhookEventId)
      [sql| INSERT INTO github_webhook_events
              (repo_link_id, github_delivery_id, event_type, status, received_at)
            VALUES (?, ?, ?, 'processing'::github_event_status, ?)
            ON CONFLICT (github_delivery_id) DO NOTHING
            RETURNING id |]
      (linkId, deliveryId, eventType, fromThyme receivedAt)
  pure $ headMay rows

-- | Update a previously claimed webhook delivery row to its final state.
finalizeWebhookDelivery :: GitHubWebhookEventId -> GitHubWebhookEvent -> DBM ()
finalizeWebhookDelivery (GitHubWebhookEventId rowId) ev =
  void $
    pexec
      [sql| UPDATE github_webhook_events
            SET pr_number = ?,
                pr_author_github_login = ?,
                time_spent_parsed = ?,
                status = ?::github_event_status,
                error_message = ?,
                work_event_id = ?,
                user_id = ?,
                user_created = ?,
                processed_at = ?
            WHERE id = ? |]
      ( _webhookEventPRNumber ev,
        fmap (\(GitHubUsername u) -> u) (_webhookEventAuthorLogin ev),
        fmap (round . C.toSeconds' :: C.NominalDiffTime -> Int64) (_webhookEventTimeSpent ev),
        statusToText (_webhookEventStatus ev),
        _webhookEventErrorMessage ev,
        fmap (\(EventId eid) -> eid) (_webhookEventWorkEventId ev),
        fmap (\(UserId uid) -> uid) (_webhookEventUserId ev),
        _webhookEventUserCreated ev,
        fmap fromThyme (_webhookEventProcessedAt ev),
        rowId
      )

-- | Check if a delivery has already been processed
isDeliveryProcessed :: Text -> DBM Bool
isDeliveryProcessed deliveryId = do
  results <-
    pquery
      (field :: RowParser Bool)
      [sql| SELECT EXISTS(
              SELECT 1 FROM github_webhook_events
              WHERE github_delivery_id = ?
            ) |]
      (Only deliveryId)
  pure $ fromMaybe False (headMay results)

-- | Convert event status to database text representation
statusToText :: GitHubEventStatus -> Text
statusToText = \case
  EventProcessing -> "processing"
  EventProcessed -> "processed"
  EventSkipped -> "skipped"
  EventError -> "error"
