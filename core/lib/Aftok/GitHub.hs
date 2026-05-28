{-# LANGUAGE TemplateHaskell #-}

module Aftok.GitHub
  ( -- * Types
    GitHubRepoLink (..),
    repoLinkProjectId,
    repoLinkOwner,
    repoLinkRepo,
    repoLinkWebhookSecret,
    repoLinkCreatedBy,
    repoLinkCreatedAt,
    GitHubEventStatus (..),
    GitHubWebhookEvent (..),
    webhookEventDeliveryId,
    webhookEventType,
    webhookEventPRNumber,
    webhookEventAuthorLogin,
    webhookEventTimeSpent,
    webhookEventStatus,
    webhookEventErrorMessage,
    webhookEventWorkEventId,
    webhookEventUserId,
    webhookEventUserCreated,
    webhookEventReceivedAt,
    webhookEventProcessedAt,

    -- * Duration parsing
    Duration (..),
    durationHours,
    durationMinutes,
    parseDuration,
    parseTimeSpent,
    durationToNDT,

    -- * Username normalization
    normalizeGitHubUsername,

    -- * Skip reasons
    SkipReason (..),
    skipReasonText,

    -- * Webhook signature verification
    verifySignature,
  )
where

import Aftok.TimeLog (EventId)
import Aftok.Types
  ( GitHubUsername (..),
    ProjectId,
    UserId,
  )
import Control.Lens (makeLenses)
import qualified Crypto.Hash as Hash
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.Attoparsec.Text as A
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Thyme.Clock as C

-- | A link between a GitHub repository and an Aftok project
data GitHubRepoLink = GitHubRepoLink
  { _repoLinkProjectId :: !ProjectId,
    _repoLinkOwner :: !Text,
    _repoLinkRepo :: !Text,
    _repoLinkWebhookSecret :: !Text,
    _repoLinkCreatedBy :: !UserId,
    _repoLinkCreatedAt :: !C.UTCTime
  }
  deriving (Show, Eq)

makeLenses ''GitHubRepoLink

-- | Status of a processed webhook event.
data GitHubEventStatus
  = -- | The delivery is currently being processed (transient claim row,
    -- updated to a terminal status when the handler finishes).
    EventProcessing
  | EventProcessed
  | EventSkipped
  | EventError
  deriving (Show, Eq)

-- | A record of a GitHub webhook event for auditing/idempotency
data GitHubWebhookEvent = GitHubWebhookEvent
  { _webhookEventDeliveryId :: !Text,
    _webhookEventType :: !Text,
    _webhookEventPRNumber :: !(Maybe Int),
    _webhookEventAuthorLogin :: !(Maybe GitHubUsername),
    _webhookEventTimeSpent :: !(Maybe C.NominalDiffTime),
    _webhookEventStatus :: !GitHubEventStatus,
    _webhookEventErrorMessage :: !(Maybe Text),
    _webhookEventWorkEventId :: !(Maybe EventId),
    _webhookEventUserId :: !(Maybe UserId),
    _webhookEventUserCreated :: !Bool,
    _webhookEventReceivedAt :: !C.UTCTime,
    _webhookEventProcessedAt :: !(Maybe C.UTCTime)
  }
  deriving (Show, Eq)

makeLenses ''GitHubWebhookEvent

-- | A duration in hours and minutes
data Duration = Duration
  { _durationHours :: !Int,
    _durationMinutes :: !Int
  }
  deriving (Show, Eq)

makeLenses ''Duration

-- | Convert a Duration to NominalDiffTime
durationToNDT :: Duration -> C.NominalDiffTime
durationToNDT (Duration h m) = C.fromSeconds' $ toRational $ (h * 3600) + (m * 60)

-- | Parse a duration string in various formats:
-- - "2h30m", "2h 30m", "2h", "30m"
-- - "2 hours 30 minutes", "2 hours", "30 minutes"
-- - "2.5h", "2.5 hours"
-- - "150m", "150 minutes"
parseDuration :: Text -> Maybe Duration
parseDuration input =
  case parseOnly durationParser (T.strip $ T.toLower input) of
    Right d -> Just d
    Left _ -> Nothing

-- | Parser for duration values
durationParser :: Parser Duration
durationParser =
  hoursMinutesParser
    <|> decimalHoursParser
    <|> totalMinutesParser

-- | Parse "2h30m", "2h 30m", "2 hours 30 minutes", etc. Also tolerates a
-- single component being absent (just hours, just minutes). Any minutes
-- value >= 60 is carried into hours so the resulting 'Duration' is
-- normalized.
hoursMinutesParser :: Parser Duration
hoursMinutesParser = do
  h <- option 0 hoursComponent
  skipSpace
  m <- option 0 minutesComponent
  endOfInput
  let totalMinutes = (h * 60) + m
      h' = totalMinutes `div` 60
      m' = totalMinutes `mod` 60
  if totalMinutes == 0
    then fail "Duration must be non-zero"
    else pure $ Duration h' m'

-- | Parse hours component: "2h", "2 hours", "2 hour". Longest-match-first
-- alternation is required because attoparsec's 'string' commits on
-- partial matches; matching "h" first would leave "ours" unconsumed.
hoursComponent :: Parser Int
hoursComponent = do
  n <- decimal
  skipSpace
  _ <- string "hours" <|> string "hour" <|> string "h"
  pure n

-- | Parse minutes component: "30m", "30 minutes", "30 minute"
minutesComponent :: Parser Int
minutesComponent = do
  n <- decimal
  skipSpace
  _ <- string "minutes" <|> string "minute" <|> string "m"
  pure n

-- | Parse decimal hours: "2.5h", "2.5 hours"
decimalHoursParser :: Parser Duration
decimalHoursParser = do
  n <- double
  skipSpace
  _ <- string "hours" <|> string "hour" <|> string "h"
  endOfInput
  let totalMinutes = round (n * 60)
      h = totalMinutes `div` 60
      m = totalMinutes `mod` 60
  pure $ Duration h m

-- | Parse total minutes: "150m", "150 minutes"
totalMinutesParser :: Parser Duration
totalMinutesParser = do
  n <- decimal
  skipSpace
  _ <- string "minutes" <|> string "minute" <|> string "m"
  endOfInput
  let h = n `div` 60
      m = n `mod` 60
  pure $ Duration h m

-- | Extract "Time Spent: <duration>" from a PR body (case-insensitive)
-- Returns the parsed duration if found
parseTimeSpent :: Text -> Maybe Duration
parseTimeSpent body =
  let patterns = ["time spent:", "time-spent:", "timespent:"]
   in case mapMaybe tryPattern patterns of
        (d : _) -> Just d
        [] -> Nothing
  where
    tryPattern pat =
      let (_, after) = T.breakOn pat (T.toLower body)
       in if T.null after
            then Nothing
            else
              let -- Extract the duration text after the pattern
                  afterPattern = T.drop (T.length pat) after
                  -- Take characters until end of line or next whitespace block
                  durationText = T.strip $ T.takeWhile (not . isEndOfDuration) $ T.dropWhile Char.isSpace afterPattern
               in parseDuration durationText

    isEndOfDuration c = c == '\n' || c == '\r'

-- | Normalize a GitHub username to its canonical form. GitHub treats
-- usernames case-insensitively, so we lowercase at every store and lookup
-- site.
normalizeGitHubUsername :: Text -> Text
normalizeGitHubUsername = T.toLower

-- | Structured reasons a webhook delivery is recorded but not turned into
-- work events. Stored in the audit log via 'skipReasonText'.
data SkipReason
  = -- | The webhook payload did not parse to the expected PR-event shape.
    SRInvalidPayload
  | -- | A pull_request event whose action/merged state is uninteresting
    -- (only closed-and-merged PRs are processed).
    SRPRNotMerged !Text !Bool
  | -- | The PR body did not contain a parseable "Time Spent:" marker.
    SRNoTimeSpent
  | -- | The PR merger and author are both unknown to Aftok, so no user
    -- can be credited.
    SRUnknownAuthor
  deriving (Show, Eq)

-- | Render a 'SkipReason' as audit-log text.
skipReasonText :: SkipReason -> Text
skipReasonText = \case
  SRInvalidPayload -> "Could not parse PR info from payload"
  SRPRNotMerged action merged ->
    "PR action=" <> action <> ", merged=" <> (if merged then "True" else "False")
  SRNoTimeSpent -> "No 'Time Spent:' found in PR body"
  SRUnknownAuthor -> "Merger not project member and author not found"

-- | Verify a GitHub-style HMAC-SHA256 signature header (the
-- @X-Hub-Signature-256@ value, of the form @sha256=<hex>@) against the
-- raw request body bytes. The body MUST be the exact bytes GitHub sent;
-- re-encoding parsed JSON yields a different byte sequence and fails
-- this check.
--
-- The comparison is performed in constant time on the raw digest bytes
-- (after stripping the @sha256=@ prefix and hex-decoding the provided
-- signature), not on hex-encoded strings.
verifySignature :: Text -> Text -> ByteString -> Bool
verifySignature secret signature payload =
  let secretBytes = T.encodeUtf8 secret
      digest = hmac secretBytes payload :: HMAC Hash.SHA256
      expectedBytes = BA.convert (hmacGetDigest digest) :: ByteString
      sigBs = T.encodeUtf8 signature
   in case BS.stripPrefix "sha256=" sigBs of
        Nothing -> False
        Just hexPart ->
          case B16.decode hexPart of
            Right providedBytes
              | BS.length providedBytes == BS.length expectedBytes ->
                  BA.constEq providedBytes expectedBytes
            _ -> False
