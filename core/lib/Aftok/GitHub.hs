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
    webhookEventAuthorEmail,
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
  )
where

import Aftok.TimeLog (EventId)
import Aftok.Types
  ( GitHubUsername (..),
    ProjectId,
    UserId,
  )
import Control.Lens (makeLenses)
import Data.Attoparsec.Text as A
import qualified Data.Char as Char
import qualified Data.Text as T
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

-- | Status of a processed webhook event
data GitHubEventStatus
  = EventProcessed
  | EventSkipped
  | EventError
  deriving (Show, Eq)

-- | A record of a GitHub webhook event for auditing/idempotency
data GitHubWebhookEvent = GitHubWebhookEvent
  { _webhookEventDeliveryId :: !Text,
    _webhookEventType :: !Text,
    _webhookEventPRNumber :: !(Maybe Int),
    _webhookEventAuthorEmail :: !(Maybe Text),
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

-- | Parse "2h30m", "2h 30m", "2 hours 30 minutes", etc.
hoursMinutesParser :: Parser Duration
hoursMinutesParser = do
  h <- option 0 hoursComponent
  skipSpace
  m <- option 0 minutesComponent
  endOfInput
  if h == 0 && m == 0
    then fail "Duration must be non-zero"
    else pure $ Duration h m

-- | Parse hours component: "2h", "2 hours", "2 hour"
hoursComponent :: Parser Int
hoursComponent = do
  n <- decimal
  skipSpace
  _ <- string "h" <|> string "hours" <|> string "hour"
  pure n

-- | Parse minutes component: "30m", "30 minutes", "30 minute"
minutesComponent :: Parser Int
minutesComponent = do
  n <- decimal
  skipSpace
  _ <- string "m" <|> string "minutes" <|> string "minute"
  pure n

-- | Parse decimal hours: "2.5h", "2.5 hours"
decimalHoursParser :: Parser Duration
decimalHoursParser = do
  n <- double
  skipSpace
  _ <- string "h" <|> string "hours" <|> string "hour"
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
  _ <- string "m" <|> string "minutes" <|> string "minute"
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
