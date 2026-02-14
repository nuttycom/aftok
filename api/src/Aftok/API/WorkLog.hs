{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | WorkLog API types for the Aftok API.
module Aftok.API.WorkLog
  ( -- * API Types
    WorkLogAPI,

    -- * Request Types
    LogStartRequest (..),
    LogEndRequest (..),
    EventAmendmentRequest (..),
    EventAmendmentType (..),

    -- * Response Types
    ExtendedLogEntryResponse (..),
    KeyedLogEntryResponse (..),
    WorkIndexResponse (..),
    WorkIndexEntry (..),
    IntervalResponse (..),
    AmendEventResponse (..),
  )
where

import Aftok.API.Types ()
import Aftok.Json (creditToJSON, parseCreditToV2)
import Aftok.TimeLog (AmendmentId (..), EventId (..), LogEvent (..), eventName, eventTime)
import Aftok.Types (CreditTo (..), ProjectId, UserId)
import Control.Lens ((^.))
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (Object),
    object,
    (.:),
    (.:?),
    (.=),
  )
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Pair)
import qualified Data.Text as T
import qualified Data.Thyme.Clock as C
import Data.Thyme.Format.Aeson ()
import qualified Data.UUID as UUID
import Servant.API

-- | WorkLog API for user-specific project operations
type WorkLogAPI =
  "user"
    :> "projects"
    :> Capture "projectId" ProjectId
    :> ( -- POST /user/projects/:projectId/logStart
         "logStart" :> ReqBody '[JSON] LogStartRequest :> Post '[JSON] ExtendedLogEntryResponse
           -- POST /user/projects/:projectId/logEnd
           :<|> "logEnd" :> ReqBody '[JSON] LogEndRequest :> Post '[JSON] ExtendedLogEntryResponse
           -- GET /user/projects/:projectId/events
           :<|> "events"
             :> QueryParam "after" C.UTCTime
             :> QueryParam "before" C.UTCTime
             :> QueryParam "limit" Int
             :> Get '[JSON] [KeyedLogEntryResponse]
           -- GET /user/projects/:projectId/workIndex
           :<|> "workIndex" :> Get '[JSON] WorkIndexResponse
       )
    -- Event amendment
    :<|> "events"
      :> Capture "eventId" Text
      :> "amend"
      :> ReqBody '[JSON] EventAmendmentRequest
      :> Put '[JSON] AmendEventResponse

-- | Log start request
data LogStartRequest = LogStartRequest
  { lsrCreditTo :: Maybe CreditTo,
    lsrEventMeta :: Maybe Value
  }
  deriving (Generic)

instance FromJSON LogStartRequest where
  parseJSON (Object o) = do
    creditTo' <- o .:? "creditTo" >>= maybe (pure Nothing) (fmap Just . parseCreditToV2)
    eventMeta' <- o .:? "eventMeta"
    pure $ LogStartRequest creditTo' eventMeta'
  parseJSON _ = mzero

-- | Log end request
data LogEndRequest = LogEndRequest
  { lerCreditTo :: Maybe CreditTo,
    lerEventMeta :: Maybe Value
  }
  deriving (Generic)

instance FromJSON LogEndRequest where
  parseJSON (Object o) = do
    creditTo' <- o .:? "creditTo" >>= maybe (pure Nothing) (fmap Just . parseCreditToV2)
    eventMeta' <- o .:? "eventMeta"
    pure $ LogEndRequest creditTo' eventMeta'
  parseJSON _ = mzero

-- | Event amendment request
data EventAmendmentRequest = EventAmendmentRequest
  { earAmendment :: EventAmendmentType
  }
  deriving (Generic)

-- | Event amendment type
data EventAmendmentType
  = TimeChangeReq C.UTCTime
  | CreditToChangeReq CreditTo
  | MetadataChangeReq Value

instance FromJSON EventAmendmentRequest where
  parseJSON (Object o) = do
    amendType <- o .: "amendment"
    amendment <- case (amendType :: Text) of
      "timeChange" -> TimeChangeReq <$> o .: "eventTime"
      "creditToChange" -> CreditToChangeReq <$> parseCreditToV2 o
      "metadataChange" -> MetadataChangeReq <$> o .: "eventMeta"
      other -> fail $ "Amendment type " <> T.unpack other <> " not recognized."
    pure $ EventAmendmentRequest amendment
  parseJSON val = fail $ "Value " <> show val <> " is not a JSON object."

--------------------------------------------------------------------------------
-- Response Types
--------------------------------------------------------------------------------

-- | Response for logStart/logEnd (extended log entry with project/user context)
data ExtendedLogEntryResponse = ExtendedLogEntryResponse
  { elrProjectId :: ProjectId,
    elrLoggedBy :: UserId,
    elrEventId :: EventId,
    elrCreditTo :: CreditTo,
    elrEvent :: LogEvent,
    elrEventMeta :: Maybe Value
  }

instance ToJSON ExtendedLogEntryResponse where
  toJSON r =
    object $
      [ "projectId" .= elrProjectId r,
        "loggedBy" .= elrLoggedBy r
      ]
        <> keyedLogEntryFields (elrEventId r) (elrCreditTo r) (elrEvent r) (elrEventMeta r)

-- | Response for events list items
data KeyedLogEntryResponse = KeyedLogEntryResponse
  { klrEventId :: EventId,
    klrCreditTo :: CreditTo,
    klrEvent :: LogEvent,
    klrEventMeta :: Maybe Value
  }

instance ToJSON KeyedLogEntryResponse where
  toJSON r =
    object $ keyedLogEntryFields (klrEventId r) (klrCreditTo r) (klrEvent r) (klrEventMeta r)

-- | Work index response
data WorkIndexResponse = WorkIndexResponse
  { wirWorkIndex :: [WorkIndexEntry]
  }

instance ToJSON WorkIndexResponse where
  toJSON r = object ["workIndex" .= wirWorkIndex r]

-- | Work index entry (per credit-to)
data WorkIndexEntry = WorkIndexEntry
  { wieCreditTo :: CreditTo,
    wieIntervals :: [IntervalResponse]
  }

instance ToJSON WorkIndexEntry where
  toJSON r =
    object
      [ "creditTo" .= creditToJSON (wieCreditTo r),
        "intervals" .= wieIntervals r
      ]

-- | Interval in the work index
data IntervalResponse = IntervalResponse
  { irStart :: KeyedLogEntryResponse,
    irEnd :: KeyedLogEntryResponse
  }

instance ToJSON IntervalResponse where
  toJSON r = object ["start" .= irStart r, "end" .= irEnd r]

-- | Event amendment result
data AmendEventResponse = AmendEventResponse
  { aerReplacementEvent :: EventId,
    aerAmendmentId :: AmendmentId
  }

instance ToJSON AmendEventResponse where
  toJSON r =
    object
      [ "replacement_event" .= (let EventId u = aerReplacementEvent r in UUID.toText u),
        "amendment_id" .= (let AmendmentId u = aerAmendmentId r in UUID.toText u)
      ]

--------------------------------------------------------------------------------
-- Serialization Helpers
--------------------------------------------------------------------------------

-- | Serialize a log event to JSON
logEventToJSON :: LogEvent -> Value
logEventToJSON ev =
  object [fromText (eventName ev) .= object ["eventTime" .= (ev ^. eventTime)]]

-- | Common fields for keyed log entries
keyedLogEntryFields :: EventId -> CreditTo -> LogEvent -> Maybe Value -> [Pair]
keyedLogEntryFields eid ct ev meta =
  [ "eventId" .= (let EventId u = eid in UUID.toText u),
    "creditTo" .= creditToJSON ct,
    "event" .= logEventToJSON ev,
    "eventMeta" .= meta
  ]
