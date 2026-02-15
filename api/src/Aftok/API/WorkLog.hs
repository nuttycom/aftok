{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import Aftok.API.Codec ()
import Aftok.API.Types ()
import Aftok.Json (parseCreditToV2)
import Aftok.TimeLog (AmendmentId (..), EventId (..), LogEvent (..))
import Aftok.Types (CreditTo (..), ProjectId, UserId)
import Autodocodec (HasCodec (..), object, optionalField', requiredField')
import qualified Autodocodec as AC
import Autodocodec.Aeson (parseJSONViaCodec, toJSONViaCodec)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (Object),
    (.:),
  )
import qualified Data.Text as T
import qualified Data.Thyme.Clock as C
import Data.Thyme.Format.Aeson ()
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

instance FromJSON LogStartRequest where parseJSON = parseJSONViaCodec

instance HasCodec LogStartRequest where
  codec =
    object "LogStartRequest" $
      LogStartRequest
        <$> optionalField' "creditTo" AC..= lsrCreditTo
        <*> optionalField' "eventMeta" AC..= lsrEventMeta

-- | Log end request
data LogEndRequest = LogEndRequest
  { lerCreditTo :: Maybe CreditTo,
    lerEventMeta :: Maybe Value
  }
  deriving (Generic)

instance FromJSON LogEndRequest where parseJSON = parseJSONViaCodec

instance HasCodec LogEndRequest where
  codec =
    object "LogEndRequest" $
      LogEndRequest
        <$> optionalField' "creditTo" AC..= lerCreditTo
        <*> optionalField' "eventMeta" AC..= lerEventMeta

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

instance HasCodec ExtendedLogEntryResponse where
  codec =
    object "ExtendedLogEntryResponse" $
      ExtendedLogEntryResponse
        <$> requiredField' "projectId" AC..= elrProjectId
        <*> requiredField' "loggedBy" AC..= elrLoggedBy
        <*> requiredField' "eventId" AC..= elrEventId
        <*> requiredField' "creditTo" AC..= elrCreditTo
        <*> requiredField' "event" AC..= elrEvent
        <*> requiredField' "eventMeta" AC..= elrEventMeta

instance ToJSON ExtendedLogEntryResponse where toJSON = toJSONViaCodec

-- | Response for events list items
data KeyedLogEntryResponse = KeyedLogEntryResponse
  { klrEventId :: EventId,
    klrCreditTo :: CreditTo,
    klrEvent :: LogEvent,
    klrEventMeta :: Maybe Value
  }

instance HasCodec KeyedLogEntryResponse where
  codec =
    object "KeyedLogEntryResponse" $
      KeyedLogEntryResponse
        <$> requiredField' "eventId" AC..= klrEventId
        <*> requiredField' "creditTo" AC..= klrCreditTo
        <*> requiredField' "event" AC..= klrEvent
        <*> requiredField' "eventMeta" AC..= klrEventMeta

instance ToJSON KeyedLogEntryResponse where toJSON = toJSONViaCodec

-- | Work index response
data WorkIndexResponse = WorkIndexResponse
  { wirWorkIndex :: [WorkIndexEntry]
  }

instance HasCodec WorkIndexResponse where
  codec =
    object "WorkIndexResponse" $
      WorkIndexResponse
        <$> requiredField' "workIndex" AC..= wirWorkIndex

instance ToJSON WorkIndexResponse where toJSON = toJSONViaCodec

-- | Work index entry (per credit-to)
data WorkIndexEntry = WorkIndexEntry
  { wieCreditTo :: CreditTo,
    wieIntervals :: [IntervalResponse]
  }

instance HasCodec WorkIndexEntry where
  codec =
    object "WorkIndexEntry" $
      WorkIndexEntry
        <$> requiredField' "creditTo" AC..= wieCreditTo
        <*> requiredField' "intervals" AC..= wieIntervals

instance ToJSON WorkIndexEntry where toJSON = toJSONViaCodec

-- | Interval in the work index
data IntervalResponse = IntervalResponse
  { irStart :: KeyedLogEntryResponse,
    irEnd :: KeyedLogEntryResponse
  }

instance HasCodec IntervalResponse where
  codec =
    object "IntervalResponse" $
      IntervalResponse
        <$> requiredField' "start" AC..= irStart
        <*> requiredField' "end" AC..= irEnd

instance ToJSON IntervalResponse where toJSON = toJSONViaCodec

-- | Event amendment result
data AmendEventResponse = AmendEventResponse
  { aerReplacementEvent :: EventId,
    aerAmendmentId :: AmendmentId
  }

instance HasCodec AmendEventResponse where
  codec =
    object "AmendEventResponse" $
      AmendEventResponse
        <$> requiredField' "replacement_event" AC..= aerReplacementEvent
        <*> requiredField' "amendment_id" AC..= aerAmendmentId

instance ToJSON AmendEventResponse where toJSON = toJSONViaCodec
