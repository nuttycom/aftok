{-# OPTIONS_GHC -Wwarn -fno-warn-orphans #-}

module Aftok.Http.Json where

import Aftok.Database
import Aftok.Interval
  ( Interval (..),
    intervalJSON,
  )
import Aftok.Json
import Aftok.TimeLog
  ( AmendmentId,
    EventAmendment (..),
    EventId (..),
    LogEntry (LogEntry),
    LogEvent,
    ModTime (..),
    WorkIndex (..),
    eventName,
    eventTime,
    _AmendmentId,
    _EventId,
  )
import Aftok.Types
  ( CreditTo (..),
    ProjectId,
    UserId,
    _ProjectId,
    _UserId,
  )
import Control.Lens ((^.))
import Data.Aeson (ToJSON, Value (Object), object, toJSON, (.:), (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Pair, Parser)
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as MS
import qualified Data.Text as T

----------------------
-- Parsing
----------------------

parseEventAmendment ::
  ModTime ->
  Value ->
  Parser EventAmendment
parseEventAmendment t = \case
  Object o ->
    let parseA :: Text -> Parser EventAmendment
        parseA "timeChange" = TimeChange t <$> o .: "eventTime"
        parseA "creditToChange" = CreditToChange t <$> parseCreditToV2 o
        parseA "metadataChange" = MetadataChange t <$> o .: "eventMeta"
        parseA tid =
          fail . T.unpack $ "Amendment type " <> tid <> " not recognized."
     in o .: "amendment" >>= parseA
  val ->
    fail $ "Value " <> show val <> " is not a JSON object."

----------------------
-- Rendering
----------------------

logEventJSON :: LogEvent -> Value
logEventJSON ev =
  object [fromText (eventName ev) .= object ["eventTime" .= (ev ^. eventTime)]]

logEntryFields :: LogEntry -> [Pair]
logEntryFields (LogEntry c ev m) =
  [ "creditTo" .= creditToJSON c,
    "event" .= logEventJSON ev,
    "eventMeta" .= m
  ]

keyedLogEntryFields :: KeyedLogEntry -> [Pair]
keyedLogEntryFields (KeyedLogEntry eid le) =
  ["eventId" .= idValue _EventId eid] <> logEntryFields le

keyedLogEntryJSON :: KeyedLogEntry -> Value
keyedLogEntryJSON kle =
  object (keyedLogEntryFields kle)

instance ToJSON KeyedLogEntry where
  toJSON = keyedLogEntryJSON

extendedLogEntryJSON :: (ProjectId, UserId, KeyedLogEntry) -> Value
extendedLogEntryJSON (pid, uid, le) =
  v1
    . obj
    $ [ "projectId" .= idValue _ProjectId pid,
        "loggedBy" .= idValue _UserId uid
      ]
      <> keyedLogEntryFields le

workIndexJSON :: forall t. (t -> Value) -> WorkIndex t -> Value
workIndexJSON leJSON (WorkIndex widx) =
  v1 $
    obj ["workIndex" .= fmap widxRec (MS.assocs widx)]
  where
    widxRec :: (CreditTo, NonEmpty (Interval t)) -> Value
    widxRec (c, l) =
      object
        [ "creditTo" .= creditToJSON c,
          "intervals" .= (intervalJSON leJSON <$> L.toList l)
        ]

amendEventResultJSON :: (EventId, AmendmentId) -> Value
amendEventResultJSON (eid, aid) =
  object
    [ "replacement_event" .= idValue _EventId eid,
      "amendment_id" .= idValue _AmendmentId aid
    ]
