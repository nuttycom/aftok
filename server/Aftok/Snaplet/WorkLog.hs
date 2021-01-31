{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Aftok.Snaplet.WorkLog where

import Aftok.Database
import Aftok.Interval
  ( Interval (..),
    intervalJSON,
  )
import Aftok.Json
import Aftok.Snaplet
import Aftok.Snaplet.Auth
import Aftok.Snaplet.Util
import Aftok.TimeLog
  ( AmendmentId,
    EventAmendment (..),
    EventId (..),
    LogEntry (LogEntry),
    LogEvent,
    ModTime (..),
    WorkIndex (..),
    _AmendmentId,
    _EventId,
    eventName,
    eventTime,
    workIndex,
  )
import Aftok.Types
  ( CreditTo (..),
    ProjectId,
    UserId,
    _ProjectId,
    _UserId,
  )
import Control.Lens ((^.), view)
import Data.Aeson ((.:), (.=), Value (Object), eitherDecode, object)
import Data.Aeson.Types (Pair, Parser, parseEither)
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as MS
import qualified Data.Text as T
import Data.Thyme.Clock as C
import Data.UUID as U
import Snap.Core
import Snap.Snaplet as S

----------------------
-- Handlers
----------------------

logWorkHandler ::
  (C.UTCTime -> LogEvent) ->
  S.Handler App App (ProjectId, UserId, KeyedLogEntry)
logWorkHandler evCtr = do
  uid <- requireUserId
  pid <- requireProjectId
  requestBody <- readRequestBody 4096
  timestamp <- liftIO C.getCurrentTime
  case (eitherDecode requestBody >>= parseEither (parseLogEntry uid evCtr)) of
    Left err ->
      snapError 400 $
        "Unable to parse log entry "
          <> (show requestBody)
          <> ": "
          <> show err
    Right entry -> do
      eid <- snapEval $ createEvent pid uid (entry timestamp)
      ev <- snapEval $ findEvent eid
      maybe
        ( snapError 500 $
            "An error occured retrieving the newly created event record."
        )
        pure
        ev

amendEventHandler :: S.Handler App App (EventId, AmendmentId)
amendEventHandler = do
  uid <- requireUserId
  eventIdBytes <- getParam "eventId"
  eventId <-
    maybe
      (snapError 400 "eventId parameter is required")
      (pure . EventId)
      (eventIdBytes >>= U.fromASCIIBytes)
  modTime <- ModTime <$> liftIO C.getCurrentTime
  requestJSON <- readRequestJSON 4096
  either
    (snapError 400 . T.pack)
    (snapEval . amendEvent uid eventId)
    (parseEither (parseEventAmendment modTime) requestJSON)

projectWorkIndex :: S.Handler App App (WorkIndex KeyedLogEntry)
projectWorkIndex = do
  uid <- requireUserId
  pid <- requireProjectId
  snapEval $ readWorkIndex pid uid

userEvents :: S.Handler App App [KeyedLogEntry]
userEvents = do
  uid <- requireUserId
  pid <- requireProjectId
  ival <- rangeQueryParam
  limit <- Limit . fromMaybe 1 <$> decimalParam "limit"
  snapEval $ findEvents pid uid ival limit

userWorkIndex :: S.Handler App App (WorkIndex KeyedLogEntry)
userWorkIndex = workIndex (view logEntry) <$> userEvents

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
  object [eventName ev .= object ["eventTime" .= (ev ^. eventTime)]]

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
