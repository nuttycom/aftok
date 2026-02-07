{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.WorkLog
  ( -- * API Types (re-exported from aftok-api)
    WorkLogAPI,
    LogStartRequest (..),
    LogEndRequest (..),
    EventAmendmentRequest (..),
    EventAmendmentType (..),

    -- * Handlers
    workLogServer,

    -- * JSON helpers
    logEventJSON,
    logEntryFields,
    keyedLogEntryJSON,
    extendedLogEntryJSON,
    workIndexJSON,
    amendEventResultJSON,
  )
where

import Aftok.API.WorkLog
  ( EventAmendmentRequest (..),
    EventAmendmentType (..),
    LogEndRequest (..),
    LogStartRequest (..),
    WorkLogAPI,
  )
import Aftok.Database
  ( KeyedLogEntry (..),
    Limit (..),
    amendEvent,
    createEvent,
    findEvent,
    findEvents,
  )
import Aftok.Interval
  ( Interval (..),
    RangeQuery (..),
    intervalJSON,
  )
import Aftok.Json (creditToJSON, idValue)
import Aftok.Servant.App (AppM, runDB)
import Aftok.Servant.Auth (AuthenticatedUser (..))
import Aftok.TimeLog
  ( AmendmentId,
    EventAmendment (..),
    EventId (..),
    LogEntry (LogEntry),
    LogEvent (StartWork, StopWork),
    ModTime (..),
    WorkIndex (..),
    eventName,
    eventTime,
    logEntry,
    workIndex,
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
import Control.Lens (view, (^.))
import Data.Aeson
  ( ToJSON (..),
    Value,
    object,
    (.=),
  )
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Pair)
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as MS
import qualified Data.Thyme.Clock as C
import qualified Data.UUID as U
import Servant
import Servant.Auth.Server (AuthResult (..))

-- | WorkLog server implementation
workLogServer ::
  AuthResult AuthenticatedUser ->
  ServerT WorkLogAPI AppM
workLogServer authResult =
  projectWorkLogServer authResult
    :<|> amendEventHandler authResult

-- | Project-specific work log handlers
projectWorkLogServer ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  ServerT
    ( "logStart" :> ReqBody '[JSON] LogStartRequest :> Post '[JSON] Value
        :<|> "logEnd" :> ReqBody '[JSON] LogEndRequest :> Post '[JSON] Value
        :<|> "events"
          :> QueryParam "after" C.UTCTime
          :> QueryParam "before" C.UTCTime
          :> QueryParam "limit" Int
          :> Get '[JSON] Value
        :<|> "workIndex" :> Get '[JSON] Value
    )
    AppM
projectWorkLogServer authResult pid =
  logStartHandler authResult pid
    :<|> logEndHandler authResult pid
    :<|> userEventsHandler authResult pid
    :<|> userWorkIndexHandler authResult pid

-- | Log start of work
logStartHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  LogStartRequest ->
  AppM Value
logStartHandler (Authenticated user) pid req = do
  let uid = auUserId user
      creditTo' = fromMaybe (CreditToUser uid) (lsrCreditTo req)
  timestamp <- liftIO C.getCurrentTime
  let entry = LogEntry creditTo' (StartWork timestamp) (lsrEventMeta req)
  eid <- runDB $ createEvent pid uid entry
  ev <- runDB $ findEvent eid
  maybe
    (throwError err500 {errBody = "Failed to retrieve newly created event"})
    (\(pid', uid', kle) -> pure $ extendedLogEntryJSON (pid', uid', kle))
    ev
logStartHandler _ _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Log end of work
logEndHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  LogEndRequest ->
  AppM Value
logEndHandler (Authenticated user) pid req = do
  let uid = auUserId user
      creditTo' = fromMaybe (CreditToUser uid) (lerCreditTo req)
  timestamp <- liftIO C.getCurrentTime
  let entry = LogEntry creditTo' (StopWork timestamp) (lerEventMeta req)
  eid <- runDB $ createEvent pid uid entry
  ev <- runDB $ findEvent eid
  maybe
    (throwError err500 {errBody = "Failed to retrieve newly created event"})
    (\(pid', uid', kle) -> pure $ extendedLogEntryJSON (pid', uid', kle))
    ev
logEndHandler _ _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Get user events for a project
userEventsHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  Maybe C.UTCTime ->
  Maybe C.UTCTime ->
  Maybe Int ->
  AppM Value
userEventsHandler (Authenticated user) pid afterTime beforeTime limitParam = do
  let uid = auUserId user
      rangeQuery = case (afterTime, beforeTime) of
        (Just s, Just e) -> During s e
        (Nothing, Just e) -> Before e
        (Just s, Nothing) -> After s
        (Nothing, Nothing) -> Always
      limit = Limit $ maybe 1 fromIntegral limitParam
  events <- runDB $ findEvents pid uid rangeQuery limit
  pure $ toJSON $ fmap keyedLogEntryJSON events
userEventsHandler _ _ _ _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Get user's work index for a project
userWorkIndexHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  AppM Value
userWorkIndexHandler (Authenticated user) pid = do
  let uid = auUserId user
      rangeQuery = Always
      limit = Limit 1000 -- reasonable default
  events <- runDB $ findEvents pid uid rangeQuery limit
  let widx = workIndex (view logEntry) events
  pure $ workIndexJSON keyedLogEntryJSON widx
userWorkIndexHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Amend an event
amendEventHandler ::
  AuthResult AuthenticatedUser ->
  Text ->
  EventAmendmentRequest ->
  AppM Value
amendEventHandler (Authenticated user) eventIdText req = do
  let uid = auUserId user
  eventId <- case U.fromText eventIdText of
    Nothing -> throwError err400 {errBody = "Invalid event ID"}
    Just uuid -> pure $ EventId uuid
  modTime <- ModTime <$> liftIO C.getCurrentTime
  let amendment = case earAmendment req of
        TimeChangeReq t -> TimeChange modTime t
        CreditToChangeReq c -> CreditToChange modTime c
        MetadataChangeReq m -> MetadataChange modTime m
  result <- runDB $ amendEvent uid eventId amendment
  pure $ amendEventResultJSON result
amendEventHandler _ _ _ =
  throwError err401 {errBody = "Authentication required"}

--------------------------------------------------------------------------------
-- JSON Serializers
--------------------------------------------------------------------------------

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

extendedLogEntryJSON :: (ProjectId, UserId, KeyedLogEntry) -> Value
extendedLogEntryJSON (pid, uid, le) =
  object $
    [ "projectId" .= idValue _ProjectId pid,
      "loggedBy" .= idValue _UserId uid
    ]
      <> keyedLogEntryFields le

workIndexJSON :: forall t. (t -> Value) -> WorkIndex t -> Value
workIndexJSON leJSON (WorkIndex widx) =
  object ["workIndex" .= fmap widxRec (MS.assocs widx)]
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
