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
  )
where

import Aftok.API.WorkLog
  ( AmendEventResponse (..),
    EventAmendmentRequest (..),
    EventAmendmentType (..),
    ExtendedLogEntryResponse (..),
    IntervalResponse (..),
    KeyedLogEntryResponse (..),
    LogEndRequest (..),
    LogStartRequest (..),
    WorkIndexEntry (..),
    WorkIndexResponse (..),
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
  )
import Aftok.Servant.App (AppM, runDB)
import Aftok.Servant.Auth (AuthenticatedUser (..))
import Aftok.TimeLog
  ( EventAmendment (..),
    EventId (..),
    LogEntry (LogEntry),
    LogEvent (StartWork, StopWork),
    ModTime (..),
    WorkIndex (..),
    logEntry,
    workIndex,
  )
import Aftok.Types
  ( CreditTo (..),
    ProjectId,
    UserId,
  )
import Control.Lens (view)
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
    ( "logStart" :> ReqBody '[JSON] LogStartRequest :> Post '[JSON] ExtendedLogEntryResponse
        :<|> "logEnd" :> ReqBody '[JSON] LogEndRequest :> Post '[JSON] ExtendedLogEntryResponse
        :<|> "events"
          :> QueryParam "after" C.UTCTime
          :> QueryParam "before" C.UTCTime
          :> QueryParam "limit" Int
          :> Get '[JSON] [KeyedLogEntryResponse]
        :<|> "workIndex" :> Get '[JSON] WorkIndexResponse
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
  AppM ExtendedLogEntryResponse
logStartHandler (Authenticated user) pid req = do
  let uid = auUserId user
      creditTo' = fromMaybe (CreditToUser uid) (lsrCreditTo req)
  timestamp <- liftIO C.getCurrentTime
  let entry = LogEntry creditTo' (StartWork timestamp) (lsrEventMeta req)
  eid <- runDB $ createEvent pid uid entry
  ev <- runDB $ findEvent eid
  maybe
    (throwError err500 {errBody = "Failed to retrieve newly created event"})
    (\(pid', uid', kle) -> pure $ toExtendedLogEntryResponse pid' uid' kle)
    ev
logStartHandler _ _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Log end of work
logEndHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  LogEndRequest ->
  AppM ExtendedLogEntryResponse
logEndHandler (Authenticated user) pid req = do
  let uid = auUserId user
      creditTo' = fromMaybe (CreditToUser uid) (lerCreditTo req)
  timestamp <- liftIO C.getCurrentTime
  let entry = LogEntry creditTo' (StopWork timestamp) (lerEventMeta req)
  eid <- runDB $ createEvent pid uid entry
  ev <- runDB $ findEvent eid
  maybe
    (throwError err500 {errBody = "Failed to retrieve newly created event"})
    (\(pid', uid', kle) -> pure $ toExtendedLogEntryResponse pid' uid' kle)
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
  AppM [KeyedLogEntryResponse]
userEventsHandler (Authenticated user) pid afterTime beforeTime limitParam = do
  let uid = auUserId user
      rangeQuery = case (afterTime, beforeTime) of
        (Just s, Just e) -> During s e
        (Nothing, Just e) -> Before e
        (Just s, Nothing) -> After s
        (Nothing, Nothing) -> Always
      limit = Limit $ maybe 1 fromIntegral limitParam
  events <- runDB $ findEvents pid uid rangeQuery limit
  pure $ fmap toKeyedLogEntryResponse events
userEventsHandler _ _ _ _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Get user's work index for a project
userWorkIndexHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  AppM WorkIndexResponse
userWorkIndexHandler (Authenticated user) pid = do
  let uid = auUserId user
      rangeQuery = Always
      limit = Limit 1000 -- reasonable default
  events <- runDB $ findEvents pid uid rangeQuery limit
  let widx = workIndex (view logEntry) events
  pure $ toWorkIndexResponse widx
userWorkIndexHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Amend an event
amendEventHandler ::
  AuthResult AuthenticatedUser ->
  Text ->
  EventAmendmentRequest ->
  AppM AmendEventResponse
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
  (eid, aid) <- runDB $ amendEvent uid eventId amendment
  pure $ AmendEventResponse eid aid
amendEventHandler _ _ _ =
  throwError err401 {errBody = "Authentication required"}

--------------------------------------------------------------------------------
-- Response Constructors
--------------------------------------------------------------------------------

-- | Convert a KeyedLogEntry to a KeyedLogEntryResponse
toKeyedLogEntryResponse :: KeyedLogEntry -> KeyedLogEntryResponse
toKeyedLogEntryResponse (KeyedLogEntry eid (LogEntry ct ev meta)) =
  KeyedLogEntryResponse
    { klrEventId = eid,
      klrCreditTo = ct,
      klrEvent = ev,
      klrEventMeta = meta
    }

-- | Convert to an ExtendedLogEntryResponse
toExtendedLogEntryResponse :: ProjectId -> UserId -> KeyedLogEntry -> ExtendedLogEntryResponse
toExtendedLogEntryResponse pid uid (KeyedLogEntry eid (LogEntry ct ev meta)) =
  ExtendedLogEntryResponse
    { elrProjectId = pid,
      elrLoggedBy = uid,
      elrEventId = eid,
      elrCreditTo = ct,
      elrEvent = ev,
      elrEventMeta = meta
    }

-- | Convert a WorkIndex to a WorkIndexResponse
toWorkIndexResponse :: WorkIndex KeyedLogEntry -> WorkIndexResponse
toWorkIndexResponse (WorkIndex widx) =
  WorkIndexResponse
    { wirWorkIndex = fmap toEntry (MS.assocs widx)
    }
  where
    toEntry :: (CreditTo, NonEmpty (Interval KeyedLogEntry)) -> WorkIndexEntry
    toEntry (ct, ivals) =
      WorkIndexEntry
        { wieCreditTo = ct,
          wieIntervals = fmap toIntervalResponse (L.toList ivals)
        }

    toIntervalResponse :: Interval KeyedLogEntry -> IntervalResponse
    toIntervalResponse (Interval s e) =
      IntervalResponse
        { irStart = toKeyedLogEntryResponse s,
          irEnd = toKeyedLogEntryResponse e
        }
