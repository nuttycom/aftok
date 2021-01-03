{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Aftok.Database.PostgreSQL.Events
  ( storeEvent,
    storeEvent',
    createEvent,
    findEvent,
    findEvents,
    amendEvent,
    readWorkIndex,
  )
where

import Aftok.Database
  ( DBError (EventStorageFailed),
    DBOp
      ( CreateBillable,
        CreatePayment,
        CreateSubscription,
        StorePaymentRequest
      ),
    KeyedLogEntry,
    Limit(..),
  )
import Aftok.Database.PostgreSQL.Json
  ( nativeRequestJSON,
    paymentJSON,
  )
import Aftok.Database.PostgreSQL.Types
  ( DBM,
    creditToName,
    creditToParser,
    idParser,
    pinsert,
    pquery,
    utcParser,
  )
import Aftok.Interval
import Aftok.Json
  ( billableJSON,
    createSubscriptionJSON,
  )
import Aftok.Payments.Types
import Aftok.TimeLog
import Aftok.Types
import Control.Lens ((^.), _Just, preview)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson
  ( Value,
  )
import Data.Thyme.Clock as C
import Data.Thyme.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
  ( sql,
  )
import Safe (headMay)
import Prelude hiding (null)

eventTypeParser :: FieldParser (C.UTCTime -> LogEvent)
eventTypeParser f v = do
  tn <- typename f
  if tn /= "event_t"
    then returnError Incompatible f "column was not of type event_t"
    else
      maybe
        (returnError UnexpectedNull f "event type may not be null")
        ( maybe (returnError Incompatible f "unrecognized event type value") pure
            . nameEvent
            . decodeUtf8
        )
        v

logEntryParser :: RowParser LogEntry
logEntryParser =
  LogEntry
    <$> creditToParser
    <*> (fieldWith eventTypeParser <*> utcParser)
    <*> field

keyedLogEntryParser :: RowParser KeyedLogEntry
keyedLogEntryParser =
  (,,) <$> idParser ProjectId <*> idParser UserId <*> logEntryParser

storeEvent :: DBOp a -> Maybe (DBM EventId)
storeEvent = \case
  (CreateBillable uid b) ->
    Just $ storeEventJSON (Just uid) "create_billable" (billableJSON b)
  (CreateSubscription uid bid t) ->
    Just $
      storeEventJSON
        (Just uid)
        "create_subscription"
        (createSubscriptionJSON uid bid t)
  (StorePaymentRequest req) ->
    Just $
      storeEventJSON Nothing "create_payment_request" (nativeRequestJSON (req ^. nativeRequest))
  (CreatePayment p) ->
    Just $ do
      nmode <- asks fst
      storeEventJSON Nothing "create_payment" (paymentJSON nmode p)
  _ -> Nothing

storeEvent' :: DBOp a -> DBM EventId
storeEvent' = maybe (lift $ throwE EventStorageFailed) id . storeEvent

type EventType = Text

storeEventJSON :: Maybe UserId -> EventType -> Value -> DBM EventId
storeEventJSON uid etype v = do
  timestamp <- liftIO C.getCurrentTime
  pinsert
    EventId
    [sql| INSERT INTO aftok_events
          (event_time, created_by, event_type, event_json)
          VALUES (?, ?, ?, ?) RETURNING id |]
    (fromThyme timestamp, preview (_Just . _UserId) uid, etype, v)

createEvent :: ProjectId -> UserId -> LogEntry -> DBM EventId
createEvent (ProjectId pid) (UserId uid) (LogEntry c e m) = case c of
  CreditToAccount aid' -> do
    pinsert
      EventId
      [sql| INSERT INTO work_events
              ( project_id, user_id, credit_to_type, credit_to_account,
              , event_type, event_time, event_metadata )
              VALUES (?, ?, ?, ?, ?, ?, ?)
              RETURNING id |]
      ( pid,
        uid,
        creditToName c,
        aid' ^. _AccountId,
        eventName e,
        fromThyme $ e ^. eventTime,
        m
      )
  CreditToProject pid' ->
    pinsert
      EventId
      [sql| INSERT INTO work_events
              ( project_id, user_id, credit_to_type, credit_to_project_id
              , event_type, event_time, event_metadata )
              VALUES (?, ?, ?, ?, ?, ?, ?)
              RETURNING id |]
      ( pid,
        uid,
        creditToName c,
        pid' ^. _ProjectId,
        eventName e,
        fromThyme $ e ^. eventTime,
        m
      )
  CreditToUser uid' ->
    pinsert
      EventId
      [sql| INSERT INTO work_events
              ( project_id, user_id, credit_to_type, credit_to_user_id
              , event_type, event_time, event_metadata)
              VALUES (?, ?, ?, ?, ?, ?, ?)
              RETURNING id |]
      ( pid,
        uid,
        creditToName c,
        uid' ^. _UserId,
        eventName e,
        fromThyme $ e ^. eventTime,
        m
      )

findEvent :: EventId -> DBM (Maybe KeyedLogEntry)
findEvent (EventId eid) = do
  headMay
    <$> pquery
      keyedLogEntryParser
      [sql| SELECT project_id, user_id,
                 credit_to_type, credit_to_account, credit_to_user_id, credit_to_project_id,
                 event_type, event_time, event_metadata FROM work_events
          WHERE id = ? |]
      (Only eid)

findEvents :: ProjectId -> UserId -> RangeQuery -> Limit -> DBM [LogEntry]
findEvents (ProjectId pid) (UserId uid) rquery (Limit limit) = do
  case rquery of
    (Before e) ->
      pquery
        logEntryParser
        [sql| SELECT credit_to_type,
                   credit_to_account, credit_to_user_id, credit_to_project_id,
                   event_type, event_time,
                   event_metadata
            FROM work_events
            WHERE project_id = ? AND user_id = ? AND event_time <= ?
            ORDER BY event_time DESC
            LIMIT ?
            |]
        (pid, uid, fromThyme e, limit)
    (During s e) ->
      pquery
        logEntryParser
        [sql| SELECT credit_to_type,
                   credit_to_account, credit_to_user_id, credit_to_project_id,
                   event_type, event_time, event_metadata
            FROM work_events
            WHERE project_id = ? AND user_id = ?
            AND event_time >= ? AND event_time <= ?
            ORDER BY event_time DESC
            LIMIT ?
            |]
        (pid, uid, fromThyme s, fromThyme e, limit)
    (After s) ->
      pquery
        logEntryParser
        [sql| SELECT credit_to_type,
                   credit_to_account, credit_to_user_id, credit_to_project_id,
                   event_type, event_time, event_metadata
            FROM work_events
            WHERE project_id = ? AND user_id = ? AND event_time >= ?
            ORDER BY event_time DESC
            LIMIT ?
            |]
        (pid, uid, fromThyme s, limit)
    (Always) ->
      pquery
        logEntryParser
        [sql| SELECT credit_to_type,
                   credit_to_account, credit_to_user_id, credit_to_project_id,
                   event_type, event_time, event_metadata
            FROM work_events
            WHERE project_id = ? AND user_id = ?
            ORDER BY event_time DESC
            LIMIT ?
            |]
        (pid, uid, limit)

amendEvent :: EventId -> EventAmendment -> DBM AmendmentId
amendEvent (EventId eid) = \case
  (TimeChange mt t) ->
    pinsert
      AmendmentId
      [sql| INSERT INTO event_time_amendments
              (event_id, amended_at, event_time)
              VALUES (?, ?, ?) RETURNING id |]
      (eid, fromThyme $ mt ^. _ModTime, fromThyme t)
  (CreditToChange mt c@(CreditToAccount acctId)) ->
    pinsert
      AmendmentId
      [sql| INSERT INTO event_credit_to_amendments
            (event_id, amended_at, credit_to_type, credit_to_account)
            VALUES (?, ?, ?, ?) RETURNING id |]
      (eid, fromThyme $ mt ^. _ModTime, creditToName c, acctId ^. _AccountId)
  (CreditToChange mt c@(CreditToProject pid)) ->
    pinsert
      AmendmentId
      [sql| INSERT INTO event_credit_to_amendments
              (event_id, amended_at, credit_to_type, credit_to_project_id)
              VALUES (?, ?, ?, ?) RETURNING id |]
      (eid, fromThyme $ mt ^. _ModTime, creditToName c, pid ^. _ProjectId)
  (CreditToChange mt c@(CreditToUser uid)) ->
    pinsert
      AmendmentId
      [sql| INSERT INTO event_credit_to_amendments
              (event_id, amended_at, credit_to_type, credit_to_user_id)
              VALUES (?, ?, ?, ?) RETURNING id |]
      (eid, fromThyme $ mt ^. _ModTime, creditToName c, uid ^. _UserId)
  (MetadataChange mt v) ->
    pinsert
      AmendmentId
      [sql| INSERT INTO event_metadata_amendments
              (event_id, amended_at, event_metadata)
              VALUES (?, ?, ?) RETURNING id |]
      (eid, fromThyme $ mt ^. _ModTime, v)

readWorkIndex :: ProjectId -> DBM WorkIndex
readWorkIndex (ProjectId pid) = do
  logEntries <-
    pquery
      logEntryParser
      [sql| SELECT credit_to_type,
                 credit_to_account, credit_to_user_id, credit_to_project_id,
                 event_type, event_time, event_metadata
          FROM work_events
          WHERE project_id = ? |]
      (Only pid)
  pure $ workIndex logEntries
