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

import qualified Aftok.Billing as B
import Aftok.Database
  ( DBError (EventStorageFailed),
    DBOp
      ( CreateBillable,
        CreatePayment,
        CreateSubscription,
        StorePaymentRequest
      ),
    KeyedLogEntry (KeyedLogEntry),
    Limit (..),
    logEntry,
    workId,
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
    pexec,
    pinsert,
    pquery,
    ptransact,
    utcParser,
  )
import Aftok.Interval
import Aftok.Json
  ( billableJSON,
    idValue,
    obj,
    v1,
  )
import Aftok.Payments.Types
import Aftok.TimeLog
  ( AmendmentId (..),
    EventAmendment (..),
    EventId (..),
    LogEntry (LogEntry),
    LogEvent (..),
    WorkIndex,
    _AmendmentId,
    _EventId,
    _ModTime,
    creditTo,
    event,
    eventMeta,
    eventName,
    eventTime,
    nameEvent,
    workIndex,
  )
import Aftok.Types
import Control.Lens ((^.), _Just, preview, set, view)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson
  ( (.=),
    Value,
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
  KeyedLogEntry <$> idParser EventId <*> logEntryParser

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

createSubscriptionJSON :: UserId -> B.BillableId -> Day -> Value
createSubscriptionJSON uid bid d =
  v1 $
    obj
      [ "user_id" .= idValue _UserId uid,
        "billable_id" .= idValue B._BillableId bid,
        "start_date" .= showGregorian d
      ]

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

findEvent :: EventId -> DBM (Maybe (ProjectId, UserId, KeyedLogEntry))
findEvent (EventId eid) = do
  headMay
    <$> pquery
      ((,,) <$> idParser ProjectId <*> idParser UserId <*> keyedLogEntryParser)
      [sql| SELECT project_id, user_id, id,
                 credit_to_type, credit_to_account, credit_to_user_id, credit_to_project_id,
                 event_type, event_time, event_metadata
            FROM work_events
            WHERE id = ?
            AND replacement_id IS NULL
            |]
      (Only eid)

findEvents :: ProjectId -> UserId -> RangeQuery -> Limit -> DBM [KeyedLogEntry]
findEvents (ProjectId pid) (UserId uid) rquery (Limit limit) = do
  case rquery of
    (Before e) ->
      pquery
        keyedLogEntryParser
        [sql| SELECT id, credit_to_type,
                     credit_to_account, credit_to_user_id, credit_to_project_id,
                     event_type, event_time,
                     event_metadata
              FROM work_events
              WHERE project_id = ? AND user_id = ? AND event_time <= ?
              AND replacement_id IS NULL
              ORDER BY event_time DESC
              LIMIT ?
              |]
        (pid, uid, fromThyme e, limit)
    (During s e) ->
      pquery
        keyedLogEntryParser
        [sql| SELECT id, credit_to_type,
                     credit_to_account, credit_to_user_id, credit_to_project_id,
                     event_type, event_time, event_metadata
              FROM work_events
              WHERE project_id = ? AND user_id = ?
              AND replacement_id IS NULL
              AND event_time >= ? AND event_time <= ?
              ORDER BY event_time DESC
              LIMIT ?
              |]
        (pid, uid, fromThyme s, fromThyme e, limit)
    (After s) ->
      pquery
        keyedLogEntryParser
        [sql| SELECT id, credit_to_type,
                     credit_to_account, credit_to_user_id, credit_to_project_id,
                     event_type, event_time, event_metadata
              FROM work_events
              WHERE project_id = ? AND user_id = ? AND event_time >= ?
              AND replacement_id IS NULL
              ORDER BY event_time DESC
              LIMIT ?
              |]
        (pid, uid, fromThyme s, limit)
    (Always) ->
      pquery
        keyedLogEntryParser
        [sql| SELECT id, credit_to_type,
                     credit_to_account, credit_to_user_id, credit_to_project_id,
                     event_type, event_time, event_metadata
              FROM work_events
              WHERE project_id = ? AND user_id = ?
              AND replacement_id IS NULL
              ORDER BY event_time DESC
              LIMIT ?
              |]
        (pid, uid, limit)

readWorkIndex :: ProjectId -> DBM (WorkIndex KeyedLogEntry)
readWorkIndex (ProjectId pid) = do
  logEntries <-
    pquery
      keyedLogEntryParser
      [sql| SELECT id, credit_to_type,
                 credit_to_account, credit_to_user_id, credit_to_project_id,
                 event_type, event_time, event_metadata
          FROM work_events
          WHERE project_id = ? |]
      (Only pid)
  pure $ workIndex (view logEntry) logEntries

amendEvent :: ProjectId -> UserId -> KeyedLogEntry -> EventAmendment -> DBM (EventId, AmendmentId)
amendEvent pid uid kle amendment = ptransact $ do
  (amendId, replacement, amend_t :: Text) <- amend
  newEventId <- createEvent pid uid (replacement ^. logEntry)
  void $
    pexec
      [sql| UPDATE work_events
          SET replacement_id = ?, amended_by_id = ?, amended_by_type = ?
          WHERE id = ? |]
      (newEventId ^. _EventId, amendId ^. _AmendmentId, amend_t, kle ^. workId . _EventId)
  pure (newEventId, amendId)
  where
    amend = case amendment of
      (TimeChange mt t) -> do
        aid <-
          pinsert
            AmendmentId
            [sql| INSERT INTO event_time_amendments
                  (work_event_id, amended_at, event_time)
                  VALUES (?, ?, ?) RETURNING id |]
            (kle ^. workId . _EventId, fromThyme $ mt ^. _ModTime, fromThyme t)
        pure (aid, set (logEntry . event . eventTime) t kle, "amend_event_time")
      (CreditToChange mt c@(CreditToAccount acctId)) -> do
        aid <-
          pinsert
            AmendmentId
            [sql| INSERT INTO event_credit_to_amendments
                  (work_event_id, amended_at, credit_to_type, credit_to_account)
                  VALUES (?, ?, ?, ?) RETURNING id |]
            (kle ^. workId . _EventId, fromThyme $ mt ^. _ModTime, creditToName c, acctId ^. _AccountId)
        pure (aid, set (logEntry . creditTo) c kle, "amend_credit_to")
      (CreditToChange mt c@(CreditToProject cpid)) -> do
        aid <-
          pinsert
            AmendmentId
            [sql| INSERT INTO event_credit_to_amendments
                  (work_event_id, amended_at, credit_to_type, credit_to_project_id)
                  VALUES (?, ?, ?, ?) RETURNING id |]
            (kle ^. workId . _EventId, fromThyme $ mt ^. _ModTime, creditToName c, cpid ^. _ProjectId)
        pure (aid, set (logEntry . creditTo) c kle, "amend_credit_to")
      (CreditToChange mt c@(CreditToUser cuid)) -> do
        aid <-
          pinsert
            AmendmentId
            [sql| INSERT INTO event_credit_to_amendments
                  (work_event_id, amended_at, credit_to_type, credit_to_user_id)
                  VALUES (?, ?, ?, ?) RETURNING id |]
            (kle ^. workId . _EventId, fromThyme $ mt ^. _ModTime, creditToName c, cuid ^. _UserId)
        pure (aid, set (logEntry . creditTo) c kle, "amend_credit_to")
      (MetadataChange mt v) -> do
        aid <-
          pinsert
            AmendmentId
            [sql| INSERT INTO event_metadata_amendments
                  (work_event_id, amended_at, event_metadata)
                  VALUES (?, ?, ?) RETURNING id |]
            (kle ^. workId . _EventId, fromThyme $ mt ^. _ModTime, v)
        pure (aid, set (logEntry . eventMeta) (Just v) kle, "amend_metadata")
