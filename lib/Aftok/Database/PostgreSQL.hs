{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aftok.Database.PostgreSQL (QDBM(), runQDBM) where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Trans.Either
import           Data.Aeson                           (Value, toJSON)
import           Data.Hourglass
import qualified Data.List                            as L
import           Data.ProtocolBuffers                 (decodeMessage,
                                                       encodeMessage)
import           Data.Serialize.Get                   (runGet)
import           Data.Serialize.Put                   (runPut)
import           Data.Thyme.Clock                     as C
import           Data.Thyme.Time
import           Data.UUID                            (UUID)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types     (Null)

import           Network.Haskoin.Crypto               (addrToBase58)

import           Aftok
import qualified Aftok.Auction                        as A
import qualified Aftok.Billables                      as B
import           Aftok.Database
import           Aftok.Interval
import           Aftok.Json                           (billableJSON,
                                                       createSubscriptionJSON,
                                                       paymentJSON,
                                                       paymentRequestJSON)
import           Aftok.Payments.Types
import qualified Aftok.Project                        as P
import           Aftok.TimeLog
import           Aftok.Types

newtype QDBM a = QDBM (ReaderT Connection (EitherT DBError IO) a)
  deriving (Functor, Applicative, Monad)

instance MonadIO QDBM where
  liftIO = QDBM . lift . lift

runQDBM :: Connection -> QDBM a -> EitherT DBError IO a
runQDBM conn (QDBM r) = runReaderT r conn

idParser :: (UUID -> a) -> RowParser a
idParser f = f <$> field

btcAddrParser :: FieldParser BtcAddr
btcAddrParser f v = do
  addrMay <- parseBtcAddr <$> fromField f v
  let err = returnError ConversionFailed f "could not deserialize value to a valid BTC address"
  maybe err pure addrMay

btcParser :: RowParser Satoshi
btcParser = (Satoshi . fromInteger) <$> field

utcParser :: RowParser C.UTCTime
utcParser = toThyme <$> field

nullField :: RowParser Null
nullField = field

eventTypeParser :: FieldParser (C.UTCTime -> LogEvent)
eventTypeParser f v = do
  tn <- typename f
  if tn /= "event_t"
    then returnError Incompatible f "column was not of type event_t"
    else maybe (returnError UnexpectedNull f "event type may not be null") (nameEvent . decodeUtf8) v

nominalDiffTimeParser :: FieldParser NominalDiffTime
nominalDiffTimeParser f v =
  C.fromSeconds' <$> fromField f v

creditToParser :: RowParser CreditTo
creditToParser = join $ fieldWith creditToParser'

creditToParser' :: FieldParser (RowParser CreditTo)
creditToParser' f v =
  let parser :: Text -> RowParser CreditTo
      parser "credit_to_btc_addr" = CreditToAddress <$> (fieldWith btcAddrParser <* nullField <* nullField)
      parser "credit_to_user"     = CreditToUser    <$> (nullField *> idParser UserId <* nullField)
      parser "credit_to_project"  = CreditToProject <$> (nullField *> nullField *> idParser P.ProjectId)
      parser _ = empty
  in do
    tn <- typename f
    if tn /= "credit_to_t"
      then returnError Incompatible f "column was not of type credit_to_t"
      else maybe empty (pure . parser . decodeUtf8) v

logEntryParser :: RowParser LogEntry
logEntryParser =
  LogEntry <$> creditToParser
           <*> (fieldWith eventTypeParser <*> utcParser)
           <*> field

qdbLogEntryParser :: RowParser KeyedLogEntry
qdbLogEntryParser =
  (,,) <$> idParser P.ProjectId
       <*> idParser UserId
       <*> logEntryParser

auctionParser :: RowParser A.Auction
auctionParser =
  A.Auction <$> idParser P.ProjectId
            <*> idParser UserId
            <*> utcParser
            <*> btcParser
            <*> utcParser
            <*> utcParser

bidParser :: RowParser A.Bid
bidParser =
  A.Bid <$> idParser UserId
        <*> (Seconds <$> field)
        <*> btcParser
        <*> utcParser

userParser :: RowParser User
userParser =
  User <$> (UserName <$> field)
       <*> fieldWith (optionalField btcAddrParser)
       <*> (Email <$> field)

projectParser :: RowParser P.Project
projectParser =
  P.Project <$> field
            <*> utcParser
            <*> idParser UserId
            <*> fieldWith fromJSONField

invitationParser :: RowParser P.Invitation
invitationParser =
  P.Invitation <$> idParser P.ProjectId
               <*> idParser UserId
               <*> fmap Email field
               <*> utcParser
               <*> fmap (fmap toThyme) field

billableParser :: RowParser B.Billable
billableParser =
  B.Billable <$> idParser P.ProjectId
             <*> idParser UserId
             <*> field
             <*> field
             <*> recurrenceParser
             <*> btcParser
             <*> field
             <*> fieldWith (optionalField nominalDiffTimeParser)

recurrenceParser :: RowParser B.Recurrence
recurrenceParser =
  let prec :: Text -> RowParser B.Recurrence
      prec "annually" = nullField *> pure B.Annually
      prec "monthly"  = B.Monthly <$> field
      --prec "semimonthly" = nullField *> pure B.SemiMonthly
      prec "weekly"   = B.Weekly <$> field
      prec "onetime"  = nullField *> pure B.OneTime
      prec s          = fail $ "Unrecognized recurrence type: " ++ show s
  in  field >>= prec

subscriptionParser :: RowParser B.Subscription
subscriptionParser =
  B.Subscription <$> idParser UserId
                 <*> idParser B.BillableId
                 <*> (toThyme <$> field)
                 <*> ((fmap toThyme) <$> field)

paymentRequestParser :: RowParser PaymentRequest
paymentRequestParser =
  PaymentRequest <$> (B.SubscriptionId <$> field)
                 <*> (field >>= (either fail pure . runGet decodeMessage))
                 <*> (toThyme <$> field)
                 <*> (toThyme <$> field)

paymentParser :: RowParser Payment
paymentParser =
  Payment <$> (PaymentRequestId <$> field)
          <*> (field >>= (either fail pure . runGet decodeMessage))
          <*> (toThyme <$> field)

pexec :: (ToRow d) => Query -> d -> QDBM Int64
pexec q d = QDBM $ do
  conn <- ask
  lift . lift $ execute conn q d

pinsert :: (ToRow d) => (UUID -> r) -> Query -> d -> QDBM r
pinsert f q d = QDBM $ do
  conn <- ask
  ids  <- lift . lift $ query conn q d
  pure . f . fromOnly $ L.head ids

pquery :: (ToRow d) => RowParser r -> Query -> d -> QDBM [r]
pquery p q d = QDBM $ do
  conn <- ask
  lift . lift $ queryWith p conn q d

transactQDBM :: QDBM a -> QDBM a
transactQDBM (QDBM rt) = QDBM $ do
  conn <- ask
  lift . EitherT $ withTransaction conn (runEitherT $ runReaderT rt conn)

storeEvent :: DBOp a -> Maybe (QDBM EventId)
storeEvent (CreateBillable uid b) =
  Just $ storeEventJSON uid "create_billable" (billableJSON b)

storeEvent (CreateSubscription uid bid) =
  Just $ storeEventJSON uid "create_subscription" (createSubscriptionJSON uid bid)

storeEvent (CreatePaymentRequest uid req) =
  Just $ storeEventJSON uid "create_payment_request" (paymentRequestJSON req)

storeEvent (CreatePayment uid req) =
  Just $ storeEventJSON uid "create_payment" (paymentJSON req)

storeEvent _ = Nothing

type EventType = Text

storeEventJSON :: UserId -> EventType -> Value -> QDBM EventId
storeEventJSON uid t v = do
  timestamp <- liftIO C.getCurrentTime
  pinsert EventId
    "INSERT INTO aftok_events \
    \(event_time, created_by, event_type, event_json) \
    \VALUES (?, ?, ?, ?) RETURNING id"
    (fromThyme timestamp, uid ^. _UserId, t, v)

pgEval :: DBOp a -> QDBM a
pgEval (CreateEvent (P.ProjectId pid) (UserId uid) (LogEntry c e m)) =
  case c of
    CreditToAddress addr ->
      pinsert EventId
        "INSERT INTO work_events \
        \(project_id, user_id, credit_to_type, credit_to_btc_addr, event_type, event_time, event_metadata) \
        \VALUES (?, ?, ?, ?, ?, ?, ?) \
        \RETURNING id"
        ( pid, uid, creditToName c, addr ^. _BtcAddr . to addrToBase58, eventName e, fromThyme $ e ^. eventTime, m)

    CreditToProject pid' ->
      pinsert EventId
        "INSERT INTO work_events \
        \(project_id, user_id, credit_to_type, credit_to_project_id, event_type, event_time, event_metadata) \
        \VALUES (?, ?, ?, ?, ?, ?, ?) \
        \RETURNING id"
        ( pid, uid, creditToName c, pid' ^. P._ProjectId, eventName e, fromThyme $ e ^. eventTime, m)

    CreditToUser uid' ->
      pinsert EventId
        "INSERT INTO work_events \
        \(project_id, user_id, credit_to_type, credit_to_user_id, event_type, event_time, event_metadata) \
        \VALUES (?, ?, ?, ?, ?, ?, ?) \
        \RETURNING id"
        ( pid, uid, creditToName c, uid' ^. _UserId, eventName e, fromThyme $ e ^. eventTime, m)

pgEval (FindEvent (EventId eid)) =
  headMay <$> pquery qdbLogEntryParser
    "SELECT project_id, user_id, \
    \credit_to_type, credit_to_btc_addr, credit_to_user_id, credit_to_project_id, \
    \event_type, event_time, event_metadata FROM work_events \
    \WHERE id = ?"
    (Only eid)

pgEval (FindEvents (P.ProjectId pid) (UserId uid) ival) =
  let q (Before e) = pquery logEntryParser
        "SELECT btc_addr, event_type, event_time, event_metadata FROM work_events \
        \WHERE project_id = ? AND user_id = ? AND event_time <= ?"
        (pid, uid, fromThyme e)
      q (During s e) = pquery logEntryParser
        "SELECT btc_addr, event_type, event_time, event_metadata FROM work_events \
        \WHERE project_id = ? AND user_id = ? \
        \AND event_time >= ? AND event_time <= ?"
        (pid, uid, fromThyme s, fromThyme e)
      q (After s) = pquery logEntryParser
        "SELECT btc_addr, event_type, event_time, event_metadata FROM work_events \
        \WHERE project_id = ? AND user_id = ? AND event_time >= ?"
        (pid, uid, fromThyme s)
  in  q ival

pgEval (AmendEvent (EventId eid) (TimeChange mt t)) =
  pinsert AmendmentId
    "INSERT INTO event_time_amendments \
    \(event_id, amended_at, event_time) \
    \VALUES (?, ?, ?) RETURNING id"
    ( eid, fromThyme $ mt ^. _ModTime, fromThyme t )

pgEval (AmendEvent (EventId eid) (CreditToChange mt c)) =
  case c of
    CreditToAddress addr ->
      pinsert AmendmentId
        "INSERT INTO event_credit_to_amendments \
        \(event_id, amended_at, credit_to_type, credit_to_btc_addr) \
        \VALUES (?, ?, ?, ?) RETURNING id"
        ( eid, fromThyme $ mt ^. _ModTime, creditToName c, addr ^. _BtcAddr . to addrToBase58 )

    CreditToProject pid ->
      pinsert AmendmentId
        "INSERT INTO event_credit_to_amendments \
        \(event_id, amended_at, credit_to_type, credit_to_project_id) \
        \VALUES (?, ?, ?, ?) RETURNING id"
        ( eid, fromThyme $ mt ^. _ModTime, creditToName c, pid ^. P._ProjectId )

    CreditToUser uid ->
      pinsert AmendmentId
        "INSERT INTO event_credit_to_amendments \
        \(event_id, amended_at, credit_to_type, credit_to_user_id) \
        \VALUES (?, ?, ?, ?) RETURNING id"
        ( eid, fromThyme $ mt ^. _ModTime, creditToName c, uid ^. _UserId )

pgEval (AmendEvent (EventId eid) (MetadataChange mt v)) =
  pinsert AmendmentId
    "INSERT INTO event_metadata_amendments \
    \(event_id, amended_at, event_metadata) \
    \VALUES (?, ?, ?) RETURNING id"
    ( eid, fromThyme $ mt ^. _ModTime, v)

pgEval (ReadWorkIndex (P.ProjectId pid)) = do
  logEntries <- pquery logEntryParser
    "SELECT btc_addr, event_type, event_time, event_metadata FROM work_events WHERE project_id = ?"
    (Only pid)
  pure $ workIndex logEntries

pgEval (CreateAuction auc) =
  pinsert A.AuctionId
    "INSERT INTO auctions (project_id, user_id, raise_amount, end_time) \
    \VALUES (?, ?, ?, ?) RETURNING id"
    ( auc ^. (A.projectId . P._ProjectId)
    , auc ^. (A.initiator . _UserId)
    , auc ^. (A.raiseAmount . satoshi)
    , auc ^. (A.auctionEnd . to fromThyme)
    )

pgEval (FindAuction aucId) =
  headMay <$> pquery auctionParser
    "SELECT project_id, initiator_id, created_at, raise_amount, start_time, end_time FROM auctions WHERE id = ?"
    (Only (aucId ^. A._AuctionId))

pgEval (CreateBid (A.AuctionId aucId) bid) =
  pinsert A.BidId
    "INSERT INTO bids (auction_id, bidder_id, bid_seconds, bid_amount, bid_time) \
    \VALUES (?, ?, ?, ?, ?) RETURNING id"
    ( aucId
    , bid ^. (A.bidUser . _UserId)
    , case bid ^. A.bidSeconds of (Seconds i) -> i
    , bid ^. (A.bidAmount . satoshi)
    , bid ^. (A.bidTime . to fromThyme)
    )

pgEval (FindBids aucId) =
  pquery ((,) <$> idParser A.BidId <*> bidParser)
    "SELECT id, user_id, bid_seconds, bid_amount, bid_time FROM bids WHERE auction_id = ?"
    (Only (aucId ^. A._AuctionId))

pgEval (CreateUser user') =
  let addrMay :: Maybe ByteString
      addrMay = user' ^? (userAddress . traverse . _BtcAddr . to addrToBase58)
  in  pinsert UserId
    "INSERT INTO users (handle, btc_addr, email) VALUES (?, ?, ?) RETURNING id"
    ( user' ^. (username._UserName)
    , addrMay
    , user' ^. userEmail._Email
    )

pgEval (FindUser (UserId uid)) =
  headMay <$> pquery userParser
    "SELECT handle, btc_addr, email FROM users WHERE id = ?"
    (Only uid)

pgEval (FindUserByName (UserName h)) =
  headMay <$> pquery ((,) <$> idParser UserId <*> userParser)
    "SELECT id, handle, btc_addr, email FROM users WHERE handle = ?"
    (Only h)

pgEval (CreateInvitation (P.ProjectId pid) (UserId uid) (Email e) t) = do
  invCode <- liftIO P.randomInvCode
  void $ pexec
    "INSERT INTO invitations (project_id, invitor_id, invitee_email, invitation_key, invitation_time) \
    \VALUES (?, ?, ?, ?, ?)"
    (pid, uid, e, P.renderInvCode invCode, fromThyme t)
  pure invCode

pgEval (FindInvitation ic) =
  headMay <$> pquery invitationParser
    "SELECT project_id, invitor_id, invitee_email, invitation_time, acceptance_time \
    \FROM invitations WHERE invitation_key = ?"
    (Only $ P.renderInvCode ic)

pgEval (AcceptInvitation (UserId uid) ic t) = transactQDBM $ do
  void $ pexec
    "UPDATE invitations SET acceptance_time = ? WHERE invitation_key = ?"
    (fromThyme t, P.renderInvCode ic)
  void $ pexec
    "INSERT INTO project_companions (project_id, user_id, invited_by, joined_at) \
    \SELECT i.project_id, ?, i.invitor_id, ? \
    \FROM invitations i \
    \WHERE i.invitation_key = ?"
    (uid, fromThyme t, P.renderInvCode ic)

pgEval (CreateProject p) =
  pinsert P.ProjectId
    "INSERT INTO projects (project_name, inception_date, initiator_id, depreciation_fn) \
    \VALUES (?, ?, ?, ?) RETURNING id"
    (p ^. P.projectName, p ^. (P.inceptionDate . to fromThyme), p ^. (P.initiator . _UserId), toJSON $ p ^. P.depf)

pgEval (FindProject (P.ProjectId pid)) =
  headMay <$> pquery projectParser
    "SELECT project_name, inception_date, initiator_id, depreciation_fn FROM projects WHERE id = ?"
    (Only pid)

pgEval (FindUserProjects (UserId uid)) =
  pquery ((,) <$> idParser P.ProjectId <*> projectParser)
    "SELECT p.id, p.project_name, p.inception_date, p.initiator_id, p.depreciation_fn \
    \FROM projects p LEFT OUTER JOIN project_companions pc ON pc.project_id = p.id \
    \WHERE pc.user_id = ? \
    \OR p.initiator_id = ?"
    (uid, uid)

pgEval (AddUserToProject pid current new) = void $
  pexec
    "INSERT INTO project_companions (project_id, user_id, invited_by) VALUES (?, ?, ?)"
    (pid ^. P._ProjectId, new ^. _UserId, current ^. _UserId)

pgEval dbop @ (CreateBillable _ b) = do
  eventId <- requireEventId dbop
  pinsert B.BillableId
    "INSERT INTO billables \
    \(project_id, event_id, name, description, recurrence_type, recurrence_count, billing_amount, grace_period_days) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING id"
    ( b ^. (B.project . P._ProjectId)
    , eventId ^. _EventId
    , b ^. B.name
    , b ^. B.description
    , b ^. (B.recurrence . to B.recurrenceName)
    , b ^. (B.recurrence . to B.recurrenceCount)
    , b ^. (B.amount . satoshi)
    , b ^. (B.gracePeriod)
    )

pgEval (FindBillable bid) =
  headMay <$> pquery billableParser
    "SELECT b.project_id, e.created_by, b.name, b.description, b.recurrence_type, b.recurrence_count, \
    \       b.billing_amount, b.grace_period_days \
    \FROM billables b JOIN aftok_events e ON e.id = b.event_id \
    \WHERE b.id = ?"
    (Only (bid ^. B._BillableId))

pgEval dbop @ (CreateSubscription uid bid) = do
  eventId <- requireEventId dbop
  pinsert B.SubscriptionId
    "INSERT INTO subscriptions \
    \(user_id, billable_id, event_id) \
    \VALUES (?, ?, ?) RETURNING id"
    ( view _UserId uid
    , view B._BillableId bid
    , view _EventId eventId
    )

pgEval (FindSubscription sid) =
  headMay <$> pquery subscriptionParser
    "SELECT id, billable_id, start_date, end_date \
    \FROM subscriptions s \
    \WHERE s.id = ?"
    (Only (sid ^. B._SubscriptionId))

pgEval (FindSubscriptions uid pid) =
  pquery ((,) <$> idParser B.SubscriptionId <*> subscriptionParser)
    "SELECT id, user_id, billable_id, start_date, end_date \
    \FROM subscriptions s \
    \JOIN billables b ON b.id = s.billable_id \
    \WHERE s.user_id = ? \
    \AND b.project_id = ?"
    (uid ^. _UserId, pid ^. P._ProjectId)


pgEval dbop @ (CreatePaymentRequest _ req) = do
  eventId <- requireEventId dbop
  pinsert PaymentRequestId
    "INSERT INTO payment_requests \
    \(subscription_id, event_id, request_data, request_time, billing_date) \
    \VALUES (?, ?, ?, ?, ?) RETURNING id"
    ( req ^. (subscription . B._SubscriptionId)
    , eventId ^. _EventId
    , req ^. (paymentRequest . to (runPut . encodeMessage))
    , req ^. (paymentRequestTime . to fromThyme)
    , req ^. (billingDate . to fromThyme)
    )

pgEval (FindPaymentRequest rid) =
  headMay <$> pquery paymentRequestParser
  "SELECT subscription_id, request_data, request_time, billing_date \
  \FROM payment_requests \
  \WHERE id = ?"
  (Only (rid ^. _PaymentRequestId))

pgEval (FindPaymentRequests sid) =
  pquery ((,) <$> idParser PaymentRequestId <*> paymentRequestParser)
  "SELECT id, subscription_id, request_data, request_time, billing_date \
  \FROM payment_requests \
  \WHERE subscription_id = ?"
  (Only (sid ^. B._SubscriptionId))

pgEval (FindUnpaidRequests sid) =
  let rowp :: RowParser (PaymentRequestId, PaymentRequest, B.Subscription, B.Billable)
      rowp = (,,,) <$> idParser PaymentRequestId
                   <*> paymentRequestParser
                   <*> subscriptionParser
                   <*> billableParser
  in  pquery rowp
      "SELECT id, \
      \       r.subscription_id, r.request_data, r.request_time, r.billing_date, \
      \       s.user_id, s.billable_id, s.start_date, s.end_date, \
      \       b.project_id, e.created_by, b.name, b.description, b.recurrence_type, \
      \       b.recurrence_count, b.billing_amount, b.grace_period_days \
      \FROM payment_requests r \
      \JOIN subscriptions s on s.id = r.subscription_id \
      \JOIN billables b on b.id = s.billable_id \
      \WHERE subscription_id = ? \
      \AND r.id NOT IN (SELECT payment_request_id FROM payments)"
      (Only (sid ^. B._SubscriptionId))

pgEval dbop @ (CreatePayment _ p) = do
  eventId <- requireEventId dbop
  pinsert PaymentId
    "INSERT INTO payments \
    \(payment_request_id, event_id, payment_data, payment_date) \
    \VALUES (?, ?, ?, ?) RETURNING id"
    ( p ^. (request . _PaymentRequestId)
    , eventId ^. _EventId
    , p ^. (payment . to (runPut . encodeMessage))
    , p ^. (paymentDate . to fromThyme)
    )

pgEval (FindPayments rid) =
  pquery ((,) <$> idParser PaymentId <*> paymentParser)
  "SELECT id, payment_request_id, payment_data, payment_date \
  \FROM payments \
  \WHERE payment_request_id = ?"
  (Only (rid ^. _PaymentRequestId))

pgEval (RaiseDBError err _) = raiseError err

requireEventId :: DBOp a -> QDBM EventId
requireEventId = maybe (raiseError EventStorageFailed) id . storeEvent

raiseError :: DBError -> QDBM a
raiseError = QDBM . lift . left

instance MonadDB QDBM where
  liftdb = pgEval
