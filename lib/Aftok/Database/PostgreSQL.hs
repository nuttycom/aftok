{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aftok.Database.PostgreSQL (QDBM(), runQDBM) where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Trans.Either
import           Data.Aeson                           (toJSON, Value)
import qualified Data.ByteString.Char8                as B
import           Data.Hourglass
import           Data.List                            as L
import Data.ProtocolBuffers (encodeMessage)
import           Data.Serialize.Put                   (runPut)
import           Data.Thyme.Clock                     as C
import           Data.Thyme.Time
import           Data.UUID                            (UUID)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types     (Null)

import           Network.Haskoin.Crypto (addrToBase58)

import           Aftok
import qualified Aftok.Auction                        as A
import qualified Aftok.Billables                      as BI
import           Aftok.Database
import           Aftok.Interval
import           Aftok.Json                           (billableJSON, subscriptionJSON, paymentRequestJSON, paymentJSON)
import           Aftok.Payments
import qualified Aftok.Project                        as P
import           Aftok.Time                           (Days(..), _Days)
import           Aftok.TimeLog
import           Aftok.Types

newtype QDBM a = QDBM (ReaderT Connection (EitherT DBError IO) a)
  deriving (Functor, Applicative, Monad)

instance MonadIO QDBM where
  liftIO = QDBM . lift . lift

runQDBM :: Connection -> QDBM a -> EitherT DBError IO a
runQDBM conn (QDBM r) = runReaderT r conn

uidParser :: FieldParser UserId
uidParser f v = UserId <$> fromField f v

pidParser :: FieldParser P.ProjectId
pidParser f v = P.ProjectId <$> fromField f v

secondsParser :: FieldParser Seconds
secondsParser f v = Seconds <$> fromField f v

usernameParser :: FieldParser UserName
usernameParser f v = UserName <$> fromField f v

emailParser :: FieldParser Email
emailParser f v = Email <$> fromField f v

btcAddrParser :: FieldParser BtcAddr
btcAddrParser f v = do
  addrMay <- parseBtcAddr <$> fromField f v
  let err = ConversionFailed { errSQLType = "text"
                             , errSQLTableOid = tableOid f
                             , errSQLField = maybe "" B.unpack (name f)
                             , errHaskellType = "BtcAddr"
                             , errMessage = "could not deserialize value to a valid BTC address"
                             }
  maybe (conversionError err) pure addrMay

btcParser :: FieldParser Satoshi
btcParser f v = (Satoshi . fromInteger) <$> fromField f v

utcParser :: FieldParser C.UTCTime
utcParser f v = toThyme <$> fromField f v

nullField :: RowParser Null
nullField = field

recurrenceParser :: RowParser BI.Recurrence
recurrenceParser =
  let prec :: Text -> RowParser BI.Recurrence
      prec "annually" = nullField *> pure BI.Annually
      prec "monthly"  = BI.Monthly <$> field
      prec "semimonthly" = nullField *> pure BI.SemiMonthly
      prec "weekly"   = BI.Weekly <$> field
      prec "onetime"  = nullField *> pure BI.OneTime
      prec _ = empty
  in  field >>= prec

eventTypeParser :: FieldParser (C.UTCTime -> LogEvent)
eventTypeParser f v = do
  tn <- typename f
  if tn /= "event_t"
    then returnError Incompatible f "column was not of type event_t"
    else maybe (returnError UnexpectedNull f "event type may not be null") (nameEvent . decodeUtf8) v

creditToParser :: RowParser CreditTo
creditToParser = join $ fieldWith creditToParser'

creditToParser' :: FieldParser (RowParser CreditTo)
creditToParser' f v =   
  let parser :: Text -> RowParser CreditTo
      parser "credit_to_btc_addr" = CreditToAddress <$> (fieldWith btcAddrParser <* nullField <* nullField)
      parser "credit_to_user"     = CreditToUser    <$> (nullField *> fieldWith uidParser <* nullField)
      parser "credit_to_project"  = CreditToProject <$> (nullField *> nullField *> fieldWith pidParser)
      parser _ = empty
  in do
    tn <- typename f
    if tn /= "credit_to_t" 
      then returnError Incompatible f "column was not of type credit_to_t"
      else maybe empty (pure . parser . decodeUtf8) v

logEntryParser :: RowParser LogEntry
logEntryParser =
  LogEntry <$> creditToParser
           <*> (fieldWith eventTypeParser <*> fieldWith utcParser)
           <*> field

qdbLogEntryParser :: RowParser KeyedLogEntry
qdbLogEntryParser =
  (,,) <$> fieldWith pidParser
       <*> fieldWith uidParser
       <*> logEntryParser

auctionParser :: RowParser A.Auction
auctionParser =
  A.Auction <$> fieldWith pidParser
            <*> fieldWith uidParser
            <*> fieldWith utcParser
            <*> fieldWith btcParser
            <*> fieldWith utcParser
            <*> fieldWith utcParser

bidParser :: RowParser A.Bid
bidParser =
  A.Bid <$> fieldWith uidParser
        <*> fieldWith secondsParser
        <*> fieldWith btcParser
        <*> fieldWith utcParser

userParser :: RowParser User
userParser =
  User <$> fieldWith usernameParser
       <*> fieldWith (optionalField btcAddrParser)
       <*> (Email <$> field)

qdbUserParser :: RowParser KeyedUser
qdbUserParser =
  (,) <$> fieldWith uidParser
      <*> userParser

projectParser :: RowParser P.Project
projectParser =
  P.Project <$> field
            <*> fieldWith utcParser
            <*> fieldWith uidParser
            <*> fieldWith fromJSONField

invitationParser :: RowParser P.Invitation
invitationParser =
  P.Invitation <$> fieldWith pidParser
               <*> fieldWith uidParser
               <*> fieldWith emailParser
               <*> fieldWith utcParser
               <*> fmap (fmap toThyme) field

qdbProjectParser :: RowParser KeyedProject
qdbProjectParser =
  (,) <$> fieldWith pidParser
      <*> projectParser

billableParser :: RowParser BI.Billable
billableParser =
  BI.Billable <$> fieldWith pidParser
              <*> fieldWith uidParser
              <*> field
              <*> field
              <*> recurrenceParser
              <*> fieldWith btcParser
              <*> (Days <$> field)

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
  Just $ storeEventJSON uid "create_subscription" (subscriptionJSON uid bid)

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

updateCache :: DBOp a -> QDBM a
updateCache (CreateEvent (P.ProjectId pid) (UserId uid) (LogEntry c e m)) =
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

updateCache (FindEvent (EventId eid)) =
  headMay <$> pquery qdbLogEntryParser
    "SELECT project_id, user_id, \
    \credit_to_type, credit_to_btc_addr, credit_to_user_id, credit_to_project_id, \
    \event_type, event_time, event_metadata FROM work_events \
    \WHERE id = ?"
    (Only eid)

updateCache (FindEvents (P.ProjectId pid) (UserId uid) ival) =
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

updateCache (AmendEvent (EventId eid) (TimeChange mt t)) =
  pinsert AmendmentId
    "INSERT INTO event_time_amendments \
    \(event_id, amended_at, event_time) \
    \VALUES (?, ?, ?) RETURNING id"
    ( eid, fromThyme $ mt ^. _ModTime, fromThyme t )

updateCache (AmendEvent (EventId eid) (CreditToChange mt c)) =
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

updateCache (AmendEvent (EventId eid) (MetadataChange mt v)) =
  pinsert AmendmentId
    "INSERT INTO event_metadata_amendments \
    \(event_id, amended_at, event_metadata) \
    \VALUES (?, ?, ?) RETURNING id"
    ( eid, fromThyme $ mt ^. _ModTime, v)

updateCache (ReadWorkIndex (P.ProjectId pid)) = do
  logEntries <- pquery logEntryParser
    "SELECT btc_addr, event_type, event_time, event_metadata FROM work_events WHERE project_id = ?"
    (Only pid)
  pure $ workIndex logEntries

updateCache (CreateAuction auc) =
  pinsert A.AuctionId
    "INSERT INTO auctions (project_id, user_id, raise_amount, end_time) \
    \VALUES (?, ?, ?, ?) RETURNING id"
    ( auc ^. (A.projectId . P._ProjectId)
    , auc ^. (A.initiator . _UserId)
    , auc ^. (A.raiseAmount . satoshi)
    , auc ^. (A.auctionEnd . to fromThyme)
    )

updateCache (FindAuction aucId) =
  headMay <$> pquery auctionParser
    "SELECT project_id, initiator_id, created_at, raise_amount, start_time, end_time FROM auctions WHERE id = ?"
    (Only (aucId ^. A._AuctionId))

updateCache (CreateBid (A.AuctionId aucId) bid) =
  pinsert A.BidId
    "INSERT INTO bids (auction_id, bidder_id, bid_seconds, bid_amount, bid_time) \
    \VALUES (?, ?, ?, ?, ?) RETURNING id"
    ( aucId
    , bid ^. (A.bidUser . _UserId)
    , case bid ^. A.bidSeconds of (Seconds i) -> i
    , bid ^. (A.bidAmount . satoshi)
    , bid ^. (A.bidTime . to fromThyme)
    )

updateCache (ReadBids aucId) =
  pquery bidParser
    "SELECT user_id, bid_seconds, bid_amount, bid_time FROM bids WHERE auction_id = ?"
    (Only (aucId ^. A._AuctionId))

updateCache (CreateUser user') =
  let addrMay :: Maybe ByteString
      addrMay = user' ^? (userAddress . traverse . _BtcAddr . to addrToBase58)
  in  pinsert UserId
    "INSERT INTO users (handle, btc_addr, email) VALUES (?, ?, ?) RETURNING id"
    ( user' ^. (username._UserName)
    , addrMay
    , user' ^. userEmail._Email
    )

updateCache (FindUser (UserId uid)) =
  headMay <$> pquery userParser
    "SELECT handle, btc_addr, email FROM users WHERE id = ?"
    (Only uid)

updateCache (FindUserByName (UserName h)) =
  headMay <$> pquery qdbUserParser
    "SELECT id, handle, btc_addr, email FROM users WHERE handle = ?"
    (Only h)

updateCache (CreateInvitation (P.ProjectId pid) (UserId uid) (Email e) t) = do
  invCode <- liftIO P.randomInvCode
  void $ pexec
    "INSERT INTO invitations (project_id, invitor_id, invitee_email, invitation_key, invitation_time) \
    \VALUES (?, ?, ?, ?, ?)"
    (pid, uid, e, P.renderInvCode invCode, fromThyme t)
  pure invCode

updateCache (FindInvitation ic) =
  headMay <$> pquery invitationParser
    "SELECT project_id, invitor_id, invitee_email, invitation_time, acceptance_time \
    \FROM invitations WHERE invitation_key = ?"
    (Only $ P.renderInvCode ic)

updateCache (AcceptInvitation (UserId uid) ic t) = transactQDBM $ do
  void $ pexec
    "UPDATE invitations SET acceptance_time = ? WHERE invitation_key = ?"
    (fromThyme t, P.renderInvCode ic)
  void $ pexec
    "INSERT INTO project_companions (project_id, user_id, invited_by, joined_at) \
    \SELECT i.project_id, ?, i.invitor_id, ? \
    \FROM invitations i \
    \WHERE i.invitation_key = ?"
    (uid, fromThyme t, P.renderInvCode ic)

updateCache (CreateProject p) =
  pinsert P.ProjectId
    "INSERT INTO projects (project_name, inception_date, initiator_id, depreciation_fn) \
    \VALUES (?, ?, ?, ?) RETURNING id"
    (p ^. P.projectName, p ^. (P.inceptionDate . to fromThyme), p ^. (P.initiator . _UserId), toJSON $ p ^. P.depf)

updateCache (FindProject (P.ProjectId pid)) =
  headMay <$> pquery projectParser
    "SELECT project_name, inception_date, initiator_id, depreciation_fn FROM projects WHERE id = ?"
    (Only pid)

updateCache (FindUserProjects (UserId uid)) =
  pquery qdbProjectParser
    "SELECT p.id, p.project_name, p.inception_date, p.initiator_id, p.depreciation_fn \
    \FROM projects p LEFT OUTER JOIN project_companions pc ON pc.project_id = p.id \
    \WHERE pc.user_id = ? \
    \OR p.initiator_id = ?"
    (uid, uid)

updateCache (AddUserToProject pid current new) = void $
  pexec
    "INSERT INTO project_companions (project_id, user_id, invited_by) VALUES (?, ?, ?)"
    (pid ^. P._ProjectId, new ^. _UserId, current ^. _UserId)

updateCache dbop @ (CreateBillable _ b) = do
  eventId <- requireEventId dbop
  pinsert BI.BillableId
    "INSERT INTO billables \
    \(project_id, event_id, name, description, recurrence_type, recurrence_count, billing_amount, grace_period_days) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING id"
    ( b ^. (BI.project . P._ProjectId)
    , eventId ^. _EventId
    , b ^. BI.name
    , b ^. BI.description
    , b ^. (BI.recurrence . to BI.recurrenceName)
    , b ^. (BI.recurrence . to BI.recurrenceCount)
    , b ^. (BI.amount . satoshi)
    , b ^. (BI.gracePeriod . _Days)
    )

updateCache (ReadBillable bid) =
  headMay <$> pquery billableParser
    "SELECT b.project_id, e.created_by, b.name, b.description, b.recurrence_type, b.recurrence_count, \
    \       b.billing_amount, b.grace_period_days \
    \FROM billables b JOIN aftok_events e ON e.id = b.event_id \
    \WHERE b.id = ?"
    (Only (bid ^. BI._BillableId))

updateCache dbop @ (CreateSubscription uid bid) = do
  eventId <- requireEventId dbop
  pinsert BI.SubscriptionId
    "INSERT INTO subscriptions \
    \(user_id, billable_id, event_id) \
    \VALUES (?, ?, ?) RETURNING id"
    (uid ^. _UserId, bid ^. BI._BillableId, eventId ^. _EventId)

updateCache dbop @ (CreatePaymentRequest _ req) = do
  eventId <- requireEventId dbop
  pinsert PaymentRequestId
    "INSERT INTO payment_requests \
    \(subscription_id, event_id, request_data) \
    \VALUES (?, ?, ?) RETURNING id"
    ( req ^. (subscription . BI._SubscriptionId)
    , eventId ^. _EventId 
    , req ^. (paymentRequest . to (runPut . encodeMessage))
    )
  
updateCache dbop @ (CreatePayment _ req) = do
  eventId <- requireEventId dbop
  pinsert PaymentId
    "INSERT INTO payments \
    \(payment_request_id, event_id, payment_data) \
    \VALUES (?, ?, ?) RETURNING id"
    ( req ^. (request . _PaymentRequestId)
    , eventId ^. _EventId
    , req ^. (payment . to (runPut . encodeMessage))
    )

updateCache (RaiseDBError err _) = raiseError err

requireEventId :: DBOp a -> QDBM EventId
requireEventId = maybe (raiseError EventStorageFailed) id . storeEvent

raiseError :: DBError -> QDBM a
raiseError = QDBM . lift . left

instance DBEval QDBM where
  dbEval e = updateCache e
