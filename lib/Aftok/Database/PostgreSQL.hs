{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE QuasiQuotes                #-}

module Aftok.Database.PostgreSQL
  ( QDBM(..)
  , runQDBM
  )
where

import           Prelude                 hiding ( null )
import           Control.Lens
import           Control.Monad.Trans.Except     ( ExceptT(..)
                                                , throwE
                                                , runExceptT
                                                )
import           Crypto.Random.Types            ( MonadRandom
                                                , getRandomBytes
                                                )
import           Data.Aeson                     ( Value
                                                , toJSON
                                                )
import           Data.Hourglass
import qualified Data.List                     as L
import           Data.ProtocolBuffers           ( decodeMessage
                                                , encodeMessage
                                                )
import           Data.Serialize.Get             ( runGet )
import           Data.Serialize.Put             ( runPut )
import qualified Data.Text                     as T
import           Data.Thyme.Clock              as C
import           Data.Thyme.Time
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Database.PostgreSQL.Simple.Types
                                                ( Null )
import           Safe                           ( headMay )

import qualified Aftok.Auction                 as A
import qualified Aftok.Billables               as B
import           Aftok.Currency.Bitcoin
import           Aftok.Database
import           Aftok.Database.PostgreSQL.Types
                                                ( SerDepFunction(..) )
import           Aftok.Interval
import           Aftok.Json                     ( billableJSON
                                                , createSubscriptionJSON
                                                , paymentJSON
                                                , paymentRequestJSON
                                                )
import           Aftok.Payments.Types
import qualified Aftok.Project                 as P
import           Aftok.TimeLog
import           Aftok.Types
import           Network.Bippy.Types            ( Satoshi(..) )
import           Network.Haskoin.Address        ( Address
                                                , stringToAddr
                                                , addrToString
                                                )
import           Network.Haskoin.Constants      ( Network )

newtype QDBM a = QDBM (ReaderT (NetworkMode, Connection) (ExceptT DBError IO) a)
  deriving (Functor, Applicative, Monad)

instance MonadIO QDBM where
  liftIO = QDBM . lift . lift

instance MonadRandom QDBM where
  getRandomBytes = QDBM . lift . lift . getRandomBytes

instance MonadDB QDBM where
  liftdb = pgEval

runQDBM :: NetworkMode -> Connection -> QDBM a -> ExceptT DBError IO a
runQDBM mode conn (QDBM r) = runReaderT r (mode, conn)

null :: RowParser Null
null = field

idParser :: (UUID -> a) -> RowParser a
idParser f = f <$> field

networkIdParser :: FieldParser NetworkId
networkIdParser f b = do
  networkName <- fromField f b
  case networkName of
    Just "btc" -> pure BTC
    Just "bch" -> pure BCH
    Just other -> returnError
      ConversionFailed
      f
      ("Network identifier " <> other <> " is not supported.")
    Nothing -> pure BTC

addressParser :: NetworkMode -> RowParser (NetworkId, Address)
addressParser mode = do
  networkId <- fieldWith (networkIdParser)
  address   <- fieldWith $ addrFieldParser (toNetwork mode networkId)
  pure (networkId, address)

addrFieldParser :: Network -> FieldParser Address
addrFieldParser n f v = do
  fieldValue <- fromField f v
  let addrMay = stringToAddr n fieldValue
  let err = returnError ConversionFailed
                        f
                        ("could not deserialize value " <> T.unpack fieldValue <> " to a valid BTC address")
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
    else maybe
      (returnError UnexpectedNull f "event type may not be null")
      ( maybe (returnError Incompatible f "unrecognized event type value") pure
      . nameEvent
      . decodeUtf8
      )
      v

nominalDiffTimeParser :: FieldParser NominalDiffTime
nominalDiffTimeParser f v = C.fromSeconds' <$> fromField f v

creditToParser :: NetworkMode -> RowParser (CreditTo (NetworkId, Address))
creditToParser mode = join $ fieldWith (creditToParser' mode)

creditToParser'
  :: NetworkMode -> FieldParser (RowParser (CreditTo (NetworkId, Address)))
creditToParser' mode f v =
  let
    parser :: Text -> RowParser (CreditTo (NetworkId, Address))
    parser "credit_to_address" =
      CreditToCurrency <$> (addressParser mode <* nullField <* nullField)
    parser "credit_to_user" =
      CreditToUser <$> (nullField *> nullField *> idParser UserId <* nullField)
    parser "credit_to_project" =
      CreditToProject
        <$> (nullField *> nullField *> nullField *> idParser ProjectId)
    parser _ = empty
  in
    do
      tn <- typename f
      if tn /= "credit_to_t"
        then returnError Incompatible f "column was not of type credit_to_t"
        else maybe empty (pure . parser . decodeUtf8) v

logEntryParser :: NetworkMode -> RowParser (LogEntry (NetworkId, Address))
logEntryParser mode =
  LogEntry
    <$> creditToParser mode
    <*> (fieldWith eventTypeParser <*> utcParser)
    <*> field

qdbLogEntryParser
  :: NetworkMode -> RowParser (KeyedLogEntry (NetworkId, Address))
qdbLogEntryParser mode =
  (,,) <$> idParser ProjectId <*> idParser UserId <*> logEntryParser mode

auctionParser :: RowParser A.Auction
auctionParser =
  A.Auction
    <$> idParser ProjectId
    <*> idParser UserId
    <*> utcParser
    <*> btcParser
    <*> utcParser
    <*> utcParser

bidParser :: RowParser A.Bid
bidParser =
  A.Bid <$> idParser UserId <*> (Seconds <$> field) <*> btcParser <*> utcParser

userParser :: NetworkMode -> RowParser BTCUser
userParser mode =
  User
    <$> (UserName <$> field)
    <*> ((null *> null *> pure Nothing) <|> fmap Just (addressParser mode))
    <*> (Email <$> field)

projectParser :: RowParser P.Project
projectParser =
  P.Project
    <$> field
    <*> utcParser
    <*> idParser UserId
    <*> (unSerDepFunction <$> fieldWith fromJSONField)

invitationParser :: RowParser P.Invitation
invitationParser =
  P.Invitation
    <$> idParser ProjectId
    <*> idParser UserId
    <*> fmap Email field
    <*> utcParser
    <*> fmap (fmap toThyme) field

billableParser :: RowParser B.Billable
billableParser =
  B.Billable
    <$> idParser ProjectId
    <*> idParser UserId
    <*> field
    <*> field
    <*> recurrenceParser
    <*> btcParser
    <*> field
    <*> fieldWith (optionalField nominalDiffTimeParser)
    <*> field
    <*> field

recurrenceParser :: RowParser B.Recurrence
recurrenceParser =
  let prec :: Text -> RowParser B.Recurrence
      prec = \case
        "annually" -> nullField *> pure B.Annually
        "monthly"  -> B.Monthly <$> field
        --"semimonthly" = nullField *> pure B.SemiMonthly
        "weekly"   -> B.Weekly <$> field
        "onetime"  -> nullField *> pure B.OneTime
        _          -> empty
  in  field >>= prec

subscriptionParser :: RowParser B.Subscription
subscriptionParser =
  B.Subscription
    <$> idParser UserId
    <*> idParser B.BillableId
    <*> (toThyme <$> field)
    <*> ((fmap toThyme) <$> field)

paymentRequestParser :: RowParser PaymentRequest
paymentRequestParser =
  PaymentRequest
    <$> fmap B.SubscriptionId field
    <*> ((either (const empty) pure . runGet decodeMessage) =<< field)
    <*> fmap PaymentKey field
    <*> fmap toThyme    field
    <*> fmap toThyme    field

paymentParser :: RowParser Payment
paymentParser =
  Payment
    <$> (PaymentRequestId <$> field)
    <*> (field >>= (either (const empty) pure . runGet decodeMessage))
    <*> (toThyme <$> field)
    <*> field

pexec :: (ToRow d) => Query -> d -> QDBM Int64
pexec q d = QDBM $ do
  conn <- asks snd
  lift . lift $ execute conn q d

pinsert :: (ToRow d) => (UUID -> r) -> Query -> d -> QDBM r
pinsert f q d = QDBM $ do
  conn <- asks snd
  ids  <- lift . lift $ query conn q d
  pure . f . fromOnly $ L.head ids

pquery :: (ToRow d) => RowParser r -> Query -> d -> QDBM [r]
pquery p q d = QDBM $ do
  conn <- asks snd
  lift . lift $ queryWith p conn q d

transactQDBM :: QDBM a -> QDBM a
transactQDBM (QDBM rt) = QDBM $ do
  env <- ask
  lift . ExceptT $ withTransaction (snd env) (runExceptT $ runReaderT rt env)

storeEvent :: DBOp a -> Maybe (QDBM EventId)
storeEvent (CreateBillable uid b) =
  Just $ storeEventJSON (Just uid) "create_billable" (billableJSON b)

storeEvent (CreateSubscription uid bid t) = Just $ storeEventJSON
  (Just uid)
  "create_subscription"
  (createSubscriptionJSON uid bid t)

storeEvent (CreatePaymentRequest req) = Just
  $ storeEventJSON Nothing "create_payment_request" (paymentRequestJSON req)

storeEvent (CreatePayment req) =
  Just $ storeEventJSON Nothing "create_payment" (paymentJSON req)

storeEvent _ = Nothing

type EventType = Text

storeEventJSON :: Maybe UserId -> EventType -> Value -> QDBM EventId
storeEventJSON uid t v = do
  timestamp <- liftIO C.getCurrentTime
  pinsert
    EventId
    [sql| INSERT INTO aftok_events
          (event_time, created_by, event_type, event_json)
          VALUES (?, ?, ?, ?) RETURNING id |]
    (fromThyme timestamp, preview (_Just . _UserId) uid, t, v)

askNetworkMode :: QDBM NetworkMode
askNetworkMode = QDBM $ asks fst

pgEval :: DBOp a -> QDBM a
pgEval (CreateEvent (ProjectId pid) (UserId uid) (LogEntry c e m)) = case c of
  CreditToCurrency (nid, addr) -> do
    mode <- askNetworkMode
    let network = toNetwork mode nid
    pinsert
      EventId
      [sql| INSERT INTO work_events
              ( project_id, user_id, credit_to_type, credit_to_network, credit_to_address
              , event_type, event_time, event_metadata )
              VALUES (?, ?, ?, ?, ?, ?, ?)
              RETURNING id |]
      ( pid
      , uid
      , creditToName c
      , renderNetworkId nid
      , addrToString network addr
      , eventName e
      , fromThyme $ e ^. eventTime
      , m
      )

  CreditToProject pid' -> pinsert
    EventId
    [sql| INSERT INTO work_events
              ( project_id, user_id, credit_to_type, credit_to_project_id
              , event_type, event_time, event_metadata )
              VALUES (?, ?, ?, ?, ?, ?, ?)
              RETURNING id |]
    ( pid
    , uid
    , creditToName c
    , pid' ^. _ProjectId
    , eventName e
    , fromThyme $ e ^. eventTime
    , m
    )

  CreditToUser uid' -> pinsert
    EventId
    [sql| INSERT INTO work_events
              (project_id, user_id, credit_to_type, credit_to_user_id, event_type, event_time, event_metadata)
              VALUES (?, ?, ?, ?, ?, ?, ?)
              RETURNING id |]
    ( pid
    , uid
    , creditToName c
    , uid' ^. _UserId
    , eventName e
    , fromThyme $ e ^. eventTime
    , m
    )

pgEval (FindEvent (EventId eid)) = do
  mode <- askNetworkMode
  headMay <$> pquery
    (qdbLogEntryParser mode)
    [sql| SELECT project_id, user_id,
                 credit_to_type,
                 credit_to_network, credit_to_address, credit_to_user_id, credit_to_project_id,
                 event_type, event_time, event_metadata FROM work_events
          WHERE id = ? |]
    (Only eid)

pgEval (FindEvents (ProjectId pid) (UserId uid) ival) = do
  mode <- askNetworkMode
  let
    q (Before e) = pquery
      (logEntryParser mode)
      [sql| SELECT credit_to_type,
                     credit_to_network, credit_to_address, credit_to_user_id, credit_to_project_id,
                     event_type, event_time,
                     event_metadata
              FROM work_events
              WHERE project_id = ? AND user_id = ? AND event_time <= ? |]
      (pid, uid, fromThyme e)
    q (During s e) = pquery
      (logEntryParser mode)
      [sql| SELECT credit_to_type,
                     credit_to_network, credit_to_address, credit_to_user_id, credit_to_project_id,
                     event_type, event_time, event_metadata
              FROM work_events
              WHERE project_id = ? AND user_id = ?
              AND event_time >= ? AND event_time <= ? |]
      (pid, uid, fromThyme s, fromThyme e)
    q (After s) = pquery
      (logEntryParser mode)
      [sql| SELECT credit_to_type,
                     credit_to_network, credit_to_address, credit_to_user_id, credit_to_project_id,
                     event_type, event_time, event_metadata
              FROM work_events
              WHERE project_id = ? AND user_id = ? AND event_time >= ? |]
      (pid, uid, fromThyme s)
  q ival

pgEval (AmendEvent (EventId eid) (TimeChange mt t)) = pinsert
  AmendmentId
  [sql| INSERT INTO event_time_amendments
          (event_id, amended_at, event_time)
          VALUES (?, ?, ?) RETURNING id |]
  (eid, fromThyme $ mt ^. _ModTime, fromThyme t)

pgEval (AmendEvent (EventId eid) (CreditToChange mt c)) = do
  mode <- askNetworkMode
  case c of
    CreditToCurrency (nid, addr) -> do
      let network = toNetwork mode nid
      pinsert
        AmendmentId
        [sql| INSERT INTO event_credit_to_amendments
              (event_id, amended_at, credit_to_type, credit_to_network, credit_to_address)
              VALUES (?, ?, ?, ?) RETURNING id |]
        ( eid
        , fromThyme $ mt ^. _ModTime
        , creditToName c
        , renderNetworkId nid
        , addrToString network addr
        )

    CreditToProject pid -> pinsert
      AmendmentId
      [sql| INSERT INTO event_credit_to_amendments
              (event_id, amended_at, credit_to_type, credit_to_project_id)
              VALUES (?, ?, ?, ?) RETURNING id |]
      (eid, fromThyme $ mt ^. _ModTime, creditToName c, pid ^. _ProjectId)

    CreditToUser uid -> pinsert
      AmendmentId
      [sql| INSERT INTO event_credit_to_amendments
              (event_id, amended_at, credit_to_type, credit_to_user_id)
              VALUES (?, ?, ?, ?) RETURNING id |]
      (eid, fromThyme $ mt ^. _ModTime, creditToName c, uid ^. _UserId)

pgEval (AmendEvent (EventId eid) (MetadataChange mt v)) = pinsert
  AmendmentId
  [sql| INSERT INTO event_metadata_amendments
          (event_id, amended_at, event_metadata)
          VALUES (?, ?, ?) RETURNING id |]
  (eid, fromThyme $ mt ^. _ModTime, v)

pgEval (ReadWorkIndex (ProjectId pid)) = do
  mode       <- askNetworkMode
  logEntries <- pquery
    (logEntryParser mode)
    [sql| SELECT credit_to_type,
                 credit_to_network, credit_to_address, credit_to_user_id, credit_to_project_id,
                 event_type, event_time, event_metadata
          FROM work_events
          WHERE project_id = ? |]
    (Only pid)
  pure $ workIndex logEntries

pgEval (CreateAuction auc) = pinsert
  A.AuctionId
  [sql| INSERT INTO auctions (project_id, initiator_id, raise_amount, end_time)
          VALUES (?, ?, ?, ?) RETURNING id |]
  ( auc ^. (A.projectId . _ProjectId)
  , auc ^. (A.initiator . _UserId)
  , auc ^. (A.raiseAmount . satoshi)
  , auc ^. (A.auctionEnd . to fromThyme)
  )

pgEval (FindAuction aucId) = headMay <$> pquery
  auctionParser
  [sql| SELECT project_id, initiator_id, created_at, raise_amount, start_time, end_time
          FROM auctions
          WHERE id = ? |]
  (Only (aucId ^. A._AuctionId))

pgEval (CreateBid (A.AuctionId aucId) bid) = pinsert
  A.BidId
  [sql| INSERT INTO bids (auction_id, bidder_id, bid_seconds, bid_amount, bid_time)
          VALUES (?, ?, ?, ?, ?) RETURNING id |]
  ( aucId
  , bid ^. (A.bidUser . _UserId)
  , case bid ^. A.bidSeconds of
    (Seconds i) -> i
  , bid ^. (A.bidAmount . satoshi)
  , bid ^. (A.bidTime . to fromThyme)
  )

pgEval (FindBids aucId) = pquery
  ((,) <$> idParser A.BidId <*> bidParser)
  [sql| SELECT id, bidder_id, bid_seconds, bid_amount, bid_time FROM bids WHERE auction_id = ? |]
  (Only (aucId ^. A._AuctionId))

pgEval (CreateUser user') = do
  mode <- askNetworkMode
  let nidMay = fst <$> _userAddress user'
      addrMay :: Maybe Text
      addrMay = do
        network <- toNetwork mode <$> nidMay
        address <- snd <$> _userAddress user'
        pure $ addrToString network address
  pinsert
    UserId
    [sql| INSERT INTO users (handle, default_payment_network, default_payment_addr, email)
          VALUES (?, ?, ?, ?) RETURNING id |]
    ( user' ^. (username . _UserName)
    , renderNetworkId <$> nidMay
    , addrMay
    , user' ^. userEmail . _Email
    )

pgEval (FindUser (UserId uid)) = do
  mode <- askNetworkMode
  headMay <$> pquery
    (userParser mode)
    [sql| SELECT handle, default_payment_network, default_payment_addr, email FROM users WHERE id = ? |]
    (Only uid)

pgEval (FindUserByName (UserName h)) = do
  mode <- askNetworkMode
  headMay <$> pquery
    ((,) <$> idParser UserId <*> userParser mode)
    [sql| SELECT id, handle, default_payment_network, default_payment_addr, email FROM users WHERE handle = ? |]
    (Only h)

pgEval (CreateInvitation (ProjectId pid) (UserId uid) (Email e) t) = do
  invCode <- liftIO P.randomInvCode
  void $ pexec
    [sql| INSERT INTO invitations (project_id, invitor_id, invitee_email, invitation_key, invitation_time)
          VALUES (?, ?, ?, ?, ?) |]
    (pid, uid, e, P.renderInvCode invCode, fromThyme t)
  pure invCode

pgEval (FindInvitation ic) = headMay <$> pquery
  invitationParser
  [sql| SELECT project_id, invitor_id, invitee_email, invitation_time, acceptance_time
          FROM invitations WHERE invitation_key = ? |]
  (Only $ P.renderInvCode ic)

pgEval (AcceptInvitation (UserId uid) ic t) = transactQDBM $ do
  void $ pexec
    [sql| UPDATE invitations SET acceptance_time = ? WHERE invitation_key = ? |]
    (fromThyme t, P.renderInvCode ic)

  void $ pexec
    [sql| INSERT INTO project_companions (project_id, user_id, invited_by, joined_at)
          SELECT i.project_id, ?, i.invitor_id, ?
          FROM invitations i
          WHERE i.invitation_key = ? |]
    (uid, fromThyme t, P.renderInvCode ic)

pgEval (CreateProject p) = pinsert
  ProjectId
  [sql| INSERT INTO projects (project_name, inception_date, initiator_id, depreciation_fn)
          VALUES (?, ?, ?, ?) RETURNING id |]
  ( p ^. P.projectName
  , p ^. (P.inceptionDate . to fromThyme)
  , p ^. (P.initiator . _UserId)
  , toJSON $ p ^. P.depf . to SerDepFunction
  )

pgEval ListProjects =
  pquery (idParser ProjectId) [sql| SELECT id FROM projects |] ()

pgEval (FindSubscribers pid) = pquery
  (idParser UserId)
  [sql| SELECT s.user_id
          FROM subscripions s
          JOIN billables b ON s.billable_id = b.id
          WHERE b.project_id = ? |]
  (Only (pid ^. _ProjectId))

pgEval (FindProject (ProjectId pid)) = headMay <$> pquery
  projectParser
  [sql| SELECT project_name, inception_date, initiator_id, depreciation_fn FROM projects WHERE id = ? |]
  (Only pid)

pgEval (FindUserProjects (UserId uid)) = pquery
  ((,) <$> idParser ProjectId <*> projectParser)
  [sql| SELECT DISTINCT ON (p.inception_date, p.id) p.id, p.project_name, p.inception_date, p.initiator_id, p.depreciation_fn
          FROM projects p LEFT OUTER JOIN project_companions pc ON pc.project_id = p.id
          WHERE pc.user_id = ?
          OR p.initiator_id = ?
          ORDER BY p.inception_date, p.id |]
  (uid, uid)

pgEval (AddUserToProject pid current new) = void $ pexec
  [sql| INSERT INTO project_companions (project_id, user_id, invited_by) VALUES (?, ?, ?) |]
  (pid ^. _ProjectId, new ^. _UserId, current ^. _UserId)

pgEval dbop@(CreateBillable _ b) = do
  eventId <- requireEventId dbop
  pinsert
    B.BillableId
    [sql| INSERT INTO billables
          ( project_id, event_id, name, description
          , recurrence_type, recurrence_count
          , billing_amount, grace_period_days
          , payment_request_email_template
          , payment_request_memo_template)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id |]
    ( b ^. (B.project . _ProjectId)
    , eventId ^. _EventId
    , b ^. B.name
    , b ^. B.description
    , b ^. (B.recurrence . to B.recurrenceName)
    , b ^. (B.recurrence . to B.recurrenceCount)
    , b ^. (B.amount . satoshi)
    , b ^. (B.gracePeriod)
    , b ^. (B.paymentRequestEmailTemplate)
    , b ^. (B.paymentRequestMemoTemplate)
    )

pgEval (FindBillable bid) = headMay <$> pquery
  billableParser
  [sql| SELECT b.project_id, e.created_by, b.name, b.description,
                 b.recurrence_type, b.recurrence_count,
                 b.billing_amount, b.grace_period_days,
                 b.payment_request_email_template, b.payment_request_memo_template
          FROM billables b JOIN aftok_events e ON e.id = b.event_id
          WHERE b.id = ? |]
  (Only (bid ^. B._BillableId))

pgEval (FindBillables pid) = pquery
  ((,) <$> idParser B.BillableId <*> billableParser)
  [sql| SELECT b.id, b.project_id, e.created_by, b.name, b.description,
                 b.recurrence_type, b.recurrence_count,
                 b.billing_amount, b.grace_period_days
                 b.payment_request_email_template, b.payment_request_memo_template
          FROM billables b JOIN aftok_events e ON e.id = b.event_id
          WHERE b.project_id = ? |]
  (Only (pid ^. _ProjectId))

pgEval dbop@(CreateSubscription uid bid start_date) = do
  eventId <- requireEventId dbop
  pinsert
    B.SubscriptionId
    [sql| INSERT INTO subscriptions
          (user_id, billable_id, event_id, start_date)
          VALUES (?, ?, ?, ?) RETURNING id |]
    ( view _UserId       uid
    , view B._BillableId bid
    , view _EventId      eventId
    , fromThyme start_date
    )

pgEval (FindSubscription sid) = headMay <$> pquery
  subscriptionParser
  [sql| SELECT id, billable_id, start_date, end_date
          FROM subscriptions s
          WHERE s.id = ? |]
  (Only (sid ^. B._SubscriptionId))

pgEval (FindSubscriptions uid pid) = pquery
  ((,) <$> idParser B.SubscriptionId <*> subscriptionParser)
  [sql| SELECT s.id, user_id, billable_id, start_date, end_date
          FROM subscriptions s
          JOIN billables b ON b.id = s.billable_id
          WHERE s.user_id = ?
          AND b.project_id = ? |]
  (uid ^. _UserId, pid ^. _ProjectId)


pgEval dbop@(CreatePaymentRequest req) = do
  eventId <- requireEventId dbop
  pinsert
    PaymentRequestId
    [sql| INSERT INTO payment_requests
          (subscription_id, event_id, request_data, url_key, request_time, billing_date)
          VALUES (?, ?, ?, ?, ?, ?) RETURNING id |]
    ( req ^. (subscription . B._SubscriptionId)
    , eventId ^. _EventId
    , req ^. (paymentRequest . to (runPut . encodeMessage))
    , req ^. (paymentKey . _PaymentKey)
    , req ^. (paymentRequestTime . to fromThyme)
    , req ^. (billingDate . to fromThyme)
    )

pgEval (FindPaymentRequest (PaymentKey k)) = headMay <$> pquery
  ((,) <$> idParser PaymentRequestId <*> paymentRequestParser)
  [sql| SELECT id, subscription_id, request_data, url_key, request_time, billing_date
        FROM payment_requests
        WHERE url_key = ?
        AND id NOT IN (SELECT payment_request_id FROM payments) |]
  (Only k)

pgEval (FindPaymentRequestId (PaymentRequestId prid)) = headMay <$> pquery
  paymentRequestParser
  [sql| SELECT subscription_id, request_data, url_key, request_time, billing_date
        FROM payment_requests
        WHERE id = ? |]
  (Only prid)

pgEval (FindPaymentRequests sid) = pquery
  ((,) <$> idParser PaymentRequestId <*> paymentRequestParser)
  [sql| SELECT id, subscription_id, request_data, url_key, request_time, billing_date
        FROM payment_requests
        WHERE subscription_id = ? |]
  (Only (sid ^. B._SubscriptionId))

pgEval (FindUnpaidRequests sid) =
  let rowp :: RowParser (PaymentKey, PaymentRequest, B.Subscription, B.Billable)
      rowp =
          (,,,)
            <$> (PaymentKey <$> field)
            <*> paymentRequestParser
            <*> subscriptionParser
            <*> billableParser
  in  pquery
        rowp
        [sql| SELECT r.url_key,
                   r.subscription_id, r.request_data, r.url_key, r.request_time, r.billing_date,
                   s.user_id, s.billable_id, s.start_date, s.end_date,
                   b.project_id, e.created_by, b.name, b.description, b.recurrence_type,
                   b.recurrence_count, b.billing_amount, b.grace_period_days,
                   b.payment_request_email_template, b.payment_request_memo_template
            FROM payment_requests r
            JOIN subscriptions s on s.id = r.subscription_id
            JOIN billables b on b.id = s.billable_id
            JOIN aftok_events e on e.id = b.event_id
            WHERE subscription_id = ?
            AND r.id NOT IN (SELECT payment_request_id FROM payments) |]
        (Only (sid ^. B._SubscriptionId))

pgEval dbop@(CreatePayment p) = do
  eventId <- requireEventId dbop
  pinsert
    PaymentId
    [sql| INSERT INTO payments
          (payment_request_id, event_id, payment_data, payment_date, exchange_rates)
          VALUES (?, ?, ?, ?, ?) RETURNING id |]
    ( p ^. (request . _PaymentRequestId)
    , eventId ^. _EventId
    , p ^. (payment . to (runPut . encodeMessage))
    , p ^. (paymentDate . to fromThyme)
    , p ^. exchangeRates
    )

pgEval (FindPayments rid) = pquery
  ((,) <$> idParser PaymentId <*> paymentParser)
  [sql| SELECT id, payment_request_id, payment_data, payment_date
        FROM payments
        WHERE payment_request_id = ? |]
  (Only (rid ^. _PaymentRequestId))

pgEval (RaiseDBError err _) = raiseError err

requireEventId :: DBOp a -> QDBM EventId
requireEventId = maybe (raiseError EventStorageFailed) id . storeEvent

raiseError :: DBError -> QDBM a
raiseError = QDBM . lift . throwE
