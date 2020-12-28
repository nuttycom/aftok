{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Aftok.Database.PostgreSQL.Billing
  ( createBillable,
    findBillable,
    findBillables,
    createSubscription,
    findSubscription,
    findSubscriptions,
    findSubscribers,
    storePaymentRequest,
    findPaymentRequestByKey,
    findPaymentRequestById,
    findSubscriptionPaymentRequests,
    findSubscriptionUnpaidRequests,
    createPayment,
    findPayments,
  )
where

import Aftok.Billing
  ( Billable,
    Billable' (..),
    BillableId (..),
    ContactChannel (..),
    Recurrence (..),
    Subscription,
    Subscription' (..),
    SubscriptionId (..),
    _BillableId,
    _SubscriptionId,
    amount,
    description,
    gracePeriod,
    name,
    paymentRequestEmailTemplate,
    paymentRequestMemoTemplate,
    project,
    recurrence,
    recurrenceCount,
    recurrenceName,
  )
import Aftok.Currency (Amount (..), Currency (..))
import qualified Aftok.Currency.Bitcoin as Bitcoin
import Aftok.Currency.Bitcoin (Satoshi)
import Aftok.Currency.Zcash (Zatoshi)
import qualified Aftok.Currency.Zcash.Zip321 as Zip321
import Aftok.Database.PostgreSQL.Json (parseBitcoinPaymentJSON, parseZcashPaymentJSON, paymentJSON)
import Aftok.Database.PostgreSQL.Types
  ( DBM,
    currencyAmountParser,
    currencyType,
    currencyValue,
    idParser,
    nominalDiffTimeParser,
    nullField,
    pinsert,
    pquery,
  )
import Aftok.Payments.Types
  ( NativePayment (..),
    NativeRequest (..),
    Payment,
    Payment' (Payment),
    PaymentId (..),
    PaymentKey (..),
    PaymentRequest,
    PaymentRequest' (..),
    PaymentRequestId (..),
    PaymentRequestId,
    SomePaymentRequest (..),
    SomePaymentRequestDetail,
    _PaymentRequestId,
    paymentDate,
    paymentRequest,
  )
import Aftok.TimeLog
  ( EventId (..),
    _EventId,
  )
import Aftok.Types
  ( Email (..),
    ProjectId (..),
    UserId (..),
    _ProjectId,
    _UserId,
  )
import Control.Lens ((.~), (^.), to, view)
import Data.Aeson (encode)
import Data.Aeson.Types (parseEither)
import Data.ProtocolBuffers (decodeMessage)
import Data.Serialize.Get (runGet)
import qualified Data.Thyme.Clock as C
import qualified Data.Thyme.Time as C
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.FromRow (RowParser, field, fieldWith)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Safe (headMay)
import Prelude hiding (null)

billableParser :: RowParser (Billable Amount)
billableParser =
  Billable
    <$> idParser ProjectId
    <*> idParser UserId
    <*> field
    <*> field
    <*> field
    <*> recurrenceParser
    <*> currencyAmountParser
    <*> field
    <*> fieldWith nominalDiffTimeParser
    <*> field
    <*> field

recurrenceParser :: RowParser Recurrence
recurrenceParser =
  let prec :: Text -> RowParser Recurrence
      prec = \case
        "annually" -> nullField *> pure Annually
        "monthly" -> Monthly <$> field
        --"semimonthly" = nullField *> pure SemiMonthly
        "weekly" -> Weekly <$> field
        "onetime" -> nullField *> pure OneTime
        _ -> empty
   in field >>= prec

subscriptionParser :: RowParser Subscription
subscriptionParser =
  Subscription
    <$> idParser UserId
    <*> idParser BillableId
    <*> (EmailChannel . Email <$> field)
    <*> (C.toThyme <$> field)
    <*> ((fmap C.toThyme) <$> field)

bip70RequestParser :: RowParser (NativeRequest Satoshi)
bip70RequestParser =
  Bip70Request <$> ((either (const empty) pure . runGet decodeMessage) =<< field)

zip321RequestParser :: RowParser (NativeRequest Zatoshi)
zip321RequestParser =
  Zip321Request <$> ((either (const empty) pure . Zip321.parseZip321URI) =<< field)

paymentRequestParser :: RowParser SomePaymentRequestDetail
paymentRequestParser = do
  billable <- billableParser
  createdAt :: C.UTCTime <- C.toThyme <$> field
  billingDate :: C.Day <- C.toThyme <$> field
  case billable ^. amount of
    (Amount BTC sats) -> do
      nativeReq <- bip70RequestParser
      pure . SomePaymentRequest $ PaymentRequest (billable & amount .~ sats) createdAt billingDate nativeReq
    (Amount ZEC zats) -> do
      nativeReq <- zip321RequestParser
      pure . SomePaymentRequest $ PaymentRequest (billable & amount .~ zats) createdAt billingDate nativeReq

paymentParser :: Bitcoin.NetworkMode -> PaymentRequestId -> Currency a c -> RowParser (Payment c)
paymentParser nmode prid ccy = do
  d :: C.UTCTime <- C.toThyme <$> field
  case ccy of
    BTC -> Payment (Const prid) d <$> bitcoinPaymentParser nmode
    ZEC -> Payment (Const prid) d <$> zcashPaymentParser

bitcoinPaymentParser :: Bitcoin.NetworkMode -> RowParser (NativePayment Satoshi)
bitcoinPaymentParser nmode = do
  pvalue <- field
  either
    (const empty)
    (pure . BitcoinPayment)
    (parseEither (parseBitcoinPaymentJSON nmode) pvalue)

zcashPaymentParser :: RowParser (NativePayment Zatoshi)
zcashPaymentParser = do
  pvalue <- field
  either
    (const empty)
    (pure . ZcashPayment)
    (parseEither parseZcashPaymentJSON pvalue)

createBillable :: EventId -> UserId -> Billable Amount -> DBM BillableId
createBillable eventId _ b = do
  pinsert
    BillableId
    [sql| INSERT INTO billables
          ( project_id, event_id, name, description
          , recurrence_type, recurrence_count
          , billing_currency, billing_amount
          , grace_period_days
          , payment_request_email_template
          , payment_request_memo_template)
          VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id |]
    ( b ^. (project . _ProjectId),
      eventId ^. _EventId,
      b ^. name,
      b ^. description,
      b ^. (recurrence . to recurrenceName),
      b ^. (recurrence . to recurrenceCount),
      b ^. (amount . to currencyType),
      b ^. (amount . to currencyValue),
      b ^. (gracePeriod),
      b ^. (paymentRequestEmailTemplate),
      b ^. (paymentRequestMemoTemplate)
    )

findBillable :: BillableId -> DBM (Maybe (Billable Amount))
findBillable bid =
  headMay
    <$> pquery
      billableParser
      [sql| SELECT b.project_id, e.created_by, b.name, b.description,
                 b.recurrence_type, b.recurrence_count,
                 b.billing_currency, b.billing_amount,
                 b.grace_period_days,
                 b.payment_request_email_template, b.payment_request_memo_template
          FROM billables b JOIN aftok_events e ON e.id = b.event_id
          WHERE b.id = ? |]
      (Only (bid ^. _BillableId))

findBillables :: ProjectId -> DBM [(BillableId, Billable Amount)]
findBillables pid =
  pquery
    ((,) <$> idParser BillableId <*> billableParser)
    [sql| SELECT b.id, b.project_id, e.created_by, b.name, b.description,
                 b.recurrence_type, b.recurrence_count,
                 b.billing_currency, b.billing_amount,
                 b.grace_period_days
                 b.payment_request_email_template, b.payment_request_memo_template
          FROM billables b JOIN aftok_events e ON e.id = b.event_id
          WHERE b.project_id = ? |]
    (Only (pid ^. _ProjectId))

createSubscription :: EventId -> UserId -> BillableId -> C.Day -> DBM SubscriptionId
createSubscription eventId uid bid start_date =
  pinsert
    SubscriptionId
    [sql| INSERT INTO subscriptions
          (user_id, billable_id, event_id, start_date)
          VALUES (?, ?, ?, ?) RETURNING id |]
    ( view _UserId uid,
      view _BillableId bid,
      view _EventId eventId,
      C.fromThyme start_date
    )

findSubscription :: SubscriptionId -> DBM (Maybe Subscription)
findSubscription sid =
  headMay
    <$> pquery
      subscriptionParser
      [sql| SELECT id, billable_id, contact_email, start_date, end_date
          FROM subscriptions s
          WHERE s.id = ? |]
      (Only (sid ^. _SubscriptionId))

findSubscriptions :: ProjectId -> UserId -> DBM [(SubscriptionId, Subscription)]
findSubscriptions pid uid =
  pquery
    ((,) <$> idParser SubscriptionId <*> subscriptionParser)
    [sql| SELECT s.id, user_id, billable_id, contact_email, start_date, end_date
          FROM subscriptions s
          JOIN billables b ON b.id = s.billable_id
          WHERE s.user_id = ?
          AND b.project_id = ? |]
    (uid ^. _UserId, pid ^. _ProjectId)

findSubscribers :: ProjectId -> DBM [UserId]
findSubscribers pid =
  pquery
    (idParser UserId)
    [sql| SELECT s.user_id
          FROM subscripions s
          JOIN billables b ON s.billable_id = b.id
          WHERE b.project_id = ? |]
    (Only (pid ^. _ProjectId))

storePaymentRequest :: EventId -> PaymentRequestId -> PaymentRequest c -> DBM ()
storePaymentRequest _ _ = error "TODO"

--storeBip70PaymentRequest   :: EventId -> PaymentRequestId -> PaymentRequest c -> DBM ()
--storeBip70PaymentRequest rid req =
--  pinsert
--    PaymentRequestId
--    [sql| INSERT INTO payment_requests
--          (subscription_id, event_id, request_data, url_key, request_time, billing_date)
--          VALUES (?, ?, ?, ?, ?, ?) RETURNING id |]
--    ( req ^. (subscription . _SubscriptionId)
--    , eventId ^. _EventId
--    , req ^. (paymentRequest . to (runPut . encodeMessage))
--    , req ^. (paymentKey . _PaymentKey)
--    , req ^. (paymentRequestTime . to C.fromThyme)
--    , req ^. (billingDate . to C.fromThyme)
--    )

findPaymentRequestByKey :: PaymentKey -> DBM (Maybe (PaymentRequestId, SomePaymentRequestDetail))
findPaymentRequestByKey (PaymentKey k) =
  headMay
    <$> pquery
      ((,) <$> idParser PaymentRequestId <*> paymentRequestParser)
      [sql| SELECT id, subscription_id, request_data, url_key, request_time, billing_date
        FROM payment_requests
        WHERE url_key = ?
        AND id NOT IN (SELECT payment_request_id FROM payments) |]
      (Only k)

findPaymentRequestById :: PaymentRequestId -> DBM (Maybe SomePaymentRequestDetail)
findPaymentRequestById (PaymentRequestId prid) =
  headMay
    <$> pquery
      paymentRequestParser
      [sql| SELECT subscription_id, request_data, url_key, request_time, billing_date
        FROM payment_requests
        WHERE id = ? |]
      (Only prid)

findSubscriptionPaymentRequests :: SubscriptionId -> DBM [(PaymentRequestId, SomePaymentRequestDetail)]
findSubscriptionPaymentRequests sid =
  pquery
    ((,) <$> idParser PaymentRequestId <*> paymentRequestParser)
    [sql| SELECT id, subscription_id, request_data, url_key, request_time, billing_date
        FROM payment_requests
        WHERE subscription_id = ? |]
    (Only (sid ^. _SubscriptionId))

findSubscriptionUnpaidRequests :: SubscriptionId -> DBM [(PaymentRequestId, SomePaymentRequestDetail)]
findSubscriptionUnpaidRequests sid =
  pquery
    ((,) <$> idParser PaymentRequestId <*> paymentRequestParser)
    [sql| SELECT r.url_key,
             r.subscription_id, r.request_data, r.url_key, r.request_time, r.billing_date,

             s.user_id, s.billable_id, s.contact_email, s.start_date, s.end_date,

             b.project_id, e.created_by, b.name, b.description, b.recurrence_type,
             b.recurrence_count, b.billing_currency, b.billing_amount, b.grace_period_days,
             b.payment_request_email_template, b.payment_request_memo_template
      FROM payment_requests r
      JOIN subscriptions s on s.id = r.subscription_id
      JOIN billables b on b.id = s.billable_id
      JOIN aftok_events e on e.id = b.event_id
      WHERE subscription_id = ?
      AND r.id NOT IN (SELECT payment_request_id FROM payments) |]
    (Only (sid ^. _SubscriptionId))

createPayment :: EventId -> Payment c -> DBM PaymentId
createPayment eventId p = do
  nmode <- asks fst
  pinsert
    PaymentId
    [sql| INSERT INTO payments
          (payment_request_id, event_id, payment_data, payment_date)
          VALUES (?, ?, ?, ?) RETURNING id |]
    ( p ^. (paymentRequest . to getConst . _PaymentRequestId),
      eventId ^. _EventId,
      p ^. (to (paymentJSON nmode) . to encode),
      p ^. (paymentDate . to C.fromThyme)
    )

findPayments :: Currency a c -> PaymentRequestId -> DBM [(PaymentId, Payment c)]
findPayments ccy rid = do
  nmode <- asks fst
  pquery
    ((,) <$> idParser PaymentId <*> paymentParser nmode rid ccy)
    [sql| SELECT id, payment_request_id, payment_date, payment_data
        FROM payments
        WHERE payment_request_id = ? |]
    (Only (rid ^. _PaymentRequestId))
