{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Billing
  ( billableCreateHandler,
    billableListHandler,
    subscribeHandler,
    createPaymentRequestHandler,
  )
where

import Aftok.Billing
  ( Billable,
    Billable' (..),
    BillableId (..),
    Recurrence (..),
    SubscriptionId,
  )
import qualified Aftok.Billing as B
import Aftok.Currency (Amount (..), Currency (..))
import Aftok.Currency.Bitcoin (Satoshi (..))
import Aftok.Currency.Zcash (Zatoshi (..))
import Aftok.Database
  ( DBOp (..),
    createBillable,
    liftdb,
    withProjectAuth,
  )
import Aftok.Json (Version (..), badVersion, unversion)
import Aftok.Payments
  ( PaymentRequestId,
    PaymentsConfig,
    SomePaymentRequest (..),
    SomePaymentRequestDetail,
    createPaymentRequest,
    zcashPaymentsConfig,
  )
import Aftok.Payments.Types
  ( PaymentRequestError (..),
  )
import qualified Aftok.Payments.Zcash as Zcash
import Aftok.Snaplet
  ( App,
    readRequestJSON,
    requireId,
    requireProjectId,
    snapError,
    snapEval,
  )
import Aftok.Snaplet.Auth (requireUserId)
import Aftok.Types (ProjectId, UserId)
import Control.Lens ((.~), (^.))
-- import Data.Aeson ()
import Data.Aeson.Types
  ( (.:),
    (.:?),
    Object,
    Parser,
    Value (..),
    parseEither,
    parseJSON,
  )
import qualified Data.HashMap.Strict as O
import qualified Data.Thyme.Clock as C
import Data.Thyme.Time.Core (toThyme)
import Snap.Core (MonadSnap)
import qualified Snap.Snaplet as S

parseCreateBillable :: UserId -> ProjectId -> Value -> Parser (Billable Amount)
parseCreateBillable uid pid = unversion "Billable" p
  where
    amountParser = \case
      "ZEC" -> pure (Amount ZEC . Zatoshi)
      "BTC" -> pure (Amount BTC . Satoshi)
      c -> fail ("Currency " <> c <> " not recognized.")
    p (Version 1 0) o =
      Billable
        <$> pure pid
        <*> pure uid
        <*> (o .: "name")
        <*> (o .: "description")
        <*> (o .: "message")
        <*> (parseRecurrence' =<< o .: "recurrence")
        <*> ((o .: "currency" >>= amountParser) <*> o .: "amount")
        <*> (o .: "gracePeriod")
        <*> (toThyme <$> o .: "requestExpiryPeriod")
        <*> (o .:? "paymentRequestEmailTemplate")
        <*> (o .:? "paymentRequestMemoTemplate")
    p ver o = badVersion "Billable" ver o

billableCreateHandler :: S.Handler App App BillableId
billableCreateHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  requestBody <- readRequestJSON 4096
  b <-
    either (snapError 400 . show) pure $
      parseEither (parseCreateBillable uid pid) requestBody
  snapEval $ createBillable uid b

billableListHandler :: S.Handler App App [(BillableId, Billable Amount)]
billableListHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  snapEval $ withProjectAuth pid uid (FindBillables pid)

subscribeHandler :: S.Handler App App SubscriptionId
subscribeHandler = do
  uid <- requireUserId
  bid <- requireId "billableId" BillableId
  t <- liftIO C.getCurrentTime
  snapEval . liftdb $ CreateSubscription uid bid (t ^. C._utctDay)

createPaymentRequestHandler ::
  MonadSnap m =>
  PaymentsConfig m ->
  S.Handler App App (PaymentRequestId, SomePaymentRequestDetail)
createPaymentRequestHandler cfg = do
  uid <- requireUserId
  pid <- requireProjectId
  bid <- requireId "billableId" BillableId
  billable <- snapEval $ withProjectAuth pid uid (FindBillable bid)
  now <- liftIO C.getCurrentTime
  let billDay = now ^. C._utctDay
  case billable of
    -- check that the billable is actually related to the project that the user
    -- is authorized for & the URL specifies
    Just b | (b ^. B.project == pid) ->
      case b ^. B.amount of
        Amount ZEC v -> do
          let ops = Zcash.paymentOps (cfg ^. zcashPaymentsConfig)
          res <- snapEval . runExceptT $ createPaymentRequest ops now bid (b & B.amount .~ v) billDay
          case res of
            Left AmountInvalid -> snapError 400 $ "Invalid payment amount requested."
            Left NoRecipients -> snapError 400 $ "This project has no payable members."
            Right (reqId, detail) ->
              pure (reqId, SomePaymentRequest detail)
        Amount BTC _ ->
          snapError 400 $ "Bitcoin payment requests not yet supported."
    _ ->
      snapError 404 $ "Billable not found."

-- subscriptionJSON :: Subscription -> Value
-- subscriptionJSON = v1 . obj . subscriptionKV
--
-- subscriptionKV :: (KeyValue kv) => Subscription -> [kv]
-- subscriptionKV sub =
--   [ "user_id" .= idValue (customer . _UserId) sub,
--     "billable_id" .= idValue (billable . _BillableId) sub,
--     "start_time" .= view startTime sub,
--     "end_time" .= view endTime sub
--   ]

-- paymentRequestDetailsJSON :: [PaymentRequestDetail Amount] -> Value
-- paymentRequestDetailsJSON r = v1 $ obj ["payment_requests" .= fmap paymentRequestDetailJSON r]
--
-- paymentRequestDetailJSON :: PaymentRequestDetail Amount -> Object
-- paymentRequestDetailJSON r = obj $ concat
--   [ ["payment_request_id" .= view () r]
--   , paymentRequestKV $ view _2 r
--   , subscriptionKV $ view _3 r
--   , billableKV $ view _4 r
--   ]

parseRecurrence :: Object -> Parser Recurrence
parseRecurrence o =
  let parseAnnually o' = const (pure Annually) <$> O.lookup "annually" o'
      parseMonthly o' = fmap Monthly . parseJSON <$> O.lookup "monthly" o'
      parseWeekly o' = fmap Weekly . parseJSON <$> O.lookup "weekly" o'
      parseOneTime o' = const (pure OneTime) <$> O.lookup "onetime" o'
      notFound =
        fail $ "Value " <> show o <> " does not represent a Recurrence value."
      parseV val =
        parseAnnually val
          <|> parseMonthly val
          <|> parseWeekly val
          <|> parseOneTime val
   in fromMaybe notFound $ parseV o

parseRecurrence' :: Value -> Parser Recurrence
parseRecurrence' = \case
  (Object o) -> parseRecurrence o
  val -> fail $ "Value " <> show val <> " is not a JSON object."
