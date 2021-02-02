{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Billing
  ( billableCreateHandler,
    billableListHandler,
    subscribeHandler,
  )
where

import Aftok.Billing as B
import Aftok.Currency (Amount (..), Currency (..))
import Aftok.Currency.Bitcoin (Satoshi (..))
import Aftok.Currency.Zcash (Zatoshi (..))
import Aftok.Database
  ( DBOp (..),
    createBillable,
    liftdb,
    withProjectAuth,
  )
import Aftok.Json
import Aftok.Snaplet
import Aftok.Snaplet.Auth
import Aftok.Types
import Control.Lens ((^.))
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as O
import Data.Thyme.Clock as C
import Data.Thyme.Time.Core (toThyme)
import Snap.Snaplet as S

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

-- subscriptionJSON :: B.Subscription -> Value
-- subscriptionJSON = v1 . obj . subscriptionKV
--
-- subscriptionKV :: (KeyValue kv) => B.Subscription -> [kv]
-- subscriptionKV sub =
--   [ "user_id" .= idValue (B.customer . _UserId) sub,
--     "billable_id" .= idValue (B.billable . B._BillableId) sub,
--     "start_time" .= view B.startTime sub,
--     "end_time" .= view B.endTime sub
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

parseRecurrence :: Object -> Parser B.Recurrence
parseRecurrence o =
  let parseAnnually o' = const (pure B.Annually) <$> O.lookup "annually" o'
      parseMonthly o' = fmap B.Monthly . parseJSON <$> O.lookup "monthly" o'
      parseWeekly o' = fmap B.Weekly . parseJSON <$> O.lookup "weekly" o'
      parseOneTime o' = const (pure B.OneTime) <$> O.lookup "onetime" o'
      notFound =
        fail $ "Value " <> show o <> " does not represent a Recurrence value."
      parseV val =
        parseAnnually val
          <|> parseMonthly val
          <|> parseWeekly val
          <|> parseOneTime val
   in fromMaybe notFound $ parseV o

parseRecurrence' :: Value -> Parser B.Recurrence
parseRecurrence' = \case
  (Object o) -> parseRecurrence o
  val -> fail $ "Value " <> show val <> " is not a JSON object."
