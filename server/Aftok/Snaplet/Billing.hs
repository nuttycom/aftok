{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Billing
  ( billableCreateHandler,
    billableListHandler,
    subscribeHandler,
  )
where

import Aftok.Billing
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
