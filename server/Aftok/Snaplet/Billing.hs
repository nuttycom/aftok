{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Billing
  ( billableCreateHandler
  , billableListHandler
  , subscribeHandler
  )
where



import           Control.Lens                   ( (^.) )
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Thyme.Clock              as C
import           Data.Thyme.Time.Core           ( toThyme )

import           Snap.Snaplet                  as S

import           Aftok.Types                    ( UserId )
import           Aftok.Billables
import           Network.Bippy.Types            ( Satoshi(..) )
import           Aftok.Json
import           Aftok.Types
import           Aftok.Database                 ( createBillable
                                                , withProjectAuth
                                                , liftdb
                                                , DBOp(..)
                                                )

import           Aftok.Snaplet
import           Aftok.Snaplet.Auth

parseCreateBillable :: UserId -> ProjectId -> Value -> Parser Billable
parseCreateBillable uid pid = unversion "Billable" p where
  p (Version 1 0) o =
    Billable
      <$> pure pid
      <*> pure uid
      <*> o
      .:  "name"
      <*> o
      .:  "description"
      <*> (parseRecurrence' =<< o .: "recurrence")
      <*> (Satoshi <$> o .: "amount")
      <*> o
      .:  "gracePeriod"
      <*> (fmap toThyme <$> o .: "requestExpiryPeriod")
      <*> o
      .:? "paymentRequestEmailTemplate"
      <*> o
      .:? "paymentRequestMemoTemplate"

  p ver o = badVersion "Billable" ver o

billableCreateHandler :: S.Handler App App BillableId
billableCreateHandler = do
  uid         <- requireUserId
  pid         <- requireProjectId
  requestBody <- readRequestJSON 4096
  b           <- either (snapError 400 . show) pure
    $ parseEither (parseCreateBillable uid pid) requestBody
  snapEval $ createBillable uid b

billableListHandler :: S.Handler App App [(BillableId, Billable)]
billableListHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  snapEval $ withProjectAuth pid uid (FindBillables pid)

subscribeHandler :: S.Handler App App SubscriptionId
subscribeHandler = do
  uid <- requireUserId
  bid <- requireId "billableId" BillableId
  t   <- liftIO C.getCurrentTime
  snapEval . liftdb $ CreateSubscription uid bid (t ^. C._utctDay)

