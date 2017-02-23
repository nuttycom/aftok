module Aftok.Snaplet.Payments where

import           ClassyPrelude

import           Control.Lens        (view, _1, _2)
import           Data.Thyme.Clock    as C
import           Network.Bippy.Proto as P

import           Snap.Snaplet        as S

import           Aftok.Billables
import           Aftok.Payments

import           Aftok.Snaplet
import           Aftok.Snaplet.Auth

listPayableRequestsHandler :: S.Handler App App [BillDetail]
listPayableRequestsHandler = do
  uid <- requireUserId
  sid <- requireId "subscriptionId" SubscriptionId
  now <- liftIO $ C.getCurrentTime
  snapEval $ findPayableRequests uid sid now

getPaymentRequestHandler :: S.Handler App App P.PaymentRequest
getPaymentRequestHandler = do
  uid <- requireUserId
  sid <- requireId "subscriptionId" SubscriptionId
  rid <- requireId "paymentRequestId" PaymentRequestId
  now <- liftIO $ C.getCurrentTime
  requests <- snapEval $ findPayableRequests uid sid now
  let prMay = fmap (view (_2 . paymentRequest)) . headMay $ filter ((==) rid . view _1) requests
  maybe (snapError 404 $ "Outstanding payment request not found for id " <> tshow rid) pure prMay








