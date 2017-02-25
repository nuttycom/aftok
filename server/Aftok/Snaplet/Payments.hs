module Aftok.Snaplet.Payments where

import           ClassyPrelude

import           Control.Lens        (view)
import           Data.Thyme.Clock    as C
import           Network.Bippy.Proto as P

import           Snap.Snaplet        as S

import           Aftok.Billables
import           Aftok.Payments
import           Aftok.Database

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
  pkBytes <- requireParam "paymentRequestKey" 
  pkey <- maybe 
          (snapError 400 $ "parameter paymentRequestKey is formatted incorrectly.") pure 
          (parsePaymentKey pkBytes)
  prMay <- snapEval $ findPaymentRequest pkey
  maybe (snapError 404 $ "Outstanding payment request not found for key " <> (view _PaymentKey pkey))
        (pure . view paymentRequest)
        prMay








