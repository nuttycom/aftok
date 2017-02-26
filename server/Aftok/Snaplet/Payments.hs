module Aftok.Snaplet.Payments
  ( listPayableRequestsHandler
  , getPaymentRequestHandler
  , paymentResponseHandler
  ) where

import           ClassyPrelude

import           Control.Lens         (view, _1, _2)
import           Data.ProtocolBuffers (decodeMessage)
import           Data.Serialize.Get   (runGetLazy)
import           Data.Thyme.Clock     as C
import qualified Network.Bippy.Proto  as P

import           Snap.Core            (readRequestBody)
import           Snap.Snaplet         as S

import           Aftok.Billables
import           Aftok.Database
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
getPaymentRequestHandler =
  view (_2 . paymentRequest) <$> getPaymentRequestHandler'

paymentResponseHandler :: S.Handler App App PaymentId
paymentResponseHandler = do
  requestBody <- readRequestBody 4096
  preq <- getPaymentRequestHandler'
  pmnt <- either
          (\msg -> snapError 400 $ "Could not decode payment response: " <> tshow msg)
          pure
          (runGetLazy decodeMessage requestBody)
  now  <- liftIO $ C.getCurrentTime
  snapEval . liftdb . CreatePayment $ Payment (view _1 preq) pmnt now

getPaymentRequestHandler' :: S.Handler App App (PaymentRequestId, PaymentRequest)
getPaymentRequestHandler' = do
  pkBytes <- requireParam "paymentRequestKey"
  pkey <- maybe
          (snapError 400 $ "parameter paymentRequestKey is formatted incorrectly.") pure
          (parsePaymentKey pkBytes)
  prMay <- snapEval $ findPaymentRequest pkey
  maybe (snapError 404 $ "Outstanding payment request not found for key " <> (view _PaymentKey pkey))
        pure prMay



