{-# LANGUAGE TypeApplications #-}

module Aftok.Snaplet.Payments
  ( listPayableRequestsHandler
  , getPaymentRequestHandler
  , paymentResponseHandler
  ) where

import           ClassyPrelude

import           Control.Lens         (view, _1, _2, _Right, _Left, preview, (&), (.~))
import           Data.ProtocolBuffers (decodeMessage)
import           Data.Serialize.Get   (runGetLazy)
import           Data.Thyme.Clock     as C
import qualified Network.Bippy.Proto  as P
import           Network.HTTP.Client.OpenSSL
import           Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, HttpException)
import           Network.Wreq         (asValue, responseBody, defaults, manager, getWith)
import           OpenSSL.Session (context)

import           Snap.Core            (readRequestBody, logError)
import           Snap.Snaplet         as S

import           Aftok.Billables
import           Aftok.Database
import           Aftok.Payments

import           Aftok.QConfig        as QC
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

paymentResponseHandler :: QC.BillingConfig -> S.Handler App App PaymentId
paymentResponseHandler cfg = do
  requestBody <- readRequestBody 4096
  preq <- getPaymentRequestHandler'
  pmnt <- either
          (\msg -> snapError 400 $ "Could not decode payment response: " <> tshow msg)
          pure
          (runGetLazy decodeMessage requestBody)
  now  <- liftIO $ C.getCurrentTime

  let opts = defaults & manager .~ Left (opensslManagerSettings context)
                      & manager .~ Left (defaultManagerSettings { managerResponseTimeout = Just 10000 } )

  exchResp  <- liftIO . try $ asValue =<< (withOpenSSL $ getWith opts (exchangeRateServiceURI cfg))
  _ <- traverse (logError . encodeUtf8 . tshow @ HttpException) (preview _Left exchResp)
  let newPayment = Payment (view _1 preq) pmnt now (preview (_Right . responseBody) exchResp)
  snapEval . liftdb $ CreatePayment newPayment

getPaymentRequestHandler' :: S.Handler App App (PaymentRequestId, PaymentRequest)
getPaymentRequestHandler' = do
  pkBytes <- requireParam "paymentRequestKey"
  pkey <- maybe
          (snapError 400 $ "parameter paymentRequestKey is formatted incorrectly.") pure
          (parsePaymentKey pkBytes)
  prMay <- snapEval $ findPaymentRequest pkey
  maybe (snapError 404 $ "Outstanding payment request not found for key " <> (view _PaymentKey pkey))
        pure prMay



