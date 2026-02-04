{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Payments API types for the Aftok API.
module Aftok.API.Payments
  ( -- * API Types
    PaymentsAPI,
    ProtectedPaymentsAPI,
  )
where

import Aftok.Billing (SubscriptionId (..))
import Aftok.Payments.Types (PaymentId)
import Data.Aeson (Value)
import Servant.API

--------------------------------------------------------------------------------
-- API Types
--------------------------------------------------------------------------------

-- | Public Payments API (none currently)
type PaymentsAPI = EmptyAPI

-- | Protected payments API
type ProtectedPaymentsAPI =
  -- GET /subscriptions/:subscriptionId/paymentRequests - List payable requests
  ( "subscriptions"
      :> Capture "subscriptionId" SubscriptionId
      :> "paymentRequests"
      :> Get '[JSON] Value
  )
    -- BIP70 payment endpoints
    :<|> "pay"
      :> "btc"
      :> Capture "paymentRequestKey" Text
      :> ( -- GET /pay/btc/:paymentRequestKey - Get BIP70 payment request (returns protobuf)
           Get '[OctetStream] ByteString
             -- POST /pay/btc/:paymentRequestKey - Submit BIP70 payment
             :<|> ReqBody '[OctetStream] ByteString :> Post '[JSON] PaymentId
         )
