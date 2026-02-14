{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Payments API types for the Aftok API.
module Aftok.API.Payments
  ( -- * API Types
    PaymentsAPI,
    ProtectedPaymentsAPI,

    -- * Data Types
    BIP70Data (..),
  )
where

import Aftok.Billing (SubscriptionId (..))
import Aftok.Payments.Types (PaymentId)
import Data.Aeson (Value)
import Servant.API

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- | Newtype wrapper for BIP70 protobuf binary data.
-- This is needed because openapi3 refuses to provide a ToSchema
-- instance for raw ByteString and requires a newtype wrapper.
newtype BIP70Data = BIP70Data {unBIP70Data :: ByteString}
  deriving (MimeRender OctetStream, MimeUnrender OctetStream)

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
           Get '[OctetStream] BIP70Data
             -- POST /pay/btc/:paymentRequestKey - Submit BIP70 payment
             :<|> ReqBody '[OctetStream] BIP70Data :> Post '[JSON] PaymentId
         )
