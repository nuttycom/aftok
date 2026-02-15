{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Billing API types for the Aftok API.
module Aftok.API.Billing
  ( -- * API Types
    BillingAPI,
    ProtectedBillingAPI,
    ProjectBillablesAPI,

    -- * Request Types
    BillableCreateRequest (..),
    BillableCreateResponse (..),
    SubscribeRequest (..),
    SubscribeResponse (..),
    PaymentRequestCreateRequest (..),

    -- * Response Types
    BillableResponse (..),
    PaymentRequestResponse (..),
  )
where

import Aftok.API.Codec ()
import Aftok.API.Types ()
import Aftok.Billing (BillableId (..), Recurrence (..), SubscriptionId)
import Aftok.Payments.Types (PaymentRequestId (..))
import Autodocodec (HasCodec (..), object, optionalField', requiredField')
import qualified Autodocodec as AC
import Autodocodec.Aeson (parseJSONViaCodec, toJSONViaCodec)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value,
  )
import qualified Data.Thyme.Clock as C
import Data.Thyme.Format.Aeson ()
import Servant.API

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- | Billable creation response
data BillableCreateResponse = BillableCreateResponse
  { bcrBillableId :: BillableId
  }
  deriving (Generic)

instance HasCodec BillableCreateResponse where
  codec =
    object "BillableCreateResponse" $
      BillableCreateResponse
        <$> requiredField' "billableId" AC..= bcrBillableId

instance ToJSON BillableCreateResponse where toJSON = toJSONViaCodec

-- | Subscription creation response
data SubscribeResponse = SubscribeResponse
  { srSubscriptionId :: SubscriptionId
  }
  deriving (Generic)

instance HasCodec SubscribeResponse where
  codec =
    object "SubscribeResponse" $
      SubscribeResponse
        <$> requiredField' "subscriptionId" AC..= srSubscriptionId

instance ToJSON SubscribeResponse where toJSON = toJSONViaCodec

-- | Billable creation request (nested format with schemaVersion)
data BillableCreateRequest = BillableCreateRequest
  { bcrName :: Text,
    bcrDescription :: Text,
    bcrMessage :: Text,
    bcrRecurrence :: Recurrence,
    bcrCurrency :: Text,
    bcrAmount :: Int,
    bcrGracePeriod :: Int,
    bcrRequestExpiryPeriod :: Int,
    bcrPaymentRequestEmailTemplate :: Maybe Text,
    bcrPaymentRequestMemoTemplate :: Maybe Text
  }

instance FromJSON BillableCreateRequest where parseJSON = parseJSONViaCodec

-- | Inner billable object for the codec
data BillableInner = BillableInner
  { biName :: Text,
    biDescription :: Text,
    biMessage :: Text,
    biRecurrence :: Recurrence,
    biCurrency :: Text,
    biAmount :: Int,
    biGracePeriod :: Int,
    biRequestExpiryPeriod :: Int,
    biPaymentRequestEmailTemplate :: Maybe Text,
    biPaymentRequestMemoTemplate :: Maybe Text
  }

instance HasCodec BillableInner where
  codec =
    object "BillableInner" $
      BillableInner
        <$> requiredField' "name" AC..= biName
        <*> requiredField' "description" AC..= biDescription
        <*> requiredField' "message" AC..= biMessage
        <*> requiredField' "recurrence" AC..= biRecurrence
        <*> requiredField' "currency" AC..= biCurrency
        <*> requiredField' "amount" AC..= biAmount
        <*> requiredField' "gracePeriod" AC..= biGracePeriod
        <*> requiredField' "requestExpiryPeriod" AC..= biRequestExpiryPeriod
        <*> optionalField' "paymentRequestEmailTemplate" AC..= biPaymentRequestEmailTemplate
        <*> optionalField' "paymentRequestMemoTemplate" AC..= biPaymentRequestMemoTemplate

instance HasCodec BillableCreateRequest where
  codec =
    object "BillableCreateRequest" $
      ( \_ inner ->
          BillableCreateRequest
            (biName inner)
            (biDescription inner)
            (biMessage inner)
            (biRecurrence inner)
            (biCurrency inner)
            (biAmount inner)
            (biGracePeriod inner)
            (biRequestExpiryPeriod inner)
            (biPaymentRequestEmailTemplate inner)
            (biPaymentRequestMemoTemplate inner)
      )
        <$> requiredField' "schemaVersion" AC..= (const ("1.0" :: Text))
        <*> requiredField' "Billable"
          AC..= ( \r ->
                    BillableInner
                      (bcrName r)
                      (bcrDescription r)
                      (bcrMessage r)
                      (bcrRecurrence r)
                      (bcrCurrency r)
                      (bcrAmount r)
                      (bcrGracePeriod r)
                      (bcrRequestExpiryPeriod r)
                      (bcrPaymentRequestEmailTemplate r)
                      (bcrPaymentRequestMemoTemplate r)
                )

-- | Subscribe request (currently empty, billableId comes from URL)
data SubscribeRequest = SubscribeRequest
  deriving (Generic)

instance FromJSON SubscribeRequest where
  parseJSON _ = pure SubscribeRequest

-- | Payment request creation request (currently empty)
data PaymentRequestCreateRequest = PaymentRequestCreateRequest
  deriving (Generic)

instance FromJSON PaymentRequestCreateRequest where
  parseJSON _ = pure PaymentRequestCreateRequest

-- | Billable list item response
data BillableResponse = BillableResponse
  { brBillableId :: BillableId,
    brName :: Text,
    brDescription :: Maybe Text,
    brMessage :: Maybe Text,
    brRecurrence :: Recurrence,
    brAmount :: Value,
    brGracePeriod :: Int,
    brRequestExpiryPeriod :: Int
  }

instance HasCodec BillableResponse where
  codec =
    object "BillableResponse" $
      BillableResponse
        <$> requiredField' "billableId" AC..= brBillableId
        <*> requiredField' "name" AC..= brName
        <*> requiredField' "description" AC..= brDescription
        <*> requiredField' "message" AC..= brMessage
        <*> requiredField' "recurrence" AC..= brRecurrence
        <*> requiredField' "amount" AC..= brAmount
        <*> requiredField' "gracePeriod" AC..= brGracePeriod
        <*> requiredField' "requestExpiryPeriod" AC..= brRequestExpiryPeriod

instance ToJSON BillableResponse where toJSON = toJSONViaCodec

-- | Payment request response
data PaymentRequestResponse = PaymentRequestResponse
  { prrPaymentRequestId :: PaymentRequestId,
    prrTotal :: Value,
    prrExpiresAt :: C.UTCTime,
    prrNativeRequest :: Value
  }

instance HasCodec PaymentRequestResponse where
  codec =
    object "PaymentRequestResponse" $
      PaymentRequestResponse
        <$> requiredField' "payment_request_id" AC..= prrPaymentRequestId
        <*> requiredField' "total" AC..= prrTotal
        <*> requiredField' "expires_at" AC..= prrExpiresAt
        <*> requiredField' "native_request" AC..= prrNativeRequest

instance ToJSON PaymentRequestResponse where toJSON = toJSONViaCodec

--------------------------------------------------------------------------------
-- API Types
--------------------------------------------------------------------------------

-- | Public Billing API (none currently)
type BillingAPI = EmptyAPI

-- | Protected billing API for subscriptions
type ProtectedBillingAPI =
  "subscribe"
    :> Capture "billableId" BillableId
    :> ReqBody '[JSON] SubscribeRequest
    :> Post '[JSON] SubscribeResponse

-- | Project-specific billables API (nested under projects)
type ProjectBillablesAPI =
  -- GET /projects/:projectId/billables
  Get '[JSON] [BillableResponse]
    -- POST /projects/:projectId/billables
    :<|> ReqBody '[JSON] BillableCreateRequest :> Post '[JSON] BillableCreateResponse
    -- POST /projects/:projectId/billables/:billableId/paymentRequests
    :<|> Capture "billableId" BillableId
      :> "paymentRequests"
      :> ReqBody '[JSON] PaymentRequestCreateRequest
      :> Post '[JSON] PaymentRequestResponse
