{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

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

import Aftok.API.Types ()
import Aftok.Billing (BillableId (..), Recurrence (..), SubscriptionId)
import Aftok.Payments.Types (PaymentRequestId (..))
import Data.Aeson
  ( FromJSON (..),
    Object,
    ToJSON (..),
    Value (..),
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as O
import Data.Aeson.Types (Parser)
import qualified Data.Thyme.Clock as C
import Data.Thyme.Format.Aeson ()
import qualified Data.UUID as UUID
import Servant.API

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- | Billable creation response
data BillableCreateResponse = BillableCreateResponse
  { billableId :: BillableId
  }
  deriving (Generic)

instance ToJSON BillableCreateResponse

-- | Subscription creation response
data SubscribeResponse = SubscribeResponse
  { subscriptionId :: SubscriptionId
  }
  deriving (Generic)

instance ToJSON SubscribeResponse

-- | Billable creation request
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

instance FromJSON BillableCreateRequest where
  parseJSON = A.withObject "BillableCreateRequest" $ \outer -> do
    v <- outer .: "schemaVersion"
    when ((v :: Text) /= "1.0") $ fail "Unsupported schema version"
    o <- outer .: "Billable"
    BillableCreateRequest
      <$> o .: "name"
      <*> o .: "description"
      <*> o .: "message"
      <*> (parseRecurrence' =<< o .: "recurrence")
      <*> o .: "currency"
      <*> o .: "amount"
      <*> o .: "gracePeriod"
      <*> o .: "requestExpiryPeriod"
      <*> o .:? "paymentRequestEmailTemplate"
      <*> o .:? "paymentRequestMemoTemplate"

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

instance ToJSON BillableResponse where
  toJSON r =
    A.object
      [ "billableId" .= (let BillableId u = brBillableId r in UUID.toText u),
        "name" .= brName r,
        "description" .= brDescription r,
        "message" .= brMessage r,
        "recurrence" .= recurrenceToJSON (brRecurrence r),
        "amount" .= brAmount r,
        "gracePeriod" .= brGracePeriod r,
        "requestExpiryPeriod" .= brRequestExpiryPeriod r
      ]

-- | Payment request response
data PaymentRequestResponse = PaymentRequestResponse
  { prrPaymentRequestId :: PaymentRequestId,
    prrTotal :: Value,
    prrExpiresAt :: C.UTCTime,
    prrNativeRequest :: Value
  }

instance ToJSON PaymentRequestResponse where
  toJSON r =
    A.object
      [ "payment_request_id" .= (let PaymentRequestId u = prrPaymentRequestId r in UUID.toText u),
        "total" .= prrTotal r,
        "expires_at" .= prrExpiresAt r,
        "native_request" .= prrNativeRequest r
      ]

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

--------------------------------------------------------------------------------
-- Serialization Helpers
--------------------------------------------------------------------------------

-- | Serialize recurrence to JSON (matches handler's existing shape)
recurrenceToJSON :: Recurrence -> Value
recurrenceToJSON = \case
  Annually -> A.object ["annually" .= A.Null]
  Monthly d -> A.object ["monthly" .= d]
  Weekly d -> A.object ["weekly" .= d]
  OneTime -> A.object ["onetime" .= A.Null]

--------------------------------------------------------------------------------
-- Parsing Helpers
--------------------------------------------------------------------------------

-- | Parse a recurrence value from JSON
parseRecurrence :: Object -> Parser Recurrence
parseRecurrence o =
  let parseAnnually o' = const (pure Annually) <$> O.lookup "annually" o'
      parseMonthly o' = fmap Monthly . A.parseJSON <$> O.lookup "monthly" o'
      parseWeekly o' = fmap Weekly . A.parseJSON <$> O.lookup "weekly" o'
      parseOneTime o' = const (pure OneTime) <$> O.lookup "onetime" o'
      notFound =
        fail $ "Value " <> show o <> " does not represent a Recurrence value."
      parseV val =
        parseAnnually val
          <|> parseMonthly val
          <|> parseWeekly val
          <|> parseOneTime val
   in fromMaybe notFound $ parseV o

-- | Parse a recurrence from a JSON value
parseRecurrence' :: Value -> Parser Recurrence
parseRecurrence' = \case
  (Object o) -> parseRecurrence o
  val -> fail $ "Value " <> show val <> " is not a JSON object."
