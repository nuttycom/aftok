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
    SubscribeRequest (..),
    PaymentRequestCreateRequest (..),
  )
where

import Aftok.Billing (BillableId (..), Recurrence (..), SubscriptionId)
import Data.Aeson
  ( FromJSON (..),
    Object,
    Value (..),
    (.:),
    (.:?),
  )
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as O
import Data.Aeson.Types (Parser)
import Servant.API

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

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
    :> Post '[JSON] SubscriptionId

-- | Project-specific billables API (nested under projects)
type ProjectBillablesAPI =
  -- GET /projects/:projectId/billables
  Get '[JSON] Value
    -- POST /projects/:projectId/billables
    :<|> ReqBody '[JSON] BillableCreateRequest :> Post '[JSON] BillableId
    -- POST /projects/:projectId/billables/:billableId/paymentRequests
    :<|> Capture "billableId" BillableId
      :> "paymentRequests"
      :> ReqBody '[JSON] PaymentRequestCreateRequest
      :> Post '[JSON] Value

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
