module Aftok.Api.Billing where

import Prelude
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT, except, withExceptT)
-- import Control.Monad.Except.Trans (ExceptT, runExceptT, except, withExceptT)
-- import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.BigInt (toNumber)
import Data.DateTime (DateTime)
-- import Data.DateTime.Instant (Instant, toDateTime)
import Data.Interval.Duration (Duration)
import Data.Either (Either(..), note)
-- import Data.Foldable (class Foldable, foldr, foldl, foldMapDefaultR)
-- import Data.Functor.Compose (Compose(..))
-- import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
-- import Data.Ratio (Ratio, (%))
-- import Data.Time.Duration (Hours(..), Days(..))
import Data.Tuple (Tuple)
-- import Data.Traversable (class Traversable, traverse)
import Data.UUID (UUID, parseUUID)
-- import Effect (Effect)
import Effect.Aff (Aff)
-- import Effect.Class as EC
-- import Foreign.Object (Object)
import Affjax (post, printError)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Aftok.Types
  ( ProjectId, pidStr )
import Aftok.Zcash
  ( Zatoshi )
import Aftok.Api.Types
  (APIError(..))
-- import Aftok.Api.Json
--   ( Decode
--   , decompose
--   , parseDatedResponse
--   , parseDatedResponseMay
--   )

newtype BillableId
  = BillableId UUID

derive instance billableIdEq :: Eq BillableId

derive instance billableIdOrd :: Ord BillableId

derive instance billableIdNewtype :: Newtype BillableId _

instance billableIdDecodeJson :: DecodeJson BillableId where
  decodeJson json = do
    uuidStr <- decodeJson json
    BillableId <$> (note "Failed to decode billable UUID" $ parseUUID uuidStr)

data Recurrence
  = Annually
  | Monthly Int
  | Weekly Int
  | OneTime

recurrenceJSON :: Recurrence -> Json
recurrenceJSON = case _ of
  Annually -> encodeJson $ { annually: {} }
  Monthly i -> encodeJson $ { monthly: i }
  Weekly i -> encodeJson $ { weekly: i }
  OneTime -> encodeJson $ { onetime: {} }

type Billable = 
  { name :: String
  , description :: String
  , message :: String
  , recurrence :: Recurrence
  , amount :: Zatoshi
  , gracePeriod :: Duration
  , expiryPeriod :: Duration
  }

billableJSON :: Billable -> Json
billableJSON b = encodeJson $
  { name: b.name
  , description: b.description
  , message: b.message
  , recurrence: recurrenceJSON b.recurrence
  , currency: "ZEC"
  , amount: toNumber (unwrap b.amount)
  }


newtype PaymentRequestId
  = PaymentRequestId UUID

derive instance paymentRequestIdEq :: Eq PaymentRequestId

derive instance paymentRequestIdOrd :: Ord PaymentRequestId

derive instance paymentRequestIdNewtype :: Newtype PaymentRequestId _

instance paymentRequestIdDecodeJson :: DecodeJson PaymentRequestId where
  decodeJson json = do
    uuidStr <- decodeJson json
    PaymentRequestId <$> (note "Failed to decode paymentRequest UUID" $ parseUUID uuidStr)

newtype PaymentRequest' t = PaymentRequest
  {
  }

type PaymentRequest = PaymentRequest' DateTime

createBillable :: ProjectId -> Billable -> Aff (Either APIError BillableId)
createBillable pid billable = do
  let body = RB.json $ billableJSON billable
  response <- post RF.json ("/api/projects/" <> pidStr pid <> "/billables") (Just body)
  runExceptT $ case response of 
    Left err -> throwError $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> throwError $ Forbidden
      StatusCode 200 -> withExceptT (ParseFailure r.body) <<< except $ decodeJson r.body
      other -> throwError $ Error { status: Just other, message: r.statusText }

listProjectBillables :: ProjectId -> Aff (Either APIError (Array (Tuple BillableId Billable)))
listProjectBillables pid = pure $ Left Forbidden

listUnpaidPaymentRequests :: BillableId -> Aff (Either APIError (Array (Tuple PaymentRequestId PaymentRequest)))
listUnpaidPaymentRequests billId = pure $ Left Forbidden
