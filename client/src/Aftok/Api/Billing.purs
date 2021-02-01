module Aftok.Api.Billing where

import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT)
-- import Control.Monad.Except.Trans (ExceptT, runExceptT, except, withExceptT)
-- import Control.Monad.Error.Class (throwError)
-- import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.DateTime (DateTime)
-- import Data.DateTime.Instant (Instant, toDateTime)
import Data.Either (Either(..), note)
-- import Data.Foldable (class Foldable, foldr, foldl, foldMapDefaultR)
-- import Data.Functor.Compose (Compose(..))
-- import Data.Map as M
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
-- import Data.Ratio (Ratio, (%))
-- import Data.Time.Duration (Hours(..), Days(..))
import Data.Tuple (Tuple(..))
-- import Data.Traversable (class Traversable, traverse)
import Data.UUID (UUID, parseUUID)
-- import Effect (Effect)
import Effect.Aff (Aff)
-- import Effect.Class as EC
-- import Foreign.Object (Object)
-- import Affjax (get)
-- import Affjax.ResponseFormat as RF
import Aftok.Types
  ( ProjectId )
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

newtype Billable' t = Billable
  {
  }

type Billable = Billable' DateTime

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
createBillable pid bid = pure $ Left Forbidden

listProjectBillables :: ProjectId -> Aff (Either APIError (Array (Tuple BillableId Billable)))
listProjectBillables pid = pure $ Left Forbidden

listUnpaidPaymentRequests :: BillableId -> Aff (Either APIError (Array (Tuple PaymentRequestId PaymentRequest)))
listUnpaidPaymentRequests billId = pure $ Left Forbidden
