module Aftok.Api.Billing where

import Prelude
import Control.Alternative ((<|>))
-- import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT)
-- import Control.Monad.Except.Trans (ExceptT, runExceptT, except, withExceptT)
-- import Control.Monad.Error.Class (throwError)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError(..), (.:), (.:?))
import Data.Argonaut.Encode (encodeJson)
import Data.BigInt (toNumber) as BigInt
import Data.DateTime (DateTime)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..), note)
import Data.Foldable (class Foldable, foldMapDefaultR)
-- import Data.Functor.Compose (Compose(..))
-- import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
-- import Data.Ratio (Ratio, (%))
import Data.Time.Duration (Hours(..), Days(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (class Traversable, traverse, sequence)
import Data.UUID (UUID, parseUUID, toString)
-- import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
-- import Effect.Class as EC
import Foreign.Object (Object)
import Affjax (post, get)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.Web (driver)
-- import Affjax.StatusCode (StatusCode(..))
import Aftok.Types
  ( ProjectId
  , pidStr
  )
import Aftok.Zcash
  ( Zatoshi
  )
import Aftok.Api.Types
  ( APIError(..)
  )
import Aftok.Api.Json
  ( parseResponse
  , parseDatedResponse
  , parseZatoshi
  )

newtype BillableId = BillableId UUID

derive instance billableIdEq :: Eq BillableId

derive instance billableIdOrd :: Ord BillableId

derive instance billableIdNewtype :: Newtype BillableId _

billableIdStr :: BillableId -> String
billableIdStr (BillableId uuid) = toString uuid

parseBillableIdJSON :: String -> Either JsonDecodeError BillableId
parseBillableIdJSON uuidStr =
  BillableId <$> note (TypeMismatch "Failed to decode billable UUID") (parseUUID uuidStr)

instance billableIdDecodeJson :: DecodeJson BillableId where
  decodeJson json = do
    obj <- decodeJson json
    parseBillableIdJSON =<< obj .: "billableId"

data Recurrence
  = Annually
  | Monthly Int
  | Weekly Int
  | OneTime

instance showRecurrence :: Show Recurrence where
  show = case _ of
    Annually -> "Annually"
    Monthly i -> "Monthly " <> show i
    Weekly i -> "Weekly " <> show i
    OneTime -> "OneTime"

recurrenceStr :: Recurrence -> String
recurrenceStr = case _ of
  Annually -> "Annually"
  Monthly i -> "Every " <> show i <> " months"
  Weekly i -> "Every " <> show i <> " weeks"
  OneTime -> "One-time purchase"

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
  , gracePeriod :: Days
  , expiryPeriod :: Hours
  }

billableJSON :: Billable -> Json
billableJSON b = encodeJson $
  { schemaVersion: "1.0"
  , name: b.name
  , description: b.description
  , message: b.message
  , recurrence: recurrenceJSON b.recurrence
  , currency: "ZEC"
  , amount: BigInt.toNumber (unwrap b.amount)
  -- API requires grace period as days
  , gracePeriod: unwrap b.gracePeriod
  -- API requires expiry period as seconds
  , requestExpiryPeriod: unwrap b.expiryPeriod * 60.0 * 60.0
  }

parseRecurrence :: Json -> Either JsonDecodeError Recurrence
parseRecurrence json = do
  obj <- decodeJson json
  let
    parseInner f outer inner = map f ((MaybeT <<< (_ .:? inner)) =<< MaybeT (obj .:? outer))
    annually = traverse (map \(_ :: Json) -> Annually) (obj .:? "annually")
    monthly = sequence $ runMaybeT (parseInner Monthly "monthly" "months")
    weekly = sequence $ runMaybeT (parseInner Weekly "weekly" "weeks")
    onetime = traverse (map \(_ :: Json) -> OneTime) (obj .:? "onetime")
  join $ note (UnexpectedValue json) (annually <|> monthly <|> weekly <|> onetime)

parseBillableJSON :: Object Json -> Either JsonDecodeError (Tuple BillableId Billable)
parseBillableJSON obj = do
  billableId <- parseBillableIdJSON =<< obj .: "billableId"
  bobj <- obj .: "billable"
  name :: String <- bobj .: "name"
  description :: String <- bobj .: "description"
  let message = ""
  recurrence <- parseRecurrence =<< bobj .: "recurrence"
  amount <- parseZatoshi =<< (bobj .: "amount")
  gracePeriod <- Days <$> bobj .: "gracePeriod"
  expiryPeriod <- Hours <$> bobj .: "gracePeriod"
  pure $ Tuple billableId { name, description, message, recurrence, amount, gracePeriod, expiryPeriod }

createBillable :: ProjectId -> Billable -> Aff (Either APIError BillableId)
createBillable pid billable = do
  let body = RB.json $ billableJSON billable
  response <- post driver RF.json ("/api/projects/" <> pidStr pid <> "/billables") (Just body)
  parseResponse decodeJson response

listProjectBillables :: ProjectId -> Aff (Either APIError (Array (Tuple BillableId Billable)))
listProjectBillables pid = do
  response <- get driver RF.json ("/api/projects/" <> pidStr pid <> "/billables")
  parseResponse (traverse parseBillableJSON <=< decodeJson) response

newtype PaymentRequestId = PaymentRequestId UUID

derive instance paymentRequestIdEq :: Eq PaymentRequestId

derive instance paymentRequestIdOrd :: Ord PaymentRequestId

derive instance paymentRequestIdNewtype :: Newtype PaymentRequestId _

instance paymentRequestIdDecodeJson :: DecodeJson PaymentRequestId where
  decodeJson json = do
    uuidStr <- decodeJson json
    PaymentRequestId <$> note (TypeMismatch "Failed to decode paymentRequest UUID") (parseUUID uuidStr)

newtype PaymentRequest' t = PaymentRequest
  { payment_request_id :: String
  , native_request ::
      { zip321_request :: String
      , schemaVersion :: String
      }
  , expires_at :: t
  , total :: Zatoshi
  }

derive instance paymentRequestFunctor :: Functor PaymentRequest'

instance paymentRequestFoldable :: Foldable PaymentRequest' where
  foldr f b (PaymentRequest r) =
    f r.expires_at b
  foldl f b (PaymentRequest r) =
    f b r.expires_at
  foldMap = foldMapDefaultR

instance paymentRequestTraversable :: Traversable PaymentRequest' where
  traverse f (PaymentRequest r) =
    map (\b -> PaymentRequest (r { expires_at = b })) (f r.expires_at)
  sequence = traverse identity

type PaymentRequest = PaymentRequest' DateTime

type PaymentRequestMeta =
  { requestName :: String
  , requestDesc :: Maybe String
  }

decodePaymentRequest :: Json -> Either JsonDecodeError (PaymentRequest' String)
decodePaymentRequest json = do
  obj <- decodeJson json
  payment_request_id <- obj .: "payment_request_id"
  native_request <- obj .: "native_request"
  expires_at <- obj .: "expires_at"
  total <- parseZatoshi =<< (obj .: "total")
  pure $ PaymentRequest { payment_request_id, native_request, expires_at, total }

createPaymentRequest
  :: ProjectId
  -> BillableId
  -> PaymentRequestMeta
  -> Aff (Either APIError PaymentRequest)
createPaymentRequest pid bid m = do
  let
    body = RB.json (encodeJson m)
    uri = "/api/projects/" <> pidStr pid <> "/billables/" <> billableIdStr bid <> "/paymentRequests"
  response <- post driver RF.json uri (Just body)
  liftEffect
    <<< runExceptT
    <<< map (map toDateTime)
    $ parseDatedResponse decodePaymentRequest response

listUnpaidPaymentRequests
  :: BillableId
  -> Aff (Either APIError (Array (Tuple PaymentRequestId PaymentRequest)))
listUnpaidPaymentRequests _ = pure $ Left Forbidden

