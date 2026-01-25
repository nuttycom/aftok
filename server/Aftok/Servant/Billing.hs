{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.Billing
  ( -- * API Types
    BillingAPI,
    ProtectedBillingAPI,
    ProjectBillablesAPI,

    -- * Handlers
    protectedBillingServer,
    projectBillablesServer,

    -- * Request/Response Types
    BillableCreateRequest (..),
    SubscribeRequest (..),
    PaymentRequestCreateRequest (..),

    -- * JSON helpers
    billableJSON,
    paymentRequestDetailJSON,
  )
where

import Aftok.Billing
  ( Billable,
    Billable' (..),
    BillableId (..),
    Recurrence (..),
    SubscriptionId,
  )
import qualified Aftok.Billing as B
import Aftok.Currency (Amount (..), Currency (..))
import Aftok.Currency.Bitcoin (Satoshi (..))
import Aftok.Currency.Bitcoin.Bip70 (protoBase64)
import qualified Aftok.Currency.Bitcoin.Payments as Bitcoin
import Aftok.Currency.Zcash (Zatoshi (..))
import Aftok.Database
  ( DBOp (..),
    createBillable,
    liftdb,
    withProjectAuth,
  )
import Aftok.Database.PostgreSQL (QDBM, runQDBM)
import Aftok.Json (obj, satsJSON, v1, zatsJSON)
import Aftok.Payments
  ( PaymentRequest' (..),
    PaymentRequestId,
    PaymentsConfig,
    SomePaymentRequest (..),
    SomePaymentRequestDetail,
    createPaymentRequest,
    zcashBillingOps,
    zcashPaymentsConfig,
  )
import Aftok.Payments.Types
  ( NativeRequest (..),
    PaymentRequestError (..),
    billable,
    createdAt,
    nativeRequest,
    _PaymentRequestId,
  )
import qualified Aftok.Payments.Zcash as Zcash
import Aftok.Servant.App (AppM, envNetworkMode, envDbPool, runDB)
import Aftok.Servant.Auth (AuthenticatedUser (..))
import Aftok.Servant.Json (zip321PaymentRequestJSON)
import Aftok.Types (ProjectId, UserId)
import Control.Lens (to, (.~), (^.))
import Data.Aeson
  ( FromJSON (..),
    Object,
    Value (..),
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as O
import Data.Aeson.Types (Pair, Parser)
import Data.AffineSpace ((.+^))
import Data.Pool (withResource)
import qualified Data.Thyme.Clock as C
import Data.Thyme.Time.Core (toThyme)
import Servant
import Servant.Auth.Server (AuthResult (..))

--------------------------------------------------------------------------------
-- Data Types (must be defined before API types that reference them)
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
-- Handlers
--------------------------------------------------------------------------------

-- | Protected billing server (for subscriptions)
protectedBillingServer ::
  AuthResult AuthenticatedUser ->
  BillableId ->
  SubscribeRequest ->
  AppM SubscriptionId
protectedBillingServer (Authenticated user) bid _ = do
  let uid = auUserId user
  t <- liftIO C.getCurrentTime
  runDB . liftdb $ CreateSubscription uid bid (t ^. C._utctDay)
protectedBillingServer _ _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Project billables server
projectBillablesServer ::
  PaymentsConfig QDBM ->
  AuthResult AuthenticatedUser ->
  ProjectId ->
  ServerT ProjectBillablesAPI AppM
projectBillablesServer cfg authResult pid =
  billableListHandler authResult pid
    :<|> billableCreateHandler authResult pid
    :<|> createPaymentRequestHandler cfg authResult pid

-- | List billables for a project
billableListHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  AppM Value
billableListHandler (Authenticated user) pid = do
  let uid = auUserId user
  billables <- runDB $ withProjectAuth pid uid (FindBillables pid)
  pure $ A.toJSON $ fmap billableJSON billables
billableListHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Create a new billable
billableCreateHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  BillableCreateRequest ->
  AppM BillableId
billableCreateHandler (Authenticated user) pid req = do
  let uid = auUserId user
  amount <- case bcrCurrency req of
    "ZEC" -> pure $ Amount ZEC (Zatoshi $ fromIntegral $ bcrAmount req)
    "BTC" -> pure $ Amount BTC (Satoshi $ fromIntegral $ bcrAmount req)
    c -> throwError err400 {errBody = "Currency " <> encodeUtf8 c <> " not recognized."}
  let b = Billable
        { _project = pid,
          _creator = uid,
          _name = bcrName req,
          _description = Just $ bcrDescription req,
          _messageText = Just $ bcrMessage req,
          _recurrence = bcrRecurrence req,
          _amount = amount,
          _gracePeriod = bcrGracePeriod req,
          _requestExpiryPeriod = toThyme $ fromIntegral $ bcrRequestExpiryPeriod req,
          _paymentRequestEmailTemplate = bcrPaymentRequestEmailTemplate req,
          _paymentRequestMemoTemplate = bcrPaymentRequestMemoTemplate req
        }
  runDB $ createBillable uid b
billableCreateHandler _ _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Create a payment request for a billable
createPaymentRequestHandler ::
  PaymentsConfig QDBM ->
  AuthResult AuthenticatedUser ->
  ProjectId ->
  BillableId ->
  PaymentRequestCreateRequest ->
  AppM Value
createPaymentRequestHandler cfg (Authenticated user) pid bid _ = do
  let uid = auUserId user
  billableMay <- runDB $ withProjectAuth pid uid (FindBillable bid)
  now <- liftIO C.getCurrentTime
  let billDay = now ^. C._utctDay
  case billableMay of
    Just b | (b ^. B.project == pid) ->
      case b ^. B.amount of
        Amount ZEC v -> do
          let ops = Zcash.paymentOps (cfg ^. zcashBillingOps) (cfg ^. zcashPaymentsConfig)
          env <- ask
          let nmode = env ^. envNetworkMode
              pool = env ^. envDbPool
          -- ExceptT PaymentRequestError QDBM a
          -- Run runExceptT to get QDBM (Either PaymentRequestError a)
          -- Run runQDBM to get ExceptT DBError IO (Either PaymentRequestError a)
          -- Run runExceptT to get IO (Either DBError (Either PaymentRequestError a))
          res <- liftIO $ withResource pool $ \conn ->
            runExceptT $ runQDBM nmode conn $ runExceptT $
              createPaymentRequest ops now bid (b & B.amount .~ v) billDay
          case res of
            Left dbErr ->
              throwError err500 {errBody = "Database error: " <> show dbErr}
            Right (Left AmountInvalid) ->
              throwError err400 {errBody = "Invalid payment amount requested."}
            Right (Left NoRecipients) ->
              throwError err400 {errBody = "This project has no payable members."}
            Right (Right (reqId, detail)) ->
              pure $ v1 $ paymentRequestDetailJSON (reqId, SomePaymentRequest detail)
        Amount BTC _ ->
          throwError err400 {errBody = "Bitcoin payment requests not yet supported."}
    _ ->
      throwError err404 {errBody = "Billable not found."}
createPaymentRequestHandler _ _ _ _ _ =
  throwError err401 {errBody = "Authentication required"}

--------------------------------------------------------------------------------
-- JSON Serializers
--------------------------------------------------------------------------------

-- | Serialize a billable to JSON
billableJSON :: (BillableId, Billable Amount) -> Value
billableJSON (bid, b) =
  v1 $
    obj
      [ "billableId" .= (bid ^. B._BillableId),
        "name" .= (b ^. B.name),
        "description" .= (b ^. B.description),
        "message" .= (b ^. B.messageText),
        "recurrence" .= recurrenceJSON (b ^. B.recurrence),
        "amount" .= amountJSON (b ^. B.amount),
        "gracePeriod" .= (b ^. B.gracePeriod),
        "requestExpiryPeriod" .= (round (C.toSeconds' (b ^. B.requestExpiryPeriod)) :: Int)
      ]

-- | Serialize recurrence to JSON
recurrenceJSON :: Recurrence -> Value
recurrenceJSON = \case
  Annually -> A.object ["annually" .= A.Null]
  Monthly d -> A.object ["monthly" .= d]
  Weekly d -> A.object ["weekly" .= d]
  OneTime -> A.object ["onetime" .= A.Null]

-- | Serialize amount to JSON
amountJSON :: Amount -> Value
amountJSON (Amount ZEC (Zatoshi z)) = A.object ["currency" .= ("ZEC" :: Text), "zatoshi" .= z]
amountJSON (Amount BTC (Satoshi s)) = A.object ["currency" .= ("BTC" :: Text), "satoshi" .= s]

-- | Serialize payment request detail to JSON
paymentRequestDetailJSON :: (PaymentRequestId, SomePaymentRequestDetail) -> Object
paymentRequestDetailJSON (rid, (SomePaymentRequest req)) =
  obj $
    ["payment_request_id" .= (rid ^. _PaymentRequestId)] <> fields req
  where
    fields :: PaymentRequest' (Billable' ProjectId UserId) c -> [Pair]
    fields r = case r ^. nativeRequest of
      (Zip321Request req') ->
        [ "total" .= (r ^. billable . B.amount . to zatsJSON),
          "expires_at" .= ((r ^. createdAt) .+^ (r ^. billable . B.requestExpiryPeriod)),
          "native_request" .= zip321PaymentRequestJSON req'
        ]
      (Bip70Request req') ->
        [ "total" .= (r ^. billable . B.amount . to satsJSON),
          "expires_at" .= ((r ^. createdAt) .+^ (r ^. billable . B.requestExpiryPeriod)),
          "native_request" .= bip70PaymentRequestJSON req'
        ]

-- | Serialize BIP70 payment request to JSON
bip70PaymentRequestJSON :: Bitcoin.PaymentRequest -> Value
bip70PaymentRequestJSON r =
  v1 . obj $
    [ "bip70_request"
        .= A.object
          [ "payment_key" .= (r ^. Bitcoin.paymentRequestKey . Bitcoin._PaymentKey),
            "payment_request_protobuf_64" .= (r ^. Bitcoin.bip70Request . to protoBase64)
          ]
    ]

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
