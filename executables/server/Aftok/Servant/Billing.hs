{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.Billing
  ( -- * API Types (re-exported from aftok-api)
    BillingAPI,
    ProtectedBillingAPI,
    ProjectBillablesAPI,
    BillableCreateRequest (..),
    BillableCreateResponse (..),
    BillableResponse (..),
    PaymentRequestResponse (..),
    SubscribeRequest (..),
    SubscribeResponse (..),
    PaymentRequestCreateRequest (..),

    -- * Handlers
    protectedBillingServer,
    projectBillablesServer,

    -- * Conversion helpers
    toPaymentRequestResponse,
  )
where

import Aftok.API.Billing
  ( BillableCreateRequest (..),
    BillableCreateResponse (..),
    BillableResponse (..),
    BillingAPI,
    PaymentRequestCreateRequest (..),
    PaymentRequestResponse (..),
    ProjectBillablesAPI,
    ProtectedBillingAPI,
    SubscribeRequest (..),
    SubscribeResponse (..),
  )
import Aftok.Billing
  ( Billable,
    Billable' (..),
    BillableId (..),
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
import Aftok.Json (satsJSON, zatsJSON)
import Aftok.Payments
  ( PaymentsConfig,
    SomePaymentRequest (..),
    SomePaymentRequestDetail,
    createPaymentRequest,
    zcashBillingOps,
    zcashPaymentsConfig,
  )
import Aftok.Payments.Types
  ( NativeRequest (..),
    PaymentRequestError (..),
    PaymentRequestId,
    billable,
    createdAt,
    nativeRequest,
  )
import qualified Aftok.Payments.Zcash as Zcash
import Aftok.Servant.App (AppM, envDbPool, envNetworkMode, runDB)
import Aftok.Servant.Auth (AuthenticatedUser (..))
import Aftok.Servant.Json (zip321PaymentRequestJSON)
import Aftok.Types (ProjectId)
import Control.Lens (to, (.~), (^.))
import Data.Aeson
  ( Value (..),
    (.=),
  )
import qualified Data.Aeson as A
import Data.AffineSpace ((.+^))
import Data.Pool (withResource)
import qualified Data.Thyme.Clock as C
import Data.Thyme.Time.Core (toThyme)
import Servant
import Servant.Auth.Server (AuthResult (..))

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

-- | Protected billing server (for subscriptions)
protectedBillingServer ::
  AuthResult AuthenticatedUser ->
  BillableId ->
  SubscribeRequest ->
  AppM SubscribeResponse
protectedBillingServer (Authenticated user) bid _ = do
  let uid = auUserId user
  t <- liftIO C.getCurrentTime
  sid <- runDB . liftdb $ CreateSubscription uid bid (t ^. C._utctDay)
  pure $ SubscribeResponse sid
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
  AppM [BillableResponse]
billableListHandler (Authenticated user) pid = do
  let uid = auUserId user
  billables <- runDB $ withProjectAuth pid uid (FindBillables pid)
  pure $ fmap toBillableResponse billables
billableListHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Create a new billable
billableCreateHandler ::
  AuthResult AuthenticatedUser ->
  ProjectId ->
  BillableCreateRequest ->
  AppM BillableCreateResponse
billableCreateHandler (Authenticated user) pid req = do
  let uid = auUserId user
  amount <- case bcrCurrency req of
    "ZEC" -> pure $ Amount ZEC (Zatoshi $ fromIntegral $ bcrAmount req)
    "BTC" -> pure $ Amount BTC (Satoshi $ fromIntegral $ bcrAmount req)
    c -> throwError err400 {errBody = "Currency " <> encodeUtf8 c <> " not recognized."}
  let b =
        Billable
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
  bid' <- runDB $ createBillable uid b
  pure $ BillableCreateResponse bid'
billableCreateHandler _ _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Create a payment request for a billable
createPaymentRequestHandler ::
  PaymentsConfig QDBM ->
  AuthResult AuthenticatedUser ->
  ProjectId ->
  BillableId ->
  PaymentRequestCreateRequest ->
  AppM PaymentRequestResponse
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
          res <- liftIO $
            withResource pool $ \conn ->
              runExceptT $
                runQDBM nmode conn $
                  runExceptT $
                    createPaymentRequest ops now bid (b & B.amount .~ v) billDay
          case res of
            Left dbErr ->
              throwError err500 {errBody = "Database error: " <> show dbErr}
            Right (Left AmountInvalid) ->
              throwError err400 {errBody = "Invalid payment amount requested."}
            Right (Left NoRecipients) ->
              throwError err409 {errBody = "{\"error\":\"noPayableMembers\"}"}
            Right (Right (reqId, detail)) ->
              pure $ toPaymentRequestResponse reqId (SomePaymentRequest detail)
        Amount BTC _ ->
          throwError err400 {errBody = "Bitcoin payment requests not yet supported."}
    _ ->
      throwError err404 {errBody = "Billable not found."}
createPaymentRequestHandler _ _ _ _ _ =
  throwError err401 {errBody = "Authentication required"}

--------------------------------------------------------------------------------
-- Response Constructors
--------------------------------------------------------------------------------

-- | Convert a billable to a BillableResponse
toBillableResponse :: (BillableId, Billable Amount) -> BillableResponse
toBillableResponse (bid, b) =
  BillableResponse
    { brBillableId = bid,
      brName = b ^. B.name,
      brDescription = b ^. B.description,
      brMessage = b ^. B.messageText,
      brRecurrence = b ^. B.recurrence,
      brAmount = amountJSON (b ^. B.amount),
      brGracePeriod = b ^. B.gracePeriod,
      brRequestExpiryPeriod = round (C.toSeconds' (b ^. B.requestExpiryPeriod)) :: Int
    }

-- | Serialize amount to JSON value
amountJSON :: Amount -> Value
amountJSON (Amount ZEC (Zatoshi z)) = A.object ["currency" .= ("ZEC" :: Text), "zatoshi" .= z]
amountJSON (Amount BTC (Satoshi s)) = A.object ["currency" .= ("BTC" :: Text), "satoshi" .= s]

-- | Convert payment request detail to PaymentRequestResponse
toPaymentRequestResponse :: PaymentRequestId -> SomePaymentRequestDetail -> PaymentRequestResponse
toPaymentRequestResponse rid (SomePaymentRequest req) =
  case req ^. nativeRequest of
    Zip321Request req' ->
      PaymentRequestResponse
        { prrPaymentRequestId = rid,
          prrTotal = req ^. billable . B.amount . to zatsJSON,
          prrExpiresAt = (req ^. createdAt) .+^ (req ^. billable . B.requestExpiryPeriod),
          prrNativeRequest = zip321PaymentRequestJSON req'
        }
    Bip70Request req' ->
      PaymentRequestResponse
        { prrPaymentRequestId = rid,
          prrTotal = req ^. billable . B.amount . to satsJSON,
          prrExpiresAt = (req ^. createdAt) .+^ (req ^. billable . B.requestExpiryPeriod),
          prrNativeRequest = bip70PaymentRequestJSON req'
        }

-- | Serialize BIP70 payment request to JSON
bip70PaymentRequestJSON :: Bitcoin.PaymentRequest -> Value
bip70PaymentRequestJSON r =
  A.object
    [ "bip70_request"
        .= A.object
          [ "payment_key" .= (r ^. Bitcoin.paymentRequestKey . Bitcoin._PaymentKey),
            "payment_request_protobuf_64" .= (r ^. Bitcoin.bip70Request . to protoBase64)
          ]
    ]
