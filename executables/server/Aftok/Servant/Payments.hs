{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.Payments
  ( -- * API Types
    PaymentsAPI,
    ProtectedPaymentsAPI,

    -- * Handlers
    protectedPaymentsServer,
  )
where

import qualified Aftok.Config as AC
import qualified Aftok.Currency.Bitcoin.Payments as Bitcoin
import Aftok.Billing (SubscriptionId (..))
import Aftok.Database
  ( DBOp (..),
    findPaymentRequestByKey,
    liftdb,
  )
import qualified Aftok.Payments as Payments
import Aftok.Database.PostgreSQL (QDBM)
import Aftok.Payments
  ( PaymentsConfig,
    SomePaymentRequest (..),
  )
import Aftok.Payments.Types
  ( NativePayment (..),
    NativeRequest (..),
    Payment' (..),
    PaymentId,
    nativeRequest,
  )
import Aftok.Servant.App (AppM, runDB)
import Aftok.Servant.Auth (AuthenticatedUser (..))
import Aftok.Servant.Billing (paymentRequestDetailJSON)
import Aftok.Util (fromMaybeT)
import Control.Lens ((^.))
import Control.Monad.Trans.Maybe (mapMaybeT)
import Data.Aeson (Value, toJSON)
import Data.ProtocolBuffers (decodeMessage, encodeMessage)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import qualified Data.Thyme.Clock as C
import Servant
import Servant.Auth.Server (AuthResult (..))

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

--------------------------------------------------------------------------------
-- Handlers
--------------------------------------------------------------------------------

-- | Protected payments server
protectedPaymentsServer ::
  AC.BitcoinConfig ->
  PaymentsConfig QDBM ->
  AuthResult AuthenticatedUser ->
  ServerT ProtectedPaymentsAPI AppM
protectedPaymentsServer btcCfg payCfg authResult =
  listPayableRequestsHandler authResult
    :<|> bip70Server btcCfg payCfg authResult

-- | BIP70 payment server
bip70Server ::
  AC.BitcoinConfig ->
  PaymentsConfig QDBM ->
  AuthResult AuthenticatedUser ->
  Text ->
  ServerT
    ( Get '[OctetStream] ByteString
        :<|> ReqBody '[OctetStream] ByteString :> Post '[JSON] PaymentId
    )
    AppM
bip70Server btcCfg payCfg authResult paymentKey =
  getBip70PaymentRequestHandler authResult paymentKey
    :<|> bip70PaymentResponseHandler btcCfg payCfg authResult paymentKey

-- | List payable (unpaid) payment requests for a subscription
listPayableRequestsHandler ::
  AuthResult AuthenticatedUser ->
  SubscriptionId ->
  AppM Value
listPayableRequestsHandler (Authenticated user) sid = do
  let uid = auUserId user
  requests <- runDB $ Payments.findPayableRequests uid sid
  pure $ toJSON $ fmap paymentRequestDetailJSON requests
listPayableRequestsHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Get a BIP-70 payment request as protobuf
getBip70PaymentRequestHandler ::
  AuthResult AuthenticatedUser ->
  Text ->
  AppM ByteString
getBip70PaymentRequestHandler (Authenticated _) paymentKeyText = do
  let pkey = Bitcoin.PaymentKey paymentKeyText
  (_, SomePaymentRequest preq) <-
    fromMaybeT
      (throwError err404 {errBody = "Payment request not found for key " <> encodeUtf8 paymentKeyText})
      (mapMaybeT runDB $ findPaymentRequestByKey pkey)
  case preq ^. nativeRequest of
    Bip70Request bp -> pure $ runPut $ encodeMessage (bp ^. Bitcoin.bip70Request)
    _ -> throwError err400 {errBody = "Not a BIP-70 bitcoin payment request."}
getBip70PaymentRequestHandler _ _ =
  throwError err401 {errBody = "Authentication required"}

-- | Handle BIP-70 payment response
bip70PaymentResponseHandler ::
  AC.BitcoinConfig ->
  PaymentsConfig QDBM ->
  AuthResult AuthenticatedUser ->
  Text ->
  ByteString ->
  AppM PaymentId
bip70PaymentResponseHandler _ _ (Authenticated _) paymentKeyText requestBody = do
  let pkey = Bitcoin.PaymentKey paymentKeyText
  (prid, SomePaymentRequest preq) <-
    fromMaybeT
      (throwError err404 {errBody = "Payment request not found for key " <> encodeUtf8 paymentKeyText})
      (mapMaybeT runDB $ findPaymentRequestByKey pkey)
  case preq ^. nativeRequest of
    Bip70Request bp -> do
      pmnt <-
        either
          (\msg -> throwError err400 {errBody = "Could not decode payment response: " <> show msg})
          (pure . Bitcoin.Payment Nothing Nothing Nothing (bp ^. Bitcoin.paymentRequestKey))
          (runGet decodeMessage requestBody)
      now <- liftIO C.getCurrentTime
      let newPayment = Payment (Const prid) now (BitcoinPayment pmnt)
      runDB . liftdb $ CreatePayment newPayment
    _ -> throwError err400 {errBody = "Not a BIP-70 bitcoin payment request."}
bip70PaymentResponseHandler _ _ _ _ _ =
  throwError err401 {errBody = "Authentication required"}
