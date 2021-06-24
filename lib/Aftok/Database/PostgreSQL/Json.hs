{-# LANGUAGE TypeApplications #-}

module Aftok.Database.PostgreSQL.Json where

import Aftok.Currency.Bitcoin (NetworkMode, Satoshi (..), getNetwork, _Satoshi)
import Aftok.Currency.Zcash (Zatoshi (..), _Zatoshi)
import qualified Aftok.Currency.Zcash.Types as Zcash
import Aftok.Json (idValue, obj, parseBtcAddr, v1)
import qualified Aftok.Payments.Bitcoin.Bip70 as Bip70
import qualified Aftok.Payments.Bitcoin.Types as Bitcoin
import Aftok.Payments.Common (PaymentKey (..), _PaymentKey)
import Aftok.Payments.Types
  ( NativePayment (..),
    NativeRequest (..),
    Payment,
    nativePayment,
    paymentDate,
    paymentRequest,
    _PaymentRequestId,
  )
import qualified Aftok.Payments.Zcash.Types as Zcash
import qualified Aftok.Payments.Zcash.Zip321 as Zip321
import Aftok.Types (Email (..), _Email)
import Control.Lens (review, to, view, (^.), (^?), _Just)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (unpack)
import Haskoin.Address (addrToText)

--
-- Bitcoin payment requests
--

bitcoinChannelJSON :: Bitcoin.Channel -> Value
bitcoinChannelJSON = \case
  Bitcoin.EmailChannel email ->
    object ["email" .= (email ^. _Email)]
  Bitcoin.WebChannel ->
    String "web"

parseBitcoinChannelJSON :: Value -> Parser Bitcoin.Channel
parseBitcoinChannelJSON = \case
  Object o -> Bitcoin.EmailChannel . Email <$> o .: "email"
  String "web" -> pure Bitcoin.WebChannel
  other -> fail $ "Value " <> show other <> " is not a recognized Bitcoin payment channel encoding."

bitcoinPaymentRequestJSON :: Bitcoin.PaymentRequest -> Value
bitcoinPaymentRequestJSON r =
  v1 . obj $
    [ "bip70_request"
        .= object
          [ "payment_key" .= (r ^. Bitcoin.paymentRequestKey . _PaymentKey),
            "channel" .= (r ^. Bitcoin.paymentRequestChannel . to bitcoinChannelJSON),
            "payment_request_protobuf_64" .= (r ^. Bitcoin.bip70Request . to Bip70.protoBase64)
          ]
    ]

parseBitcoinPaymentRequestJSON :: Value -> Parser Bitcoin.PaymentRequest
parseBitcoinPaymentRequestJSON = \case
  Object wrapper -> do
    o <- wrapper .: "bip70_request"
    Bitcoin.PaymentRequest
      <$> (PaymentKey <$> o .: "paymentKey")
      <*> (parseBitcoinChannelJSON =<< o .: "channel")
      <*> ( either (fail . toString) pure . Bip70.fromBase64Proto =<< (o .: "payment_request_protobuf_64")
          )
  nonobject ->
    fail $ "Value " <> show nonobject <> " is not a JSON object."

--
-- Zcash payment requests
--

zcashChannelJSON :: Zcash.Channel -> Value
zcashChannelJSON = \case
  Zcash.EmailChannel email ->
    object ["email" .= (email ^. _Email)]
  Zcash.MemoChannel addr ->
    object ["memo" .= (addr ^. Zcash._Address)]
  Zcash.WebChannel ->
    String "web"

parseZcashChannelJSON :: Value -> Parser Zcash.Channel
parseZcashChannelJSON = \case
  Object o ->
    (Zcash.EmailChannel . Email <$> o .: "email")
      <|> (Zcash.MemoChannel . Zcash.Address <$> o .: "memo")
  String "web" -> pure Zcash.WebChannel
  other -> fail $ "Value " <> show other <> " is not a recognized Zcash payment channel encoding."

parseZip321PaymentRequestJSON :: Value -> Parser Zip321.PaymentRequest
parseZip321PaymentRequestJSON = \case
  Object o ->
    either fail pure . Zip321.parseURI =<< (o .: "zip321_request")
  nonobject ->
    fail $ "Value " <> show nonobject <> " is not a JSON object."

parseZcashPaymentRequestJSON :: Value -> Parser Zcash.PaymentRequest
parseZcashPaymentRequestJSON v = case v of
  Object o ->
    Zcash.PaymentRequest
      <$> (PaymentKey <$> o .: "paymentKey")
      <*> (parseZcashChannelJSON =<< o .: "channel")
      <*> parseZip321PaymentRequestJSON v
  nonobject ->
    fail $ "Value " <> show nonobject <> " is not a JSON object."

nativeRequestJSON :: NativeRequest c -> Value
nativeRequestJSON = \case
  Bip70Request r -> bitcoinPaymentRequestJSON r
  Zip321Request r ->
    object
      [ "payment_key" .= (r ^. Zcash.paymentRequestKey . _PaymentKey),
        "channel" .= (r ^. Zcash.paymentRequestChannel . to zcashChannelJSON),
        "zip321_request" .= (r ^. Zcash.zip321Request . to (toJSON . Zip321.toURI))
      ]

bitcoinPaymentJSON :: NetworkMode -> Bitcoin.Payment -> Value
bitcoinPaymentJSON nmode bp =
  object
    [ "amount" .= (bp ^? Bitcoin.amount . _Just . _Satoshi),
      "txid" .= (bp ^. Bitcoin.txid),
      "address" .= addrText,
      "payment_key" .= (bp ^. Bitcoin.paymentKey . _PaymentKey),
      "payment_protobuf_64" .= (bp ^. Bitcoin.bip70Payment . to Bip70.protoBase64)
    ]
  where
    addrText = addrToText (getNetwork nmode) <$> (bp ^. Bitcoin.address)

parseBitcoinPaymentJSON :: NetworkMode -> Value -> Parser Bitcoin.Payment
parseBitcoinPaymentJSON nmode = \case
  Object o ->
    Bitcoin.Payment
      <$> (fmap Satoshi <$> o .:? "amount")
      <*> (o .:? "txid")
      <*> (traverse (parseBtcAddr nmode) =<< o .:? "address")
      <*> (PaymentKey <$> o .: "paymentKey")
      <*> ( either (fail . unpack) pure . Bip70.fromBase64Proto =<< (o .: "payment_protobuf_64")
          )
  nonobject ->
    fail $ "Value " <> show nonobject <> " is not a JSON object."

zcashPaymentJSON :: Zcash.Payment -> Value
zcashPaymentJSON zp =
  v1 . obj $
    [ "amount" .= (zp ^. Zcash.amount . _Zatoshi),
      "txid" .= (zp ^. Zcash.txid . Zcash._TxId)
    ]

parseZcashPaymentJSON :: Value -> Parser Zcash.Payment
parseZcashPaymentJSON = \case
  (Object o) ->
    Zcash.Payment
      <$> (Zatoshi <$> o .: "amount")
      <*> (review Zcash._TxId <$> o .: "txid")
  val ->
    fail $ "Value " <> show val <> " is not a JSON object."

paymentJSON :: NetworkMode -> Payment c -> Value
paymentJSON nmode p =
  v1 . obj $
    [ "payment_request_id" .= idValue (paymentRequest . to getConst . _PaymentRequestId) p,
      "payment_date" .= view paymentDate p,
      "payment_value" .= nativePaymentValue
    ]
  where
    nativePaymentValue :: Value
    nativePaymentValue = case view nativePayment p of
      BitcoinPayment bp -> bitcoinPaymentJSON nmode bp
      ZcashPayment bp -> zcashPaymentJSON bp
