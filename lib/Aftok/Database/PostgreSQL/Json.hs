{-# LANGUAGE TypeApplications #-}

module Aftok.Database.PostgreSQL.Json where

import Aftok.Currency.Bitcoin (NetworkMode, Satoshi (..), _Satoshi, getNetwork)
import qualified Aftok.Currency.Bitcoin.Bip70 as Bip70
import qualified Aftok.Currency.Bitcoin.Payments as Bitcoin
import Aftok.Currency.Zcash (Zatoshi (..), _Zatoshi)
import qualified Aftok.Currency.Zcash.Payments as Zcash
import qualified Aftok.Currency.Zcash.Zip321 as Zip321
import Aftok.Json (idValue, obj, parseBtcAddr, v1)
import Aftok.Payments.Types
  ( NativePayment (..),
    NativeRequest (..),
    Payment,
    _PaymentRequestId,
    nativePayment,
    paymentDate,
    paymentRequest,
  )
-- import qualified Bippy.Proto as BP
import Control.Lens ((^.), (^?), _Just, review, to, view)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (unpack)
-- import Data.Thyme.Calendar (showGregorian)
import Haskoin.Address (addrToText)

bip70PaymentRequestJSON :: Bitcoin.PaymentRequest -> Value
bip70PaymentRequestJSON r =
  v1 . obj $
    [ "bip70_request"
        .= object
          [ "payment_key" .= (r ^. Bitcoin.paymentRequestKey . Bitcoin._PaymentKey),
            "payment_request_protobuf_64" .= (r ^. Bitcoin.bip70Request . to Bip70.protoBase64)
          ]
    ]

parseBip70PaymentRequestJSON :: Value -> Parser Bitcoin.PaymentRequest
parseBip70PaymentRequestJSON = \case
  Object wrapper -> do
    o <- wrapper .: "bip70_request"
    Bitcoin.PaymentRequest
      <$> (Bitcoin.PaymentKey <$> o .: "paymentKey")
      <*> ( either (fail . toString) pure . Bip70.fromBase64Proto =<< (o .: "payment_request_protobuf_64")
          )
  nonobject ->
    fail $ "Value " <> show nonobject <> " is not a JSON object."

zip321PaymentRequestJSON :: Zip321.PaymentRequest -> Value
zip321PaymentRequestJSON r =
  v1 . obj $
    ["zip321_request" .= (toJSON . Zip321.toURI $ r)]

parseZip321PaymentRequestJSON :: Value -> Parser Zip321.PaymentRequest
parseZip321PaymentRequestJSON = \case
  Object o ->
    either fail pure . Zip321.parseURI =<< (o .: "zip321_request")
  nonobject ->
    fail $ "Value " <> show nonobject <> " is not a JSON object."

nativeRequestJSON :: NativeRequest c -> Value
nativeRequestJSON = \case
  Bip70Request r -> bip70PaymentRequestJSON r
  Zip321Request r -> zip321PaymentRequestJSON r

bitcoinPaymentJSON :: NetworkMode -> Bitcoin.Payment -> Value
bitcoinPaymentJSON nmode bp =
  object
    [ "amount" .= (bp ^? Bitcoin.amount . _Just . _Satoshi),
      "txid" .= (bp ^. Bitcoin.txid),
      "address" .= addrText,
      "payment_key" .= (bp ^. Bitcoin.paymentKey . Bitcoin._PaymentKey),
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
      <*> (Bitcoin.PaymentKey <$> o .: "paymentKey")
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
