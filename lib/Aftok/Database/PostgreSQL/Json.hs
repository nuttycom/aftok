{-# LANGUAGE TypeApplications #-}

module Aftok.Database.PostgreSQL.Json where

import Aftok.Currency.Bitcoin (NetworkMode, Satoshi (..), _Satoshi, getNetwork)
import qualified Aftok.Currency.Bitcoin.Payments as Bitcoin
import Aftok.Currency.Zcash (Zatoshi (..))
import qualified Aftok.Currency.Zcash.Payments as Zcash
import Aftok.Json (idValue, obj, parseBtcAddr, v1)
import Aftok.Payments.Types
  ( NativePayment (..),
    NativeRequest (..),
    Payment,
    PaymentRequest,
    PaymentRequestId,
    _PaymentRequestId,
    billingDate,
    createdAt,
    nativePayment,
    nativeRequest,
    paymentDate,
    paymentRequest,
  )
import qualified Bippy.Proto as BP
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Base64 as B64
import Data.ProtocolBuffers (decodeMessage, encodeMessage)
import Data.Serialize.Get (runGet)
import Data.Serialize.Put (runPut)
import Data.Text (unpack)
import Data.Thyme.Calendar (showGregorian)
import Haskoin.Address (addrToText)

bip70PaymentRequestJSON :: PaymentRequestId -> PaymentRequest Satoshi -> Value
bip70PaymentRequestJSON _ = (v1 . obj) . bip70PaymentRequestKV

bip70PaymentRequestKV :: (KeyValue kv) => PaymentRequest Satoshi -> [kv]
bip70PaymentRequestKV r = case (r ^. nativeRequest) of
  Bip70Request req ->
    [ "payment_request_protobuf_64" .= prBytes req,
      "payment_request_time" .= view createdAt r,
      "billing_date" .= view (billingDate . to showGregorian) r
    ]
  where
    prBytes = B64.encodeBase64 . runPut . encodeMessage

paymentJSON :: NetworkMode -> Payment c -> Value
paymentJSON nmode p =
  v1 $
    obj
      [ "payment_request_id" .= idValue (paymentRequest . to getConst . _PaymentRequestId) p,
        "payment_date" .= view paymentDate p,
        "payment_value" .= nativePaymentValue
      ]
  where
    nativePaymentValue :: Value
    nativePaymentValue = case view nativePayment p of
      BitcoinPayment bp -> bitcoinPaymentJSON nmode bp
      ZcashPayment bp -> zcashPaymentJSON bp

bitcoinPaymentJSON :: NetworkMode -> Bitcoin.Payment -> Value
bitcoinPaymentJSON nmode bp =
  object
    [ "amount" .= (bp ^? Bitcoin.amount . _Just . _Satoshi),
      "txid" .= (bp ^. Bitcoin.txid),
      "address" .= addrText,
      "payment_protobuf_64" .= (B64.encodeBase64 . runPut . encodeMessage $ bp ^. Bitcoin.bip70Payment)
    ]
  where
    addrText = addrToText (getNetwork nmode) <$> (bp ^. Bitcoin.address)

zcashPaymentJSON :: Zcash.Payment -> Value
zcashPaymentJSON _ = object []

decodeBip70Payment :: ByteString -> Parser BP.Payment
decodeBip70Payment =
  either fail pure . runGet decodeMessage

parseBitcoinPaymentJSON :: NetworkMode -> Value -> Parser Bitcoin.Payment
parseBitcoinPaymentJSON nmode = \case
  Object o ->
    Bitcoin.Payment
      <$> (fmap Satoshi <$> o .:? "amount")
      <*> (o .:? "txid")
      <*> (traverse (parseBtcAddr nmode) =<< o .:? "address")
      <*> (either (fail . unpack) decodeBip70Payment =<< B64.decodeBase64 . encodeUtf8 @Text @ByteString <$> (o .: "payment_protobuf_64"))
  val ->
    fail $ "Value " <> show val <> " is not a JSON object."

parseZcashPaymentJSON :: Value -> Parser Zcash.Payment
parseZcashPaymentJSON = \case
  (Object o) ->
    Zcash.Payment <$> (Zatoshi <$> o .: "amount")
      <*> (fromInteger <$> o .: "txid")
  val ->
    fail $ "Value " <> show val <> " is not a JSON object."
