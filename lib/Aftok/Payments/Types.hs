{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TemplateHaskell #-}

module Aftok.Payments.Types where

import Aftok.Billing
  ( Billable,
    Billable',
    BillableId,
    requestExpiryPeriod,
  )
import Aftok.Currency (Currency (..), Currency' (..))
import Aftok.Currency.Bitcoin (Satoshi)
import qualified Aftok.Currency.Bitcoin.Payments as B
import Aftok.Currency.Zcash (Zatoshi)
import qualified Aftok.Currency.Zcash.Payments as Z
import qualified Aftok.Currency.Zcash.Zip321 as Z
import Aftok.Types (ProjectId, UserId)
import Control.Lens
  ( (^.),
    makeLenses,
    makePrisms,
  )
import Data.AffineSpace ((.+^))
import Data.Thyme.Clock as C
import Data.Thyme.Time as C
import Data.UUID
import Haskoin.Address (decodeBase58Check)

newtype PaymentRequestId = PaymentRequestId UUID deriving (Show, Eq)

makePrisms ''PaymentRequestId

newtype PaymentId = PaymentId UUID deriving (Show, Eq)

makePrisms ''PaymentId

-- A unique identifier for the payment request, suitable
-- for URL embedding.
newtype PaymentKey = PaymentKey Text deriving (Eq)

makePrisms ''PaymentKey

parsePaymentKey :: ByteString -> Maybe PaymentKey
parsePaymentKey bs =
  (PaymentKey . decodeUtf8) <$> decodeBase58Check (decodeUtf8 bs)

data NativeRequest currency where
  Bip70Request :: B.PaymentRequest -> NativeRequest Satoshi
  Zip321Request :: Z.PaymentRequest -> NativeRequest Zatoshi

data PaymentOps currency m
  = PaymentOps
      { newPaymentRequest ::
          Billable currency -> -- billing information
          PaymentRequestId -> -- identifier for the payment request
          C.Day -> -- payout date (billing date)
          C.UTCTime -> -- timestamp of payment request creation
          m (NativeRequest currency)
      }

data NativePayment currency where
  BitcoinPayment :: B.Payment -> NativePayment Satoshi
  ZcashPayment :: Z.Payment -> NativePayment Zatoshi

data PaymentRequest' (billable :: * -> *) currency
  = PaymentRequest
      { _billable :: billable currency,
        _createdAt :: C.UTCTime,
        _billingDate :: C.Day,
        _nativeRequest :: NativeRequest currency
      }

makeLenses ''PaymentRequest'

type PaymentRequest currency = PaymentRequest' (Const BillableId) currency

data SomePaymentRequest (b :: * -> *) = forall c. SomePaymentRequest (PaymentRequest' b c)

type PaymentRequestDetail currency = PaymentRequest' (Billable' ProjectId UserId) currency

type SomePaymentRequestDetail = SomePaymentRequest (Billable' ProjectId UserId)

paymentRequestCurrency :: PaymentRequest' b c -> Currency' c
paymentRequestCurrency pr = case _nativeRequest pr of
  Bip70Request _ -> Currency' BTC
  Zip321Request _ -> Currency' ZEC

data Payment' (paymentRequest :: * -> *) currency
  = Payment
      { _paymentRequest :: paymentRequest currency,
        _paymentDate :: C.UTCTime,
        _nativePayment :: NativePayment currency
      }

makeLenses ''Payment'

data PaymentRequestError
  = AmountInvalid
  | NoRecipients

type Payment currency = Payment' (Const PaymentRequestId) currency

type PaymentDetail currency = Payment' (PaymentRequest' (Billable' ProjectId UserId)) currency

isExpired :: forall c. UTCTime -> PaymentRequestDetail c -> Bool
isExpired now req =
  let expiresAt = (req ^. createdAt) .+^ (req ^. (billable . requestExpiryPeriod))
   in now >= expiresAt
