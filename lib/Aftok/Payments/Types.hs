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
  ( makeLenses,
    makePrisms,
    (^.),
  )
import Data.AffineSpace ((.+^))
import Data.Thyme.Clock as C
import Data.Thyme.Time as C
import Data.UUID

newtype PaymentRequestId = PaymentRequestId UUID deriving (Show, Eq)

makePrisms ''PaymentRequestId

newtype PaymentId = PaymentId UUID deriving (Show, Eq)

makePrisms ''PaymentId

data NativeRequest currency where
  Bip70Request :: B.PaymentRequest -> NativeRequest Satoshi
  Zip321Request :: Z.PaymentRequest -> NativeRequest Zatoshi

bip70Request :: NativeRequest currency -> Maybe B.PaymentRequest
bip70Request = \case
  Bip70Request r -> Just r
  _ -> Nothing

zip321Request :: NativeRequest currency -> Maybe Z.PaymentRequest
zip321Request = \case
  Zip321Request r -> Just r
  _ -> Nothing

data NativePayment currency where
  BitcoinPayment :: B.Payment -> NativePayment Satoshi
  ZcashPayment :: Z.Payment -> NativePayment Zatoshi

data PaymentOps currency m = PaymentOps
  { newPaymentRequest ::
      Billable currency -> -- billing information
      C.Day -> -- payout date (billing date)
      C.UTCTime -> -- timestamp of payment request creation
      m (NativeRequest currency)
  }

data PaymentRequest' (billable :: Type -> Type) currency = PaymentRequest
  { _billable :: billable currency,
    _createdAt :: C.UTCTime,
    _billingDate :: C.Day,
    _nativeRequest :: NativeRequest currency
  }

makeLenses ''PaymentRequest'

type PaymentRequest currency = PaymentRequest' (Const BillableId) currency

type PaymentRequestDetail currency = PaymentRequest' (Billable' ProjectId UserId) currency

data SomePaymentRequest (b :: Type -> Type) = forall c. SomePaymentRequest (PaymentRequest' b c)

type SomePaymentRequestDetail = SomePaymentRequest (Billable' ProjectId UserId)

paymentRequestCurrency :: PaymentRequest' b c -> Currency' c
paymentRequestCurrency pr = case _nativeRequest pr of
  Bip70Request _ -> Currency' BTC
  Zip321Request _ -> Currency' ZEC

isExpired :: forall c. UTCTime -> PaymentRequestDetail c -> Bool
isExpired now req =
  let expiresAt = (req ^. createdAt) .+^ (req ^. (billable . requestExpiryPeriod))
   in now >= expiresAt

data Payment' (paymentRequest :: Type -> Type) currency = Payment
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
