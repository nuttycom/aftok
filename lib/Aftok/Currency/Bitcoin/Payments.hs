{-# LANGUAGE TemplateHaskell #-}

module Aftok.Currency.Bitcoin.Payments
  ( PaymentKey (..),
    _PaymentKey,
    Payment (..),
    PaymentRequest (..),
    amount,
    txid,
    address,
    bip70Payment,
    paymentKey,
    bip70Request,
    paymentRequestKey,
  )
where

import qualified Bippy.Proto as B
import Bippy.Types (Satoshi)
import Control.Lens (makeLenses, makePrisms)
import Haskoin.Address (Address (..))

-- A unique identifier for a payment request, suitable
-- for URL embedding.
newtype PaymentKey = PaymentKey Text deriving (Eq)

makePrisms ''PaymentKey

data PaymentRequest = PaymentRequest
  { _paymentRequestKey :: PaymentKey,
    _bip70Request :: B.PaymentRequest
  }

makeLenses ''PaymentRequest

data Payment = Payment
  { _amount :: Maybe Satoshi,
    _txid :: Maybe Text,
    _address :: Maybe Address,
    _paymentKey :: PaymentKey,
    _bip70Payment :: B.Payment
  }

makeLenses ''Payment
