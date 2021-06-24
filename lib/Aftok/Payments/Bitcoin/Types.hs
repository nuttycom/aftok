{-# LANGUAGE TemplateHaskell #-}

module Aftok.Payments.Bitcoin.Types
  ( Payment (..),
    PaymentRequest (..),
    Channel (..),
    amount,
    txid,
    address,
    bip70Payment,
    paymentKey,
    bip70Request,
    paymentRequestKey,
    paymentRequestChannel,
  )
where

import Aftok.Payments.Common (PaymentKey)
import Aftok.Types (Email)
import qualified Bippy.Proto as B
import Bippy.Types (Satoshi)
import Control.Lens (makeLenses)
import Haskoin.Address (Address (..))

data Channel
  = EmailChannel Email
  | WebChannel

data PaymentRequest = PaymentRequest
  { _paymentRequestKey :: PaymentKey,
    _paymentRequestChannel :: Channel,
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
