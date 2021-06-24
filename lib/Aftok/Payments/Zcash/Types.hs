{-# LANGUAGE TemplateHaskell #-}

module Aftok.Payments.Zcash.Types where

import Aftok.Currency.Zcash.Types (Address, Zatoshi)
import Aftok.Payments.Common (PaymentKey)
import qualified Aftok.Payments.Zcash.Zip321 as Zip321
import Aftok.Types (Email)
import Control.Lens (makeLenses, makePrisms)

newtype TxId = TxId Text

makePrisms ''TxId

data Channel
  = EmailChannel Email
  | MemoChannel Address
  | WebChannel

data PaymentRequest = PaymentRequest
  { _paymentRequestKey :: PaymentKey,
    _paymentRequestChannel :: Channel,
    _zip321Request :: Zip321.PaymentRequest
  }

makeLenses ''PaymentRequest

data Payment = Payment
  { _amount :: Zatoshi,
    _txid :: TxId
  }

makeLenses ''Payment
