{-# LANGUAGE TemplateHaskell #-}

module Aftok.Currency.Bitcoin.Payments
  ( Payment (..),
    amount,
    txid,
    address,
    bip70Payment,
    B.PaymentRequest (..),
  )
where

import qualified Bippy.Proto as B
import Bippy.Types (Satoshi)
import Control.Lens (makeLenses)
import Haskoin.Address (Address (..))

data Payment
  = Payment
      { _amount :: Maybe Satoshi,
        _txid :: Maybe Text,
        _address :: Maybe Address,
        _bip70Payment :: B.Payment
      }

makeLenses ''Payment
