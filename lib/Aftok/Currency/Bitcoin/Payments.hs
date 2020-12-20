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
      { _amount :: Satoshi,
        _txid :: Text,
        _address :: Address,
        _bip70Payment :: B.Payment
      }

makeLenses ''Payment
