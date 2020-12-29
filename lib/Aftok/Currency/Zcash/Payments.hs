{-# LANGUAGE TemplateHaskell #-}

module Aftok.Currency.Zcash.Payments where

import Aftok.Currency.Zcash.Types (Zatoshi)
import Control.Lens (makeLenses, makePrisms)

newtype TxId = TxId Text

makePrisms ''TxId

data Payment
  = Payment
      { _amount :: Zatoshi,
        _txid :: TxId
      }

makeLenses ''Payment
