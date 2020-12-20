{-# LANGUAGE TemplateHaskell #-}

module Aftok.Currency.Zcash.Payments where

import Aftok.Currency.Zcash.Types (Zatoshi)
import Basement.Types.Word256 (Word256)
import Control.Lens (makeLenses)

data Payment
  = Payment
      { _amount :: Zatoshi,
        _txid :: Word256
      }

makeLenses ''Payment
