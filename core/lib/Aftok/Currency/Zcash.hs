{-# LANGUAGE TemplateHaskell #-}

module Aftok.Currency.Zcash
  ( Z.Address (..),
    Z._Address,
    Z.IVK (..),
    Z._IVK,
    ZcashConfig (..),
    Z.Zatoshi (..),
    Z._Zatoshi,
    Z.ToZatoshi (..),
    Z.zsub,
    Z.Memo (..),
  )
where

import Aftok.Currency.Zcash.Types as Z
import qualified Lrzhs.Types

data ZcashConfig = ZcashConfig
  {zcashNetwork :: Lrzhs.Types.Network}
