{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Aftok.Currency where

import qualified Aftok.Currency.Zcash as Zcash
import qualified Bippy.Types as Bitcoin
import Control.Lens (view)
import qualified Haskoin.Address as Bitcoin

data Currency a c where
  BTC :: Currency Bitcoin.Address Bitcoin.Satoshi
  ZEC :: Currency Zcash.Address Zcash.Zatoshi

data Currency' c = forall a. Currency' (Currency a c)

data Amount
  = forall a c.
    Amount
      { currency :: !(Currency a c),
        value :: !c
      }

scaleCurrency :: Currency a c -> c -> Rational -> Maybe c
scaleCurrency c amount factor = case c of
  BTC -> (\(Bitcoin.Satoshi amt) -> Just $ Bitcoin.Satoshi ((round $ toRational amt * factor) :: Word64)) amount
  ZEC -> (\amt -> Zcash.toZatoshi ((round $ toRational (view Zcash._Zatoshi amt) * factor) :: Word64)) amount
