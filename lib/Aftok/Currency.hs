{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Aftok.Currency where

import qualified Aftok.Currency.Bitcoin as B
import qualified Aftok.Currency.Zcash as Z
import Control.Lens (Iso')
import qualified Text.Show

-- | The currency in which a payment may be made.
data
  Currency
    a -- \^ The type of address used for payments in the currency.
    c -- \^ The type in which payment amounts are denominated.
  where
  BTC :: Currency B.Address B.Satoshi
  ZEC :: Currency Z.Address Z.Zatoshi

instance Eq (Currency a c) where
  BTC == BTC = True
  ZEC == ZEC = True

instance Show (Currency a c) where
  show = \case
    BTC -> "BTC"
    ZEC -> "ZEC"

data Currency' c = forall a. Currency' (Currency a c)

instance Eq (Currency' c) where
  (Currency' BTC) == (Currency' BTC) = True
  (Currency' ZEC) == (Currency' ZEC) = True

instance Show (Currency' c) where
  show (Currency' c) = show c

data Amount = forall a c.
  Amount
  { currency :: !(Currency a c),
    value :: !c
  }

class (Eq c, Ord c, Monoid c) => IsCurrency c where
  csub :: c -> c -> Maybe c
  cscale :: c -> Rational -> Maybe c
  _Units :: Iso' c Word64
  currency' :: Currency' c

instance IsCurrency B.Satoshi where
  csub = B.ssub
  cscale (B.Satoshi amt) factor =
    let r = toRational amt * factor
     in if (r >= 0) then Just (B.Satoshi . round $ r) else Nothing
  _Units = B._Satoshi
  currency' = Currency' BTC

instance IsCurrency Z.Zatoshi where
  csub = Z.zsub
  cscale (Z.Zatoshi amt) factor =
    let r = toRational amt * factor
     in if (r >= 0) then Just (Z.Zatoshi . round $ r) else Nothing
  _Units = Z._Zatoshi
  currency' = Currency' ZEC
