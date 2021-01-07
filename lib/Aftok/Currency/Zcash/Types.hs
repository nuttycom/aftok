{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Aftok.Currency.Zcash.Types where

import Control.Lens (makePrisms)

coin :: Word64
coin = 100000000

maxMoney :: Word64
maxMoney = 21000000 * coin

newtype IVK = IVK {ivkText :: Text}
  deriving (Eq, Ord, Show)

makePrisms ''IVK

newtype Address = Address {zaddrText :: Text}
  deriving (Eq, Ord, Show)

makePrisms ''Address

newtype Zatoshi = Zatoshi Word64
  deriving stock (Eq, Ord, Show)

makePrisms ''Zatoshi

class ToZatoshi a where
  toZatoshi :: a -> Maybe Zatoshi

instance ToZatoshi Word64 where
  toZatoshi amt =
    if amt > maxMoney then Nothing else Just (Zatoshi amt)

instance Semigroup Zatoshi where
  (Zatoshi a) <> (Zatoshi b) = Zatoshi (a + b)

instance Monoid Zatoshi where
  mempty = Zatoshi 0

zsub :: Zatoshi -> Zatoshi -> Maybe Zatoshi
zsub (Zatoshi a) (Zatoshi b) | a > b = Just . Zatoshi $ (a - b)
zsub _ _ = Nothing

data ZAddrType
  = Sprout
  | Sapling

decodeAddrType :: Text -> Maybe ZAddrType
decodeAddrType = \case
  "sprout" -> Just Sprout
  "sapling" -> Just Sapling
  _ -> Nothing

newtype Memo = Memo ByteString
