{-# LANGUAGE NoImplicitPrelude #-}

module Quixotic
  ( BtcAddr(address)
  , parseBtcAddr
  , BTC(..)
  ) where

import ClassyPrelude

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types as JV
import Data.Group

newtype BtcAddr = BtcAddr { address :: T.Text } deriving (Show, Eq, Ord)

newtype BTC = BTC { runBTC :: Integer } deriving (Show, Eq, Ord)

instance Semigroup BTC where
  (<>) b1 b2 = BTC $ runBTC b1 + runBTC b2

instance Monoid BTC where
  mempty = BTC 0
  mappend = (<>)

instance Group BTC where
  invert b = BTC . negate . runBTC $ b

instance Abelian BTC where

parseBtcAddr :: T.Text -> Maybe BtcAddr
parseBtcAddr = Just . BtcAddr -- this will be changed to do validation

instance FromJSON BtcAddr where
  parseJSON (JV.String t) = return $ BtcAddr t
  parseJSON _ = mzero

--instance FromField BtcAddr where
--  fromField f m = fmap BtcAddr $ fromField f m 
  
