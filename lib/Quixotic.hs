{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module Quixotic where

import ClassyPrelude
import Control.Lens
import Data.Aeson
import Data.Aeson.Types as JV
import Data.Group

newtype BtcAddr = BtcAddr { _address :: Text } deriving (Show, Eq, Ord)
makeLenses ''BtcAddr

newtype BTC = BTC { _btc :: Integer } deriving (Show, Eq, Ord)
makeLenses ''BTC

instance Semigroup BTC where
  (<>) b1 b2 = BTC $ _btc b1 + _btc b2

instance Monoid BTC where
  mempty = BTC 0
  mappend = (<>)

instance Group BTC where
  invert b = BTC . negate . _btc $ b

instance Abelian BTC where

parseBtcAddr :: Text -> Maybe BtcAddr
parseBtcAddr = Just . BtcAddr -- this will be changed to do validation

instance FromJSON BtcAddr where
  parseJSON (JV.String t) = return $ BtcAddr t
  parseJSON _ = mzero

--instance FromField BtcAddr where
--  fromField f m = fmap BtcAddr $ fromField f m 
  
