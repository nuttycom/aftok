{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module Quixotic where

import ClassyPrelude
import Control.Lens
import Data.Aeson
import Data.Aeson.Types as JV
import Data.Group

newtype BtcAddr = BtcAddr { _address :: Text } deriving (Show, Eq, Ord)
makeLenses ''BtcAddr

newtype BTC = BTC { _satoshis :: Int64 } deriving (Show, Eq, Ord)
makeLenses ''BTC

instance Semigroup BTC where
  (<>) (BTC b1) (BTC b2) = BTC $ b1 + b2

instance Monoid BTC where
  mempty = BTC 0
  mappend = (<>)

instance Group BTC where
  invert (BTC b) = BTC . negate $ b

instance Abelian BTC where

parseBtcAddr :: Text -> Maybe BtcAddr
parseBtcAddr = Just . BtcAddr -- this will be changed to do validation

instance FromJSON BtcAddr where
  parseJSON (JV.String t) = return $ BtcAddr t
  parseJSON _ = mzero

--instance FromField BtcAddr where
--  fromField f m = fmap BtcAddr $ fromField f m 
  
