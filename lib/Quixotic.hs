{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module Quixotic where

import ClassyPrelude
import Control.Lens
import Data.Aeson
import Data.Aeson.Types as JV

newtype BtcAddr = BtcAddr { _address :: Text } deriving (Show, Eq, Ord)
makeLenses ''BtcAddr

parseBtcAddr :: Text -> Maybe BtcAddr
parseBtcAddr = Just . BtcAddr -- this will be changed to do validation

instance FromJSON BtcAddr where
  parseJSON (JV.String t) = return $ BtcAddr t
  parseJSON _ = mzero

--instance FromField BtcAddr where
--  fromField f m = fmap BtcAddr $ fromField f m 
  
