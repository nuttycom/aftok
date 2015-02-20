{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Json where

import ClassyPrelude

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Map

import Quixotic
import Quixotic.Interval
import Quixotic.TimeLog

newtype PayoutsJ = PayoutsJ Payouts 
makePrisms ''PayoutsJ

instance ToJSON PayoutsJ where
  toJSON (PayoutsJ p) = 
    toJSON $ mapKeys (^. _BtcAddr) p

instance FromJSON PayoutsJ where
  parseJSON v = 
    PayoutsJ . mapKeys BtcAddr <$> parseJSON v

newtype IntervalJ = IntervalJ Interval
makePrisms ''IntervalJ

instance ToJSON IntervalJ where
  toJSON (IntervalJ ival) = 
    object ["start" .= (ival ^. start), "end" .= (ival ^. end)]

instance FromJSON IntervalJ where
  parseJSON (Object v) = 
    fmap IntervalJ $ interval <$> v .: "start" <*> v .: "end"
  parseJSON _ = mzero


