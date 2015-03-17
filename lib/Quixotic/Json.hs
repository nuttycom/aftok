{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Json where

import ClassyPrelude

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Map as M

import Quixotic
import Quixotic.Interval
import Quixotic.TimeLog

newtype PayoutsJ = PayoutsJ Payouts 
makePrisms ''PayoutsJ

instance ToJSON PayoutsJ where
  toJSON (PayoutsJ p) = 
    toJSON $ M.mapKeys (^. _BtcAddr) p

instance FromJSON PayoutsJ where
  parseJSON v = 
    PayoutsJ . M.mapKeys BtcAddr <$> parseJSON v

newtype IntervalJ = IntervalJ Interval
makePrisms ''IntervalJ

instance ToJSON IntervalJ where
  toJSON (IntervalJ ival) = 
    object ["start" .= (ival ^. start), "end" .= (ival ^. end)]

instance FromJSON IntervalJ where
  parseJSON (Object v) = 
    fmap IntervalJ $ interval <$> v .: "start" <*> v .: "end"
  parseJSON _ = mzero

newtype ProjectJ = ProjectJ Project
makePrisms ''ProjectJ

instance ToJSON ProjectJ where
  toJSON (ProjectJ p) = 
    object [ "projectName"    .= (p ^. projectName)
           , "inceptionDate"  .= (p ^. inceptionDate)
           , "initiator"      .= (p ^. (initiator._UserId)) ]

newtype WidxJ = WidxJ WorkIndex
makePrisms ''WidxJ

instance ToJSON WidxJ where
  toJSON (WidxJ widx) = 
    toJSON $ (fmap IntervalJ) <$> (M.mapKeysWith (++) (^._BtcAddr) widx)
