{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Quixotic.Json where

import ClassyPrelude
import Control.Lens
import Data.Aeson
import Data.Map

import Quixotic
import Quixotic.TimeLog

newtype PayoutsResponse = PayoutsResponse { runPayoutsResponse :: Payouts }

instance ToJSON PayoutsResponse where
  toJSON (PayoutsResponse p) = 
    toJSON $ mapKeys (^. address) p

instance FromJSON PayoutsResponse where
  parseJSON v = 
    fmap (PayoutsResponse . mapKeys BtcAddr) $ parseJSON v

