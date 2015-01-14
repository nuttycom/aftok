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

newtype PayoutsResponse = PayoutsResponse Payouts

instance ToJSON PayoutsResponse where
  toJSON (PayoutsResponse p) = toJSON m where
    m :: Map Text Double
    m = fmap fromRational $ mapKeys (^. address) p


