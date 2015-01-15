{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}

module Quixotic.Client where

import ClassyPrelude 

import Control.Lens
import Network.Wreq

import Quixotic.Json
import Quixotic.TimeLog

data QCConfig = QCConfig
  { quixoticHost :: String
  , quixoticPort :: Int
  }

currentPayouts :: QCConfig -> IO Payouts
currentPayouts cfg = do
  resp <- get (quixoticHost cfg <> "/payouts")
  payoutsResponse <- asJSON resp
  pure . runPayoutsResponse $ payoutsResponse ^. responseBody

