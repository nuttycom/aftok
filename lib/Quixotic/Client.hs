{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}

module Quixotic.Client where

import ClassyPrelude 

import Control.Lens
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Network.Wreq

import Quixotic.Json
import Quixotic.TimeLog

data QCConfig = QCConfig
  { quixoticUrl :: String
  } deriving Show

parseQCConfig :: CT.Config -> IO QCConfig
parseQCConfig cfg = 
  QCConfig <$> C.require cfg "quixoticUrl" 

currentPayouts :: QCConfig -> IO Payouts
currentPayouts cfg = do
  resp <- get (quixoticUrl cfg <> "payouts")
  payoutsResponse <- asJSON resp
  pure . runPayoutsResponse $ payoutsResponse ^. responseBody


