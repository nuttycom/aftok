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
  { quixoticHost :: String
  , quixoticPort :: Int
  } deriving Show

parseQCConfig :: CT.Config -> IO QCConfig
parseQCConfig cfg = 
  QCConfig <$> C.require cfg "quixoticHost" 
           <*> C.require cfg "quixoticPort"

currentPayouts :: QCConfig -> IO Payouts
currentPayouts cfg = do
  resp <- get (quixoticHost cfg <> "/payouts")
  payoutsResponse <- asJSON resp
  pure . runPayoutsResponse $ payoutsResponse ^. responseBody


