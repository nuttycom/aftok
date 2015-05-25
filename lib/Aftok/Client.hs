{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}

module Aftok.Client where

import ClassyPrelude 

import Control.Lens
import Data.Aeson.Types
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Network.Wreq

import Aftok.Json
import Aftok.TimeLog

data QCConfig = QCConfig
  { aftokUrl :: String
  } deriving Show

parseQCConfig :: CT.Config -> IO QCConfig
parseQCConfig cfg = 
  QCConfig <$> C.require cfg "aftokUrl" 

currentPayouts :: QCConfig -> IO Payouts
currentPayouts cfg = do
  resp <- get (aftokUrl cfg <> "payouts")
  valueResponse <- asValue resp
  either fail pure (parseEither parsePayoutsJSON $ valueResponse ^. responseBody)


