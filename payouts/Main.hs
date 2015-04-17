{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}

module Main where

import ClassyPrelude 

import Network.Bitcoin
import Control.Concurrent
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Vector as V

import Quixotic.Client
import Quixotic.TimeLog

main :: IO ()
main = do
  cfg <- loadConfig "quixotic-payouts.cfg"
  loop cfg

loop :: QPConfig -> IO ()
loop cfg = do
  distributePayouts cfg
  threadDelay (pollingInterval cfg)
  loop cfg

data QPConfig = QPConfig
  { pollingInterval :: Int
  , bitcoindUrl :: String
  , bitcoindUser :: ByteString
  , bitcoindPassword :: ByteString
  , payoutMinConfirmations :: Int
  , qcConfig :: QCConfig
  } deriving Show

loadConfig :: FilePath -> IO QPConfig
loadConfig cfgFile = do 
  cfg <- C.load [C.Required (fpToString cfgFile)]
  parseQPConfig cfg

parseQPConfig :: CT.Config -> IO QPConfig
parseQPConfig cfg =  
  QPConfig <$> C.require cfg "pollingInterval" 
           <*> C.require cfg "bitcoindUrl" 
           <*> C.require cfg "bitcoindUser" 
           <*> C.require cfg "bitcoindPassword" 
           <*> C.require cfg "payoutMinConfirmations" 
           <*> parseQCConfig (C.subconfig "qcConfig" cfg)

btcClient :: QPConfig -> IO Client
btcClient = getClient <$> bitcoindUrl <*> bitcoindUser <*> bitcoindPassword 

distributePayouts :: QPConfig -> IO ()
distributePayouts cfg = do
  -- find unspent transactions
  client <- btcClient cfg
  unspent <- listUnspent client (Just . payoutMinConfirmations $ cfg) Nothing V.empty 
  -- get payouts amounts
  (Payouts p) <- currentPayouts (qcConfig cfg)
  -- create a new txn spending all UTXOs to payouts
  putStrLn . tshow $ unspent
  putStrLn . tshow $ p 
