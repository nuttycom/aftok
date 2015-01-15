{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}

module Main where

import ClassyPrelude 

import Network.Bitcoin
import Control.Concurrent
import qualified Data.Configurator as C
import qualified Data.Vector as V

import Quixotic.Client

main :: IO ()
main = do
  cfg <- parseConfig "quixotic-payouts.cfg"
  loop cfg

loop :: QPConfig -> IO ()
loop cfg = do
  threadDelay (pollingInterval cfg)
  distributePayouts cfg
  loop cfg

data QPConfig = QPConfig
  { pollingInterval :: Int
  , bitcoindUrl :: Text
  , bitcoindUser :: Text
  , bitcoindPassword :: Text
  , payoutMinConfirmations :: Int
  } deriving Show

parseConfig :: FilePath -> IO QPConfig
parseConfig cfgFile = do 
  cfg <- C.load [C.Required (fpToString cfgFile)]
  QPConfig <$> C.require cfg "pollingInterval" 
           <*> C.require cfg "bitcoindUrl" 
           <*> C.require cfg "bitcoindUser" 
           <*> C.require cfg "bitcoindPassword" 
           <*> C.require cfg "payoutMinConfirmations" 

auth :: QPConfig -> Auth
auth = Auth <$> bitcoindUrl <*> bitcoindUser <*> bitcoindPassword 

distributePayouts :: QPConfig -> IO ()
distributePayouts cfg = do
  -- find unspent transactions
  putStrLn $ "Searching for unspent payouts " <> tshow cfg
  unspent <- listUnspent (auth cfg) (Just . payoutMinConfirmations $ cfg) Nothing V.empty 
  putStrLn $ tshow unspent
  -- get payouts amounts
  -- create a new txn spending all UTXOs to payouts
