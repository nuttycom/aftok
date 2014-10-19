{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either
import qualified Data.Aeson as A
import Data.Map
import Data.Time.Clock
import Data.Time.Format
import Quixotic
import Quixotic.Database
import Quixotic.TimeLog
import Web.Scotty

port :: Int
port = 8028

main :: IO ()

dbMain :: a -> ADB a -> IO ()
dbMain db adb = do
  scotty port $ do
{--
Log the start time of a work interval.
Log completion of the current work interval.
Record change of a work interval start.
Record change of a work interval end.
Given a trusted token, authorize another token.
--}
    post "/logStart/:btcAddr" $ handleLogRequest adb StartWork
    post "/logEnd/:btcAddr" $ handleLogRequest db StopWork

    get "/payouts" $ currentPayouts adb

handleLogRequest :: ADB a -> (UTCTime -> WorkEvent) -> ActionM ()
handleLogRequest db ev = do 
  BtcAddrParam addr <- param "btcAddr"
  timestamp <- liftIO getCurrentTime
  liftIO . recordEvent db $ LogEntry addr (ev timestamp)

currentPayouts :: ADB a -> ActionM ()
currentPayouts db = do 
  ptime <- liftIO getCurrentTime
  let dep = linearDepreciation (Months 6) (Months 60) 
  widx <- undefined 
  json . PayoutsResponse $ payouts dep ptime widx

newtype BtcAddrParam = BtcAddrParam BtcAddr 

instance Parsable BtcAddrParam where
  parseParam t = maybe (Left "Invalid BTC address") (Right . BtcAddrParam) $ (parseBtcAddr . LT.toStrict) t

newtype PayoutsResponse = PayoutsResponse Payouts

instance A.ToJSON PayoutsResponse where
  toJSON (PayoutsResponse p) = A.toJSON (mapKeys address p)
