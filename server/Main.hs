{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either
import qualified Data.Aeson as A
import Data.Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Clock
import Database.SQLite
import Quixotic
import Quixotic.Database
import Quixotic.Database.SQLite
import Quixotic.TimeLog
import Web.Scotty

port :: Int
port = 8028

main :: IO ()
main = do 
  db <- openConnection "quixotic.db"
  adb <- sqliteADB db
  dbMain db adb

dbMain :: a -> ADB IO a -> IO ()
dbMain db adb = do
  scotty port $ do
{--
Log the start time of a work interval.
Log completion of the current work interval.
Record change of a work interval start.
Record change of a work interval end.
Given a trusted token, authorize another token.
--}
    post "/logStart/:btcAddr" $ handleLogRequest db adb StartWork
    post "/logEnd/:btcAddr" $ handleLogRequest db adb StopWork

    get "/payouts" $ currentPayouts db adb

handleLogRequest :: a -> ADB IO a -> (UTCTime -> WorkEvent) -> ActionM ()
handleLogRequest db adb ev = do 
  BtcAddrParam addr <- param "btcAddr"
  timestamp <- liftIO getCurrentTime
  liftIO $ recordEvent adb db $ LogEntry addr (ev timestamp)

currentPayouts :: a -> ADB IO a -> ActionM ()
currentPayouts db adb = do 
  ptime <- liftIO getCurrentTime
  let dep = linearDepreciation (Months 6) (Months 60) 

      buildPayoutsResponse :: WorkIndex -> ActionM ()
      buildPayoutsResponse widx = json . PayoutsResponse $ payouts dep ptime widx

      payoutsAction :: EitherT T.Text ActionM WorkIndex
      payoutsAction = mapEitherT liftIO $ readWorkIndex adb db 

  eitherT (raise . LT.fromStrict) buildPayoutsResponse payoutsAction

newtype BtcAddrParam = BtcAddrParam BtcAddr 

instance Parsable BtcAddrParam where
  parseParam t = maybe (Left "Invalid BTC address") (Right . BtcAddrParam) $ (parseBtcAddr . LT.toStrict) t

newtype PayoutsResponse = PayoutsResponse Payouts

instance A.ToJSON PayoutsResponse where
  toJSON (PayoutsResponse p) = A.toJSON (mapKeys address p)
