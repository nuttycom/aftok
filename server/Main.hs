{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.Lazy
import Web.Scotty
import Data.Time.Clock
import Database.SQLite
import Control.Monad.Trans (liftIO)
import Ananke

main :: IO ()
main = do
  db <- openConnection "ananke.db"
  _  <- defineTableOpt db True eventTable
  scotty port $ do
{--
Log the start time of a work interval.
Log completion of the current work interval.
Record change of a work interval start.
Record change of a work interval end.
Given a trusted token, authorize another token.
--}
    post "/logStart/:btcAddr" $ do
      addr <- param "btcAddr"
      timestamp <- liftIO getCurrentTime
      liftIO $ recordStart db (btcAddr addr) timestamp


recordStart :: SQLiteHandle -> BtcAddr -> UTCTime -> IO ()
recordStart = undefined

port :: Int
port = 8028

eventTable :: SQLTable
eventTable = Table "workEvents" [ Column "btcAddr"   (SQLVarChar 256) []
                                , Column "event"     (SQLVarChar 64) []
                                , Column "eventTime" (SQLDateTime DATETIME) [] ] []

newtype BtcAddrParam = BtcAddrParam { btcAddr :: BtcAddr }

instance Parsable BtcAddrParam where
  parseParam t = maybe (Left "Invalid BTC address") (Right . BtcAddrParam) $ (parseBtcAddr . toStrict) t

