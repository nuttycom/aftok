{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Database.SQLite (sqliteQDB) where

import ClassyPrelude
import Control.Error.Safe
import Control.Lens
import Control.Monad.Trans.Either
import Data.Text.Lens
import Database.SQLite.Simple

import Quixotic
import Quixotic.Auction
import Quixotic.Database
import Quixotic.TimeLog
import Quixotic.Users

newtype PLogEntry = PLogEntry LogEntry
makePrisms ''PLogEntry

instance ToRow PLogEntry where
  toRow (PLogEntry (LogEntry a e)) = 
    toRow (a ^. address, e ^. (eventType . to eventName), e ^. eventTime)

instance FromRow PLogEntry where
  fromRow = 
    let workEventParser = WorkEvent <$> (field >>= nameEvent) <*> field 
        logEntryParser  = LogEntry <$> (fmap BtcAddr field) <*> workEventParser 
    in  fmap PLogEntry logEntryParser

newtype PAuction = PAuction Auction
makePrisms ''PAuction

instance FromRow PAuction where
  fromRow = 
    let auctionParser = Auction <$> (fmap BTC field) <*> field
    in  fmap PAuction auctionParser

recordEvent' :: LogEntry -> ReaderT Connection IO ()
recordEvent' logEntry = do 
  conn <- ask
  lift $ execute conn 
    "INSERT INTO work_events (btc_addr, event_type, event_time) VALUES (?, ?, ?)" 
    (logEntry ^. (from _PLogEntry))

readWorkIndex' :: ReaderT Connection IO WorkIndex
readWorkIndex' = do
  conn <- ask
  rows <- lift $ query_ conn
    "SELECT btc_addr, event_type, event_time from workEvents" 
  lift . return . workIndex $ fmap (^. _PLogEntry) rows

newAuction' :: Auction -> ReaderT Connection IO AuctionId
newAuction' auc = do
  conn <- ask
  lift $ execute conn
    "INSERT INTO auctions (raise_amount, end_time) VALUES (?, ?)"
    (auc ^. (raiseAmount . satoshis), auc ^. auctionEnd)
  lift . fmap AuctionId $ lastInsertRowId conn

readAuction' :: AuctionId -> ReaderT Connection IO (Maybe Auction)
readAuction' (AuctionId aid) = do
  conn <- ask
  rows <- lift $ query conn
    "SELECT raise_amount, end_time FROM auctions WHERE ROWID = ?" 
    (Only aid)
  lift . return . headMay $ fmap (^. _PAuction) rows

recordBid' :: AuctionId -> Bid -> ReaderT Connection IO ()
recordBid' (AuctionId aid) bid = do
  conn <- ask
  lift $ execute conn
    "INSERT INTO bids (auction_id, user_id, bid_seconds, bid_amount, bid_time) values (?, ?, ?, ?, ?)"
    (aid, bid ^. bidUser, bid ^. bidSeconds, bid ^. bidAmount, bid ^. bidTime)

readBids' :: AuctionId -> ReaderT Connection IO [(UTCTime, Bid)]
readBids' = undefined

createUser' :: User -> ReaderT Connection IO UserId
createUser' = undefined

sqliteQDB :: QDB IO Connection
sqliteQDB = QDB 
  { recordEvent = recordEvent'
  , readWorkIndex = readWorkIndex' 
  , newAuction = newAuction'
  , readAuction = readAuction'
  , recordBid = recordBid'
  , readBids = readBids'
  , createUser = createUser'
  }
