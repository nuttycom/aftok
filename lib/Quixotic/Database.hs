{-# LANGUAGE NoImplicitPrelude #-}

module Quixotic.Database where

import ClassyPrelude
import Quixotic.Auction
import Quixotic.Users
import Quixotic.TimeLog

data QDB m a = QDB 
  { recordEvent :: LogEntry -> ReaderT a m ()
  , readWorkIndex :: ReaderT a m WorkIndex
  , newAuction :: Auction -> ReaderT a m AuctionId
  , readAuction :: AuctionId -> ReaderT a m Auction
  , recordBid :: UTCTime -> Bid -> ReaderT a m ()
  , readBids :: AuctionId -> ReaderT a m [(UTCTime, Bid)]
  , createUser :: User -> ReaderT a m UserId
  }
