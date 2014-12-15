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
  , readAuction :: AuctionId -> ReaderT a m (Maybe Auction)
  , recordBid :: AuctionId -> Bid -> ReaderT a m ()
  , readBids :: AuctionId -> ReaderT a m [Bid]
  , createUser :: User -> ReaderT a m UserId
  }
