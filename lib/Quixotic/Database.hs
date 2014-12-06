{-# LANGUAGE NoImplicitPrelude #-}

module Quixotic.Database 
  ( ADB(..)
  ) where

import Control.Monad.Trans.Either
import Quixotic.Auction
import Quixotic.Users
import Quixotic.TimeLog
import qualified Data.Text as T

data ADB m a = ADB 
  { recordEvent :: LogEntry -> ReaderT a m ()
  , readWorkIndex :: EitherT T.Text (ReaderT a m) WorkIndex
  , newAuction :: Auction -> ReaderT a m AuctionId
  , readAuction :: AuctionId -> ReaderT a m Auction
  , recordBid :: UTCTime -> Bid -> ReaderT a m ()
  , readBids :: AuctionId -> ReaderT a m [(UTCTime, Bid)]
  , createUser :: User -> ReaderT a m UserId
  }
