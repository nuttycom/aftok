{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Database where

import ClassyPrelude
import Control.Lens

import Quixotic.Auction
import Quixotic.Users
import Quixotic.TimeLog

data QDBUser = QDBUser 
  { _userId :: UserId 
  , _user :: User
  }
makeLenses ''QDBUser

data QDB m = QDB 
  { recordEvent :: UserId -> LogEntry -> m ()
  , readWorkIndex :: m WorkIndex
  , newAuction :: Auction -> m AuctionId
  , readAuction :: AuctionId -> m (Maybe Auction)
  , recordBid :: AuctionId -> Bid -> m ()
  , readBids :: AuctionId -> m [Bid]
  , createUser :: User -> m UserId
  , findUser :: UserId -> m (Maybe User)
  , findUserByUserName :: UserName -> m (Maybe QDBUser)
  }
