{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Database where

import ClassyPrelude
import Control.Lens

import Quixotic.Auctions
import Quixotic.Projects
import Quixotic.TimeLog
import Quixotic.Users

data QDBUser = QDBUser 
  { _userId :: UserId 
  , _user :: User
  }
makeLenses ''QDBUser

data QDB m = QDB 
  { recordEvent   :: ProjectId -> UserId -> LogEntry -> m ()
  , readWorkIndex :: ProjectId -> m WorkIndex
  , newAuction    :: ProjectId -> Auction -> m AuctionId
  , readAuction   :: ProjectId -> AuctionId -> m (Maybe Auction)
  , recordBid     :: ProjectId -> AuctionId -> Bid -> m ()
  , readBids      :: ProjectId -> AuctionId -> m [Bid]
  , createUser    :: User -> m UserId
  , findUser      :: UserId -> m (Maybe User)
  , findUserByUserName :: UserName -> m (Maybe QDBUser)
  }
