{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Database where

import ClassyPrelude
import Control.Lens

import Quixotic
import Quixotic.Auction
import Quixotic.TimeLog

data QDBUser = QDBUser 
  { _userId :: UserId 
  , _user :: User
  }
makeLenses ''QDBUser

data QDBProject = QDBProject
  { _projectId :: ProjectId
  , _project :: Project
  }
makeLenses ''QDBProject

data QDB m = QDB 
  { recordEvent   :: ProjectId -> UserId -> LogEntry -> m EventId
  , amendEvent    :: EventId -> LogModification -> m ()
  , readWorkIndex :: ProjectId -> m WorkIndex
  , newAuction    :: ProjectId -> Auction -> m AuctionId
  , readAuction   :: AuctionId -> m (Maybe Auction)
  , recordBid     :: AuctionId -> Bid -> m ()
  , readBids      :: AuctionId -> m [Bid]
  , createUser    :: User -> m UserId
  , findUser      :: UserId -> m (Maybe User)
  , findUserByUserName :: UserName -> m (Maybe QDBUser)
  , createProject :: Project -> m ProjectId
  , findUserProjects :: UserId -> m [QDBProject]
  }
