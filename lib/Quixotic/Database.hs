{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Database where

import ClassyPrelude
import Control.Lens

import Quixotic
import Quixotic.Auction
import Quixotic.Interval
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
  { createEvent   :: ProjectId -> UserId -> LogEntry -> m EventId
  , amendEvent    :: EventId -> LogModification -> m ()
  , findEvents    :: ProjectId -> UserId -> Interval' -> m [LogEntry]
  , readWorkIndex :: ProjectId -> m WorkIndex

  , createAuction :: ProjectId -> Auction -> m AuctionId
  , findAuction   :: AuctionId -> m (Maybe Auction)
  , createBid     :: AuctionId -> Bid -> m ()
  , readBids      :: AuctionId -> m [Bid]

  , createUser    :: User -> m UserId
  , findUser      :: UserId -> m (Maybe User)
  , findUserByUserName :: UserName -> m (Maybe QDBUser)

  , createProject :: Project -> m ProjectId
  , findProject   :: ProjectId -> m (Maybe Project)
  , findUserProjects :: UserId -> m [QDBProject]
  }
