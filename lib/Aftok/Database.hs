{-# LANGUAGE TemplateHaskell #-}

module Aftok.Database where

import ClassyPrelude

import Aftok
import Aftok.Auction
import Aftok.Interval
import Aftok.TimeLog

type QDBUser     = (UserId, User)
type QDBLogEntry = (EventId, ProjectId, UserId, LogEntry)
type QDBProject  = (ProjectId, Project)

data QDB m = QDB 
  { createEvent   :: ProjectId -> UserId -> LogEntry -> m EventId
  , amendEvent    :: EventId -> EventAmendment -> m AmendmentId
  , findEvent     :: EventId -> m (Maybe QDBLogEntry)
  , findEvents    :: ProjectId -> UserId -> Interval' -> m [LogEntry]
  , readWorkIndex :: ProjectId -> m WorkIndex

  , createAuction :: ProjectId -> Auction -> m AuctionId
  , findAuction   :: AuctionId -> m (Maybe Auction)

  , createBid     :: AuctionId -> Bid -> m BidId
  , readBids      :: AuctionId -> m [Bid]

  , createUser    :: User -> m UserId
  , findUser      :: UserId -> m (Maybe User)
  , findUserByUserName :: UserName -> m (Maybe QDBUser)

  , createProject :: Project -> m ProjectId
  , findProject   :: ProjectId -> m (Maybe Project)
  , findUserProjects :: UserId -> m [QDBProject]
  }
