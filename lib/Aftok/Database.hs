{-# LANGUAGE GADTs #-}

module Aftok.Database where

import ClassyPrelude
import Control.Lens

import Aftok
import Aftok.Auction
import Aftok.Interval
import Aftok.TimeLog
import Aftok.Util

type KeyedUser     = (UserId, User)
type KeyedLogEntry = (ProjectId, UserId, LogEntry)
type KeyedProject  = (ProjectId, Project)
type InvitingUID   = UserId
type InvitedUID    = UserId

type DBProg a = Program DBOp a

data DBOp a where
  CreateUser       :: User -> DBOp UserId
  FindUser         :: UserId -> DBOp (Maybe User)
  FindUserByName   :: UserName -> DBOp (Maybe KeyedUser)

  CreateProject    :: Project -> DBOp ProjectId
  FindProject      :: ProjectId -> DBOp (Maybe Project)
  FindUserProjects :: UserId -> DBOp [KeyedProject]
  AddUserToProject :: ProjectId -> InvitingUID -> InvitedUID -> DBOp ()

  CreateEvent      :: ProjectId -> UserId -> LogEntry -> DBOp EventId
  AmendEvent       :: EventId -> EventAmendment -> DBOp AmendmentId
  FindEvent        :: EventId -> DBOp (Maybe KeyedLogEntry)
  FindEvents       :: ProjectId -> UserId -> Interval' -> DBOp [LogEntry]
  ReadWorkIndex    :: ProjectId -> DBOp WorkIndex

  CreateAuction    :: ProjectId -> Auction -> DBOp AuctionId
  FindAuction      :: AuctionId -> DBOp (Maybe Auction)
  CreateBid        :: AuctionId -> Bid -> DBOp BidId
  ReadBids         :: AuctionId -> DBOp [Bid]

  OpForbidden      :: forall x. UserId -> OpForbiddenReason -> DBOp x -> DBOp x
  SubjectNotFound  :: forall x. DBOp x -> DBOp x

data OpForbiddenReason = UserNotProjectMember
                       | UserNotEventLogger
                       deriving (Eq, Show)

class DBEval m where
  dbEval :: DBOp a -> m a

-- User ops

createUser :: User -> DBProg UserId
createUser = fc . CreateUser

findUser :: UserId -> DBProg (Maybe User)
findUser = fc . FindUser

findUserByName :: UserName -> DBProg (Maybe KeyedUser)
findUserByName = fc . FindUserByName

-- Project ops

createProject :: Project -> DBProg ProjectId
createProject p = do
  pid <- fc $ CreateProject p
  addUserToProject pid (p ^. initiator) (p ^. initiator)
  return pid

findProject :: ProjectId -> UserId -> DBProg (Maybe Project)
findProject pid uid = do
  kps <- findUserProjects uid 
  pure $ fmap snd (find (\(pid', _) -> pid' == pid) kps)
  
findUserProjects :: UserId -> DBProg [KeyedProject]
findUserProjects = fc . FindUserProjects

addUserToProject :: ProjectId -> InvitingUID -> InvitedUID -> DBProg ()
addUserToProject pid current new = 
  withProjectAuth pid current $ AddUserToProject pid current new

withProjectAuth :: ProjectId -> UserId -> DBOp a -> DBProg a
withProjectAuth pid uid act = do
  px <- findUserProjects uid
  fc $ if any (\(pid', _) -> pid' == pid) px 
    then act 
    else OpForbidden uid UserNotProjectMember act

-- Log ops

-- TODO: ignore "duplicate" events within some small time limit?
createEvent :: ProjectId -> UserId -> LogEntry -> DBProg EventId
createEvent p u l = withProjectAuth p u $ CreateEvent p u l

amendEvent :: UserId -> EventId -> EventAmendment -> DBProg AmendmentId
amendEvent uid eid a = do
  ev <- findEvent eid
  let act = AmendEvent eid a
      forbidden = OpForbidden uid UserNotEventLogger act
      missing = SubjectNotFound act
  fc $ maybe missing (\(_, uid', _) -> if uid' == uid then act else forbidden) ev

findEvent :: EventId -> DBProg (Maybe KeyedLogEntry)
findEvent = fc . FindEvent

findEvents :: ProjectId -> UserId -> Interval' -> DBProg [LogEntry]
findEvents p u i = fc $ FindEvents p u i

readWorkIndex :: ProjectId -> UserId -> DBProg WorkIndex
readWorkIndex pid uid = withProjectAuth pid uid $ ReadWorkIndex pid

