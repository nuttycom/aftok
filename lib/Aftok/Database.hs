{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE GADTs              #-}

module Aftok.Database where

import           ClassyPrelude
import           Control.Lens
import           Data.AffineSpace
import           Data.Thyme.Clock as C

import           Aftok
import           Aftok.Auction    as A
import           Aftok.Billables  as B
import           Aftok.Interval
import           Aftok.Payments
import           Aftok.Project    as P
import           Aftok.TimeLog
import           Aftok.Util

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
  CreateInvitation :: ProjectId -> InvitingUID -> Email -> C.UTCTime -> DBOp InvitationCode
  FindInvitation   :: InvitationCode -> DBOp (Maybe Invitation)
  AcceptInvitation :: UserId -> InvitationCode -> C.UTCTime -> DBOp ()

  CreateEvent      :: ProjectId -> UserId -> LogEntry -> DBOp EventId
  AmendEvent       :: EventId -> EventAmendment -> DBOp AmendmentId
  FindEvent        :: EventId -> DBOp (Maybe KeyedLogEntry)
  FindEvents       :: ProjectId -> UserId -> Interval' -> DBOp [LogEntry]
  ReadWorkIndex    :: ProjectId -> DBOp WorkIndex

  CreateAuction    :: Auction -> DBOp AuctionId
  FindAuction      :: AuctionId -> DBOp (Maybe Auction)
  CreateBid        :: AuctionId -> Bid -> DBOp BidId
  ReadBids         :: AuctionId -> DBOp [Bid]

  CreateBillable   :: Billable -> DBOp BillableId
  ReadBillable     :: BillableId -> DBOp (Maybe Billable)

  CreateSubscription :: UserId -> BillableId -> DBOp SubscriptionId

  CreatePaymentRequest :: PaymentRequest -> DBOp PaymentRequestId
  CreatePayment        :: Payment -> DBOp PaymentId

  RaiseDBError     :: forall x y. DBError -> DBOp x -> DBOp y

data OpForbiddenReason = UserNotProjectMember
                       | UserNotEventLogger
                       | InvitationExpired
                       | InvitationAlreadyAccepted
                       | AuctionEnded
                       deriving (Eq, Show, Typeable)

data DBError = OpForbidden UserId OpForbiddenReason
             | SubjectNotFound
             | EventStorageFailed
             deriving (Eq, Show, Typeable)

instance Exception DBError

raiseOpForbidden :: UserId -> OpForbiddenReason -> DBOp x -> DBOp x
raiseOpForbidden uid r = RaiseDBError (OpForbidden uid r)

raiseSubjectNotFound :: DBOp y -> DBOp x
raiseSubjectNotFound = RaiseDBError SubjectNotFound

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
  addUserToProject pid (p ^. P.initiator) (p ^. P.initiator)
  return pid

findProject :: ProjectId -> UserId -> DBProg (Maybe Project)
findProject pid uid = do
  kps <- findUserProjects uid
  pure $ fmap snd (find (\(pid', _) -> pid' == pid) kps)

findUserProjects :: UserId -> DBProg [KeyedProject]
findUserProjects = fc . FindUserProjects

withProjectAuth :: ProjectId -> UserId -> DBOp a -> DBProg a
withProjectAuth pid uid act = do
  px <- findUserProjects uid
  fc $ if any (\(pid', _) -> pid' == pid) px
    then act
    else raiseOpForbidden uid UserNotProjectMember act

checkProjectAuth :: ProjectId -> UserId -> DBOp a -> DBProg ()
checkProjectAuth pid uid act = do
  px <- findUserProjects uid
  if any (\(pid', _) -> pid' == pid) px
    then pure ()
    else void . fc $ raiseOpForbidden uid UserNotProjectMember act

addUserToProject :: ProjectId -> InvitingUID -> InvitedUID -> DBProg ()
addUserToProject pid current new =
  withProjectAuth pid current $ AddUserToProject pid current new

createInvitation :: ProjectId -> InvitingUID -> Email -> C.UTCTime -> DBProg InvitationCode
createInvitation pid current email t =
  withProjectAuth pid current $ CreateInvitation pid current email t

findInvitation :: InvitationCode -> DBProg (Maybe Invitation)
findInvitation ic = fc $ FindInvitation ic

acceptInvitation :: UserId -> C.UTCTime -> InvitationCode-> DBProg ()
acceptInvitation uid t ic = do
  inv <- findInvitation ic
  let act = AcceptInvitation uid ic t
  case inv of
    Nothing ->
      fc $ raiseSubjectNotFound act
    Just i | t .-. (i ^. invitationTime) > fromSeconds (60 * 60 * 72 :: Int) ->
      fc $ raiseOpForbidden uid InvitationExpired act
    Just i | isJust (i ^. acceptanceTime) ->
      fc $ raiseOpForbidden uid InvitationAlreadyAccepted act
    Just i ->
      withProjectAuth (i ^. P.projectId) (i ^. P.invitingUser) act

-- Log ops

-- TODO: ignore "duplicate" events within some small time limit?
createEvent :: ProjectId -> UserId -> LogEntry -> DBProg EventId
createEvent p u l = withProjectAuth p u $ CreateEvent p u l

amendEvent :: UserId -> EventId -> EventAmendment -> DBProg AmendmentId
amendEvent uid eid a = do
  ev <- findEvent eid
  let act = AmendEvent eid a
      forbidden = raiseOpForbidden uid UserNotEventLogger act
      missing = raiseSubjectNotFound act
  fc $ maybe missing (\(_, uid', _) -> if uid' == uid then act else forbidden) ev

findEvent :: EventId -> DBProg (Maybe KeyedLogEntry)
findEvent = fc . FindEvent

findEvents :: ProjectId -> UserId -> Interval' -> DBProg [LogEntry]
findEvents p u i = fc $ FindEvents p u i

readWorkIndex :: ProjectId -> UserId -> DBProg WorkIndex
readWorkIndex pid uid = withProjectAuth pid uid $ ReadWorkIndex pid

-- Billing ops

createBillable :: Billable -> DBProg BillableId
createBillable b = withProjectAuth (b ^. B.project) (b ^. B.creator) $ CreateBillable b

readBillable :: BillableId -> DBProg (Maybe Billable)
readBillable = fc . ReadBillable

--createPaymentRequest :: BillableId -> DBProg PaymentRequestId
--createPaymentRequest bid = do
--  billable <- readBillable bid
  

readPaymentHistory :: UserId -> DBProg [Payment]
readPaymentHistory = error "Not yet implemented"

-- Auction ops

createAuction :: Auction -> DBProg AuctionId
createAuction a = do
  withProjectAuth (a ^. A.projectId) (a ^. A.initiator) $ CreateAuction a

findAuction :: AuctionId -> UserId -> DBProg (Maybe Auction)
findAuction aid uid =
  let findOp = FindAuction aid
  in  do
    maybeAuc <- fc findOp
    _ <- traverse (\auc -> checkProjectAuth (auc ^. A.projectId) uid findOp) maybeAuc
    pure maybeAuc

findAuction' :: AuctionId -> UserId -> DBProg Auction
findAuction' aid uid =
  let findOp = FindAuction aid
  in  do
    maybeAuc <- fc findOp
    _ <- traverse (\auc -> checkProjectAuth (auc ^. A.projectId) uid findOp) maybeAuc
    maybe (fc $ raiseSubjectNotFound findOp) pure maybeAuc

createBid :: AuctionId -> UserId -> Bid -> DBProg BidId
createBid aid uid bid =
  let createOp = CreateBid aid bid
  in  do
    auc <- findAuction' aid uid
    fc $ if view bidTime bid > view auctionEnd auc
      then raiseOpForbidden uid AuctionEnded createOp
      else createOp
