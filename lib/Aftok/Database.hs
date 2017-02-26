{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TupleSections      #-}

module Aftok.Database where

import           ClassyPrelude
import           Control.Lens              (view, (^.))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.AffineSpace
import           Data.Thyme.Clock          as C

import           Aftok
import           Aftok.Auction             as A
import           Aftok.Billables           as B
import           Aftok.Interval
import           Aftok.Payments.Types
import           Aftok.Project             as P
import           Aftok.TimeLog
import           Aftok.Util

type KeyedLogEntry = (ProjectId, UserId, LogEntry)
type InvitingUID   = UserId
type InvitedUID    = UserId

data DBOp a where
  CreateUser       :: User -> DBOp UserId
  FindUser         :: UserId -> DBOp (Maybe User)
  FindUserByName   :: UserName -> DBOp (Maybe (UserId, User))

  CreateProject    :: Project -> DBOp ProjectId
  FindProject      :: ProjectId -> DBOp (Maybe Project)
  FindUserProjects :: UserId -> DBOp [(ProjectId, Project)]
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
  FindBids         :: AuctionId -> DBOp [(BidId, Bid)]

  CreateBillable   :: UserId -> Billable -> DBOp BillableId
  FindBillable     :: BillableId -> DBOp (Maybe Billable)

  CreateSubscription :: UserId -> BillableId -> DBOp SubscriptionId
  FindSubscription   :: SubscriptionId -> DBOp (Maybe Subscription)
  FindSubscriptions  :: UserId -> ProjectId -> DBOp [(SubscriptionId, Subscription)]

  CreatePaymentRequest  :: PaymentRequest -> DBOp PaymentRequestId
  FindPaymentRequests   :: SubscriptionId -> DBOp [(PaymentRequestId, PaymentRequest)]
  FindUnpaidRequests    :: SubscriptionId -> DBOp [BillDetail]
  FindPaymentRequest    :: PaymentKey -> DBOp (Maybe (PaymentRequestId, PaymentRequest))

  CreatePayment  :: Payment -> DBOp PaymentId
  FindPayments   :: PaymentRequestId -> DBOp [(PaymentId, Payment)]

  RaiseDBError     :: forall x y. DBError -> DBOp x -> DBOp y

data OpForbiddenReason = UserNotProjectMember
                       | UserNotEventLogger
                       | UserNotSubscriber SubscriptionId
                       | InvitationExpired
                       | InvitationAlreadyAccepted
                       | AuctionEnded
                       deriving (Eq, Show, Typeable)

data DBError = OpForbidden UserId OpForbiddenReason
             | SubjectNotFound
             | EventStorageFailed
             deriving (Eq, Show, Typeable)

instance Exception DBError

class (Monad m) => MonadDB (m :: * -> *) where
  liftdb :: DBOp x -> m x

instance MonadDB (Program DBOp) where
  liftdb = fc

raiseOpForbidden :: (MonadDB m) => UserId -> OpForbiddenReason -> DBOp x -> m x
raiseOpForbidden uid r op = liftdb $ RaiseDBError (OpForbidden uid r) op

raiseSubjectNotFound :: (MonadDB m) => DBOp y -> m x
raiseSubjectNotFound op = liftdb $ RaiseDBError SubjectNotFound op

-- User ops

createUser :: (MonadDB m) => User -> m UserId
createUser = liftdb . CreateUser

findUser :: (MonadDB m) => UserId -> m (Maybe User)
findUser = liftdb . FindUser

findUserByName :: (MonadDB m) => UserName -> m (Maybe (UserId, User))
findUserByName = liftdb . FindUserByName

-- Project ops

createProject :: (MonadDB m) => Project -> m ProjectId
createProject p = do
  pid <- liftdb $ CreateProject p
  addUserToProject pid (p ^. P.initiator) (p ^. P.initiator)
  return pid

findProject :: (MonadDB m) => ProjectId -> UserId -> m (Maybe Project)
findProject pid uid = do
  kps <- findUserProjects uid
  pure $ fmap snd (find (\(pid', _) -> pid' == pid) kps)

findUserProjects :: (MonadDB m) => UserId -> m [(ProjectId, Project)]
findUserProjects = liftdb . FindUserProjects

withProjectAuth :: (MonadDB m) => ProjectId -> UserId -> DBOp a -> m a
withProjectAuth pid uid act = do
  px <- findUserProjects uid
  if any (\(pid', _) -> pid' == pid) px
    then liftdb act
    else raiseOpForbidden uid UserNotProjectMember act

checkProjectAuth :: (MonadDB m) => ProjectId -> UserId -> DBOp a -> m ()
checkProjectAuth pid uid act = do
  px <- findUserProjects uid
  if any (\(pid', _) -> pid' == pid) px
    then pure ()
    else void $ raiseOpForbidden uid UserNotProjectMember act

addUserToProject :: (MonadDB m) => ProjectId -> InvitingUID -> InvitedUID -> m ()
addUserToProject pid current new =
  withProjectAuth pid current $ AddUserToProject pid current new

createInvitation :: (MonadDB m) => ProjectId -> InvitingUID -> Email -> C.UTCTime -> m InvitationCode
createInvitation pid current email t =
  withProjectAuth pid current $ CreateInvitation pid current email t

findInvitation :: (MonadDB m) => InvitationCode -> m (Maybe Invitation)
findInvitation ic = liftdb $ FindInvitation ic

acceptInvitation :: (MonadDB m) => UserId -> C.UTCTime -> InvitationCode-> m ()
acceptInvitation uid t ic = do
  inv <- findInvitation ic
  let act = AcceptInvitation uid ic t
  case inv of
    Nothing ->
      raiseSubjectNotFound act
    Just i | t .-. (i ^. invitationTime) > fromSeconds (60 * 60 * 72 :: Int) ->
      raiseOpForbidden uid InvitationExpired act
    Just i | isJust (i ^. acceptanceTime) ->
      raiseOpForbidden uid InvitationAlreadyAccepted act
    Just i ->
      withProjectAuth (i ^. P.projectId) (i ^. P.invitingUser) act

-- Log ops

-- TODO: ignore "duplicate" events within some small time limit?
createEvent :: (MonadDB m) => ProjectId -> UserId -> LogEntry -> m EventId
createEvent p u l = withProjectAuth p u $ CreateEvent p u l

amendEvent :: (MonadDB m) => UserId -> EventId -> EventAmendment -> m AmendmentId
amendEvent uid eid a = do
  ev <- findEvent eid
  let act = AmendEvent eid a
      forbidden = raiseOpForbidden uid UserNotEventLogger act
      missing   = raiseSubjectNotFound act
  maybe missing (\(_, uid', _) -> if uid' == uid then liftdb act else forbidden) ev

findEvent :: (MonadDB m) => EventId -> m (Maybe KeyedLogEntry)
findEvent = liftdb . FindEvent

findEvents :: (MonadDB m) => ProjectId -> UserId -> Interval' -> m [LogEntry]
findEvents p u i = liftdb $ FindEvents p u i

readWorkIndex :: (MonadDB m) => ProjectId -> UserId -> m WorkIndex
readWorkIndex pid uid = withProjectAuth pid uid $ ReadWorkIndex pid

-- Billing ops

createBillable :: (MonadDB m) => UserId -> Billable -> m BillableId
createBillable uid b =
  withProjectAuth (b ^. B.project) uid $ CreateBillable uid b

findBillable :: (MonadDB m) => BillableId -> MaybeT m Billable
findBillable = MaybeT . liftdb . FindBillable

findSubscriptions :: (MonadDB m) => UserId -> ProjectId -> m [(SubscriptionId, Subscription)]
findSubscriptions uid pid = liftdb $ FindSubscriptions uid pid

findSubscriptionBillable :: (MonadDB m) => SubscriptionId -> MaybeT m (Subscription' UserId Billable)
findSubscriptionBillable sid = do
  sub <- MaybeT . liftdb $ FindSubscription sid
  traverse findBillable sub

findPaymentRequests :: (MonadDB m) => SubscriptionId -> m [(PaymentRequestId, PaymentRequest)]
findPaymentRequests = liftdb . FindPaymentRequests

findPaymentRequest :: (MonadDB m) => PaymentKey -> m (Maybe (PaymentRequestId, PaymentRequest))
findPaymentRequest = liftdb . FindPaymentRequest

-- this could be implemented in terms of other operations, but it's
-- much cleaner to just do the joins in the database
findUnpaidRequests :: (MonadDB m) => SubscriptionId -> m [BillDetail]
findUnpaidRequests = liftdb . FindUnpaidRequests

findPayment :: (MonadDB m) => PaymentRequestId -> m (Maybe Payment)
findPayment prid = (fmap snd . headMay) <$> liftdb (FindPayments prid)

-- Auction ops

createAuction :: (MonadDB m) => Auction -> m AuctionId
createAuction a = do
  withProjectAuth (a ^. A.projectId) (a ^. A.initiator) $ CreateAuction a

findAuction :: (MonadDB m) => AuctionId -> UserId -> m (Maybe Auction)
findAuction aid uid =
  let findOp = FindAuction aid
  in  do
    maybeAuc <- liftdb findOp
    _ <- traverse (\auc -> checkProjectAuth (auc ^. A.projectId) uid findOp) maybeAuc
    pure maybeAuc

findAuction' :: (MonadDB m) => AuctionId -> UserId -> m Auction
findAuction' aid uid =
  let findOp = FindAuction aid
  in  do
    maybeAuc <- liftdb findOp
    _ <- traverse (\auc -> checkProjectAuth (auc ^. A.projectId) uid findOp) maybeAuc
    maybe (raiseSubjectNotFound findOp) pure maybeAuc

createBid :: (MonadDB m) => AuctionId -> UserId -> Bid -> m BidId
createBid aid uid bid =
  let createOp = CreateBid aid bid
  in  do
    auc <- findAuction' aid uid
    if view bidTime bid > view auctionEnd auc
      then raiseOpForbidden uid AuctionEnded createOp
      else liftdb createOp
