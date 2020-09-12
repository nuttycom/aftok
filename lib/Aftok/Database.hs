{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Aftok.Database where


import           Control.Lens                   ( view
                                                , (^.)
                                                , makeClassyPrisms
                                                , traverseOf
                                                )
import           Data.AffineSpace
import           Data.Thyme.Clock              as C
import           Data.Thyme.Time               as T
                                                ( Day )
import           Safe                           ( headMay )

import           Aftok.Types
import           Aftok.Auction                 as A
import           Aftok.Billables               as B
import           Aftok.Currency.Bitcoin         ( NetworkId )
import           Aftok.Interval
import           Aftok.Payments.Types
import           Aftok.Project                 as P
import           Aftok.TimeLog
import           Aftok.Util

import           Haskoin.Address                ( Address )

type KeyedLogEntry a = (ProjectId, UserId, LogEntry a)
type InvitingUID = UserId
type InvitedUID = UserId

type BTCNet = (NetworkId, Address)
type BTCUser = User BTCNet

data DBOp a where
  CreateUser       :: BTCUser -> DBOp UserId
  FindUser         :: UserId -> DBOp (Maybe BTCUser)
  FindUserByName   :: UserName -> DBOp (Maybe (UserId, BTCUser))

  CreateProject    :: Project -> DBOp ProjectId
  FindProject      :: ProjectId -> DBOp (Maybe Project)
  ListProjects     :: DBOp [ProjectId]
  FindSubscribers  :: ProjectId -> DBOp [UserId]
  FindUserProjects :: UserId -> DBOp [(ProjectId, Project)]
  AddUserToProject :: ProjectId -> InvitingUID -> InvitedUID -> DBOp ()
  CreateInvitation :: ProjectId -> InvitingUID -> Email -> C.UTCTime -> DBOp InvitationCode
  FindInvitation   :: InvitationCode -> DBOp (Maybe Invitation)
  AcceptInvitation :: UserId -> InvitationCode -> C.UTCTime -> DBOp ()

  CreateEvent      :: ProjectId -> UserId -> LogEntry BTCNet -> DBOp EventId
  AmendEvent       :: EventId -> EventAmendment BTCNet -> DBOp AmendmentId
  FindEvent        :: EventId -> DBOp (Maybe (KeyedLogEntry BTCNet))
  FindEvents       :: ProjectId -> UserId -> RangeQuery -> Word32 -> DBOp [LogEntry BTCNet]
  ReadWorkIndex    :: ProjectId -> DBOp (WorkIndex BTCNet)

  CreateAuction    :: Auction -> DBOp AuctionId
  FindAuction      :: AuctionId -> DBOp (Maybe Auction)
  CreateBid        :: AuctionId -> Bid -> DBOp BidId
  FindBids         :: AuctionId -> DBOp [(BidId, Bid)]

  CreateBillable   :: UserId -> Billable -> DBOp BillableId
  FindBillable     :: BillableId -> DBOp (Maybe Billable)
  FindBillables    :: ProjectId  -> DBOp [(BillableId, Billable)]

  CreateSubscription :: UserId -> BillableId -> T.Day -> DBOp SubscriptionId
  FindSubscription   :: SubscriptionId -> DBOp (Maybe Subscription)
  FindSubscriptions  :: UserId -> ProjectId -> DBOp [(SubscriptionId, Subscription)]

  CreatePaymentRequest  :: PaymentRequest -> DBOp PaymentRequestId
  FindPaymentRequests   :: SubscriptionId -> DBOp [(PaymentRequestId, PaymentRequest)]
  FindUnpaidRequests    :: SubscriptionId -> DBOp [BillDetail]
  FindPaymentRequest    :: PaymentKey -> DBOp (Maybe (PaymentRequestId, PaymentRequest))
  FindPaymentRequestId  :: PaymentRequestId -> DBOp (Maybe PaymentRequest)

  CreatePayment  :: Payment -> DBOp PaymentId
  FindPayments   :: PaymentRequestId -> DBOp [(PaymentId, Payment)]

  RaiseDBError   :: forall x y. DBError -> DBOp x -> DBOp y

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
makeClassyPrisms ''DBError

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

createUser :: (MonadDB m) => BTCUser -> m UserId
createUser = liftdb . CreateUser

findUser :: (MonadDB m) => UserId -> MaybeT m BTCUser
findUser = MaybeT . liftdb . FindUser

findUserByName :: (MonadDB m) => UserName -> MaybeT m (UserId, BTCUser)
findUserByName = MaybeT . liftdb . FindUserByName

-- Project ops

createProject :: (MonadDB m) => Project -> m ProjectId
createProject p = do
  pid <- liftdb $ CreateProject p
  addUserToProject pid (p ^. P.initiator) (p ^. P.initiator)
  return pid

listProjects :: (MonadDB m) => m [ProjectId]
listProjects = liftdb ListProjects

findSubscribers :: (MonadDB m) => ProjectId -> m [UserId]
findSubscribers = liftdb . FindSubscribers

findProject :: (MonadDB m) => ProjectId -> MaybeT m Project
findProject = MaybeT . liftdb . FindProject

findProjectOrError :: (MonadDB m) => ProjectId -> m Project
findProjectOrError pid =
  fromMaybeT (raiseSubjectNotFound $ FindProject pid) (findProject pid)

findUserProject :: (MonadDB m) => UserId -> ProjectId -> MaybeT m Project
findUserProject uid pid = do
  kps <- lift $ findUserProjects uid
  MaybeT . pure $ fmap snd (find (\(pid', _) -> pid' == pid) kps)

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

addUserToProject
  :: (MonadDB m) => ProjectId -> InvitingUID -> InvitedUID -> m ()
addUserToProject pid current new =
  withProjectAuth pid current $ AddUserToProject pid current new

createInvitation
  :: (MonadDB m)
  => ProjectId
  -> InvitingUID
  -> Email
  -> C.UTCTime
  -> m InvitationCode
createInvitation pid current email t =
  withProjectAuth pid current $ CreateInvitation pid current email t

findInvitation :: (MonadDB m) => InvitationCode -> m (Maybe Invitation)
findInvitation ic = liftdb $ FindInvitation ic

acceptInvitation :: (MonadDB m) => UserId -> C.UTCTime -> InvitationCode -> m ()
acceptInvitation uid t ic = do
  inv <- findInvitation ic
  let act = AcceptInvitation uid ic t
  case inv of
    Nothing -> raiseSubjectNotFound act
    Just i | t .-. (i ^. invitationTime) > fromSeconds (60 * 60 * 72 :: Int) ->
      raiseOpForbidden uid InvitationExpired act
    Just i | isJust (i ^. acceptanceTime) ->
      raiseOpForbidden uid InvitationAlreadyAccepted act
    Just i -> withProjectAuth (i ^. P.projectId) (i ^. P.invitingUser) act

-- Log ops

-- TODO: ignore "duplicate" events within some small time limit?
createEvent
  :: (MonadDB m) => ProjectId -> UserId -> LogEntry BTCNet -> m EventId
createEvent p u l = withProjectAuth p u $ CreateEvent p u l

amendEvent
  :: (MonadDB m) => UserId -> EventId -> EventAmendment BTCNet -> m AmendmentId
amendEvent uid eid a = do
  ev <- findEvent eid
  let act       = AmendEvent eid a
      forbidden = raiseOpForbidden uid UserNotEventLogger act
      missing   = raiseSubjectNotFound act
  maybe missing
        (\(_, uid', _) -> if uid' == uid then liftdb act else forbidden)
        ev

findEvent :: (MonadDB m) => EventId -> m (Maybe (KeyedLogEntry BTCNet))
findEvent = liftdb . FindEvent

findEvents
  :: (MonadDB m)
  => ProjectId
  -> UserId
  -> RangeQuery
  -> Word32
  -> m [LogEntry BTCNet]
findEvents p u i l = liftdb $ FindEvents p u i l

readWorkIndex :: (MonadDB m) => ProjectId -> UserId -> m (WorkIndex BTCNet)
readWorkIndex pid uid = withProjectAuth pid uid $ ReadWorkIndex pid

-- Billing ops

createBillable :: (MonadDB m) => UserId -> Billable -> m BillableId
createBillable uid b =
  withProjectAuth (b ^. B.project) uid $ CreateBillable uid b

findBillable :: (MonadDB m) => BillableId -> MaybeT m Billable
findBillable = MaybeT . liftdb . FindBillable

findSubscriptions
  :: (MonadDB m) => UserId -> ProjectId -> m [(SubscriptionId, Subscription)]
findSubscriptions uid pid = liftdb $ FindSubscriptions uid pid

findSubscriptionBillable
  :: (MonadDB m) => SubscriptionId -> MaybeT m (Subscription' UserId Billable)
findSubscriptionBillable sid = do
  sub <- MaybeT . liftdb $ FindSubscription sid
  traverseOf B.billable findBillable sub

findPaymentRequests
  :: (MonadDB m) => SubscriptionId -> m [(PaymentRequestId, PaymentRequest)]
findPaymentRequests = liftdb . FindPaymentRequests

findPaymentRequest
  :: (MonadDB m) => PaymentKey -> MaybeT m (PaymentRequestId, PaymentRequest)
findPaymentRequest = MaybeT . liftdb . FindPaymentRequest

findPaymentRequestId
  :: (MonadDB m) => PaymentRequestId -> MaybeT m PaymentRequest
findPaymentRequestId = MaybeT . liftdb . FindPaymentRequestId

-- this could be implemented in terms of other operations, but it's
-- much cleaner to just do the joins in the database
findUnpaidRequests :: (MonadDB m) => SubscriptionId -> m [BillDetail]
findUnpaidRequests = liftdb . FindUnpaidRequests

findPayment :: (MonadDB m) => PaymentRequestId -> MaybeT m Payment
findPayment prid = MaybeT $ (fmap snd . headMay) <$> liftdb (FindPayments prid)

-- Auction ops

createAuction :: (MonadDB m) => Auction -> m AuctionId
createAuction a = do
  withProjectAuth (a ^. A.projectId) (a ^. A.initiator) $ CreateAuction a

findAuction :: (MonadDB m) => AuctionId -> UserId -> MaybeT m Auction
findAuction aid uid =
  let findOp = FindAuction aid
  in  do
        auc <- MaybeT $ liftdb findOp
        _   <- lift $ checkProjectAuth (auc ^. A.projectId) uid findOp
        pure auc

findAuction' :: (MonadDB m) => AuctionId -> UserId -> m Auction
findAuction' aid uid =
  let findOp = FindAuction aid
  in  do
        maybeAuc <- liftdb findOp
        _        <- traverse
          (\auc -> checkProjectAuth (auc ^. A.projectId) uid findOp)
          maybeAuc
        maybe (raiseSubjectNotFound findOp) pure maybeAuc

createBid :: (MonadDB m) => AuctionId -> UserId -> Bid -> m BidId
createBid aid uid bid =
  let createOp = CreateBid aid bid
  in  do
        auc <- findAuction' aid uid
        if view bidTime bid > view auctionEnd auc
          then raiseOpForbidden uid AuctionEnded createOp
          else liftdb createOp
