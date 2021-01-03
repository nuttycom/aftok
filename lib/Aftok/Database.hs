{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- brittany --disable-next-binding
module Aftok.Database where

import qualified Aftok.Auction as A
import Aftok.Billing as B
import Aftok.Currency (Amount, Currency)
import Aftok.Currency.Bitcoin.Payments (PaymentKey)
import qualified Aftok.Currency.Zcash as Zcash
import Aftok.Interval (RangeQuery)
import Aftok.Payments.Types
  ( Payment,
    PaymentId,
    PaymentRequest,
    PaymentRequestId,
    SomePaymentRequestDetail,
  )
import Aftok.Project as P
import Aftok.TimeLog
  ( AmendmentId,
    EventAmendment,
    EventId,
    LogEntry,
    WorkIndex,
  )
import Aftok.Types
  ( AccountId,
    Email,
    ProjectId,
    User,
    UserId,
    UserName,
  )
import Aftok.Util (Program, fc, fromMaybeT)
import Control.Lens
  ( (^.),
    makeClassyPrisms,
    traverseOf,
    view,
  )
import Data.AffineSpace ((.-.))
import Data.Thyme.Clock as C
import Data.Thyme.Time as T
  ( Day,
  )
import Safe (headMay)

type KeyedLogEntry = (ProjectId, UserId, LogEntry)

type InvitingUID = UserId

type InvitedUID = UserId

data Limit = Limit Word32

data DBOp a where
  CreateUser :: User -> DBOp UserId
  FindUser :: UserId -> DBOp (Maybe User)
  FindUserByName :: UserName -> DBOp (Maybe (UserId, User))
  FindUserPaymentAddress :: UserId -> Currency a c -> DBOp (Maybe a)
  FindAccountPaymentAddress :: AccountId -> Currency a c -> DBOp (Maybe a)
  FindAccountZcashIVK :: AccountId -> DBOp (Maybe Zcash.IVK)
  CreateProject :: Project -> DBOp ProjectId
  FindProject :: ProjectId -> DBOp (Maybe Project)
  ListProjects :: DBOp [ProjectId]
  FindUserProjects :: UserId -> DBOp [(ProjectId, Project)]
  AddUserToProject :: ProjectId -> InvitingUID -> InvitedUID -> DBOp ()
  ListProjectContributors :: ProjectId -> DBOp [(UserId, UserName, C.UTCTime)]
  CreateInvitation :: ProjectId -> InvitingUID -> Email -> C.UTCTime -> DBOp InvitationCode
  FindInvitation :: InvitationCode -> DBOp (Maybe Invitation)
  AcceptInvitation :: UserId -> InvitationCode -> C.UTCTime -> DBOp ()
  CreateEvent :: ProjectId -> UserId -> LogEntry -> DBOp EventId
  AmendEvent :: EventId -> EventAmendment -> DBOp AmendmentId
  FindEvent :: EventId -> DBOp (Maybe KeyedLogEntry)
  FindEvents :: ProjectId -> UserId -> RangeQuery -> Limit -> DBOp [LogEntry]
  ReadWorkIndex :: ProjectId -> DBOp WorkIndex
  ListAuctions :: ProjectId -> RangeQuery -> Limit -> DBOp [A.Auction]
  CreateAuction :: A.Auction -> DBOp A.AuctionId
  FindAuction :: A.AuctionId -> DBOp (Maybe A.Auction)
  CreateBid :: A.AuctionId -> A.Bid -> DBOp A.BidId
  FindBids :: A.AuctionId -> DBOp [(A.BidId, A.Bid)]
  CreateBillable :: UserId -> Billable Amount -> DBOp BillableId
  FindBillable :: BillableId -> DBOp (Maybe (Billable Amount))
  FindBillables :: ProjectId -> DBOp [(BillableId, Billable Amount)]
  CreateSubscription :: UserId -> BillableId -> T.Day -> DBOp SubscriptionId
  FindSubscription :: SubscriptionId -> DBOp (Maybe Subscription)
  FindSubscriptions :: ProjectId -> UserId -> DBOp [(SubscriptionId, Subscription)]
  FindSubscribers :: ProjectId -> DBOp [UserId]
  StorePaymentRequest :: PaymentRequest c -> DBOp PaymentRequestId
  FindPaymentRequestByKey :: PaymentKey -> DBOp (Maybe (PaymentRequestId, SomePaymentRequestDetail))
  FindPaymentRequestById :: PaymentRequestId -> DBOp (Maybe SomePaymentRequestDetail)
  FindSubscriptionPaymentRequests :: SubscriptionId -> DBOp [(PaymentRequestId, SomePaymentRequestDetail)]
  FindSubscriptionUnpaidRequests :: SubscriptionId -> DBOp [(PaymentRequestId, SomePaymentRequestDetail)]
  CreatePayment :: Payment c -> DBOp PaymentId
  FindPayments :: Currency a c -> PaymentRequestId -> DBOp [(PaymentId, Payment c)]
  RaiseDBError :: forall x y. DBError -> DBOp x -> DBOp y

data OpForbiddenReason
  = UserNotProjectMember
  | UserNotEventLogger
  | UserNotSubscriber SubscriptionId
  | InvitationExpired
  | InvitationAlreadyAccepted
  | AuctionEnded
  deriving (Eq, Show, Typeable)

data DBError
  = OpForbidden UserId OpForbiddenReason
  | SubjectNotFound
  | EventStorageFailed
  deriving (Eq, Show, Typeable)

makeClassyPrisms ''DBError

instance Exception DBError

class (Monad m) => MonadDB (m :: * -> *) where
  liftdb :: DBOp x -> m x

instance MonadDB (Program DBOp) where
  liftdb = fc

instance MonadDB m => MonadDB (ExceptT e m) where
  liftdb = lift . liftdb

raiseOpForbidden :: (MonadDB m) => UserId -> OpForbiddenReason -> DBOp x -> m x
raiseOpForbidden uid r op = liftdb $ RaiseDBError (OpForbidden uid r) op

raiseSubjectNotFound :: (MonadDB m) => DBOp y -> m x
raiseSubjectNotFound op = liftdb $ RaiseDBError SubjectNotFound op

-- User ops

createUser :: (MonadDB m) => User -> m UserId
createUser = liftdb . CreateUser

findUser :: (MonadDB m) => UserId -> MaybeT m User
findUser = MaybeT . liftdb . FindUser

findUserByName :: (MonadDB m) => UserName -> MaybeT m (UserId, User)
findUserByName = MaybeT . liftdb . FindUserByName

findUserPaymentAddress :: (MonadDB m) => UserId -> Currency a c -> MaybeT m a
findUserPaymentAddress uid n = MaybeT . liftdb $ FindUserPaymentAddress uid n

findAccountPaymentAddress :: (MonadDB m) => AccountId -> Currency a c -> MaybeT m a
findAccountPaymentAddress uid n = MaybeT . liftdb $ FindAccountPaymentAddress uid n

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

listProjectContributors :: MonadDB m => ProjectId -> UserId -> m [(UserId, UserName, C.UTCTime)]
listProjectContributors pid uid =
  withProjectAuth pid uid (ListProjectContributors pid)

addUserToProject ::
  (MonadDB m) => ProjectId -> InvitingUID -> InvitedUID -> m ()
addUserToProject pid current new =
  withProjectAuth pid current $ AddUserToProject pid current new

createInvitation ::
  (MonadDB m) =>
  ProjectId ->
  InvitingUID ->
  Email ->
  C.UTCTime ->
  m InvitationCode
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
    Just i
      | t .-. (i ^. invitationTime) > fromSeconds (60 * 60 * 72 :: Int) ->
        raiseOpForbidden uid InvitationExpired act
    Just i
      | isJust (i ^. acceptanceTime) ->
        raiseOpForbidden uid InvitationAlreadyAccepted act
    Just i -> withProjectAuth (i ^. P.projectId) (i ^. P.invitingUser) act

-- Log ops

-- TODO: ignore "duplicate" events within some small time limit?
createEvent ::
  (MonadDB m) => ProjectId -> UserId -> LogEntry -> m EventId
createEvent p u l = withProjectAuth p u $ CreateEvent p u l

amendEvent ::
  (MonadDB m) => UserId -> EventId -> EventAmendment -> m AmendmentId
amendEvent uid eid a = do
  ev <- findEvent eid
  let act = AmendEvent eid a
      forbidden = raiseOpForbidden uid UserNotEventLogger act
      missing = raiseSubjectNotFound act
  maybe
    missing
    (\(_, uid', _) -> if uid' == uid then liftdb act else forbidden)
    ev

findEvent :: (MonadDB m) => EventId -> m (Maybe KeyedLogEntry)
findEvent = liftdb . FindEvent

findEvents ::
  (MonadDB m) =>
  ProjectId ->
  UserId ->
  RangeQuery ->
  Limit ->
  m [LogEntry]
findEvents p u i l = liftdb $ FindEvents p u i l

readWorkIndex :: (MonadDB m) => ProjectId -> UserId -> m WorkIndex
readWorkIndex pid uid = withProjectAuth pid uid $ ReadWorkIndex pid

-- Billing ops

createBillable :: (MonadDB m) => UserId -> Billable Amount -> m BillableId
createBillable uid b =
  withProjectAuth (b ^. B.project) uid $ CreateBillable uid b

findBillable :: (MonadDB m) => BillableId -> MaybeT m (Billable Amount)
findBillable = MaybeT . liftdb . FindBillable

findSubscriptions ::
  (MonadDB m) => ProjectId -> UserId -> m [(SubscriptionId, Subscription)]
findSubscriptions pid uid = liftdb $ FindSubscriptions pid uid

findSubscriptionBillable ::
  (MonadDB m) => SubscriptionId -> MaybeT m (Subscription' UserId (Billable Amount))
findSubscriptionBillable sid = do
  sub <- MaybeT . liftdb $ FindSubscription sid
  traverseOf B.billable findBillable sub

storePaymentRequest ::
  (MonadDB m) => PaymentRequest c -> m PaymentRequestId
storePaymentRequest = liftdb . StorePaymentRequest

findPaymentRequestByKey ::
  (MonadDB m) => PaymentKey -> MaybeT m (PaymentRequestId, SomePaymentRequestDetail)
findPaymentRequestByKey = MaybeT . liftdb . FindPaymentRequestByKey

findPaymentRequestById ::
  (MonadDB m) => PaymentRequestId -> MaybeT m SomePaymentRequestDetail
findPaymentRequestById = MaybeT . liftdb . FindPaymentRequestById

findSubscriptionPaymentRequests ::
  (MonadDB m) => SubscriptionId -> m [(PaymentRequestId, SomePaymentRequestDetail)]
findSubscriptionPaymentRequests = liftdb . FindSubscriptionPaymentRequests

-- this could be implemented in terms of other operations, but it's
-- much cleaner to just do the joins in the database
findSubscriptionUnpaidRequests :: (MonadDB m) => SubscriptionId -> m [(PaymentRequestId, SomePaymentRequestDetail)]
findSubscriptionUnpaidRequests = liftdb . FindSubscriptionUnpaidRequests

findPayment :: (MonadDB m) => Currency a c -> PaymentRequestId -> MaybeT m (Payment c)
findPayment currency prid = MaybeT $ (fmap snd . headMay) <$> liftdb (FindPayments currency prid)

-- Auction ops

createAuction :: (MonadDB m) => A.Auction -> m A.AuctionId
createAuction a = do
  withProjectAuth (a ^. A.projectId) (a ^. A.initiator) $ CreateAuction a

listAuctions :: (MonadDB m) =>  UserId -> ProjectId -> RangeQuery -> Limit -> m [A.Auction]
listAuctions uid pid rq l = do
  withProjectAuth pid uid $ ListAuctions pid rq l

findAuction :: (MonadDB m) => A.AuctionId -> UserId -> MaybeT m A.Auction
findAuction aid uid =
  let findOp = FindAuction aid
   in do
        auc <- MaybeT $ liftdb findOp
        _ <- lift $ checkProjectAuth (auc ^. A.projectId) uid findOp
        pure auc

findAuction' :: (MonadDB m) => A.AuctionId -> UserId -> m A.Auction
findAuction' aid uid =
  let findOp = FindAuction aid
   in do
        maybeAuc <- liftdb findOp
        _ <-
          traverse
            (\auc -> checkProjectAuth (auc ^. A.projectId) uid findOp)
            maybeAuc
        maybe (raiseSubjectNotFound findOp) pure maybeAuc

createBid :: (MonadDB m) => A.AuctionId -> UserId -> A.Bid -> m A.BidId
createBid aid uid bid =
  let createOp = CreateBid aid bid
   in do
        auc <- findAuction' aid uid
        if view A.bidTime bid > view A.auctionEnd auc
          then raiseOpForbidden uid AuctionEnded createOp
          else liftdb createOp
