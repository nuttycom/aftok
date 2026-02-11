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
    HasLogEntry,
    LogEntry,
    WorkIndex,
  )
import qualified Aftok.TimeLog as TL
import Aftok.Password (PasswordHash)
import Aftok.GitHub
  ( GitHubRepoLink,
    GitHubWebhookEvent,
  )
import Aftok.Types
  ( AccountId,
    Email,
    GitHubRepoLinkId,
    GitHubUsername,
    GitHubWebhookEventId,
    PasswordResetToken,
    PasswordResetTokenId,
    ProjectId,
    User,
    UserId,
    UserName,
  )
import Aftok.Util (Program, fc, fromMaybeT)
import Control.Lens
  ( makeClassy,
    makeClassyPrisms,
    traverseOf,
    view,
    (^.),
  )
import Data.AffineSpace ((.-.))
import Data.Thyme.Clock as C
import Data.Thyme.Time as T
  ( Day,
  )
import Safe (headMay)

data KeyedLogEntry = KeyedLogEntry
  { _workId :: !EventId,
    _logEntry :: !LogEntry
  }

makeClassy ''KeyedLogEntry

instance HasLogEntry KeyedLogEntry where
  logEntry = Aftok.Database.logEntry

type InvitingUID = UserId

type InvitedUID = UserId

data Limit = Limit Word32

data DBOp a where
  CreateUser :: User -> DBOp UserId
  CreateUserWithPassword :: User -> PasswordHash -> DBOp UserId
  FindUser :: UserId -> DBOp (Maybe User)
  FindUserProjectDetail :: UserId -> ProjectId -> DBOp (Maybe (User, C.UTCTime))
  FindUserByName :: UserName -> DBOp (Maybe (UserId, User))
  FindUserByNameWithPassword :: UserName -> DBOp (Maybe (UserId, User, Maybe PasswordHash))
  FindUserPaymentAddress :: UserId -> Currency a c -> DBOp (Maybe (AccountId, a))
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
  AmendEvent :: ProjectId -> UserId -> KeyedLogEntry -> EventAmendment -> DBOp (EventId, AmendmentId)
  FindEvent :: EventId -> DBOp (Maybe (ProjectId, UserId, KeyedLogEntry))
  FindEvents :: ProjectId -> UserId -> RangeQuery -> Limit -> DBOp [KeyedLogEntry]
  ReadWorkIndex :: ProjectId -> DBOp (WorkIndex KeyedLogEntry)
  ListAuctions :: ProjectId -> RangeQuery -> Limit -> DBOp [A.Auction Amount]
  CreateAuction :: A.Auction Amount -> DBOp A.AuctionId
  FindAuction :: A.AuctionId -> DBOp (Maybe (A.Auction Amount))
  CreateBid :: A.AuctionId -> A.Bid Amount -> DBOp A.BidId
  FindBids :: A.AuctionId -> DBOp [(A.BidId, A.Bid Amount)]
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
  -- Password reset operations
  CreatePasswordResetToken :: UserId -> Text -> C.UTCTime -> DBOp PasswordResetTokenId
  FindPasswordResetToken :: Text -> DBOp (Maybe (PasswordResetTokenId, PasswordResetToken))
  MarkPasswordResetTokenUsed :: PasswordResetTokenId -> C.UTCTime -> DBOp ()
  FindUserByEmail :: Email -> DBOp (Maybe (UserId, User))
  UpdateUserPassword :: UserId -> PasswordHash -> DBOp ()
  -- Zcash address operations
  SetUserZcashAddress :: UserId -> Zcash.Address -> DBOp ()
  FindUserZcashAddress :: UserId -> DBOp (Maybe Zcash.Address)
  -- GitHub integration operations
  FindUserByGitHubUsername :: GitHubUsername -> DBOp (Maybe (UserId, User))
  LinkGitHubUsername :: UserId -> GitHubUsername -> DBOp ()
  UnlinkGitHubUsername :: UserId -> DBOp ()
  GetUserGitHubUsername :: UserId -> DBOp (Maybe GitHubUsername)
  CreateGitHubRepoLink :: GitHubRepoLink -> DBOp GitHubRepoLinkId
  FindGitHubRepoLink :: Text -> Text -> DBOp (Maybe (GitHubRepoLinkId, GitHubRepoLink))
  FindProjectGitHubRepoLinks :: ProjectId -> DBOp [(GitHubRepoLinkId, GitHubRepoLink)]
  DeleteGitHubRepoLink :: GitHubRepoLinkId -> DBOp ()
  RecordWebhookEvent :: GitHubRepoLinkId -> GitHubWebhookEvent -> DBOp GitHubWebhookEventId
  IsDeliveryProcessed :: Text -> DBOp Bool
  RaiseDBError :: forall x y. DBError -> DBOp x -> DBOp y

data InvitationError
  = InvitationExpired
  | InvitationAlreadyAccepted
  | InvitationNotFound
  deriving (Eq, Show)

data OpForbiddenReason
  = UserNotProjectMember
  | UserNotEventLogger
  | UserNotSubscriber SubscriptionId
  | InvitationError InvitationError
  | AuctionEnded
  deriving (Eq, Show, Typeable)

data DBError
  = OpForbidden UserId OpForbiddenReason
  | SubjectNotFound
  | EventStorageFailed
  | DuplicateRecord String
  deriving (Eq, Show, Typeable)

makeClassyPrisms ''DBError

instance Exception DBError

class (Monad m) => MonadDB (m :: Type -> Type) where
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

createUserWithPassword :: (MonadDB m) => User -> PasswordHash -> m UserId
createUserWithPassword user pwd = liftdb $ CreateUserWithPassword user pwd

findUser :: (MonadDB m) => UserId -> MaybeT m User
findUser = MaybeT . liftdb . FindUser

findUserProjectDetail :: (MonadDB m) => UserId -> ProjectId -> MaybeT m (User, C.UTCTime)
findUserProjectDetail uid pid = MaybeT . liftdb $ FindUserProjectDetail uid pid

findUserByName :: (MonadDB m) => UserName -> MaybeT m (UserId, User)
findUserByName = MaybeT . liftdb . FindUserByName

findUserByNameWithPassword :: (MonadDB m) => UserName -> MaybeT m (UserId, User, Maybe PasswordHash)
findUserByNameWithPassword = MaybeT . liftdb . FindUserByNameWithPassword

findUserPaymentAddress :: (MonadDB m) => UserId -> Currency a c -> MaybeT m (AccountId, a)
findUserPaymentAddress uid n = MaybeT . liftdb $ FindUserPaymentAddress uid n

findAccountPaymentAddress :: (MonadDB m) => AccountId -> Currency a c -> MaybeT m (AccountId, a)
findAccountPaymentAddress aid n = fmap (aid,) . MaybeT . liftdb $ FindAccountPaymentAddress aid n

findUserByEmail :: (MonadDB m) => Email -> MaybeT m (UserId, User)
findUserByEmail = MaybeT . liftdb . FindUserByEmail

updateUserPassword :: (MonadDB m) => UserId -> PasswordHash -> m ()
updateUserPassword uid pwd = liftdb $ UpdateUserPassword uid pwd

-- Zcash address ops

setUserZcashAddress :: (MonadDB m) => UserId -> Zcash.Address -> m ()
setUserZcashAddress uid addr = liftdb $ SetUserZcashAddress uid addr

findUserZcashAddress :: (MonadDB m) => UserId -> MaybeT m Zcash.Address
findUserZcashAddress = MaybeT . liftdb . FindUserZcashAddress

-- Password reset ops

createPasswordResetToken :: (MonadDB m) => UserId -> Text -> C.UTCTime -> m PasswordResetTokenId
createPasswordResetToken uid token expiresAt = liftdb $ CreatePasswordResetToken uid token expiresAt

findPasswordResetToken :: (MonadDB m) => Text -> MaybeT m (PasswordResetTokenId, PasswordResetToken)
findPasswordResetToken = MaybeT . liftdb . FindPasswordResetToken

markPasswordResetTokenUsed :: (MonadDB m) => PasswordResetTokenId -> C.UTCTime -> m ()
markPasswordResetTokenUsed tokenId usedAt = liftdb $ MarkPasswordResetTokenUsed tokenId usedAt

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

findCurrentInvitation :: (MonadDB m) => C.UTCTime -> InvitationCode -> m (Either InvitationError Invitation)
findCurrentInvitation t ic =
  maybe (Left InvitationNotFound) checkInvitation <$> liftdb (FindInvitation ic)
  where
    checkInvitation i
      | t .-. (i ^. invitationTime) > fromSeconds (60 * 60 * 72 :: Int) = Left InvitationExpired
      | isJust (i ^. acceptanceTime) = Left InvitationAlreadyAccepted
      | otherwise = Right i

acceptInvitation :: (MonadDB m) => UserId -> C.UTCTime -> InvitationCode -> m ()
acceptInvitation uid t ic = do
  inv <- findCurrentInvitation t ic
  let act = AcceptInvitation uid ic t
  case inv of
    Left InvitationNotFound -> raiseSubjectNotFound act
    Left InvitationExpired -> raiseOpForbidden uid (InvitationError InvitationExpired) act
    Left InvitationAlreadyAccepted -> raiseOpForbidden uid (InvitationError InvitationAlreadyAccepted) act
    Right i -> withProjectAuth (i ^. P.projectId) (i ^. P.invitingUser) act

-- Log ops

-- TODO: ignore "duplicate" events within some small time limit?
createEvent ::
  (MonadDB m) => ProjectId -> UserId -> LogEntry -> m EventId
createEvent p u l = withProjectAuth p u $ CreateEvent p u l

amendEvent ::
  (MonadDB m) => UserId -> EventId -> EventAmendment -> m (EventId, AmendmentId)
amendEvent uid eid a = do
  evMay <- findEvent eid
  maybe missing saveAmendment evMay
  where
    missing = raiseSubjectNotFound (FindEvent eid)
    saveAmendment (pid, uid', le) =
      let act = AmendEvent pid uid le a
       in if uid' == uid
            then liftdb act
            else raiseOpForbidden uid UserNotEventLogger act

findEvent :: (MonadDB m) => EventId -> m (Maybe (ProjectId, UserId, KeyedLogEntry))
findEvent = liftdb . FindEvent

findEvents ::
  (MonadDB m) =>
  ProjectId ->
  UserId ->
  RangeQuery ->
  Limit ->
  m [KeyedLogEntry]
findEvents p u i l = liftdb $ FindEvents p u i l

readWorkIndex :: (MonadDB m) => ProjectId -> UserId -> m (WorkIndex KeyedLogEntry)
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

-- GitHub integration ops

findUserByGitHubUsername :: (MonadDB m) => GitHubUsername -> MaybeT m (UserId, User)
findUserByGitHubUsername = MaybeT . liftdb . FindUserByGitHubUsername

linkGitHubUsername :: (MonadDB m) => UserId -> GitHubUsername -> m ()
linkGitHubUsername uid ghUser = liftdb $ LinkGitHubUsername uid ghUser

unlinkGitHubUsername :: (MonadDB m) => UserId -> m ()
unlinkGitHubUsername uid = liftdb $ UnlinkGitHubUsername uid

getUserGitHubUsername :: (MonadDB m) => UserId -> m (Maybe GitHubUsername)
getUserGitHubUsername = liftdb . GetUserGitHubUsername

createGitHubRepoLink :: (MonadDB m) => GitHubRepoLink -> m GitHubRepoLinkId
createGitHubRepoLink = liftdb . CreateGitHubRepoLink

findGitHubRepoLink :: (MonadDB m) => Text -> Text -> MaybeT m (GitHubRepoLinkId, GitHubRepoLink)
findGitHubRepoLink owner repo = MaybeT . liftdb $ FindGitHubRepoLink owner repo

findProjectGitHubRepoLinks :: (MonadDB m) => ProjectId -> m [(GitHubRepoLinkId, GitHubRepoLink)]
findProjectGitHubRepoLinks = liftdb . FindProjectGitHubRepoLinks

deleteGitHubRepoLink :: (MonadDB m) => GitHubRepoLinkId -> m ()
deleteGitHubRepoLink = liftdb . DeleteGitHubRepoLink

recordWebhookEvent :: (MonadDB m) => GitHubRepoLinkId -> GitHubWebhookEvent -> m GitHubWebhookEventId
recordWebhookEvent linkId ev = liftdb $ RecordWebhookEvent linkId ev

isDeliveryProcessed :: (MonadDB m) => Text -> m Bool
isDeliveryProcessed = liftdb . IsDeliveryProcessed

-- Auction ops

createAuction :: (MonadDB m) => A.Auction Amount -> m A.AuctionId
createAuction a = do
  withProjectAuth (a ^. A.projectId) (a ^. A.initiator) $ CreateAuction a

listAuctions :: (MonadDB m) => UserId -> ProjectId -> RangeQuery -> Limit -> m [A.Auction Amount]
listAuctions uid pid rq l = do
  withProjectAuth pid uid $ ListAuctions pid rq l

findAuction :: (MonadDB m) => A.AuctionId -> UserId -> MaybeT m (A.Auction Amount)
findAuction aid uid =
  let findOp = FindAuction aid
   in do
        auc <- MaybeT $ liftdb findOp
        _ <- lift $ checkProjectAuth (auc ^. A.projectId) uid findOp
        pure auc

findAuction' :: (MonadDB m) => A.AuctionId -> UserId -> m (A.Auction Amount)
findAuction' aid uid =
  let findOp = FindAuction aid
   in do
        maybeAuc <- liftdb findOp
        _ <-
          traverse
            (\auc -> checkProjectAuth (auc ^. A.projectId) uid findOp)
            maybeAuc
        maybe (raiseSubjectNotFound findOp) pure maybeAuc

createBid :: (MonadDB m) => A.AuctionId -> UserId -> A.Bid Amount -> m A.BidId
createBid aid uid bid =
  let createOp = CreateBid aid bid
   in do
        auc <- findAuction' aid uid
        if view A.bidTime bid > view A.auctionEnd auc
          then raiseOpForbidden uid AuctionEnded createOp
          else liftdb createOp
