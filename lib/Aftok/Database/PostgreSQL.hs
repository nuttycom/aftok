{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Aftok.Database.PostgreSQL
  ( QDBM (..),
    runQDBM,
  )
where

import qualified Aftok.Currency.Bitcoin as Bitcoin
import Aftok.Database
import qualified Aftok.Database.PostgreSQL.Auctions as Q
import qualified Aftok.Database.PostgreSQL.Billing as Q
import qualified Aftok.Database.PostgreSQL.Events as Q
import qualified Aftok.Database.PostgreSQL.Projects as Q
import qualified Aftok.Database.PostgreSQL.Users as Q
import Control.Monad.Trans.Except (throwE)
import Crypto.Random.Types
  ( MonadRandom,
    getRandomBytes,
  )
import Database.PostgreSQL.Simple
import Prelude hiding (null)

newtype QDBM a = QDBM (ReaderT (Bitcoin.NetworkMode, Connection) (ExceptT DBError IO) a)
  deriving (Functor, Applicative, Monad)

instance MonadIO QDBM where
  liftIO = QDBM . lift . lift

instance MonadRandom QDBM where
  getRandomBytes = QDBM . lift . lift . getRandomBytes

instance MonadDB QDBM where
  liftdb = pgEval

runQDBM :: Bitcoin.NetworkMode -> Connection -> QDBM a -> ExceptT DBError IO a
runQDBM mode conn (QDBM r) = runReaderT r (mode, conn)

pgEval :: DBOp a -> QDBM a
pgEval =
  QDBM . \case
    (CreateEvent pid uid lentry) -> Q.createEvent pid uid lentry
    (FindEvent eid) -> Q.findEvent eid
    (FindEvents pid uid rquery limit) -> Q.findEvents pid uid rquery limit
    (AmendEvent pid uid kle amendment) -> Q.amendEvent pid uid kle amendment
    (ReadWorkIndex pid) -> Q.readWorkIndex pid
    (CreateAuction auc) -> Q.createAuction auc
    (FindAuction aucId) -> Q.findAuction aucId
    (ListAuctions pid rq l) -> Q.listAuctions pid rq l
    (CreateBid aucId bid) -> Q.createBid aucId bid
    (FindBids aucId) -> Q.findBids aucId
    (CreateUser user') -> Q.createUser user'
    (FindUser uid) -> Q.findUser uid
    (FindUserByName n) -> Q.findUserByName n
    (FindUserPaymentAddress uid currency) -> Q.findUserPaymentAddress uid currency
    (FindAccountPaymentAddress aid currency) -> Q.findAccountPaymentAddress aid currency
    (FindAccountZcashIVK aid) -> Q.findAccountZcashIVK aid
    (CreateProject p) -> Q.createProject p
    ListProjects -> Q.listProjects
    (ListProjectContributors pid) -> Q.listProjectContributors pid
    (FindProject pid) -> Q.findProject pid
    (FindUserProjects uid) -> Q.findUserProjects uid
    (AddUserToProject pid current new) -> Q.addUserToProject pid current new
    (CreateInvitation pid uid e t) -> Q.createInvitation pid uid e t
    (FindInvitation ic) -> Q.findInvitation ic
    (AcceptInvitation uid ic t) -> Q.acceptInvitation uid ic t
    dbop@(CreateBillable uid b) -> do
      eventId <- Q.storeEvent' dbop
      Q.createBillable eventId uid b
    (FindBillable bid) -> Q.findBillable bid
    (FindBillables pid) -> Q.findBillables pid
    dbop@(CreateSubscription uid bid start_date) -> do
      eventId <- Q.storeEvent' dbop
      Q.createSubscription eventId uid bid start_date
    (FindSubscription sid) -> Q.findSubscription sid
    (FindSubscriptions uid pid) -> Q.findSubscriptions uid pid
    (FindSubscribers pid) -> Q.findSubscribers pid
    dbop@(StorePaymentRequest req) -> do
      eventId <- Q.storeEvent' dbop
      Q.storePaymentRequest eventId Nothing req
    (FindPaymentRequestByKey k) -> Q.findPaymentRequestByKey k
    (FindPaymentRequestById prid) -> Q.findPaymentRequestById prid
    (FindSubscriptionPaymentRequests sid) -> Q.findSubscriptionPaymentRequests sid
    (FindSubscriptionUnpaidRequests sid) -> Q.findSubscriptionUnpaidRequests sid
    dbop@(CreatePayment p) -> do
      eventId <- Q.storeEvent' dbop
      Q.createPayment eventId p
    (FindPayments ccy rid) -> Q.findPayments ccy rid
    (RaiseDBError err _) -> lift . throwE $ err
