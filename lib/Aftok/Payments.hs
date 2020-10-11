{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Aftok.Payments
  ( module Aftok.Payments
  , module Aftok.Payments.Types
  )
where



import           Control.Error.Util             ( maybeT )
import           Control.Lens                   ( makeClassy
                                                , makeClassyPrisms
                                                , review
                                                , view
                                                , (%~)
                                                , (^.)
                                                , traverseOf
                                                )
import           Control.Lens.Tuple
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                )
import qualified Crypto.PubKey.RSA.Types       as RSA
                                                ( Error(..)
                                                , PrivateKey
                                                )
import           Crypto.Random.Types            ( MonadRandom
                                                , getRandomBytes
                                                )

import           Data.AffineSpace               ( (.+^) )
import           Data.Map.Strict                ( assocs )
import           Data.Thyme.Clock              as C
import           Data.Thyme.Time               as T

import qualified Bippy                         as B
import qualified Bippy.Proto                   as P
import qualified Bippy.Types                   as BT
import           Haskoin.Address                ( Address(..) )
import           Haskoin.Address.Base58         ( encodeBase58Check )
import           Haskoin.Script                 ( ScriptOutput(..) )
import           Network.URI

import           Aftok.Types                    ( UserId
                                                , ProjectId
                                                )
import           Aftok.Billing
import           Aftok.Currency.Bitcoin         ( NetworkId(..)
                                                , NetworkMode
                                                , satoshi
                                                , toNetwork
                                                )
import           Aftok.Database
import           Aftok.Payments.Types
import           Aftok.Project                  ( depf )
import qualified Aftok.TimeLog                 as TL

data PaymentsConfig = PaymentsConfig
  { _networkMode  :: !NetworkMode
  , _signingKey   :: !RSA.PrivateKey
  , _pkiData      :: !BT.PKIData
  }
makeClassy ''PaymentsConfig

data BillingOps (m :: * -> *) = BillingOps
  { -- | generator for user memo
    memoGen    :: Subscription' UserId Billable -- ^ subscription being billed
               -> T.Day                         -- ^ billing date
               -> C.UTCTime                     -- ^ payment request generation time
               -> m (Maybe Text)
    -- | generator for payment response URL
  , uriGen     :: PaymentKey                    -- ^ payment key to be included in the URL
               -> m (Maybe URI)
    -- | generator for merchant payload
  , payloadGen :: Subscription' UserId Billable -- ^ subscription being billed
               -> T.Day                         -- ^ billing date
               -> C.UTCTime                     -- ^ payment request generation time
               -> m (Maybe ByteString)
  }

data PaymentRequestStatus
  = Paid !Payment           -- ^ the request was paid with the specified payment
  | Unpaid !PaymentRequest  -- ^ the request has not been paid, but has not yet expired
  | Expired !PaymentRequest -- ^ the request was not paid prior to the expiration date

data PaymentError
  = Overdue !SubscriptionId
  | SigningError !RSA.Error
  | IllegalAddress !Address
makeClassyPrisms ''PaymentError

{--
 - Find all the subscriptions for the specified customer, and
 - determine which if any are up for renewal. Create a payment
 - request for each such subscription.
 --}
createPaymentRequests
  :: ( MonadRandom m
     , MonadReader r m
     , HasPaymentsConfig r
     , MonadError e m
     , AsPaymentError e
     , MonadDB m
     )
  => BillingOps m -- ^ generators for payment request components
  -> C.UTCTime    -- ^ timestamp for payment request creation
  -> UserId       -- ^ customer responsible for payment
  -> ProjectId    -- ^ project whose worklog is to be paid
  -> m [PaymentRequestId]
createPaymentRequests ops now custId pid = do
  subscriptions <- findSubscriptions custId pid
  join <$> traverse (createSubscriptionPaymentRequests ops now) subscriptions

createSubscriptionPaymentRequests
  :: ( MonadRandom m
     , MonadReader r m
     , HasPaymentsConfig r
     , MonadError e m
     , AsPaymentError e
     , MonadDB m
     )
  => BillingOps m
  -> C.UTCTime
  -> (SubscriptionId, Subscription)
  -> m [PaymentRequestId]
createSubscriptionPaymentRequests ops now (sid, sub) = do
  billableSub <-
    maybeT (raiseSubjectNotFound . FindBillable $ sub ^. billable) pure
      $ traverseOf billable findBillable sub
  paymentRequests <- findPaymentRequests sid
  billableDates   <-
    findUnbilledDates now (view billable billableSub) paymentRequests
    $ takeWhile (< view _utctDay now)
    $ billingSchedule billableSub
  traverse (createPaymentRequest ops now sid billableSub) billableDates

createPaymentRequest
  :: ( MonadRandom m
     , MonadReader r m
     , HasPaymentsConfig r
     , MonadError e m
     , AsPaymentError e
     , MonadDB m
     )
  => BillingOps m
  -> C.UTCTime
  -> SubscriptionId
  -> Subscription' UserId Billable
  -> T.Day
  -> m PaymentRequestId
createPaymentRequest ops now sid sub bday = do
  cfg     <- ask
  -- TODO: maybe make pkey a function of subscription, billable, bday
  pkey    <- PaymentKey . encodeBase58Check <$> getRandomBytes 32
  memo    <- memoGen ops sub bday now
  uri     <- uriGen ops pkey
  payload <- payloadGen ops sub bday now
  details <- createPaymentDetails bday now memo uri payload (sub ^. billable)
  reqErr  <- B.createPaymentRequest (cfg ^. signingKey) (cfg ^. pkiData) details
  req     <- either (throwError . review _SigningError) pure reqErr
  liftdb $ CreatePaymentRequest (PaymentRequest sid req pkey now bday)

{-
 - FIXME: The current implementation expects the billing day to be a suitable
 - key for comparison to payment requests. This is almost certainly inadequate.
 -}
findUnbilledDates
  :: (MonadDB m, MonadError e m, AsPaymentError e)
  => C.UTCTime -- ^ the date against which payment request expiration should be checked
  -> Billable
  -> [(PaymentRequestId, PaymentRequest)] -- ^ the list of existing payment requests
  -> [T.Day]   -- ^ the list of expected billing days
  -> m [T.Day] -- ^ the list of billing days for which no payment request exists
findUnbilledDates now b (px@(p : ps)) (dx@(d : ds)) =
  case compare (view (_2 . billingDate) p) d of
    EQ -> getRequestStatus now p >>= \s -> case s of
      Expired r ->
        if view _utctDay now > addDays (view gracePeriod b) (view billingDate r)
          then throwError (review _Overdue (r ^. subscription))
          else fmap (d :) $ findUnbilledDates now b px dx -- d will be rebilled
      _ -> findUnbilledDates now b ps ds -- if paid or unpaid, nothing to do
    GT -> fmap (d :) $ findUnbilledDates now b px ds
    LT -> findUnbilledDates now b ps dx
findUnbilledDates _ _ _ ds = pure ds


{- Check whether the specified payment request has a payment associated with
 - it, and return a PaymentRequestStatus value indicating the result.
 -}
getRequestStatus
  :: (MonadDB m)
  => C.UTCTime -- ^ the date against which request expiration should be checked
  -> (PaymentRequestId, PaymentRequest) -- ^ the request for which to find a payment
  -> m PaymentRequestStatus
getRequestStatus now (reqid, req) =
  let ifUnpaid = (if isExpired now req then Expired else Unpaid) req
  in  maybe ifUnpaid Paid <$> runMaybeT (findPayment reqid)

{- Create the PaymentDetails section of the payment request.
 -}
createPaymentDetails
  :: ( MonadRandom m
     , MonadReader r m
     , HasPaymentsConfig r
     , MonadError e m
     , AsPaymentError e
     , MonadDB m
     )
  => T.Day              -- ^ payout date (billing date)
  -> C.UTCTime          -- ^ timestamp of payment request creation
  -> Maybe Text         -- ^ user memo
  -> Maybe URI          -- ^ payment response URL
  -> Maybe ByteString   -- ^ merchant payload
  -> Billable           -- ^ billing information
  -> m P.PaymentDetails
createPaymentDetails payoutDate billingTime memo uri payload b = do
  payouts <- getProjectPayouts payoutTime (b ^. project)
  outputs <- createPayoutsOutputs payoutTime (b ^. amount) payouts
  let expiry =
        (BT.Expiry . T.fromThyme . (billingTime .+^))
          <$> (b ^. requestExpiryPeriod)
  cfg <- ask
  pure $ B.createPaymentDetails (toNetwork (cfg ^. networkMode) BTC)
                                outputs
                                (T.fromThyme billingTime)
                                expiry
                                memo
                                uri
                                payload
  where payoutTime = T.mkUTCTime payoutDate (fromInteger 0)


getProjectPayouts
  :: (MonadDB m, MonadError e m, AsPaymentError e)
  => C.UTCTime
  -> ProjectId
  -> m (TL.Payouts (NetworkId, Address))
getProjectPayouts ptime pid = do
  project' <-
    let projectOp = FindProject pid
    in  maybe (raiseSubjectNotFound projectOp) pure =<< liftdb projectOp

  widx <- liftdb $ ReadWorkIndex pid
  pure $ TL.payouts (TL.toDepF $ project' ^. depf) ptime widx


createPayoutsOutputs
  :: (MonadDB m, MonadError e m, AsPaymentError e)
  => C.UTCTime
  -> BT.Satoshi
  -> TL.Payouts (NetworkId, Address)
  -> m [BT.Output]
createPayoutsOutputs t amt p =
  let payoutFractions :: [(TL.CreditTo (NetworkId, Address), BT.Satoshi)]
      payoutFractions = (_2 %~ outputAmount amt) <$> assocs (p ^. TL._Payouts)
  in  join <$> traverse (uncurry (createOutputs t)) payoutFractions


createOutputs
  :: (MonadDB m, MonadError e m, AsPaymentError e)
  => C.UTCTime
  -> TL.CreditTo (NetworkId, Address)
  -> BT.Satoshi
  -> m [BT.Output]
createOutputs _ (TL.CreditToCurrency (BTC, (PubKeyAddress addr))) amt =
  pure $ [BT.Output amt (PayPKHash addr)]

createOutputs _ (TL.CreditToCurrency (_, other)) _ =
  throwError $ review _IllegalAddress other

createOutputs _ (TL.CreditToUser uid) amt = (fmap maybeToList) . runMaybeT $ do
  (_, addr) <- findUserPaymentAddress uid
  case addr of
    PubKeyAddress a -> pure $ BT.Output amt (PayPKHash a)
    other           -> throwError $ review _IllegalAddress other

createOutputs t (TL.CreditToProject pid) amt = do
  payouts <- getProjectPayouts t pid
  createPayoutsOutputs t amt payouts


outputAmount :: BT.Satoshi -> Rational -> BT.Satoshi
outputAmount i r = BT.Satoshi . round $ toRational (i ^. satoshi) * r

findPayableRequests
  :: (MonadDB m) => UserId -> SubscriptionId -> C.UTCTime -> m [BillDetail]
findPayableRequests uid sid now = do
  requests <- liftdb findOp
  join
    <$> (traverse checkAccess $ filter (not . isExpired now . view _2) requests)
 where
  findOp = FindUnpaidRequests sid
  checkAccess d = if view (_3 . customer) d == uid
    then pure [d]
    else raiseOpForbidden uid (UserNotSubscriber sid) findOp
