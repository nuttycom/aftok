{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Aftok.Payments
  ( module Aftok.Payments,
    module Aftok.Payments.Types,
  )
where

import Aftok.Billing
  ( Billable,
    BillableId,
    Subscription,
    SubscriptionId,
    amount,
  )
import qualified Aftok.Billing as B
import Aftok.Currency (Amount (..), Currency (..), Currency' (..))
import Aftok.Database
  ( DBOp
      ( FindBillable,
        FindSubscription,
        StorePaymentRequest
      ),
    MonadDB,
    OpForbiddenReason (UserNotSubscriber),
    findBillable,
    findPayment,
    findSubscriptionPaymentRequests,
    findSubscriptionUnpaidRequests,
    findSubscriptions,
    liftdb,
    raiseOpForbidden,
    raiseSubjectNotFound,
  )
import qualified Aftok.Payments.Bitcoin as BTC
import Aftok.Payments.Types
  ( NativeRequest (..),
    Payment,
    PaymentOps (..),
    PaymentRequest,
    PaymentRequest' (..),
    PaymentRequestDetail,
    PaymentRequestId,
    SomePaymentRequest (..),
    billingDate,
    isExpired,
    paymentRequestCurrency,
  )
import qualified Aftok.Payments.Types as PT
import qualified Aftok.Payments.Zcash as Zcash
import Aftok.Types
  ( ProjectId,
    UserId,
  )
import Control.Error.Util (maybeT)
import Control.Lens
  ( (^.),
    makeClassyPrisms,
    makeLenses,
    over,
    review,
    traverseOf,
  )
import Control.Monad.Except
  ( throwError,
    withExceptT,
  )
import Control.Monad.Random.Class (MonadRandom, getRandom)
import qualified Crypto.Random.Types as CR
import Data.Thyme.Clock as C
import Data.Thyme.Time as T
import Network.URI ()

data PaymentsConfig m
  = PaymentsConfig
      { _bitcoinBillingOps :: !(BTC.BillingOps m),
        _bitcoinPaymentsConfig :: !BTC.PaymentsConfig,
        _zcashPaymentsConfig :: !Zcash.PaymentsConfig
      }

makeLenses ''PaymentsConfig

data PaymentRequestStatus c
  = -- | the request was paid with the specified payment
    Paid !(Payment c)
  | -- | the request has not been paid, but has not yet expired
    forall b. Unpaid !(PaymentRequest' b c)
  | -- | the request was not paid prior to the expiration date
    forall b. Expired !(PaymentRequest' b c)

data PaymentError
  = RequestError PT.PaymentRequestError
  | Overdue !PaymentRequestId
  | BTCPaymentError !BTC.PaymentError
  | BillableIdMismatch !BillableId !BillableId

makeClassyPrisms ''PaymentError

{--
 - Find all the subscriptions for the specified customer, and
 - determine which if any are up for renewal. Create a payment
 - request for each such subscription.
 --}
createPaymentRequests ::
  ( MonadDB m,
    CR.MonadRandom m,
    MonadRandom m
  ) =>
  -- | bitcoin payment generation setup
  PaymentsConfig m ->
  -- | timestamp for payment request creation
  C.UTCTime ->
  -- | customer responsible for payment
  UserId ->
  -- | project whose worklog is to be paid
  ProjectId ->
  ExceptT PaymentError m [PaymentRequestId]
createPaymentRequests cfg now custId pid = do
  subscriptions <- lift $ findSubscriptions custId pid
  join <$> traverse (createSubscriptionPaymentRequests cfg now) subscriptions

createSubscriptionPaymentRequests ::
  forall m.
  (MonadDB m, CR.MonadRandom m, MonadRandom m) =>
  PaymentsConfig m ->
  C.UTCTime ->
  (SubscriptionId, Subscription) ->
  ExceptT PaymentError m [PaymentRequestId]
createSubscriptionPaymentRequests cfg now (sid, sub) = do
  -- fill in the billable for the subscription
  sub' <-
    lift . maybeT (raiseSubjectNotFound . FindBillable $ billableId) pure $
      traverseOf B.billable findBillable sub
  -- get previous payment requests & augment with billable information
  paymentRequests <- lift $ findSubscriptionPaymentRequests sid
  -- find dates for which no bill has yet been issued
  billableDates <-
    findUnbilledDates now paymentRequests
      . takeWhile (< now ^. _utctDay)
      $ B.billingSchedule sub'
  -- create a payment request for the specified unbilled date
  let createPaymentRequest' :: T.Day -> ExceptT PaymentError m PaymentRequestId
      createPaymentRequest' day = case sub' ^. B.billable . amount of
        Amount BTC sats ->
          let b' = over B.amount (const sats) (sub' ^. B.billable)
              ops = BTC.paymentOps (cfg ^. bitcoinBillingOps) (cfg ^. bitcoinPaymentsConfig)
           in withExceptT BTCPaymentError $ createPaymentRequest ops now billableId b' day
        Amount ZEC zats ->
          let b' = over B.amount (const zats) (sub' ^. B.billable)
              ops = Zcash.paymentOps (cfg ^. zcashPaymentsConfig)
           in withExceptT RequestError $ createPaymentRequest ops now billableId b' day
  traverse createPaymentRequest' billableDates
  where
    billableId = sub ^. B.billable

createPaymentRequest ::
  (MonadDB m, MonadRandom m) =>
  PaymentOps currency m ->
  C.UTCTime ->
  BillableId ->
  Billable currency ->
  T.Day ->
  m PaymentRequestId
createPaymentRequest ops now billId bill bday = do
  reqId <- PT.PaymentRequestId <$> getRandom
  nativeReq <- newPaymentRequest ops bill reqId bday now
  let req =
        PaymentRequest
          { _billable = (Const billId),
            _createdAt = now,
            _billingDate = bday,
            _nativeRequest = nativeReq
          }
  liftdb $ StorePaymentRequest reqId req
  pure reqId

{-
 - FIXME: The current implementation expects the billing day to be a suitable
 - key for comparison to payment requests. This is almost certainly inadequate.
 -}
findUnbilledDates ::
  (MonadDB m) =>
  -- | the date against which payment request expiration should be checked
  C.UTCTime ->
  -- | the list of existing payment requests
  [(PaymentRequestId, PT.SomePaymentRequestDetail)] ->
  -- | the list of expected billing days
  [T.Day] ->
  -- | the list of billing days for which no payment request exists
  ExceptT PaymentError m [T.Day]
findUnbilledDates now (px@((reqId, SomePaymentRequest req) : ps)) (dx@(d : ds)) =
  let rec = findUnbilledDates now
      gracePeriod = req ^. PT.billable . B.gracePeriod
   in case compare (req ^. billingDate) d of
        EQ ->
          lift (getRequestStatus now reqId req) >>= \case
            Expired r ->
              if (now ^. _utctDay) > addDays gracePeriod (r ^. billingDate)
                then throwError (review _Overdue reqId)
                else fmap (d :) $ rec px dx -- d will be rebilled
            _ ->
              rec ps ds -- if paid or unpaid, nothing to do, keep looking
        GT ->
          fmap (d :) $ rec px ds
        LT ->
          rec ps dx
findUnbilledDates _ _ ds = pure ds

{- Check whether the specified payment request has a payment associated with
 - it, and return a PaymentRequestStatus value indicating the result.
 -}
getRequestStatus ::
  forall c m.
  (MonadDB m) =>
  -- | the date against which request expiration should be checked
  C.UTCTime ->
  PaymentRequestId ->
  -- | the request for which to find a payment
  PaymentRequestDetail c ->
  m (PaymentRequestStatus c)
getRequestStatus now reqid req =
  let ifUnpaid = if isExpired now req then Expired req else Unpaid req
      findPayment' = case paymentRequestCurrency req of
        (Currency' BTC) -> findPayment BTC reqid
        (Currency' ZEC) -> findPayment ZEC reqid
   in maybe ifUnpaid Paid <$> runMaybeT findPayment'

findPayableRequests ::
  (MonadDB m) => UserId -> SubscriptionId -> m [(PaymentRequestId, PT.SomePaymentRequestDetail)]
findPayableRequests uid sid = do
  subMay <- liftdb (FindSubscription sid)
  when (maybe True (\s -> s ^. B.customer /= uid) subMay) $
    void (raiseOpForbidden uid (UserNotSubscriber sid) (FindSubscription sid))
  findSubscriptionUnpaidRequests sid
