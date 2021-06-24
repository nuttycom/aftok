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
    Subscription',
    SubscriptionId,
    amount,
    contactChannel,
    _SubscriptionId,
  )
import qualified Aftok.Billing as B
import Aftok.Currency (Amount (..), Currency (..), Currency' (..))
import Aftok.Database
  ( DBOp
      ( FindBillable,
        FindSubscription
      ),
    MonadDB,
    OpForbiddenReason (UserNotSubscriber),
    findBillable,
    findPayment,
    findSubscriptionPaymentRequests,
    findSubscriptionUnpaidRequests,
    liftdb,
    raiseOpForbidden,
    raiseSubjectNotFound,
    storePaymentRequest,
  )
import Aftok.Orphans ()
import qualified Aftok.Payments.Bitcoin as BTC
import qualified Aftok.Payments.Bitcoin.Types as BTC
import Aftok.Payments.Common (randomPaymentKey)
import Aftok.Payments.Types
  ( NativeRequest (..),
    Payment,
    PaymentOps (..),
    PaymentRequest,
    PaymentRequest' (..),
    PaymentRequestDetail,
    PaymentRequestId,
    RequestMeta (..),
    SomePaymentRequest (..),
    SomePaymentRequestDetail,
    billingDate,
    isExpired,
    paymentRequestCurrency,
  )
import qualified Aftok.Payments.Types as PT
import qualified Aftok.Payments.Zcash as Zcash
import qualified Aftok.Payments.Zcash.Types as Zcash
import Aftok.Types
  ( UserId,
  )
import Control.Error.Util (maybeT)
import Control.Lens
  ( makeClassyPrisms,
    makeLenses,
    review,
    traverseOf,
    (.~),
    (^.),
  )
import Control.Monad.Except
  ( throwError,
    withExceptT,
  )
import qualified Crypto.Random.Types as CR
import Data.Thyme.Clock as C
import Data.Thyme.Time as T
import qualified Data.UUID as UUID
import Network.URI ()

data PaymentsConfig m = PaymentsConfig
  { _bitcoinBillingOps :: !(BTC.BillingOps m),
    _bitcoinPaymentsConfig :: !BTC.PaymentsConfig,
    _zcashBillingOps :: !(Zcash.MemoGen m),
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

createSubscriptionPaymentRequests ::
  forall m.
  (MonadDB m, CR.MonadRandom m) =>
  PaymentsConfig m ->
  C.UTCTime ->
  (SubscriptionId, Subscription) ->
  ExceptT PaymentError m [(PaymentRequestId, SomePaymentRequestDetail)]
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
  traverse (createPaymentRequest' sub') billableDates
  where
    billableId = sub ^. B.billable
    -- create a payment request for the specified unbilled date
    createPaymentRequest' ::
      Subscription' UserId (Billable Amount) ->
      T.Day ->
      ExceptT PaymentError m (PaymentRequestId, SomePaymentRequestDetail)
    createPaymentRequest' sub' day =
      let bill = sub' ^. B.billable
          requestName = "Subscription " <> UUID.toText (sid ^. _SubscriptionId) <> " for "
       in case bill ^. amount of
            -- BTC bill creation
            Amount BTC sats -> withExceptT BTCPaymentError $ do
              let ops = BTC.paymentOps (cfg ^. bitcoinBillingOps) (cfg ^. bitcoinPaymentsConfig)
                  bill' = bill & amount .~ sats
                  reqm = case sub' ^. contactChannel of
                    B.EmailChannel e -> BitcoinRequestMeta (BTC.EmailChannel e)
                    _ -> NoMeta
              second SomePaymentRequest <$> createPaymentRequest ops now billableId bill' requestName reqm day
            -- Zcash bill creation
            Amount ZEC zats -> withExceptT RequestError $ do
              let ops = Zcash.paymentOps (cfg ^. zcashBillingOps) (cfg ^. zcashPaymentsConfig)
                  bill' = bill & amount .~ zats
                  reqm = case sub' ^. contactChannel of
                    B.EmailChannel e -> ZcashRequestMeta (Zcash.EmailChannel e)
                    B.ZMessageChannel a -> ZcashRequestMeta (Zcash.MemoChannel a)
              second SomePaymentRequest <$> createPaymentRequest ops now billableId bill' requestName reqm day

createPaymentRequest ::
  (MonadDB m, CR.MonadRandom m) =>
  PaymentOps currency m ->
  C.UTCTime ->
  BillableId ->
  Billable currency ->
  Text ->
  RequestMeta currency ->
  T.Day ->
  m (PaymentRequestId, PaymentRequestDetail currency)
createPaymentRequest ops now billId bill reqName reqm bday = do
  nativeReq <- newPaymentRequest ops bill reqm bday now
  paymentKey <- randomPaymentKey
  let req =
        PaymentRequest
          { _paymentKey = paymentKey,
            _requestName = reqName,
            _billable = (Const billId),
            _createdAt = now,
            _billingDate = bday,
            _nativeRequest = nativeReq
          }
  reqId <- storePaymentRequest req
  pure (reqId, req & PT.billable .~ bill)

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
