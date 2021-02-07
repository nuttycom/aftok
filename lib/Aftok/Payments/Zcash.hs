{-# LANGUAGE TemplateHaskell #-}

module Aftok.Payments.Zcash where

import Aftok.Billing
  ( Billable,
    amount,
    messageText,
    project,
  )
import Aftok.Currency (Currency (ZEC))
import Aftok.Currency.Zcash (Address, Zatoshi)
import Aftok.Currency.Zcash.Types (Memo(..))
import Aftok.Currency.Zcash.Zip321 (PaymentItem (..), PaymentRequest (..))
import Aftok.Database (MonadDB)
import qualified Aftok.Payments.Types as PT
import Aftok.Payments.Util (MinPayout (..), getPayouts, getProjectPayoutFractions)
import Aftok.Types (AccountId)
import Control.Error.Safe (tryJust)
import Control.Lens ((^.), makeLenses)
import Data.Map.Strict (assocs)
import Data.Thyme.Clock as C
import Data.Thyme.Time as C

data PaymentsConfig
  = PaymentsConfig
      { _minAmt :: Zatoshi
      }

makeLenses ''PaymentsConfig

type MemoGen m = Billable Zatoshi -> C.Day -> C.UTCTime -> AccountId -> m (Maybe Memo)

paymentOps ::
  (MonadDB m) =>
  MemoGen m ->
  PaymentsConfig ->
  PT.PaymentOps Zatoshi (ExceptT PT.PaymentRequestError m)
paymentOps memoGen cfg =
  PT.PaymentOps
    { PT.newPaymentRequest = ((fmap PT.Zip321Request .) .) . zip321PaymentRequest cfg memoGen
    }

-- TODO: Return a richer type that can include per-item uniqueness that can
-- be used for tracking payments. A payment request, though it's a request for
-- a single transaction, is really a request for multiple payments that we need
-- to be able to verify individually since they'll be independent notes.
--
-- However, this doesn't really become important until we start generating addresses
-- from Zcash IVKs, so it's not essential for right now.
zip321PaymentRequest ::
  forall m.
  (MonadDB m) =>
  PaymentsConfig ->
  -- | generator for the memo to be associated with each item
  MemoGen m ->
  -- | billing information
  Billable Zatoshi ->
  -- | payout date (billing date)
  C.Day ->
  -- | timestamp for payment request creation
  C.UTCTime ->
  ExceptT PT.PaymentRequestError m PaymentRequest
zip321PaymentRequest cfg memoGen billable billingDay billTime = do
  let payoutTime = C.mkUTCTime billingDay (fromInteger 0)
      billTotal = billable ^. amount
  payoutFractions <- lift $ getProjectPayoutFractions payoutTime (billable ^. project)
  payouts <- getPayouts payoutTime ZEC (MinPayout $ cfg ^. minAmt) billTotal payoutFractions
  itemsMay <- lift $ nonEmpty <$> traverse toPaymentItem (assocs payouts)
  PaymentRequest <$> tryJust PT.NoRecipients itemsMay
  where
    toPaymentItem :: ((AccountId, Address), Zatoshi) -> m PaymentItem
    toPaymentItem ((aid, a), z) = do
      memo <- memoGen billable billingDay billTime aid
      pure $ PaymentItem
        { _address = a,
          _label = Nothing,
          _message = billable ^. messageText,
          _amount = z,
          _memo = memo,
          _other = []
        }
