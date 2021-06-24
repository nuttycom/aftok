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
import Aftok.Currency.Zcash.Types (Memo (..))
import Aftok.Database (MonadDB)
import Aftok.Payments.Common (randomPaymentKey)
import qualified Aftok.Payments.Types as PT
import Aftok.Payments.Util (MinPayout (..), getPayouts, getProjectPayoutFractions)
import Aftok.Payments.Zcash.Types (Channel (..), PaymentRequest (..))
import Aftok.Payments.Zcash.Zip321 (PaymentItem (..))
import qualified Aftok.Payments.Zcash.Zip321 as Zip321
import Aftok.Types (AccountId)
import Control.Error.Safe (tryJust)
import Control.Lens (makeLenses, review, (^.))
import Crypto.Random.Types
  ( MonadRandom,
  )
import Data.Map.Strict (assocs)
import Data.Thyme.Clock as C
import Data.Thyme.Time as C

data PaymentsConfig = PaymentsConfig
  { _minAmt :: Zatoshi
  }

makeLenses ''PaymentsConfig

type MemoGen m = Billable Zatoshi -> C.Day -> C.UTCTime -> AccountId -> m (Maybe Memo)

paymentOps ::
  ( MonadRandom m,
    MonadDB m
  ) =>
  MemoGen m ->
  PaymentsConfig ->
  PT.PaymentOps Zatoshi (ExceptT PT.PaymentRequestError m)
paymentOps memoGen cfg =
  PT.PaymentOps
    { PT.newPaymentRequest = (((fmap PT.Zip321Request .) .) .) . zip321PaymentRequest cfg memoGen
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
  ( MonadRandom m,
    MonadDB m
  ) =>
  PaymentsConfig ->
  -- | generator for the memo to be associated with each item
  MemoGen m ->
  -- | billing information
  Billable Zatoshi ->
  -- | zcash-specific request metadata
  PT.RequestMeta Zatoshi ->
  -- | payout date (billing date)
  C.Day ->
  -- | timestamp for payment request creation
  C.UTCTime ->
  ExceptT PT.PaymentRequestError m PaymentRequest
zip321PaymentRequest cfg memoGen billable requestMeta billingDay billTime = do
  let payoutTime = review C.utcTime $ C.UTCView billingDay (fromInteger 0)
      billTotal = billable ^. amount
  payoutFractions <- lift $ getProjectPayoutFractions payoutTime (billable ^. project)
  payouts <- getPayouts payoutTime ZEC (MinPayout $ cfg ^. minAmt) billTotal payoutFractions
  pkey <- lift randomPaymentKey
  itemsMay <- lift $ nonEmpty <$> traverse toPaymentItem (assocs payouts)
  z321Req <- Zip321.PaymentRequest <$> tryJust PT.NoRecipients itemsMay
  pure $
    PaymentRequest
      { _paymentRequestKey = pkey,
        _paymentRequestChannel = case requestMeta of
          (PT.ZcashRequestMeta channel) -> channel
          PT.NoMeta -> WebChannel,
        _zip321Request = z321Req
      }
  where
    toPaymentItem :: ((AccountId, Address), Zatoshi) -> m Zip321.PaymentItem
    toPaymentItem ((aid, a), z) = do
      memo <- memoGen billable billingDay billTime aid
      pure $
        Zip321.PaymentItem
          { _address = a,
            _label = Nothing,
            _message = billable ^. messageText,
            _amount = z,
            _memo = memo,
            _other = []
          }
