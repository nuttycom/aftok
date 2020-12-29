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
import Aftok.Currency.Zcash.Zip321 (PaymentItem (..), PaymentRequest (..))
import Aftok.Database (MonadDB)
import qualified Aftok.Payments.Types as PT
import Aftok.Payments.Util (MinPayout (..), getPayouts, getProjectPayoutFractions)
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

paymentOps ::
  (MonadDB m) =>
  PaymentsConfig ->
  PT.PaymentOps Zatoshi (ExceptT PT.PaymentRequestError m)
paymentOps cfg =
  PT.PaymentOps
    { PT.newPaymentRequest = ((fmap PT.Zip321Request .) .) . zip321PaymentRequest cfg
    }

zip321PaymentRequest ::
  (MonadDB m) =>
  PaymentsConfig ->
  -- | billing information
  Billable Zatoshi ->
  -- | payout date (billing date)
  C.Day ->
  -- | timestamp for payment request creation
  C.UTCTime ->
  ExceptT PT.PaymentRequestError m PaymentRequest
zip321PaymentRequest cfg billable billingDay _ = do
  let payoutTime = C.mkUTCTime billingDay (fromInteger 0)
      billTotal = billable ^. amount
  payoutFractions <- lift $ getProjectPayoutFractions payoutTime (billable ^. project)
  payouts <- getPayouts payoutTime ZEC (MinPayout $ cfg ^. minAmt) billTotal payoutFractions
  PaymentRequest <$> (tryJust PT.NoRecipients $ nonEmpty (toPaymentItem <$> assocs payouts))
  where
    toPaymentItem :: (Address, Zatoshi) -> PaymentItem
    toPaymentItem (a, z) =
      PaymentItem
        { _address = a,
          _label = Nothing,
          _message = Just $ billable ^. messageText,
          _amount = z,
          _memo = Nothing, -- Just . Memo $ toASCIIBytes (reqid ^. PT._PaymentRequestId),
          _other = []
        }
