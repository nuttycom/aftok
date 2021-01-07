{-# LANGUAGE TupleSections #-}

module Aftok.Payments.Util where

import Aftok.Currency (Currency, IsCurrency, cscale)
import Aftok.Database
  ( DBOp
      ( FindProject,
        ReadWorkIndex
      ),
    MonadDB,
    findAccountPaymentAddress,
    findUserPaymentAddress,
    liftdb,
    raiseSubjectNotFound,
  )
import Aftok.Payments.Types (PaymentRequestError (..))
import Aftok.Project (depf)
import qualified Aftok.TimeLog as TL
import Aftok.Types (ProjectId)
import Control.Error.Util (note)
import Control.Lens ((^.))
import Control.Monad.Trans.Except (except)
import Data.Map.Strict (assocs, fromListWith)
import Data.Thyme.Clock as C

getProjectPayoutFractions ::
  (MonadDB m) =>
  C.UTCTime ->
  ProjectId ->
  m TL.FractionalPayouts
getProjectPayoutFractions ptime pid = do
  project' <-
    let projectOp = FindProject pid
     in maybe (raiseSubjectNotFound projectOp) pure =<< liftdb projectOp
  widx <- liftdb $ ReadWorkIndex pid
  pure $ TL.payouts (TL.toDepF $ project' ^. depf) ptime widx

newtype MinPayout c = MinPayout c

getPayouts ::
  (MonadDB m, Ord a, IsCurrency c) =>
  -- | time used in computation of payouts when `creditTo` is another project
  C.UTCTime ->
  -- | the currency with which the payment will be made
  Currency a c ->
  -- | the minimum payout amount, below which values are disregarded (avoids dust)
  MinPayout c ->
  -- | the amount to pay in total
  c ->
  -- | the fractions of the total payout to pay to each recipient
  TL.FractionalPayouts ->
  ExceptT PaymentRequestError m (Map a c)
getPayouts t currency mp@(MinPayout minAmt) amt payouts =
  if amt <= minAmt
    then pure mempty
    else do
      -- Multiply the total by each payout fraction. This may fail, so traverse.
      let scaled frac = note AmountInvalid $ cscale amt frac
      payoutFractions <- except $ traverse scaled (payouts ^. TL._Payouts)
      fromListWith (<>) . join <$> traverse (uncurry (getPayoutAmounts t currency mp)) (assocs payoutFractions)

getPayoutAmounts ::
  (MonadDB m, Ord a, IsCurrency c) =>
  -- | time used in computation of payouts when `creditTo` is another project
  C.UTCTime ->
  -- | the network on which the payment will be made
  Currency a c ->
  -- | the minimum payout amount, below which amounts will be disregarded (avoids dust)
  MinPayout c ->
  -- | the recipient of the payment
  TL.CreditTo ->
  -- | the amount to pay to the recipient
  c ->
  ExceptT PaymentRequestError m [(a, c)]
getPayoutAmounts t network mp creditTo amt = case creditTo of
  (TL.CreditToAccount aid) ->
    fmap (,amt) . maybeToList <$> (lift . runMaybeT $ findAccountPaymentAddress aid network)
  (TL.CreditToUser uid) ->
    fmap (,amt) . maybeToList <$> (lift . runMaybeT $ findUserPaymentAddress uid network)
  (TL.CreditToProject pid) -> do
    payouts <- lift $ getProjectPayoutFractions t pid
    assocs <$> getPayouts t network mp amt payouts
