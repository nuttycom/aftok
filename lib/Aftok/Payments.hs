{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Aftok.Payments 
  ( module Aftok.Payments
  , module Aftok.Payments.Types
  ) where

import           ClassyPrelude

import           Control.Lens  (makeLenses, view, (%~), (^.))
import           Control.Lens.Tuple
import           Control.Monad.Except (MonadError)
import           Crypto.PubKey.RSA.Types (Error(..), PrivateKey)
import           Crypto.Random.Types (MonadRandom)

import           Data.AffineSpace ((.+^))
import           Data.Map.Strict (assocs)
import           Data.Thyme.Clock    as C
import           Data.Thyme.Time.Core (fromThyme)

import qualified Network.Bippy as B
import qualified Network.Bippy.Proto as P
import qualified Network.Bippy.Types as BT 
import           Network.Haskoin.Script (ScriptOutput(..))
import           Network.URI

import           Aftok         (UserId, BtcAddr(..), userAddress, _BtcAddr)
import           Aftok.Database
import           Aftok.Billables
import           Aftok.Payments.Types
import           Aftok.Project (ProjectId, depf)
import qualified Aftok.TimeLog as TL
import           Aftok.Types (satoshi)

data BillingConfig = BillingConfig
  { _network :: BT.Network
  , _signingKey :: PrivateKey
  , _pkiData :: BT.PKIData
  }
makeLenses ''BillingConfig


createPaymentRequests :: (MonadRandom m, MonadReader BillingConfig m, MonadError Error m, MonadDB m) => 
                         C.UTCTime -- timestamp for payment request creation
                      -> (Subscription' Billable -> m (Maybe Text))       -- generator user memo
                      -> (Subscription' Billable -> m (Maybe URI))        -- generator for payment response URL
                      -> (Subscription' Billable -> m (Maybe ByteString)) -- generator for merchant payload
                      -> UserId    -- user responsible for payment
                      -> ProjectId -- project whose worklog is to be paid out to
                      -> m [PaymentRequestId]
createPaymentRequests t memogen urigen plgen custId pid = do
  subscriptions <- findSubscriptions custId pid
  cfg <- ask
  let createPaymentDetails' s = do
        memo <- memogen s
        uri <- urigen s
        payload <- plgen s
        createPaymentDetails t memo uri payload (s ^. billable)

      createPaymentRequest (sid, s) = do
        details <- createPaymentDetails' s 
        req <- B.createPaymentRequest (cfg ^. signingKey) (cfg ^. pkiData) details
        liftdb $ CreatePaymentRequest custId (PaymentRequest sid req t)
  traverse createPaymentRequest subscriptions

createPaymentDetails :: (MonadRandom m, MonadReader BillingConfig m, MonadDB m) =>
                        C.UTCTime         -- timestamp for payment request creation
                     -> Maybe Text        -- user memo
                     -> Maybe URI         -- payment response URL
                     -> Maybe ByteString  -- merchant payload
                     -> Billable 
                     -> m P.PaymentDetails
createPaymentDetails t memo uri payload b = do
  payouts <- getProjectPayouts t (b ^. project)
  outputs <- createPayoutsOutputs t (b ^. amount) payouts
  let expiry = (BT.Expiry . fromThyme . (t .+^)) <$> (b ^. requestExpiryPeriod)
  cfg <- ask
  pure $ B.createPaymentDetails (cfg ^. network) outputs (fromThyme t) expiry memo uri payload


getProjectPayouts :: (MonadDB m) => C.UTCTime -> ProjectId -> m TL.Payouts
getProjectPayouts ptime pid = do
  project' <- 
    let projectOp = FindProject pid
    in  maybe (raiseSubjectNotFound projectOp) pure =<< liftdb projectOp

  widx <- liftdb $ ReadWorkIndex pid
  pure $ TL.payouts (TL.toDepF $ project' ^. depf) ptime widx


createPayoutsOutputs :: (MonadDB m) => C.UTCTime -> BT.Satoshi -> TL.Payouts -> m [BT.Output]
createPayoutsOutputs t amt p = 
  let payoutFractions :: [(TL.CreditTo, BT.Satoshi)]
      payoutFractions = (_2 %~ outputAmount amt) <$> assocs (p ^. TL._Payouts)

  in  join <$> traverse (uncurry (createOutputs t)) payoutFractions


createOutputs :: (MonadDB m) => C.UTCTime -> TL.CreditTo -> BT.Satoshi -> m [BT.Output]
createOutputs _ (TL.CreditToAddress (BtcAddr addr)) amt = 
  pure $ [BT.Output amt (PayPKHash addr)]

createOutputs _ (TL.CreditToUser uid) amt = do
  addrMay <- (>>= view userAddress) <$> findUser uid
  let createOutput addr = BT.Output amt (PayPKHash (addr ^. _BtcAddr))
  pure . maybeToList $ createOutput <$> addrMay

createOutputs t (TL.CreditToProject pid) amt = do
  payouts <- getProjectPayouts t pid
  createPayoutsOutputs t amt payouts


outputAmount :: BT.Satoshi -> Rational -> BT.Satoshi
outputAmount i r = BT.Satoshi . round $ toRational (i ^. satoshi) * r
