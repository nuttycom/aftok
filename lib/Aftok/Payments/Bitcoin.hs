{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Aftok.Payments.Bitcoin where

import Aftok.Billing
  ( Billable,
    amount,
    project,
    requestExpiryPeriod,
  )
import Aftok.Currency (Currency (BTC))
import Aftok.Currency.Bitcoin
  ( NetworkMode,
    getNetwork,
    _Satoshi,
  )
import Aftok.Database (MonadDB)
import Aftok.Payments.Bitcoin.Types (Channel (..), PaymentRequest (..))
import Aftok.Payments.Common (PaymentKey (..), randomPaymentKey)
import Aftok.Payments.Types
  ( NativeRequest (Bip70Request),
    PaymentOps (..),
    PaymentRequestError,
    RequestMeta (..),
  )
import Aftok.Payments.Util (MinPayout (..), getPayouts, getProjectPayoutFractions)
import Aftok.Types (AccountId)
import qualified Bippy as B
import qualified Bippy.Proto as P
import Bippy.Types
  ( Expiry (Expiry),
    Output (Output),
    PKIData,
    Satoshi (Satoshi),
    expiryTime,
    getExpires,
    getPaymentDetails,
  )
import Control.Lens
  ( makeLenses,
    review,
    (^.),
  )
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Except (except, withExceptT)
import qualified Crypto.PubKey.RSA.Types as RSA
  ( Error (..),
    PrivateKey,
  )
import Crypto.Random.Types
  ( MonadRandom,
  )
import Data.AffineSpace ((.+^))
import Data.Map.Strict (assocs)
import qualified Data.Text as T
import qualified Data.Thyme.Clock as C
import qualified Data.Thyme.Time as C
import Haskoin.Address (Address (..))
import Haskoin.Script (ScriptOutput (..))
import Network.URI (URI)

data BillingOps (m :: Type -> Type) = BillingOps
  { -- | generator for user memo
    memoGen ::
      Billable Satoshi -> -- template for the bill
      C.Day -> -- billing date
      C.UTCTime -> -- payment request generation time
      m (Maybe Text),
    -- | generator for payment response URL
    uriGen ::
      PaymentKey -> -- payment key to be included in the URL
      m (Maybe URI),
    -- | generator for merchant payload
    payloadGen ::
      Billable Satoshi -> -- template for the bill
      C.Day -> -- billing date
      C.UTCTime -> -- payment request generation time
      m (Maybe ByteString)
  }

data PaymentsConfig = PaymentsConfig
  { _networkMode :: !NetworkMode,
    _signingKey :: !RSA.PrivateKey,
    _pkiData :: !PKIData,
    _minPayment :: !Satoshi
  }

makeLenses ''PaymentsConfig

data PaymentError
  = RequestError !PaymentRequestError
  | SigningError !RSA.Error
  | IllegalAddress !Address

{- Check whether the specified payment request has expired (whether wallet software
 - will still consider the payment request valid)
 -}
isExpired :: C.UTCTime -> P.PaymentRequest -> Bool
isExpired now req =
  let check = any ((now >) . C.toThyme . expiryTime)
   in -- using error here is reasonable since it would indicate
      -- a serialization problem
      either (error . T.pack) (check . getExpires) $
        getPaymentDetails req

paymentOps ::
  ( MonadRandom m,
    MonadDB m
  ) =>
  BillingOps m ->
  PaymentsConfig ->
  PaymentOps Satoshi (ExceptT PaymentError m)
paymentOps ops cfg =
  PaymentOps
    { newPaymentRequest = ((((fmap Bip70Request) .) .) .) . bip70PaymentRequest ops cfg
    }

bip70PaymentRequest ::
  ( MonadRandom m,
    MonadDB m
  ) =>
  BillingOps m ->
  PaymentsConfig ->
  -- | bill denominated in satoshi
  Billable Satoshi ->
  -- | currency-specific request metadata
  RequestMeta Satoshi ->
  -- | billing base date
  C.Day ->
  -- | time at which the bill is being issued
  C.UTCTime ->
  -- | channel by which the request is to be delivered
  ExceptT PaymentError m PaymentRequest
bip70PaymentRequest ops cfg billable requestMeta billingDay billingTime = do
  let billTotal = billable ^. amount
      payoutTime = review C.utcTime $ C.UTCView billingDay (fromInteger 0)
  payoutFractions <- lift $ getProjectPayoutFractions payoutTime (billable ^. project)
  payouts <- withExceptT RequestError $ getPayouts payoutTime BTC (MinPayout $ cfg ^. minPayment) billTotal payoutFractions
  outputs <- except $ traverse toOutput (assocs payouts)
  pkey <- lift randomPaymentKey
  memo <- lift $ memoGen ops billable billingDay billingTime
  uri <- lift $ uriGen ops pkey
  payload <- lift $ payloadGen ops billable billingDay billingTime
  let expiry = Expiry . C.fromThyme $ billingTime .+^ (billable ^. requestExpiryPeriod)
  let details =
        B.createPaymentDetails
          (getNetwork (cfg ^. networkMode))
          outputs
          (C.fromThyme billingTime)
          (Just expiry)
          memo
          uri
          payload
  let channel = case requestMeta of
        (BitcoinRequestMeta c) -> c
        NoMeta -> WebChannel
  resp <- lift $ B.createPaymentRequest (cfg ^. signingKey) (cfg ^. pkiData) details
  either (throwError . SigningError) (pure . PaymentRequest pkey channel) resp

toOutput :: ((AccountId, Address), Satoshi) -> Either PaymentError Output
toOutput ((_, addr), amt) = case addr of
  (PubKeyAddress a) -> Right (Output amt (PayPKHash a))
  other -> Left $ IllegalAddress other

outputAmount :: Satoshi -> Rational -> Satoshi
outputAmount i r = Satoshi . round $ toRational (i ^. _Satoshi) * r
