{-# LANGUAGE TemplateHaskell #-}

module Aftok.Payments where

import           ClassyPrelude

import           Control.Lens        (makeLenses, makePrisms)

import           Data.Thyme.Clock    as C
import           Data.UUID

import qualified Network.Bippy.Proto as P

import           Aftok.Billables

newtype PaymentRequestId = PaymentRequestId UUID deriving (Show, Eq)
makePrisms ''PaymentRequestId

newtype PaymentId = PaymentId UUID deriving (Show, Eq)
makePrisms ''PaymentId

data PaymentRequest' s = PaymentRequest
  { _subscription       :: s
  , _paymentRequest     :: P.PaymentRequest
  , _paymentRequestDate :: C.UTCTime
  }
makeLenses ''PaymentRequest'

type PaymentRequest = PaymentRequest' SubscriptionId

data Payment' r = Payment
  { _request     :: r
  , _payment     :: P.Payment
  , _paymentDate :: C.UTCTime
  }
makeLenses ''Payment'

type Payment = Payment' PaymentRequestId
