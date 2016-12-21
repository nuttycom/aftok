{-# LANGUAGE TemplateHaskell #-}

module Aftok.Payments where

import           ClassyPrelude

import           Control.Lens               (makeLenses)

import           Data.Thyme.Clock           as C
import           Data.UUID

import qualified Network.Bippy.Proto as P

newtype PaymentRequestId = PaymentRequestId UUID deriving (Show, Eq)

newtype PaymentId = PaymentId UUID deriving (Show, Eq)

data PaymentRequest (p :: *) (b :: *) = PaymentRequest
  { _project :: p
  , _paymentRequest :: P.PaymentRequest
  , _paymentRequestDate :: C.UTCTime
  , _billable :: b
  }
makeLenses ''PaymentRequest

data Payment r u = Payment
  { _request :: r
  , _payment :: P.Payment
  , _paymentDate :: C.UTCTime
  , _payor :: u
  }
makeLenses ''Payment
