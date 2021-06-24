{-# LANGUAGE TemplateHaskell #-}

module Aftok.Payments.Common where

import Control.Lens
  ( makePrisms,
  )
import Crypto.Random.Types
  ( MonadRandom,
    getRandomBytes,
  )
import Haskoin.Address (encodeBase58)

-- A unique identifier for a payment request, suitable
-- for URL embedding.
newtype PaymentKey = PaymentKey Text deriving (Eq, Show)

makePrisms ''PaymentKey

randomPaymentKey :: MonadRandom m => m PaymentKey
randomPaymentKey =
  PaymentKey . encodeBase58 <$> getRandomBytes 6
