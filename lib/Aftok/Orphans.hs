{-# OPTIONS_GHC -fno-warn-orphans #-}

module Aftok.Orphans where

import qualified Crypto.Random.Types as CR

instance CR.MonadRandom m => CR.MonadRandom (ExceptT e m) where
  getRandomBytes n = lift (CR.getRandomBytes n)
