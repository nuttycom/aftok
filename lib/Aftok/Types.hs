{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aftok.Types where

import           ClassyPrelude

newtype Satoshi = Satoshi { fromSatoshi :: Word64 }
                  deriving (Show, Eq, Ord, Num, Real, Bounded)


