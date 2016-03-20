{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Aftok.Types where

import           ClassyPrelude
import           Control.Lens

newtype Satoshi = Satoshi { fromSatoshi :: Word64 }
                  deriving (Show, Eq, Ord, Num, Real, Bounded)
makePrisms ''Satoshi


