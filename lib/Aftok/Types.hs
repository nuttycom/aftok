{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aftok.Types where

import Data.Word
import ClassyPrelude

newtype Satoshi = Satoshi Word64
                  deriving (Show, Eq, Ord, Num, Real, Bounded)
    

