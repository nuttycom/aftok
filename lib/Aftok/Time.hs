{-# LANGUAGE TemplateHaskell #-}
module Aftok.Time where

import           ClassyPrelude
import           Control.Lens  (makePrisms)

newtype Days = Days Int
makePrisms ''Days

