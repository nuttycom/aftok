{-# LANGUAGE TemplateHaskell    #-}

module Aftok.Currency.ZCash where

import           Control.Lens                   ( makePrisms
                                                )

newtype ZAddr = ZAddr { zaddrText :: Text }
  deriving (Eq, Ord, Show)
makePrisms ''ZAddr

