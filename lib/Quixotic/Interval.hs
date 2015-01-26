{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Interval 
  ( Interval, interval, start, end, ilen
  ) where

import ClassyPrelude

import Control.Lens
import Data.Time.Clock
import Data.Time.LocalTime()

data Interval = Interval { _start :: UTCTime
                         , _end   :: UTCTime 
                         } deriving (Show, Eq)
makeLenses ''Interval                         

interval :: UTCTime -> UTCTime -> Interval
interval s e = if s < e then Interval s e else Interval e s

ilen :: Interval -> NominalDiffTime
ilen i = diffUTCTime (_end i) (_start i)

