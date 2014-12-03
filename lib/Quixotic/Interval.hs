{-# LANGUAGE NoImplicitPrelude #-}

module Quixotic.Interval 
  ( Interval, interval, start, end, ilen
  ) where

import ClassyPrelude
import Data.Time.Clock
import Data.Time.LocalTime()

data Interval = Interval { start :: UTCTime
                         , end   :: UTCTime 
                         } deriving (Show, Eq)

interval :: UTCTime -> UTCTime -> Interval
interval s e = if s < e then Interval s e else Interval e s

ilen :: Interval -> NominalDiffTime
ilen i = diffUTCTime (end i) (start i)

