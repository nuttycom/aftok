module Ananke.Interval 
  ( Interval, interval, start, end, ilen
  ) where

import Data.Time.Clock
import Data.Time.LocalTime

data Interval = Interval { start :: UTCTime
                         , end   :: UTCTime 
                         } deriving (Show, Eq)

interval s e = if s < e then Interval s e else Interval e s

ilen :: Interval -> NominalDiffTime
ilen i = diffUTCTime (end i) (start i)
