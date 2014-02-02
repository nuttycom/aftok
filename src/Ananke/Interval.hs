module Ananke.Interval 
  ( Interval, interval, start, end 
  ) where

import Data.Time.Clock
import Data.Time.LocalTime

data Interval = Interval { start :: UTCTime
                         , end   :: UTCTime 
                         } deriving (Show, Eq)

interval s e = if s < e then Just (Interval s e) else Nothing

