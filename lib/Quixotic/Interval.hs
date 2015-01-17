{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Quixotic.Interval 
  ( Interval, interval, start, end, ilen
  ) where

import ClassyPrelude
import Data.Aeson
import Data.Time.Clock
import Data.Time.LocalTime()

data Interval = Interval { start :: UTCTime
                         , end   :: UTCTime 
                         } deriving (Show, Eq)

instance ToJSON Interval where
  toJSON (Interval s e) = 
    object ["start" .= s, "end" .= e]

instance FromJSON Interval where
  parseJSON (Object v) = Interval <$> v .: "start" <*> v .: "end"
  parseJSON _ = mzero

interval :: UTCTime -> UTCTime -> Interval
interval s e = if s < e then Interval s e else Interval e s

ilen :: Interval -> NominalDiffTime
ilen i = diffUTCTime (end i) (start i)

