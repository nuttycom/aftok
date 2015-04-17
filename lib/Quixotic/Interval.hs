{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Interval 
  ( Interval, interval, start, end, ilen
  , intervalJSON, parseIntervalJSON
  ) where

import ClassyPrelude

import Control.Lens(makeLenses, (^.))
import Data.Aeson
import Data.Aeson.Types
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

intervalJSON :: Interval -> Value
intervalJSON ival = object ["start" .= (ival ^. start), "end" .= (ival ^. end)]

parseIntervalJSON :: Value -> Parser Interval
parseIntervalJSON (Object v) = interval <$> v .: "start" <*> v .: "end"
parseIntervalJSON _ = mzero
