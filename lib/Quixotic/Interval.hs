{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Interval 
  ( Interval(Interval), interval, start, end, ilen
  , intervalJSON, parseIntervalJSON
  , containsInclusive
  ) where

import ClassyPrelude

import Control.Lens(makeLenses, (^.))
import Data.Aeson
import Data.AffineSpace
import Data.Aeson.Types
import Data.Thyme.Clock as C
import Data.Thyme.LocalTime()
import Data.Thyme.Format.Aeson()

data Interval = Interval { _start :: C.UTCTime
                         , _end   :: C.UTCTime 
                         } deriving (Show, Eq, Ord)
makeLenses ''Interval                         

interval :: C.UTCTime -> C.UTCTime -> Interval
interval s e = if s < e then Interval s e else Interval e s

containsInclusive :: C.UTCTime -> Interval -> Bool
containsInclusive t (Interval s e) = t >= s && t <= e

ilen :: Interval -> C.NominalDiffTime
ilen i = (_end i) .-. (_start i)

intervalJSON :: Interval -> Value
intervalJSON ival = object ["start" .= (ival ^. start), "end" .= (ival ^. end)]

parseIntervalJSON :: Value -> Parser Interval
parseIntervalJSON (Object v) = interval <$> v .: "start" <*> v .: "end"
parseIntervalJSON _ = mzero
