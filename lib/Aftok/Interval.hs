{-# LANGUAGE TemplateHaskell #-}

module Aftok.Interval
  ( Interval (..),
    interval,
    start,
    end,
    ilen,
    RangeQuery (..),
    rangeQuery,
    start',
    end',
    intervalJSON,
    parseIntervalJSON,
    containsInclusive,
  )
where

import Control.Lens
  ( (^.),
    makeLenses,
  )
import Data.Aeson
import Data.Aeson.Types
import Data.AffineSpace
import Data.Thyme.Clock as C
import Data.Thyme.Format.Aeson ()
import Data.Thyme.LocalTime ()

data Interval
  = Interval
      { _start :: C.UTCTime,
        _end :: C.UTCTime
      }
  deriving (Show, Eq, Ord)

makeLenses ''Interval

data RangeQuery
  = Before {_end' :: C.UTCTime}
  | During {_start' :: C.UTCTime, _end' :: C.UTCTime}
  | After {_start' :: C.UTCTime}
  | Always

makeLenses ''RangeQuery

interval :: C.UTCTime -> C.UTCTime -> Interval
interval s e = if s < e then Interval s e else Interval e s

rangeQuery :: C.UTCTime -> C.UTCTime -> RangeQuery
rangeQuery s e = if s < e then During s e else During e s

containsInclusive :: C.UTCTime -> Interval -> Bool
containsInclusive t (Interval s e) = t >= s && t <= e

ilen :: Interval -> C.NominalDiffTime
ilen i = _end i .-. _start i

intervalJSON :: Interval -> Value
intervalJSON ival = object ["start" .= (ival ^. start), "end" .= (ival ^. end)]

parseIntervalJSON :: Value -> Parser Interval
parseIntervalJSON (Object v) = interval <$> v .: "start" <*> v .: "end"
parseIntervalJSON _ = mzero
