{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
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

data Interval t
  = Interval
      { _start :: t,
        _end :: t
      }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeLenses ''Interval

data RangeQuery
  = Before {_end' :: C.UTCTime}
  | During {_start' :: C.UTCTime, _end' :: C.UTCTime}
  | After {_start' :: C.UTCTime}
  | Always

makeLenses ''RangeQuery

interval :: Ord t => t -> t -> Interval t
interval s e = if s < e then Interval s e else Interval e s

rangeQuery :: C.UTCTime -> C.UTCTime -> RangeQuery
rangeQuery s e = if s < e then During s e else During e s

containsInclusive :: Ord t => t -> Interval t -> Bool
containsInclusive t (Interval s e) = t >= s && t <= e

ilen :: Interval C.UTCTime -> C.NominalDiffTime
ilen i = _end i .-. _start i

intervalJSON :: (t -> Value) -> Interval t -> Value
intervalJSON f ival = object ["start" .= f (ival ^. start), "end" .= f (ival ^. end)]

parseIntervalJSON :: (Ord t, FromJSON t) => Value -> Parser (Interval t)
parseIntervalJSON (Object v) = interval <$> v .: "start" <*> v .: "end"
parseIntervalJSON _ = mzero
