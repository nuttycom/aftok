{-# LANGUAGE TemplateHaskell #-}

module Aftok.Interval 
  ( Interval(..), interval, start, end, ilen
  , Interval'(..), interval', start', end'
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

data Interval' = Before { _end'  :: C.UTCTime } 
               | During { _start' :: C.UTCTime, _end' :: C.UTCTime }
               | After  { _start' :: C.UTCTime }

makeLenses ''Interval                         
makeLenses ''Interval'

interval :: C.UTCTime -> C.UTCTime -> Interval
interval s e = if s < e then Interval s e else Interval e s

interval' :: C.UTCTime -> C.UTCTime -> Interval'
interval' s e = if s < e then During s e else During e s

containsInclusive :: C.UTCTime -> Interval -> Bool
containsInclusive t (Interval s e) = t >= s && t <= e

ilen :: Interval -> C.NominalDiffTime
ilen i = _end i .-. _start i

intervalJSON :: Interval -> Value
intervalJSON ival = object ["start" .= (ival ^. start), "end" .= (ival ^. end)]

parseIntervalJSON :: Value -> Parser Interval
parseIntervalJSON (Object v) = interval <$> v .: "start" <*> v .: "end"
parseIntervalJSON _ = mzero
