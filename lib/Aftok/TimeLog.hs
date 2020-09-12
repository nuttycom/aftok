{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Aftok.TimeLog
  ( LogEntry(..)
  , creditTo
  , event
  , eventMeta
  , CreditTo(..)
  , _CreditToCurrency
  , _CreditToUser
  , _CreditToProject
  , creditToName
  , LogEvent(..)
  , eventName
  , nameEvent
  , eventTime
  , WorkIndex(WorkIndex)
  , _WorkIndex
  , workIndex
  , DepF
  , toDepF
  , EventId(EventId)
  , _EventId
  , ModTime(ModTime)
  , _ModTime
  , EventAmendment(..)
  , AmendmentId(AmendmentId)
  , _AmendmentId
  , Payouts(..)
  , _Payouts
  , payouts
  , linearDepreciation
  )
where

import           Control.Arrow                  ( (&&&) )
import           Control.Lens
import           Data.AdditiveGroup
import           Data.Aeson                    as A
import           Data.AffineSpace
import           Data.Eq                        ( Eq
                                                , (==)
                                                )
import           Data.Either                    ( Either(..)
                                                , rights
                                                )
import           Data.Foldable                 as F
import           Data.Function                  ( ($)
                                                , (.)
                                                , id
                                                )
import           Data.Functor                   ( fmap )
import           Data.Heap                     as H
import           Data.List.NonEmpty            as L
import           Data.Maybe                     ( Maybe(..) )
import           Data.Map.Strict               as MS
import           Data.Ord                       ( Ord(..)
                                                , Ordering(..)
                                                )
import           Data.Ratio                     ( Rational )
import           Data.Text                      ( Text )
import           Data.Thyme.Clock              as C
import           Data.UUID
import           Data.VectorSpace
import           Prelude                        ( (/)
                                                , (*)
                                                )
import           Text.Show                      ( Show )

import           Aftok.Interval
import           Aftok.Types

type NDT = C.NominalDiffTime

{-|
 - The depreciation function should return a value between 0 and 1;
 - this result is multiplied by the length of an interval of work to determine
 - the depreciated value of the work.
 -}
type DepF = C.UTCTime -> Interval -> NDT

data LogEvent = StartWork { _eventTime :: !C.UTCTime }
              | StopWork  { _eventTime :: !C.UTCTime }
              deriving (Show, Eq)
makeLenses ''LogEvent

instance Ord LogEvent where
  compare (StartWork t0) (StopWork  t1) = if t0 == t1 then GT else compare t0 t1
  compare (StopWork  t0) (StartWork t1) = if t0 == t1 then LT else compare t0 t1
  compare (StartWork t0) (StartWork t1) = compare t0 t1
  compare (StopWork  t0) (StopWork  t1) = compare t0 t1

eventName :: LogEvent -> Text
eventName (StartWork _) = "start"
eventName (StopWork  _) = "stop"

nameEvent :: Text -> Maybe (C.UTCTime -> LogEvent)
nameEvent "start" = Just StartWork
nameEvent "stop"  = Just StopWork
nameEvent _       = Nothing

data LogEntry a = LogEntry
  { _creditTo  :: !(CreditTo a)
  , _event     :: !LogEvent
  , _eventMeta :: !(Maybe A.Value)
  } deriving (Show, Eq)
makeLenses ''LogEntry

instance Ord a => Ord (LogEntry a) where
  compare a b =
    let ordElems e = (e ^. event, e ^. creditTo)
    in  ordElems a `compare` ordElems b

newtype EventId = EventId UUID deriving (Show, Eq)
makePrisms ''EventId

newtype ModTime = ModTime C.UTCTime
makePrisms ''ModTime

data EventAmendment a
  = TimeChange !ModTime !C.UTCTime
  | CreditToChange !ModTime !(CreditTo a)
  | MetadataChange !ModTime !A.Value

newtype AmendmentId = AmendmentId UUID deriving (Show, Eq)
makePrisms ''AmendmentId

newtype Payouts a = Payouts (Map (CreditTo a) Rational)
makePrisms ''Payouts

newtype WorkIndex a = WorkIndex (Map (CreditTo a) (NonEmpty Interval)) deriving (Show, Eq)
makePrisms ''WorkIndex

toDepF :: DepreciationFunction -> DepF
toDepF (LinearDepreciation undepLength depLength) =
  linearDepreciation undepLength depLength

{-|
 - Given a depreciation function, the "current" time, and a foldable functor of log intervals,
 - produce the total, depreciated length of work to be credited to an address.
 -}
workCredit :: (Foldable f) => DepF -> C.UTCTime -> f Interval -> NDT
workCredit df ptime ivals = getSum $ F.foldMap (Sum . df ptime) ivals

{-|
 - Payouts are determined by computing a depreciated duration value for
 - each work interval. This function computes the percentage of the total
 - work allocated to each address.
 -}
payouts :: Ord a => DepF -> C.UTCTime -> WorkIndex a -> Payouts a
payouts dep ptime (WorkIndex widx) =
  let addIntervalDiff :: (Foldable f) => NDT -> f Interval -> (NDT, NDT)
      addIntervalDiff total ivals =
          (^+^ total) &&& id $ workCredit dep ptime ivals

      (totalTime, keyTimes) = MS.mapAccum addIntervalDiff zeroV widx
  in  Payouts $ fmap ((/ toSeconds totalTime) . toSeconds) keyTimes

workIndex
  :: forall a f . (Ord a, Foldable f) => f (LogEntry a) -> (WorkIndex a)
workIndex logEntries =
  let sortedEntries = F.foldr H.insert H.empty logEntries
      rawIndex      = F.foldl' appendLogEntry MS.empty sortedEntries

      accum
        :: (CreditTo a)
        -> [Either LogEvent Interval]
        -> Map (CreditTo a) (NonEmpty Interval)
        -> Map (CreditTo a) (NonEmpty Interval)
      accum k l m = case nonEmpty (rights l) of
        Just l' -> MS.insert k l' m
        Nothing -> m
  in  WorkIndex $ MS.foldrWithKey accum MS.empty rawIndex

{-|
 - The values of the raw index map are either complete intervals (which may be
 - extended if a new start is encountered at the same instant as the end of the
 - interval) or start events awaiting completion.
 -}
type RawIndex a = Map (CreditTo a) [Either LogEvent Interval]

appendLogEntry :: (Ord a) => RawIndex a -> LogEntry a -> RawIndex a
appendLogEntry idx (LogEntry k ev _) =
  let combine :: LogEvent -> LogEvent -> Either LogEvent Interval
      combine (StartWork t) (StopWork t') | t' > t  = Right $ Interval t t'
      combine (e1@(StartWork _)) (e2@(StartWork _)) = Left $ max e1 e2 -- ignore redundant starts
      combine (e1@(StopWork  _)) (e2@(StopWork  _)) = Left $ min e1 e2 -- ignore redundant ends
      combine _                  e2                 = Left e2

      -- if the interval includes the timestamp of a start event, then allow the extension of the interval
      extension :: Interval -> LogEvent -> Maybe LogEvent
      extension ival (StartWork t) | containsInclusive t ival =
          Just $ StartWork (ival ^. start)
      extension _ _ = Nothing

      ivals = case MS.lookup k idx of
        -- if it is possible to extend an interval at the top of the stack
        -- because the end of that interval is the same
        Just (Right ival : xs) -> case extension ival ev of
          Just e' -> Left e' : xs
          Nothing -> Left ev : Right ival : xs
        -- if the top element of the stack is not an interval
        Just (Left ev' : xs) -> combine ev' ev : xs
        _                    -> [Left ev]
  in  MS.insert k ivals idx

{-|
 - A very simple linear function for calculating depreciation.
 -}
linearDepreciation
  :: Months -- ^ The number of initial months during which no depreciation occurs
  -> Months -- ^ The number of months over which each logged interval will be depreciated
  -> DepF   -- ^ The resulting configured depreciation function.
linearDepreciation undepLength depLength =
  let monthsLength :: Months -> NDT
      monthsLength (Months i) = fromSeconds $ 60 * 60 * 24 * 30 * i

      maxDepreciable :: NDT
      maxDepreciable = monthsLength undepLength ^+^ monthsLength depLength

      depPct :: NDT -> Rational
      depPct dt = if dt < monthsLength undepLength
        then 1
        else toSeconds (max zeroV (maxDepreciable ^-^ dt))
          / toSeconds maxDepreciable
  in  \ptime ival ->
        let depreciation = depPct $ ptime .-. (ival ^. end)
        in  depreciation *^ ilen ival
