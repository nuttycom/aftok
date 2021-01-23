{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Aftok.TimeLog
  ( LogEntry (..),
    HasLogEntry (..),
    CreditTo (..),
    _CreditToAccount,
    _CreditToUser,
    _CreditToProject,
    LogEvent (..),
    _StartWork,
    _StopWork,
    eventName,
    nameEvent,
    eventTime,
    WorkIndex (WorkIndex),
    _WorkIndex,
    workIndex,
    DepF,
    toDepF,
    EventId (EventId),
    _EventId,
    ModTime (ModTime),
    _ModTime,
    EventAmendment (..),
    AmendmentId (AmendmentId),
    _AmendmentId,
    Payouts (..),
    _Payouts,
    FractionalPayouts,
    payouts,
    linearDepreciation,
  )
where

import Aftok.Interval
import Aftok.Types
import Control.Lens
import Data.AdditiveGroup ()
import Data.Aeson as A
import Data.AffineSpace
import Data.Foldable as F
import qualified Data.Map.Strict as MS
import Data.Thyme.Clock as C
import Data.UUID (UUID)
import Data.VectorSpace ((*^), Sum (..), (^+^), (^-^), getSum, zeroV)
import Prelude hiding (Sum, getSum)

type NDT = C.NominalDiffTime

-- |
-- - The depreciation function should return a value between 0 and 1;
-- - this result is multiplied by the length of an interval of work to determine
-- - the depreciated value of the work.
type DepF = C.UTCTime -> Interval C.UTCTime -> NDT

data LogEvent
  = StartWork {_eventTime :: !C.UTCTime}
  | StopWork {_eventTime :: !C.UTCTime}
  deriving (Show, Eq)

makePrisms ''LogEvent

makeLenses ''LogEvent

instance Ord LogEvent where
  compare (StartWork t0) (StopWork t1) = if t0 == t1 then GT else compare t0 t1
  compare (StopWork t0) (StartWork t1) = if t0 == t1 then LT else compare t0 t1
  compare (StartWork t0) (StartWork t1) = compare t0 t1
  compare (StopWork t0) (StopWork t1) = compare t0 t1

eventName :: LogEvent -> Text
eventName = \case
  (StartWork _) -> "start"
  (StopWork _) -> "stop"

nameEvent :: Text -> Maybe (C.UTCTime -> LogEvent)
nameEvent = \case
  "start" -> Just StartWork
  "stop" -> Just StopWork
  _ -> Nothing

data LogEntry
  = LogEntry
      { _creditTo :: !CreditTo,
        _event :: !LogEvent,
        _eventMeta :: !(Maybe A.Value)
      }
  deriving (Show, Eq)

makeClassy ''LogEntry

instance Ord LogEntry where
  compare a b =
    let ordElems e = (e ^. event, e ^. creditTo)
     in ordElems a `compare` ordElems b

newtype EventId = EventId UUID deriving (Show, Eq, Ord)

makePrisms ''EventId

newtype ModTime = ModTime C.UTCTime

makePrisms ''ModTime

data EventAmendment
  = TimeChange !ModTime !C.UTCTime
  | CreditToChange !ModTime !CreditTo
  | MetadataChange !ModTime !A.Value

newtype AmendmentId = AmendmentId UUID deriving (Show, Eq, Ord)

makePrisms ''AmendmentId

newtype Payouts a = Payouts (Map CreditTo a)

makePrisms ''Payouts

type FractionalPayouts = Payouts Rational

newtype WorkIndex t = WorkIndex (Map CreditTo (NonEmpty (Interval t))) deriving (Show, Eq)

makePrisms ''WorkIndex

toDepF :: DepreciationFunction -> DepF
toDepF (LinearDepreciation undepLength depLength) =
  linearDepreciation undepLength depLength

-- |
-- - Given a depreciation function, the "current" time, and a foldable functor of log intervals,
-- - produce the total, depreciated length of work to be credited to an address.
workCredit :: (Foldable f, HasLogEntry le) => DepF -> C.UTCTime -> f (Interval le) -> NDT
workCredit df ptime ivals = getSum $ F.foldMap (Sum . df ptime . fmap (view $ event . eventTime)) ivals

-- |
-- - Payouts are determined by computing a depreciated duration value for
-- - each work interval. This function computes the percentage of the total
-- - work allocated to each unique CreditTo.
payouts :: forall le. (HasLogEntry le) => DepF -> C.UTCTime -> WorkIndex le -> FractionalPayouts
payouts dep ptime (WorkIndex widx) =
  let addIntervalDiff :: (Foldable f) => NDT -> f (Interval le) -> (NDT, NDT)
      addIntervalDiff total ivals =
        (^+^ total) &&& id $ workCredit dep ptime ivals
      (totalTime, keyTimes) = MS.mapAccum addIntervalDiff zeroV widx
   in Payouts $ fmap ((/ toSeconds totalTime) . toSeconds) keyTimes

workIndex :: (Foldable f, HasLogEntry le, Ord o) => (le -> o) -> f le -> WorkIndex le
workIndex cmp logEntries =
  let sortedEntries = sortWith cmp $ toList logEntries
      rawIndex = F.foldl' appendLogEntry MS.empty sortedEntries
      accum ::
        CreditTo ->
        [Either le (Interval le)] ->
        Map CreditTo (NonEmpty (Interval le)) ->
        Map CreditTo (NonEmpty (Interval le))
      accum k l m = case nonEmpty (rights l) of
        Just l' -> MS.insert k l' m
        Nothing -> m
   in WorkIndex $ MS.foldrWithKey accum MS.empty rawIndex

-- |
-- - The values of the raw index map are either complete intervals (which may be
-- - extended if a new start is encountered at the same instant as the end of the
-- - interval) or start events awaiting completion.
type RawIndex le = Map CreditTo [Either le (Interval le)]

appendLogEntry ::
  forall le.
  HasLogEntry le =>
  RawIndex le ->
  le ->
  RawIndex le
appendLogEntry idx logEvent =
  let k = logEvent ^. logEntry . creditTo
      ivals = case MS.lookup k idx of
        -- if it is possible to extend an interval at the top of the stack
        -- because the end of that interval is the same
        Just (Right ival : xs) ->
          case extension (view (event . eventTime) <$> ival) logEvent of
            Just e' -> Left e' : xs
            Nothing -> Left logEvent : Right ival : xs
        -- if the top element of the stack is not an interval
        Just (Left ev' : xs) ->
          combine ev' logEvent : xs
        _ -> [Left logEvent]
   in MS.insert k ivals idx
  where
    combine :: le -> le -> Either le (Interval le)
    combine e e' = case (e ^. event, e' ^. event) of
      (StartWork t, StopWork t') | t' > t -> Right $ Interval e e' -- complete interval found
      (StartWork t, StartWork t') -> Left $ if t > t' then e else e' -- ignore redundant starts
      (StopWork t, StopWork t') -> Left $ if t <= t' then e else e' -- ignore redundant ends
      _ -> Left e'
    -- if the interval includes the timestamp of a start event, then allow the extension of the interval
    extension :: (Interval C.UTCTime) -> le -> Maybe le
    extension ival newEvent@(view event -> StartWork t)
      | containsInclusive t ival =
        Just newEvent -- replace the end of the interval with the new event
    extension _ _ =
      Nothing

-- |
-- - A very simple linear function for calculating depreciation.
linearDepreciation ::
  -- | The number of initial months during which no depreciation occurs
  Months ->
  -- | The number of months over which each logged interval will be depreciated
  Months ->
  -- | The resulting configured depreciation function.
  DepF
linearDepreciation undepLength depLength =
  let monthsLength :: Months -> NDT
      monthsLength (Months i) = fromSeconds $ 60 * 60 * 24 * 30 * i
      maxDepreciable :: NDT
      maxDepreciable = monthsLength undepLength ^+^ monthsLength depLength
      depPct :: NDT -> Rational
      depPct dt =
        if dt < monthsLength undepLength
          then 1
          else
            toSeconds (max zeroV (maxDepreciable ^-^ dt))
              / toSeconds maxDepreciable
   in \ptime ival ->
        let depreciation = depPct $ ptime .-. (ival ^. end)
         in depreciation *^ ilen ival
