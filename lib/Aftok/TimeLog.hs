{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Aftok.TimeLog
  ( module Aftok.TimeLog,
    CreditTo (..),
    _CreditToAccount,
    _CreditToUser,
    _CreditToProject,
  )
where

import Aftok.Interval
import Aftok.Types
  ( CreditTo (..),
    DepreciationFunction (..),
    DepreciationRules (..),
    _CreditToAccount,
    _CreditToProject,
    _CreditToUser,
  )
import Control.Lens ((.~), Lens', (^.), makeClassy, makeLenses, makePrisms, view)
import Data.Aeson as A
import Data.AffineSpace ((.-.))
import qualified Data.Foldable as F
import qualified Data.Map.Strict as MS
import qualified Data.Thyme.Clock as C
import qualified Data.Thyme.Time as C
import Data.UUID (UUID)
import Data.VectorSpace ((*^), Sum (..), (^+^), (^-^), getSum, zeroV)
import Prelude hiding (Sum, getSum)

type NDT = C.NominalDiffTime

class HasEventTime a where
  eventTime :: Lens' a C.UTCTime

instance HasEventTime C.UTCTime where
  eventTime = id

data LogEvent
  = StartWork {_leEventTime :: !C.UTCTime}
  | StopWork {_leEventTime :: !C.UTCTime}
  deriving (Show, Eq)

makePrisms ''LogEvent

makeLenses ''LogEvent

instance HasEventTime LogEvent where
  eventTime = leEventTime

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

instance {-# OVERLAPPABLE #-} HasLogEntry a => HasEventTime a where
  eventTime = event . leEventTime

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

data WorkShare a
  = WorkShare
      { _wsLogged :: NDT,
        _wsDepreciated :: NDT,
        _wsShare :: a
      }

makeLenses ''WorkShare

data WorkShares
  = WorkShares
      { _loggedTotal :: NDT,
        _creditToShares :: Map CreditTo (WorkShare Rational)
      }

makeLenses ''WorkShares

newtype WorkIndex t = WorkIndex (Map CreditTo (NonEmpty (Interval t)))
  deriving (Show, Eq, Functor)

makePrisms ''WorkIndex

-- |
-- - The depreciation function should return a value between 0 and 1;
-- - this result is multiplied by the length of an interval of work to determine
-- - the depreciated value of the work.
--
-- arguments:
--  * date of first revenue (if applicable)
--  * date on which the depreciated value is being computed
--  *
type DepF = C.UTCTime -> Interval C.UTCTime -> NDT

toDepF :: DepreciationRules -> DepF
toDepF (DepreciationRules (LinearDepreciation undepLength depLength) firstRevenue) =
  linearDepreciation firstRevenue undepLength depLength

daysToNDT :: C.Days -> NDT
daysToNDT d = C.fromSeconds $ 60 * 60 * 24 * d

-- |
-- - A very simple linear function for calculating depreciation.
linearDepreciation ::
  -- | The date of first revenue, if applicable
  Maybe C.UTCTime ->
  -- | The number of initial days during which no depreciation occurs
  C.Days ->
  -- | The number of days over which each logged interval will be depreciated
  C.Days ->
  -- | The resulting configured depreciation function.
  DepF
linearDepreciation firstRevenue undepDays depDays =
  let -- length of a number of days as NominalDiffTime
      undepLength = daysToNDT undepDays
      depLength = daysToNDT depDays
      -- The percentage of the depreciation period that the end of the interval
      -- represents.
      depPercentage :: NDT -> Rational
      depPercentage intervalAge =
        if intervalAge < undepLength
          then 1
          else max 0 (1 - (C.toSeconds (intervalAge ^-^ undepLength) / C.toSeconds depLength))
   in \payoutDate ival ->
        let ivalEnd = case firstRevenue of
              -- if the end of the interval was before first revenue, count it as
              -- having ended at the first revenue date for the purpose of depreciation
              Just dt -> max dt (ival ^. end)
              Nothing -> ival ^. end
         in depPercentage (payoutDate .-. ivalEnd) *^ ilen ival

-- |
-- - Given a depreciation function, the "current" time, and a foldable functor of log intervals,
-- - produce the total length and depreciated length of work to be credited to an recipient.
workCredit :: (Foldable f, HasEventTime le) => DepF -> C.UTCTime -> f (Interval le) -> (NDT, NDT)
workCredit depf payoutDate ivals =
  let intervalCredit ival = (Sum . ilen &&& Sum . depf payoutDate) $ fmap (view eventTime) ival
   in bimap getSum getSum $ F.foldMap intervalCredit ivals

-- |
-- - Payouts are determined by computing a depreciated duration value for
-- - each work interval. This function computes the percentage of the total
-- - work allocated to each unique CreditTo.
payouts :: forall le. (HasEventTime le) => DepF -> C.UTCTime -> WorkIndex le -> WorkShares
payouts depf payoutDate (WorkIndex widx) =
  let addIntervalDiff :: (Foldable f) => NDT -> f (Interval le) -> (NDT, WorkShare ())
      addIntervalDiff total ivals =
        let (logged, depreciated) = workCredit depf payoutDate ivals
         in (total ^+^ depreciated, WorkShare logged depreciated ())
      (totalTime, keyTimes) = MS.mapAccum addIntervalDiff zeroV widx
      withShareFraction t = t & wsShare .~ (C.toSeconds (t ^. wsDepreciated) / C.toSeconds totalTime)
   in WorkShares totalTime (fmap withShareFraction keyTimes)

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
          case extension (view (event . leEventTime) <$> ival) logEvent of
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
      (StartWork t, StopWork t')
        | t' > t ->
          -- complete interval found
          Right $ Interval e e'
      (StartWork t, StartWork t') ->
        -- ignore redundant starts
        Left $ if t > t' then e else e'
      (StopWork t, StopWork t') ->
        -- ignore redundant ends
        Left $ if t <= t' then e else e'
      _ -> Left e'
    -- if the interval includes the timestamp of a start event, then allow the extension of the interval
    extension :: (Interval C.UTCTime) -> le -> Maybe le
    extension ival newEvent@(view event -> StartWork t)
      | containsInclusive t ival =
        Just newEvent -- replace the end of the interval with the new event
    extension _ _ =
      Nothing
