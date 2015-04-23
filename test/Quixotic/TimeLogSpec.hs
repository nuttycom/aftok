{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wwarn -fno-warn-orphans #-}

module Quixotic.TimeLogSpec where

import ClassyPrelude

import Control.Lens ((^.))
import Data.AffineSpace
import Data.List.NonEmpty as L 
import Data.Map.Strict as M
import Data.Time.ISO8601
import Data.Thyme.Time as T

import Quixotic
import Quixotic.Interval as I
import Quixotic.TimeLog 

import Test.QuickCheck
import Test.Hspec

instance Arbitrary EventType where
  arbitrary = elements [StartWork, StopWork]

newtype EventLog = EventLog [LogEntry]

instance Arbitrary BtcAddr where
  arbitrary = BtcAddr . pack <$> vectorOf 34 arbitrary

instance Arbitrary Interval where
  arbitrary = do
    startTime <- arbitrary
    delta <- arbitrary :: Gen (Positive T.NominalDiffTime)
    pure $ I.interval startTime (startTime .+^ getPositive delta)

instance Arbitrary WorkIndex where
  arbitrary = 
    let record = (,) <$> arbitrary <*> (L.fromList <$> listOf1 arbitrary)
    in  WorkIndex . M.fromList <$> listOf record

spec :: Spec
spec = do
  describe "log reduction to intervals" $ do
    it "reduces a log to a work index" $ 
      let testAddrs = catMaybes 
            [ parseBtcAddr "123"
            , parseBtcAddr "456"
            , parseBtcAddr "789" ]

          starts = toThyme <$> catMaybes 
            [ parseISO8601 "2014-01-01T00:08:00Z"
            , parseISO8601 "2014-01-01T00:12:00Z" ]

          ends   = toThyme <$> catMaybes 
            [ parseISO8601 "2014-01-01T00:12:00Z"
            , parseISO8601 "2014-01-01T00:18:00Z" ]

          testIntervals :: [(BtcAddr, Interval)]
          testIntervals = do
            addr <- testAddrs
            (start', end') <- ClassyPrelude.zip starts ends
            pure $ (addr, I.interval start' end')

          testLogEntries :: [LogEntry]
          testLogEntries = do
            (addr, Interval start' end') <- testIntervals
            LogEntry addr <$> [WorkEvent StartWork start' Nothing, WorkEvent StopWork end' Nothing]

          expected' = fromListWith (<>) $ fmap (second pure) testIntervals
          expected  = WorkIndex $ fmap (L.reverse . L.sort) expected'

      in (workIndex testLogEntries) `shouldBe` expected

    it "recovers a work index from events" $ property $
      \(WorkIndex widx) -> 
        let ivalEntries addr ival = [ LogEntry addr (WorkEvent StartWork (ival ^. start) Nothing)
                                    , LogEntry addr (WorkEvent StopWork  (ival ^. end) Nothing) ]

            acc k a b = b ++ (L.toList a >>= ivalEntries k)
            logEntries = foldrWithKey acc [] widx
        in  workIndex logEntries `shouldBe` (WorkIndex $ fmap (L.reverse . L.sort) widx)

  describe "EventType serialization" $ do
    it "serialization is invertible" $ property $
      \e -> (nameEvent . eventName) e == Just e

main :: IO ()
main = hspec spec


{--
(fromList [
(BtcAddr "S\187\156\a\SOx\229`[\133a%7o%'XBt\249\226\n\ENQ\SOH\GS<\241WU5{Si\251",
  [ Interval {_start = 1858-11-16 23:59:59.999997 UTC, _end = 1858-11-16 23:59:59.999997 UTC}
  , Interval {_start = 1858-11-16 23:59:59.999998 UTC, _end = 1858-11-17 00:00:00.000002 UTC}]),
(BtcAddr "\138;\GS\132U0\SUB\ESCf[\NAKo`\ACKR[\EMq\b\v\159\184u\ACK&jS#n#?\SI&v",
  [Interval {_start = 1858-11-16 23:59:59.999999 UTC, _end = 1858-11-17 00:00:00 UTC}
  ,Interval {_start = 1858-11-17 00:00:00.000002 UTC, _end = 1858-11-17 00:00:00.000006 UTC}
  ,Interval {_start = 1858-11-16 23:59:59.999994 UTC, _end = 1858-11-16 23:59:59.999998 UTC}])
])
 -}
