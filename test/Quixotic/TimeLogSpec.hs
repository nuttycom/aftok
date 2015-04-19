{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wwarn -fno-warn-orphans #-}

module Quixotic.TimeLogSpec where

import ClassyPrelude

import Test.Hspec
import Quixotic
import Quixotic.Interval as I
import Quixotic.TimeLog as L

import Data.AffineSpace
import Data.Map.Strict as M
import Data.Time.ISO8601
import Data.Thyme.Clock
import Data.Thyme.Time
import qualified Data.Text as T
import Test.QuickCheck

instance Arbitrary EventType where
  arbitrary = elements [StartWork, StopWork]

newtype EventLog = EventLog [LogEntry]

instance Arbitrary BtcAddr where
  arbitrary = BtcAddr . T.pack <$> vectorOf 34 arbitrary

instance Arbitrary Interval where
  arbitrary = do
    start <- arbitrary
    delta <- arbitrary
    pure $ I.interval start (start .+^ delta)
    
spec :: Spec
spec = do
  describe "log reduction to intervals" $ do
    it "reduces a log to a work index" $ do
      let testAddrs = catMaybes [ parseBtcAddr "123"
                                , parseBtcAddr "456"
                                , parseBtcAddr "789" ]

          starts    = toThyme <$> catMaybes [ parseISO8601 "2014-01-01T00:08:00Z"
                                            , parseISO8601 "2014-02-12T00:12:00Z" ]

          ends      = toThyme <$> catMaybes [ parseISO8601 "2014-01-01T00:12:00Z"
                                            , parseISO8601 "2014-02-12T00:18:00Z" ]

          testLogEntries :: [LogEntry]
          testLogEntries = do
            addr <- testAddrs
            (start', end') <- zip starts ends
            [ LogEntry addr (WorkEvent StartWork start' Nothing), LogEntry addr (WorkEvent StopWork end' Nothing) ]

          testIntervals :: [LogInterval]
          testIntervals = do
            addr <- testAddrs
            (start', end') <- zip starts ends
            return $ LogInterval addr (I.interval start' end')

          expected0 :: Map BtcAddr ([LogEntry], [LogInterval])
          expected0 = fmap (const [] &&& id) . fromListWith (++) . fmap (intervalBtcAddr &&& return) $ testIntervals

          expected :: WorkIndex
          expected = fmap (fmap workInterval . snd) expected0

      (workIndex testLogEntries) `shouldBe` expected

  describe "EventType serialization" $ do
    it "serialization is invertible" $ property $
      \e -> (nameEvent . eventName) e == Just e

main :: IO ()
main = hspec spec


