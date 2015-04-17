{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NoImplicitPrelude #-}

module Quixotic.TimeLogSpec where

import ClassyPrelude

import Test.Hspec
import Quixotic
import Quixotic.Interval as I
import Quixotic.TimeLog as L

import Data.Map.Strict as M
import Data.Time.ISO8601

spec :: Spec
spec = do
  describe "log reduction to intervals" $ do
    it "reduces a log to a work index" $ do
      let testAddrs = catMaybes [ parseBtcAddr "123"
                                , parseBtcAddr "456"
                                , parseBtcAddr "789" ]

          starts    = catMaybes [ parseISO8601 "2014-01-01T00:08:00Z"
                                , parseISO8601 "2014-02-12T00:12:00Z" ]

          ends      = catMaybes [ parseISO8601 "2014-01-01T00:12:00Z"
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

main :: IO ()
main = hspec spec


