{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Ananke.TimeLogSpec where

import Test.Hspec
import Ananke
import Ananke.Interval as I
import Ananke.TimeLog as L

import Control.Arrow
import Control.Monad
import Data.Bifunctor as B
import Data.Maybe
import Data.Monoid
import Data.Map.Strict as M
import qualified Data.Text as T
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
            (start, end) <- zip starts ends
            [ LogEntry addr (StartWork start), LogEntry addr (StopWork end) ]

          testIntervals :: [LogInterval]
          testIntervals = do
            addr <- testAddrs
            (start, end) <- zip starts ends
            return $ LogInterval addr (I.interval start end)

          expected0 :: Map BtcAddr ([LogEntry], [LogInterval])
          expected0 = M.map (const [] &&& id) . fromListWith (++) . fmap (intervalBtcAddr &&& return) $ testIntervals

          expected :: WorkIndex
          expected = M.map (bimap (fmap event) (fmap workInterval)) expected0

      (intervals testLogEntries) `shouldBe` expected

main :: IO ()
main = hspec spec


