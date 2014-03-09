{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Bifunctor as B
import Data.Monoid
import Data.Maybe
import Data.Map.Strict as M
import Control.Monad
import Control.Arrow
import Ananke
import Ananke.TimeLog as L
import Ananke.Interval as I
import Data.Time.ISO8601
import qualified Data.Text as T


main = defaultMain tests

tests :: TestTree
tests = testGroup "Intervals" [unitTests]

unitTests = testGroup "Unit Tests" [testCase "deriveIntervals" deriveIntervalsTest]

deriveIntervalsTest :: Assertion
deriveIntervalsTest = let 
  testAddrs = catMaybes [ parseBtcAddr $ T.pack "123"
                        , parseBtcAddr $ T.pack "456"
                        , parseBtcAddr $ T.pack "789" ]

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

  in assertEqual "derive log entries" (intervals testLogEntries) expected



