import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.Monoid
import Data.Maybe
import Control.Monad
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
  testAddrs = catMaybes [ Ananke.btcAddr $ T.pack "123"
                        , Ananke.btcAddr $ T.pack "456"
                        , Ananke.btcAddr $ T.pack "789" ]

  starts    = catMaybes [ parseISO8601 "2014-01-01T00:08:00Z"
                        , parseISO8601 "2014-02-12T00:12:00Z" ]

  ends      = catMaybes [ parseISO8601 "2014-01-01T00:12:00Z"
                        , parseISO8601 "2014-02-12T00:18:00Z" ]
  testLogEntries = do
    addr <- testAddrs
    (start, end) <- zip starts ends
    [ LogEntry addr start StartWork, LogEntry addr end StopWork ]
  expected = do
    addr <- testAddrs
    (start, end) <- zip starts ends
    maybeToList . fmap (LogInterval addr) $ I.interval start end

  in assertEqual "derive log entries" (intervals testLogEntries) expected
