import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Data.Maybe
import Control.Monad
import Ananke
import Ananke.TimeLog
import Data.Time.ISO8601
import qualified Data.Text as T

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
    [ LogInterval addr start end ]

  in assertEqual "derive log entries" (intervals testLogEntries) expected

main :: IO ()
main = defaultMainWithOpts
       [testCase "deriveIntervals" deriveIntervalsTest]
       mempty
