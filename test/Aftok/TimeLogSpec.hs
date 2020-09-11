{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wwarn -fno-warn-orphans #-}

module Aftok.TimeLogSpec
  ( main,
    spec,
  )
where

import Aftok.Generators (genUUID)
import qualified Aftok.Interval as I
import Aftok.TimeLog
import Aftok.Types (UserId (..))
import Control.Lens ((^.))
import Data.AffineSpace
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as M
import Data.Thyme.Time as T
import Data.Time.ISO8601
import Test.Hspec
import Test.QuickCheck

-- genInterval :: Gen I.Interval
-- genInterval = do
--   startTime <- arbitrary
--   delta     <- arbitrary :: Gen (Positive T.NominalDiffTime)
--   pure $ I.interval startTime (startTime .+^ getPositive delta)

genIntervals :: Gen (L.NonEmpty I.Interval)
genIntervals =
  let deltas =
        fmap T.fromSeconds
          <$> ((listOf $ choose (0, 72 * 60 * 60)) :: Gen [Int])
      buildIntervals :: T.UTCTime -> [NominalDiffTime] -> [I.Interval]
      buildIntervals t (d : s : dx)
        | d > 0 =
          let ival = I.interval t (t .+^ d)
           in ival : buildIntervals (ival ^. I.end .+^ s) dx
      buildIntervals _ _ = []
   in do
        startTime <- arbitrary
        intervals <- suchThat (buildIntervals startTime <$> deltas) (not . null)
        pure $ L.fromList intervals

genWorkIndex :: Gen WorkIndex
genWorkIndex =
  let recordGen :: Gen (CreditTo, L.NonEmpty I.Interval)
      recordGen = do
        uid <- UserId <$> genUUID
        ivals <- genIntervals
        pure (CreditToUser uid, ivals)
   in WorkIndex . M.fromList <$> listOf recordGen

spec :: Spec
spec = do
  describe "log reduction to intervals" $ do
    it "reduces a log to a work index" $ do
      testUsers <- take 3 <$> sample' (UserId <$> genUUID)
      let starts =
            toThyme
              <$> catMaybes
                [ parseISO8601 "2014-01-01T00:08:00Z",
                  parseISO8601 "2014-01-01T00:12:00Z"
                ]
          ends =
            toThyme
              <$> catMaybes
                [ parseISO8601 "2014-01-01T00:11:59Z",
                  parseISO8601 "2014-01-01T00:18:00Z"
                ]
          testIntervals :: [(CreditTo, I.Interval)]
          testIntervals = do
            user <- testUsers
            (start', end') <- zip starts ends
            pure $ (CreditToUser user, I.interval start' end')
          testLogEntries :: [LogEntry]
          testLogEntries = do
            (addr, I.Interval start' end') <- testIntervals
            LogEntry addr <$> [StartWork start', StopWork end'] <*> [Nothing]
          expected' = M.fromListWith (<>) $ fmap (second pure) testIntervals
          expected = WorkIndex $ fmap (L.reverse . L.sort) expected'
      (workIndex testLogEntries) `shouldBe` expected
    it "recovers a work index from events"
      $ forAll genWorkIndex
      $ \(WorkIndex widx) ->
        let mergeAdjacent ((I.Interval s e) : (I.Interval s' e') : xs)
              | e == s' = mergeAdjacent $ I.Interval s e' : xs
            mergeAdjacent (x : xs) = x : mergeAdjacent xs
            mergeAdjacent [] = []
            ivalEntries addr ival =
              LogEntry addr
                <$> [StartWork (ival ^. I.start), StopWork (ival ^. I.end)]
                <*> [Nothing]
            acc k a b = b ++ (L.toList a >>= ivalEntries k)
            widx' =
              fmap
                (L.fromList . mergeAdjacent . sortOn I._start . L.toList)
                widx
            logEntries = M.foldrWithKey acc [] widx
         in workIndex logEntries
              `shouldBe` (WorkIndex $ fmap (L.reverse . L.sort) widx')

    -- Given a base set of payouts and data
    -- describing existing (Advances) to users, if a user has received an
    -- advance, then their share of the payouts should be reallocated to
    -- the advancing party. Any excess above the advanced amount should
    -- be allocated to the original payee as usual.
    --
    -- The result of this new function should return both the new Payouts
    -- value and information that describes the earned-out advance amounts,
    -- such that the remaining debt can be adjusted accordingly.
    --
    -- newtype Debtor a = Debtor (CreditTo a)
    -- newtype Loaner a = Loaner (CreditTo a)
    -- type Advances a = Map Debtor (Amount, Loaner)

main :: IO ()
main = hspec spec
