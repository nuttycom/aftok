{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn -fno-warn-orphans #-}

module Aftok.TimeLogSpec
  ( main,
    spec,
  )
where

import Aftok.Generators (genUUID)
import qualified Aftok.Interval as I
import Aftok.TimeLog
import Aftok.Types (DepreciationFunction (..), DepreciationRules (..), UserId (..))
import Control.Lens (to, view, (^.))
import Data.AffineSpace ((.+^))
import Data.List (head, tail)
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import qualified Data.Thyme.Clock as C
import qualified Data.Thyme.Time as C
import Data.Time.ISO8601
import qualified Data.UUID as U
import Test.Hspec
import Test.QuickCheck (Gen, arbitrary, choose, forAll, listOf, sample', suchThat)
import Prelude hiding (head, tail)

-- genInterval :: Gen I.Interval
-- genInterval = do
--   startTime <- arbitrary
--   delta     <- arbitrary :: Gen (Positive C.NominalDiffTime)
--   pure $ I.interval startTime (startTime .+^ getPositive delta)

genIntervals :: Gen (L.NonEmpty (I.Interval C.UTCTime))
genIntervals =
  let deltas =
        fmap C.fromSeconds
          <$> ((listOf $ choose (0, 72 * 60 * 60)) :: Gen [Int])
      buildIntervals :: C.UTCTime -> [C.NominalDiffTime] -> [I.Interval C.UTCTime]
      buildIntervals t (d : s : dx)
        | d > 0 =
            let ival = I.interval t (t .+^ d)
             in ival : buildIntervals (ival ^. I.end .+^ s) dx
      buildIntervals _ _ = []
   in do
        startTime <- arbitrary
        intervals <- suchThat (buildIntervals startTime <$> deltas) (not . null)
        pure $ L.fromList intervals

genWorkIndex :: Gen (WorkIndex C.UTCTime)
genWorkIndex =
  let recordGen :: Gen (CreditTo, L.NonEmpty (I.Interval C.UTCTime))
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
            C.toThyme
              <$> catMaybes
                [ parseISO8601 "2014-01-01T00:08:00Z",
                  parseISO8601 "2014-01-01T00:12:00Z"
                ]
          ends =
            C.toThyme
              <$> catMaybes
                [ parseISO8601 "2014-01-01T00:11:59Z",
                  parseISO8601 "2014-01-01T00:18:00Z"
                ]
          testIntervals :: [(CreditTo, I.Interval C.UTCTime)]
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
          actual = view eventTime <$> workIndex id testLogEntries
      actual `shouldBe` expected
    it "recovers a work index from events" $
      forAll genWorkIndex $
        \(WorkIndex widx) ->
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
              expected = (WorkIndex $ fmap (L.reverse . L.sort) widx')
              actual = view eventTime <$> workIndex id logEntries
           in actual `shouldBe` expected
    it "computes correct work shares" $ do
      [u0, u1, u2] <- fmap CreditToUser . take 3 <$> sample' (UserId <$> genUUID)
      let initTime = C.toThyme . fromJust $ parseISO8601 "2014-01-01T00:08:00Z"
          len = fromInteger @C.NominalDiffTime 3600
          timestamps = iterate (.+^ len) initTime
          intervals =
            fmap (uncurry I.Interval . snd)
              . filter (\i -> fst i `mod` 2 == 0)
              $ ([(0 :: Int) ..] `zip` (timestamps `zip` tail timestamps))
          widx =
            WorkIndex $
              M.fromList
                [ (u0, L.fromList $ take 10 intervals),
                  (u1, L.fromList $ take 30 intervals),
                  (u2, L.fromList $ take 120 intervals)
                ]
          -- for this test we'll be entirely within undepreciated period
          depf = toDepF $ DepreciationRules (LinearDepreciation 180 1800) Nothing
          evalTime = I._start . head $ drop 120 intervals
          shares = payouts depf evalTime widx
      (shares ^. loggedTotal `shouldBe` (fromInteger @C.NominalDiffTime (3600 * 160)))
      (shares ^. creditToShares . to (fromJust . M.lookup u0) . wsShare) `shouldBe` (10 % 160)
      (shares ^. creditToShares . to (fromJust . M.lookup u1) . wsShare) `shouldBe` (30 % 160)
      (shares ^. creditToShares . to (fromJust . M.lookup u2) . wsShare) `shouldBe` (120 % 160)
    it "correctly handles fully depreciated work intervals" $ do
      now <- C.getCurrentTime
      let depf = toDepF $ DepreciationRules (LinearDepreciation 6 2) Nothing
          raw =
            [ ("b3ff64b7-6699-45f2-acee-38751325bf46", StartWork, "2021-02-09T15:52:13.434308+00"),
              ("b3ff64b7-6699-45f2-acee-38751325bf46", StopWork, "2021-02-09T16:12:32.936579+00"),
              ("d56ae5bd-8892-44c6-9a02-f6a8aca8636e", StartWork, "2021-02-09T16:23:10.637749+00"),
              ("d56ae5bd-8892-44c6-9a02-f6a8aca8636e", StopWork, "2021-02-09T16:27:00.082747+00"),
              ("d56ae5bd-8892-44c6-9a02-f6a8aca8636e", StartWork, "2021-02-09T16:29:10.119337+00"),
              ("d56ae5bd-8892-44c6-9a02-f6a8aca8636e", StopWork, "2021-02-09T18:54:26.778107+00")
            ]
          toEvent :: (String, C.UTCTime -> LogEvent, String) -> Maybe LogEntry
          toEvent (uuid, f, t) =
            LogEntry
              <$> (CreditToUser . UserId <$> U.fromString uuid)
              <*> (f . C.toThyme <$> parseISO8601 t)
              <*> pure Nothing
          events = catMaybes $ fmap toEvent raw
          widx = workIndex (view event) events
          p = payouts depf now widx
      p `shouldBe` WorkShares 0 M.empty

  describe "depreciation functions" $ do
    it "computes linear depreciation" $ do
      let depf fr = linearDepreciation fr 10 100
          hour = fromInteger (60 * 60)
          t0 :: C.UTCTime = C.toThyme . fromJust $ parseISO8601 "2014-01-01T00:08:00Z"
          ival = I.Interval (t0 .+^ negate hour) t0
          t1 = t0 .+^ daysToNDT 5
          t2 = t0 .+^ daysToNDT 10
          t3 = t0 .+^ daysToNDT 20
          t4 = t0 .+^ daysToNDT 60
          t5 = t0 .+^ daysToNDT 110
      daysToNDT 1 `shouldBe` (60 * 60 * 24)
      -- undepreciated; within the first 10 days
      depf Nothing t1 ival `shouldBe` 3600
      -- still undepreciated if it ends at the last depreciation moment
      depf Nothing t2 ival `shouldBe` 3600
      -- depreciated by 10% of its value
      depf Nothing t3 ival `shouldBe` 3240
      -- depreciated by 50% of its value
      depf Nothing t4 ival `shouldBe` 1800
      -- depreciated by 100% of its value
      depf Nothing t5 ival `shouldBe` 0
      -- undepreciated - before first revenue
      depf (Just t3) t1 ival `shouldBe` 3600
      -- undepreciated - before first revenue
      depf (Just t3) t2 ival `shouldBe` 3600
      -- depreciated by 10% of its value
      depf (Just t3) t3 ival `shouldBe` 3600
      -- depreciated by 30% of its value
      depf (Just t3) t4 ival `shouldBe` 2520
      -- depreciated by 80% of its value
      depf (Just t3) t5 ival `shouldBe` 720

main :: IO ()
main = hspec spec
