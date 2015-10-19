{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wwarn -fno-warn-orphans #-}

module Aftok.TimeLogSpec (main, spec) where

import           ClassyPrelude

import           Control.Lens       ((^.))
import           Data.AffineSpace
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict    as M
import           Data.Thyme.Time    as T
import           Data.Time.ISO8601

import           Aftok
import           Aftok.Interval     as I
import           Aftok.TimeLog

import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary BtcAddr where
  arbitrary = BtcAddr . pack <$> vectorOf 34 arbitrary

instance Arbitrary Interval where
  arbitrary = do
    startTime <- arbitrary
    delta <- arbitrary :: Gen (Positive T.NominalDiffTime)
    pure $ I.interval startTime (startTime .+^ getPositive delta)

newtype Intervals = Intervals (L.NonEmpty Interval)

buildIntervals :: T.UTCTime -> [NominalDiffTime] -> [Interval]
buildIntervals t (d : s : dx) | d > 0 =
  let ival = I.interval t (t .+^ d)
  in  ival : buildIntervals (ival ^. end .+^ s) dx
buildIntervals _ _ = []

instance Arbitrary Intervals where
  arbitrary = do
    startTime <- arbitrary
    let deltas = fmap T.fromSeconds <$> ((listOf $ choose (0, 72 * 60 * 60)) :: Gen[Int])
    intervals <- suchThat (buildIntervals startTime <$> deltas) (not.null)
    pure . Intervals $ L.fromList intervals

instance Arbitrary WorkIndex where
  arbitrary =
    let record = do addr <- arbitrary
                    Intervals ivals <- arbitrary
                    pure (addr, ivals)
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
            [ parseISO8601 "2014-01-01T00:11:59Z"
            , parseISO8601 "2014-01-01T00:18:00Z" ]

          testIntervals :: [(BtcAddr, Interval)]
          testIntervals = do
            addr <- testAddrs
            (start', end') <- ClassyPrelude.zip starts ends
            pure $ (addr, I.interval start' end')

          testLogEntries :: [LogEntry]
          testLogEntries = do
            (addr, Interval start' end') <- testIntervals
            LogEntry addr <$> [StartWork start', StopWork end'] <*> [Nothing]

          expected' = M.fromListWith (<>) $ fmap (second pure) testIntervals
          expected  = WorkIndex $ fmap (L.reverse . L.sort) expected'

      in (workIndex testLogEntries) `shouldBe` expected

    it "recovers a work index from events" $ property $
      \(WorkIndex widx) ->
        let mergeAdjacent ((Interval s e) : (Interval s' e') : xs) | e == s' = mergeAdjacent $ Interval s e' : xs
            mergeAdjacent (x : xs) = x : mergeAdjacent xs
            mergeAdjacent [] = []

            ivalEntries addr ival = LogEntry addr <$> [StartWork (ival ^. start), StopWork (ival ^. end)]
                                                  <*> [Nothing]

            acc k a b = b ++ (L.toList a >>= ivalEntries k)

            widx' = fmap (L.fromList . mergeAdjacent . sortOn I._start . L.toList) widx
            logEntries = M.foldrWithKey acc [] widx
        in  workIndex logEntries `shouldBe` (WorkIndex $ fmap (L.reverse . L.sort) widx')

main :: IO ()
main = hspec spec
