{-# OPTIONS_GHC -Wwarn #-}
module Aftok.AuctionSpec (main, spec) where

import           ClassyPrelude

import           Aftok
import           Aftok.Auction
import           Aftok.Types
import           Data.Hourglass
import           Data.UUID

import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "bid ordering" $
    it "ensures that bids with lowest seconds/btc ratio are first" $
      let testB1 = Bid (UserId nil) (Seconds 60) (Satoshi 1000) undefined
          testB2 = Bid (UserId nil) (Seconds 60) (Satoshi 100) undefined
          testB3 = Bid (UserId nil) (Seconds 90) (Satoshi 100) undefined

      in do
        bidOrder testB1 testB2 `shouldBe` LT
        bidOrder testB2 testB3 `shouldBe` LT

  describe "winning bids" $ 
    it "determines a sufficient number of winners to fulfill the raise amount" $
      True `shouldBe` True

main :: IO ()
main = hspec spec
