module Aftok.AuctionSpec (main, spec) where

import           ClassyPrelude

import           Control.Lens
import           Data.Hourglass
import           Data.Thyme.Clock ()
import           Data.UUID
import           Text.Read        (read)

import           Aftok
import           Aftok.Auction
import           Aftok.Generators
import           Aftok.Types


import           Test.Hspec
import           Test.HUnit.Base  (assertFailure)
import           Test.QuickCheck

genBid :: Gen Bid
genBid = Bid <$> (UserId <$> genUUID)
             <*> (Seconds <$> arbitrary `suchThat` (>= 0))
             <*> genSatoshi `suchThat` (> Satoshi 0)
             <*> arbitrary

spec :: Spec
spec =
  let testB0 = Bid (UserId nil) (Seconds 3)  (Satoshi 100) (read "2016-03-05 15:59:26.033176 UTC")
      testB1 = Bid (UserId nil) (Seconds 60) (Satoshi 1000)(read "2016-03-05 15:59:26.033177 UTC")
      testB2 = Bid (UserId nil) (Seconds 60) (Satoshi 100) (read "2016-03-05 15:59:26.033178 UTC")
      testB3 = Bid (UserId nil) (Seconds 90) (Satoshi 100) (read "2016-03-05 15:59:26.033179 UTC")
      testB4 = Bid (UserId nil) (Seconds 60) (Satoshi 100) (read "2016-03-05 15:59:26.033180 UTC")
  in do
    describe "bid ordering" $ do
      it "ensures that bids with lowest seconds/btc ratio are first" $ do
        bidOrder testB0 testB1 `shouldBe` LT
        bidOrder testB1 testB2 `shouldBe` LT
        bidOrder testB2 testB3 `shouldBe` LT

      it "ensures breaks ties in bid ordering by timestamp" $ do
        bidOrder testB2 testB4 `shouldBe` LT

    describe "winning bids" $ do
      it "determines a sufficient number of winners to fulfill the raise amount" $
        let result   = runAuction' (Satoshi 1250) [testB0, testB1, testB2, testB3, testB4]
            split    = Bid (UserId nil) (Seconds 30) (Satoshi 50) (testB4 ^. bidTime)
            expected = sortBy bidOrder [testB0, testB1, testB2, split]
        in  case result of
          WinningBids winners ->
            sortBy bidOrder winners `shouldBe` expected

          InsufficientBids _  ->
            assertFailure "Sufficinent bids were presented, but auction algorithm asserted otherwise."

      it "ensures that the raise amount is fully consumed by the winning bids" $
        forAll ((,) <$> genSatoshi <*> listOf genBid) $
          \(raiseAmount', bids) ->
            case runAuction' raiseAmount' bids of
              WinningBids xs     -> bidsTotal xs == raiseAmount'
              InsufficientBids t -> t == (raiseAmount' - bidsTotal bids)

main :: IO ()
main = hspec spec
