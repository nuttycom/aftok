module Aftok.AuctionSpec
  ( main,
    spec,
  )
where

import Aftok.Auction
import Aftok.Generators
import Aftok.Types
import Bippy.Test.Types (arbitrarySatoshi)
import Bippy.Types (Satoshi (..))
import Control.Lens
import Data.Hourglass
import Data.List ((!!))
import Data.Thyme.Clock ()
import qualified Data.UUID.V4 as U
import Haskoin.Constants (btc)
import Test.HUnit.Base (assertFailure)
import Test.Hspec
import Test.QuickCheck
import Text.Read (read)

genBid :: Gen (Bid Satoshi)
genBid =
  Bid
    <$> (UserId <$> genUUID)
    <*> (Seconds <$> arbitrary `suchThat` (>= 0))
    <*> arbitrarySatoshi btc
    `suchThat` (> Satoshi 0)
    <*> arbitrary

subs :: Satoshi -> Satoshi -> Satoshi
subs (Satoshi a) (Satoshi b) = Satoshi (a - b)

spec :: Spec
spec = do
  users <- runIO $ fmap UserId <$> replicateM 5 U.nextRandom
  let testB0 =
        Bid
          (users !! 0)
          (Seconds 3)
          (Satoshi 100)
          (read "2016-03-05 15:59:20.000000 UTC")
      testB1 =
        Bid
          (users !! 1)
          (Seconds 60)
          (Satoshi 1000)
          (read "2016-03-05 15:59:21.000000 UTC")
      testB2 =
        Bid
          (users !! 2)
          (Seconds 60)
          (Satoshi 100)
          (read "2016-03-05 15:59:22.000000 UTC")
      testB3 =
        Bid
          (users !! 3)
          (Seconds 90)
          (Satoshi 100)
          (read "2016-03-05 15:59:23.000000 UTC")
      testB4 =
        Bid
          (users !! 4)
          (Seconds 60)
          (Satoshi 100)
          (read "2016-03-05 15:59:24.000000 UTC")
  describe "bid ordering" $ do
    it "ensures that bids with lowest seconds/btc ratio are first" $ do
      bidOrder testB0 testB1 `shouldBe` LT
      bidOrder testB1 testB2 `shouldBe` LT
      bidOrder testB2 testB3 `shouldBe` LT
    it "ensures breaks ties in bid ordering by timestamp" $ do
      bidOrder testB2 testB4 `shouldBe` LT
  describe "winning bids" $ do
    it "determines a sufficient number of winners to fulfill the raise amount" $
      let result =
            runAuction' (Satoshi 1250) [testB0, testB1, testB2, testB3, testB4]
          split =
            Bid (users !! 4) (Seconds 31) (Satoshi 50) (testB4 ^. bidTime)
          expected = sortBy bidOrder [testB0, testB1, testB2, split]
       in case result of
            WinningBids winners -> sortBy bidOrder winners `shouldBe` expected
            InsufficientBids _ ->
              assertFailure
                "Sufficinent bids were presented, but auction algorithm asserted otherwise."
    it "ensures that the raise amount is fully consumed by the winning bids" $
      forAll ((,) <$> arbitrarySatoshi btc <*> listOf genBid) $
        \(raiseAmount', bids) -> case runAuction' raiseAmount' bids of
          WinningBids xs -> bidsTotal xs == raiseAmount'
          InsufficientBids t -> t == (raiseAmount' `subs` bidsTotal bids)

main :: IO ()
main = hspec spec
