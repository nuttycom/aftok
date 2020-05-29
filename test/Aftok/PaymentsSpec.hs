{-# OPTIONS_GHC -Wwarn -fno-warn-orphans #-}

module Aftok.PaymentsSpec
  ( main
  , spec
  )
where


import           Test.Hspec

spec :: Spec
spec = do
  describe "finding unbilled dates" $ do
    pure ()
      --it "returns the billing date in the presence of an expired payment request" $
      --  forAll ((,) <$> genSatoshi <*> listOf genBid) $
      --    \(raiseAmount', bids) ->
      --      case runAuction' raiseAmount' bids of
      --        WinningBids xs     -> bidsTotal xs == raiseAmount'
      --        InsufficientBids t -> t == (raiseAmount' - bidsTotal bids)

main :: IO ()
main = hspec spec

