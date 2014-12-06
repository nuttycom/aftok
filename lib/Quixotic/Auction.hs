{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}

module Quixotic.Auction where

import ClassyPrelude
import Data.Group
import Data.Hourglass
import Quixotic
import Quixotic.Users

newtype AuctionId = AuctionId Int

data Auction = Auction 
  { raiseAmount :: BTC
  , endsAt :: UTCTime 
  }

data Bid = Bid
  { userId :: UserId
  , seconds :: Seconds
  , btcAmount :: BTC
  } deriving Eq

instance Ord Bid where
  (<=) b1 b2 = 
    costRatio b1 <= costRatio b2
    where costRatio bid = (toRational . seconds $ bid) / (toRational . runBTC . btcAmount $ bid) 

-- lowest bids of seconds/btc win
winningBids :: Auction -> [Bid] -> [Bid]
winningBids auction bids = 
  let takeWinningBids :: BTC -> [Bid] -> [Bid]
      takeWinningBids total (x : xs)
        -- if the total is fully within the raise amount
        | total ++ btcAmount x < raiseAmount auction = 
          x : (takeWinningBids (total ++ btcAmount x) xs)

        -- if the last bid will exceed the raise amount, reduce it to fit
        | total < raiseAmount auction = 
          let remainder = raiseAmount auction <> invert total
              winFraction = (toRational . runBTC $ remainder) / (toRational . runBTC $ btcAmount x)
              remainderSeconds = Seconds . round $ winFraction * (toRational . seconds $ x)
          in  [Bid (userId x) remainderSeconds remainder]

        | otherwise = []
        
      takeWinningBids _ [] = []
  in  takeWinningBids mempty $ sort bids
