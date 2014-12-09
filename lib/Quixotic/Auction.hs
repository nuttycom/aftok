{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude, TemplateHaskell #-}

module Quixotic.Auction where

import ClassyPrelude
import Control.Lens
import Data.Group
import Data.Hourglass
import Quixotic
import Quixotic.Users

newtype AuctionId = AuctionId Int deriving (Show, Eq)

data Auction = Auction 
  { _raiseAmount :: BTC
  , _endsAt :: UTCTime 
  }

makeLenses ''Auction

data Bid = Bid
  { _userId :: UserId
  , _seconds :: Seconds
  , _btcAmount :: BTC
  } deriving Eq

makeLenses ''Bid

instance Ord Bid where
  (<=) b1 b2 = 
    costRatio b1 <= costRatio b2
    where 
      bidSeconds bid = toRational $ bid ^. seconds
      bidAmount  bid = toRational $ bid ^. (btcAmount . btc)
      costRatio  bid = bidSeconds bid / bidAmount bid

-- lowest bids of seconds/btc win
winningBids :: Auction -> [Bid] -> [Bid]
winningBids auction bids = 
  let takeWinningBids :: BTC -> [Bid] -> [Bid]
      takeWinningBids total (x : xs)
        -- if the total is fully within the raise amount
        | (total ++ x ^. btcAmount) < (auction ^. raiseAmount) = 
          x : (takeWinningBids (total ++ x ^. btcAmount) xs)

        -- if the last bid will exceed the raise amount, reduce it to fit
        | total < auction ^. raiseAmount = 
          let remainder = (auction ^. raiseAmount) ++ invert total
              winFraction :: Rational
              winFraction = (toRational $ remainder ^. btc) / (toRational $ x ^. (btcAmount . btc))
              remainderSeconds = Seconds . round $ winFraction * (toRational $ x ^. seconds)

          in  [x & seconds .~ remainderSeconds & btcAmount .~ remainder]

        | otherwise = []
        
      takeWinningBids _ [] = []
  in  takeWinningBids mempty $ sort bids
