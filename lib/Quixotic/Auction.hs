{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Auction where

import ClassyPrelude
import Control.Lens
import Data.Hourglass
import Data.UUID
import Network.Bitcoin

import Quixotic

newtype AuctionId = AuctionId UUID deriving (Show, Eq)
makePrisms ''AuctionId

data Auction = Auction 
  { _raiseAmount :: BTC
  , _auctionEnd :: UTCTime 
  }
makeLenses ''Auction

data Bid = Bid
  { _bidUser    :: UserId
  , _bidSeconds :: Seconds
  , _bidAmount  :: BTC
  , _bidTime    :: UTCTime
  } deriving Eq
makeLenses ''Bid

instance Ord Bid where
  (<=) b1 b2 = 
    costRatio b1 <= costRatio b2
    where 
      secs bid = toRational $ bid ^. bidSeconds
      btc  bid = toRational $ bid ^. bidAmount
      costRatio bid = secs bid / btc bid

-- lowest bids of seconds/btc win
winningBids :: Auction -> [Bid] -> [Bid]
winningBids auction bids = 
  let takeWinningBids :: BTC -> [Bid] -> [Bid]
      takeWinningBids total (x : xs)
        -- if the total is fully within the raise amount
        | total + (x ^. bidAmount) < (auction ^. raiseAmount) = 
          x : (takeWinningBids (total + (x ^. bidAmount)) xs)

        -- if the last bid will exceed the raise amount, reduce it to fit
        | total < (auction ^. raiseAmount) = 
          let remainder = (auction ^. raiseAmount) - total
              winFraction = toRational $ remainder / (x ^. bidAmount)
              remainderSeconds = Seconds . round $ winFraction * (toRational $ x ^. bidSeconds)

          in  [x & bidSeconds .~ remainderSeconds & bidAmount .~ remainder]

        | otherwise = []
        
      takeWinningBids _ [] = []
  in  takeWinningBids (fromInteger 0) $ sort bids
