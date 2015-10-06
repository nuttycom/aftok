{-# LANGUAGE TemplateHaskell #-}

module Aftok.Auction where

import ClassyPrelude
import Control.Lens
import Data.Hourglass
import Data.UUID
import Data.Thyme.Clock as C
import Network.Bitcoin

import Aftok

newtype AuctionId = AuctionId UUID deriving (Show, Eq)
makePrisms ''AuctionId

data Auction = Auction 
  { _raiseAmount :: BTC
  , _auctionEnd :: C.UTCTime 
  }
makeLenses ''Auction

newtype BidId = BidId UUID deriving (Show, Eq)
makePrisms ''BidId

data Bid = Bid
  { _bidUser    :: UserId
  , _bidSeconds :: Seconds
  , _bidAmount  :: BTC
  , _bidTime    :: C.UTCTime
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
          x : takeWinningBids (total + (x ^. bidAmount)) xs

        -- if the last bid will exceed the raise amount, reduce it to fit
        | total < (auction ^. raiseAmount) = 
          let remainder = (auction ^. raiseAmount) - total
              winFraction = toRational $ remainder / (x ^. bidAmount)
              remainderSeconds = Seconds . round $ winFraction * toRational (x ^. bidSeconds)

          in  [x & bidSeconds .~ remainderSeconds & bidAmount .~ remainder]

        | otherwise = []
        
      takeWinningBids _ [] = []
  in  takeWinningBids 0 $ sort bids
