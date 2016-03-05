{-# LANGUAGE TemplateHaskell #-}

module Aftok.Auction where

import           ClassyPrelude
import           Control.Lens
import           Data.Hourglass
import           Data.Thyme.Clock as C
import           Data.UUID

import           Aftok
import           Aftok.Types

newtype AuctionId = AuctionId UUID deriving (Show, Eq)
makePrisms ''AuctionId

data Auction = Auction
  { _raiseAmount :: Satoshi
  , _auctionEnd  :: C.UTCTime
  }
makeLenses ''Auction

newtype BidId = BidId UUID deriving (Show, Eq)
makePrisms ''BidId

data Bid = Bid
  { _bidUser    :: UserId
  , _bidSeconds :: Seconds
  , _bidAmount  :: Satoshi
  , _bidTime    :: C.UTCTime
  } deriving Eq
makeLenses ''Bid

bidOrder :: Bid -> Bid -> Ordering
bidOrder =
  comparing costRatio
  where
    secs bid = toRational $ bid ^. bidSeconds
    btc  bid = toRational $ bid ^. bidAmount
    costRatio bid = secs bid / btc bid

-- lowest bids of seconds/btc win
winningBids :: Auction -> [Bid] -> [Bid]
winningBids auction = winningBids' (auction ^. raiseAmount) 

winningBids' :: Satoshi -> [Bid] -> [Bid]
winningBids' raiseAmount' bids = 
  let takeWinningBids :: Satoshi -> [Bid] -> [Bid]
      takeWinningBids total (x : xs)
        -- if the total is fully within the raise amount
        | total + (x ^. bidAmount) < raiseAmount' =
          x : takeWinningBids (total + (x ^. bidAmount)) xs

        -- if the last bid will exceed the raise amount, reduce it to fit
        | total < raiseAmount' =
          let remainder = raiseAmount' - total
              winFraction = toRational remainder / toRational (x ^. bidAmount)
              remainderSeconds = Seconds . round $ winFraction * toRational (x ^. bidSeconds)

          in  [x & bidSeconds .~ remainderSeconds & bidAmount .~ remainder]

        | otherwise = []

      takeWinningBids _ [] = []
  in  takeWinningBids 0 $ sortBy bidOrder bids
