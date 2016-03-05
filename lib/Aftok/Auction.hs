{-# LANGUAGE TemplateHaskell #-}

module Aftok.Auction where

import           ClassyPrelude
import           Control.Lens
import           Data.Hourglass
import           Data.Thyme.Clock  as C
import           Data.Thyme.Format ()
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
  } deriving (Eq, Show)
makeLenses ''Bid

data AuctionResult
  = WinningBids [Bid]
  | InsufficientBids Satoshi
  deriving (Show, Eq)

bidsTotal :: [Bid] -> Satoshi
bidsTotal bids =
  foldl' (\s b -> s + (b^.bidAmount)) (Satoshi 0) bids

bidOrder :: Bid -> Bid -> Ordering
bidOrder =
  comparing costRatio `mappend` comparing (^. bidTime)
  where
    secs bid = toRational $ bid ^. bidSeconds
    btc  bid = toRational $ bid ^. bidAmount
    costRatio bid = secs bid / btc bid

-- lowest bids of seconds/btc win
runAuction :: Auction -> [Bid] -> AuctionResult
runAuction auction = runAuction' (auction ^. raiseAmount)

runAuction' :: Satoshi -> [Bid] -> AuctionResult
runAuction' raiseAmount' bids =
  let takeWinningBids :: Satoshi -> [Bid] -> [Bid]
      takeWinningBids total (bid : xs)
        -- if the total is fully within the raise amount
        | total + (bid ^. bidAmount) < raiseAmount' =
          bid : takeWinningBids (total + (bid ^. bidAmount)) xs

        -- if the last bid will exceed the raise amount, reduce it to fit
        | total < raiseAmount' =
          let remainder = raiseAmount' - total
              winFraction = toRational remainder / toRational (bid ^. bidAmount)
              remainderSeconds = Seconds . round $ winFraction * toRational (bid ^. bidSeconds)

          in  [bid & bidSeconds .~ remainderSeconds & bidAmount .~ remainder]

        | otherwise = []

      takeWinningBids _ [] = []

      submittedTotal = bidsTotal bids
  in  if submittedTotal >= raiseAmount'
        then WinningBids $ takeWinningBids 0 $ sortBy bidOrder bids
        else InsufficientBids (raiseAmount' - submittedTotal)

