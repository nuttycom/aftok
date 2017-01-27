{-# LANGUAGE TemplateHaskell #-}

module Aftok.Auction where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.State
import           Data.Hourglass
import           Data.Thyme.Clock    as C
import           Data.Thyme.Format   ()
import           Data.UUID

import           Aftok               (UserId)
import           Aftok.Project       (ProjectId)
import           Aftok.Types         (Satoshi (..))

newtype AuctionId = AuctionId UUID deriving (Show, Eq)
makePrisms ''AuctionId

data Auction = Auction
  { _projectId    :: ProjectId
  , _initiator    :: UserId
  , _createdAt    :: C.UTCTime
  , _raiseAmount  :: Satoshi
  , _auctionStart :: C.UTCTime
  , _auctionEnd   :: C.UTCTime
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

data Commitment = Commitment
  { _baseBid           :: Bid
  , _commitmentSeconds :: Seconds
  , _commitmentAmount  :: Satoshi
  }

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

bidCommitment :: Satoshi -> Bid -> State Satoshi (Maybe Commitment)
bidCommitment raiseAmount' bid = do
  raised <- get
  case raised of
    -- if the total is fully within the raise amount
    x | x + (bid ^. bidAmount) < raiseAmount' ->
      put (x + bid ^. bidAmount) >>
      (pure . Just $ Commitment bid (bid ^. bidSeconds) (bid ^. bidAmount))

    -- if the last bid will exceed the raise amount, reduce it to fit
    x | x < raiseAmount' ->
      let remainder = raiseAmount' - x
          winFraction = toRational remainder / toRational (bid ^. bidAmount)
          remainderSeconds = Seconds . round $ winFraction * toRational (bid ^. bidSeconds)
      in  put (x + remainder) >>
          (pure . Just $ Commitment bid (remainderSeconds) remainder)

    -- otherwise,
    _ -> pure Nothing

