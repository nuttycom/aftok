{-# LANGUAGE TemplateHaskell #-}

module Aftok.Auction where

import Aftok.Currency
  ( IsCurrency (..),
  )
import Aftok.Types
  ( ProjectId,
    UserId,
  )
import Control.Lens
import Data.Hourglass (Seconds (..))
import Data.Ratio ((%))
import Data.Thyme.Clock as C
import Data.Thyme.Format ()
import Data.UUID

newtype AuctionId = AuctionId UUID deriving (Show, Eq)

makePrisms ''AuctionId

data Auction c
  = Auction
      { _projectId :: ProjectId,
        _initiator :: UserId,
        _createdAt :: C.UTCTime,
        _name :: Text,
        _description :: Maybe Text,
        _raiseAmount :: c,
        _auctionStart :: C.UTCTime,
        _auctionEnd :: C.UTCTime
      }

makeLenses ''Auction

newtype BidId = BidId UUID deriving (Show, Eq)

makePrisms ''BidId

data Bid c
  = Bid
      { _bidUser :: UserId,
        _bidSeconds :: Seconds,
        _bidAmount :: c,
        _bidTime :: C.UTCTime
      }
  deriving (Eq, Show)

makeLenses ''Bid

data Commitment c
  = Commitment
      { _baseBid :: Bid c,
        _commitmentSeconds :: Seconds,
        _commitmentAmount :: c
      }

data AuctionResult c
  = WinningBids [Bid c]
  | InsufficientBids c
  deriving (Eq)

bidsTotal :: Monoid c => [Bid c] -> c
bidsTotal = foldMap (view bidAmount)

bidOrder ::
  forall c.
  IsCurrency c =>
  Bid c ->
  Bid c ->
  Ordering
bidOrder = comparing costRatio <> comparing (^. bidTime)
  where
    costRatio :: Bid c -> Rational
    costRatio bid = (toRational $ bid ^. bidSeconds) / (toRational $ bid ^. bidAmount . _Units)

-- lowest bids of seconds/btc win
runAuction :: IsCurrency c => Auction c -> [Bid c] -> AuctionResult c
runAuction auction = runAuction' (auction ^. raiseAmount)

runAuction' ::
  forall c.
  IsCurrency c =>
  c ->
  [Bid c] ->
  AuctionResult c
runAuction' raiseAmount' bids =
  let takeWinningBids :: c -> [Bid c] -> [Bid c]
      takeWinningBids total (bid : xs)
        | total <> (bid ^. bidAmount) < raiseAmount' =
          -- if the total is fully within the raise amount
          bid : takeWinningBids (total <> (bid ^. bidAmount)) xs
        | total < raiseAmount' =
          -- if the last bid will exceed the raise amount, reduce it to fit
          let winFraction r =
                (r ^. _Units) % (bid ^. bidAmount . _Units)
              remainderSeconds r =
                Seconds . round $ winFraction r * fromIntegral (bid ^. bidSeconds)
              adjustBid r =
                bid & bidSeconds .~ remainderSeconds r & bidAmount .~ r
           in toList $ adjustBid <$> raiseAmount' `csub` total
        | otherwise =
          []
      takeWinningBids _ [] = []
      submittedTotal = bidsTotal bids
   in maybe
        (WinningBids $ takeWinningBids mempty $ sortBy bidOrder bids)
        InsufficientBids
        (raiseAmount' `csub` submittedTotal)
