{-# LANGUAGE TemplateHaskell #-}

module Aftok.Auction where

import           Control.Lens
import           Data.Hourglass                 ( Seconds(..) )
import           Data.Ratio                     ( (%) )
import           Data.Traversable               ( for )
import           Data.Thyme.Clock              as C
import           Data.Thyme.Format              ( )
import           Data.UUID

import           Aftok.Types                    ( UserId
                                                , ProjectId
                                                )
import           Aftok.Currency.Bitcoin         ( satoshi
                                                , ssub
                                                )
import           Network.Bippy.Types            ( Satoshi(..) )

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
  } deriving (Eq)
makeLenses ''Bid

data Commitment = Commitment
  { _baseBid           :: Bid
  , _commitmentSeconds :: Seconds
  , _commitmentAmount  :: Satoshi
  }

data AuctionResult
  = WinningBids [Bid]
  | InsufficientBids Satoshi
  deriving (Eq)

bidsTotal :: [Bid] -> Satoshi
bidsTotal bids = foldl' (\s b -> s <> (b ^. bidAmount)) (Satoshi 0) bids

bidOrder :: Bid -> Bid -> Ordering
bidOrder = comparing costRatio `mappend` comparing (^. bidTime)
 where
  secs bid = toRational $ bid ^. bidSeconds
  btc bid = toRational $ bid ^. bidAmount . satoshi
  costRatio bid = secs bid / btc bid

-- lowest bids of seconds/btc win
runAuction :: Auction -> [Bid] -> AuctionResult
runAuction auction = runAuction' (auction ^. raiseAmount)

runAuction' :: Satoshi -> [Bid] -> AuctionResult
runAuction' raiseAmount' bids =
  let
    takeWinningBids :: Satoshi -> [Bid] -> [Bid]
    takeWinningBids total (bid : xs)
      |
      -- if the total is fully within the raise amount
        total <> (bid ^. bidAmount) < raiseAmount'
      = bid : takeWinningBids (total <> (bid ^. bidAmount)) xs
      |

      -- if the last bid will exceed the raise amount, reduce it to fit
        total < raiseAmount'
      = let
          winFraction r = r % (bid ^. bidAmount . satoshi)
          remainderSeconds (Satoshi r) =
            Seconds . round $ winFraction r * fromIntegral (bid ^. bidSeconds)
          adjustBid r = bid & bidSeconds .~ remainderSeconds r & bidAmount .~ r
        in
          toList $ adjustBid <$> raiseAmount' `ssub` total
      | otherwise
      = []

    takeWinningBids _ [] = []

    submittedTotal = bidsTotal bids
  in
    maybe (WinningBids $ takeWinningBids (Satoshi 0) $ sortBy bidOrder bids)
          InsufficientBids
          (raiseAmount' `ssub` submittedTotal)

bidCommitment :: Satoshi -> Bid -> State Satoshi (Maybe Commitment)
bidCommitment raiseAmount' bid = do
  raised <- get
  case raised of
    -- if the total is fully within the raise amount
    x | x <> (bid ^. bidAmount) < raiseAmount' ->
      put (x <> bid ^. bidAmount)
        >> (pure . Just $ Commitment bid (bid ^. bidSeconds) (bid ^. bidAmount))

    -- if the last bid will exceed the raise amount, reduce it to fit
    x | x < raiseAmount' ->
      let winFraction r = r % (bid ^. bidAmount . satoshi)
          remainderSeconds (Satoshi r) =
              Seconds . round $ winFraction r * fromIntegral (bid ^. bidSeconds)
      in  for (raiseAmount' `ssub` x) $ \remainder ->
            put (x <> remainder)
              *> (pure $ Commitment bid (remainderSeconds remainder) remainder)

    -- otherwise,
    _ -> pure Nothing

