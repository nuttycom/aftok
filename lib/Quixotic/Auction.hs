{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}

module Quixotic.Auction where

import ClassyPrelude

newtype AuctionId = AuctionId Int

data Auction = Auction 
  { raiseAmount :: BTC
  , endsAt :: UTCTime 
  }

data Bid = Bid
  { userId :: UserId
  , hours :: Hours
  , btcAmount :: BTC
  }

winningBids :: Foldable f -> Auction -> f Bid -> [Bid]
winningBids = 
  let 
