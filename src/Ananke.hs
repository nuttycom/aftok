module Ananke
  ( BtcAddr(address)
  , btcAddr
  ) where

import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromField

newtype BtcAddr = BtcAddr { address :: T.Text } deriving (Show, Eq)

btcAddr :: T.Text -> Maybe BtcAddr
btcAddr = Just . BtcAddr -- this will be changed to do validation

instance FromField BtcAddr where
  fromField f m = fmap BtcAddr $ fromField f m 
  
