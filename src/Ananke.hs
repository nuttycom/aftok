module Ananke
  ( BtcAddr(address)
  , parseBtcAddr
  ) where

import qualified Data.Text as T
import Database.PostgreSQL.Simple.FromField

newtype BtcAddr = BtcAddr { address :: T.Text } deriving (Show, Eq, Ord)

parseBtcAddr :: T.Text -> Maybe BtcAddr
parseBtcAddr = Just . BtcAddr -- this will be changed to do validation

instance FromField BtcAddr where
  fromField f m = fmap BtcAddr $ fromField f m 
  
