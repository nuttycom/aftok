module Ananke
  ( BtcAddr(address)
  , parseBtcAddr
  ) where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types as JV
import Control.Monad
--import Database.PostgreSQL.Simple.FromField

newtype BtcAddr = BtcAddr { address :: T.Text } deriving (Show, Eq, Ord)

parseBtcAddr :: T.Text -> Maybe BtcAddr
parseBtcAddr = Just . BtcAddr -- this will be changed to do validation

instance FromJSON BtcAddr where
  parseJSON (JV.String t) = return $ BtcAddr t
  parseJSON _ = mzero

--instance FromField BtcAddr where
--  fromField f m = fmap BtcAddr $ fromField f m 
  
