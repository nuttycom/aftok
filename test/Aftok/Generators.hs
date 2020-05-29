module Aftok.Generators where



import           Data.UUID

import           Network.Haskoin.Test           ( ArbitraryAddress(..) )

import           Aftok                          ( BtcAddr(..) )
import           Aftok.Types                    ( Satoshi(..) )

import           Test.QuickCheck

genUUID :: Gen UUID
genUUID = fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

genSatoshi :: Gen Satoshi
genSatoshi = Satoshi <$> arbitrary

genBtcAddr :: Gen BtcAddr
genBtcAddr = fmap (\(ArbitraryAddress addr) -> BtcAddr addr) arbitrary

