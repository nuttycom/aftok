module Aftok.Zcash where

import Prelude
import Data.Newtype (class Newtype)
import Data.BigInt (BigInt)
import Data.Fixed (Fixed, P1000000, TenTimes) 
                  
newtype Zatoshi = Zatoshi (BigInt)

derive instance zatoshiEq :: Eq Zatoshi

derive instance zatoshiOrd :: Ord Zatoshi

derive instance zatoshiNewtype :: Newtype Zatoshi _

type ZPrec = TenTimes (TenTimes P1000000)

newtype ZEC = ZEC (Fixed ZPrec)

derive instance zecEq :: Eq ZEC

derive instance zecOrd :: Ord ZEC

derive instance zecNewtype :: Newtype ZEC _

