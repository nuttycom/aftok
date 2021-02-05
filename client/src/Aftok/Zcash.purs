module Aftok.Zcash where

import Prelude
import Data.Newtype (class Newtype)
import Data.BigInt (BigInt)
import Data.BigInt (toString) as BigInt
import Data.Fixed (Fixed, P1000000, TenTimes, fromInt,  numerator) 
import Data.Fixed (toString) as Fixed
                  
newtype Zatoshi = Zatoshi (BigInt)

derive instance zatoshiEq :: Eq Zatoshi

derive instance zatoshiOrd :: Ord Zatoshi

derive instance zatoshiNewtype :: Newtype Zatoshi _

zatsString :: Zatoshi -> String
zatsString (Zatoshi z) = BigInt.toString z

type ZPrec = TenTimes (TenTimes P1000000)

newtype ZEC = ZEC (Fixed ZPrec)

derive instance zecEq :: Eq ZEC

derive instance zecOrd :: Ord ZEC

derive instance zecNewtype :: Newtype ZEC _

zecString :: ZEC -> String
zecString (ZEC z) = Fixed.toString z

toZatoshi :: ZEC -> Zatoshi
toZatoshi (ZEC z) =  
  Zatoshi (numerator z)

