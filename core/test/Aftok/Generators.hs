module Aftok.Generators where

import Data.UUID
import Test.QuickCheck

genUUID :: Gen UUID
genUUID = fromWords <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
