{-# LANGUAGE TemplateHaskell #-}

module Aftok.Types where

import qualified Aftok.Currency.Zcash.Types as Zcash
import Control.Lens
  ( makeLenses,
    makePrisms,
  )
import Data.Eq (Eq)
import Data.Ord (Ord)
import Data.Text (Text)
import Data.UUID (UUID)
import Text.Show (Show)
import Prelude (Integer)

newtype UserId = UserId UUID deriving (Show, Eq, Ord)

makePrisms ''UserId

newtype UserName = UserName Text deriving (Show, Eq)

makePrisms ''UserName

newtype Email = Email Text deriving (Show, Eq)

makePrisms ''Email

data RecoverBy z
  = RecoverByEmail Email
  | RecoverByZAddr z

makePrisms ''RecoverBy

data User
  = User
      { _username :: !UserName,
        _userAccountRecovery :: !(RecoverBy Zcash.Address)
      }

makeLenses ''User

newtype ProjectId = ProjectId UUID deriving (Show, Eq, Ord)

makePrisms ''ProjectId

-- Identifier for a cryptocurrency account. An account
-- is a mapping from cryptocurrency network to address;
-- this abstraction permits users to accept payment
-- in multiple currencies, or to direct payments in a
-- fashion that can change over time.
newtype AccountId = AccountId UUID deriving (Show, Eq, Ord)

makePrisms ''AccountId

data CreditTo
  = -- payouts are made directly via a cryptocurrency network
    CreditToAccount !AccountId
  | -- payouts are distributed as requested by the specified contributor
    CreditToUser !UserId
  | -- payouts are distributed to this project's contributors
    CreditToProject !ProjectId
  deriving (Show, Eq, Ord)

makePrisms ''CreditTo

data DepreciationFunction = LinearDepreciation Months Months
  deriving (Eq, Show)

newtype Months = Months Integer
  deriving (Eq, Show)
