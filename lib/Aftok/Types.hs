{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE TemplateHaskell    #-}

module Aftok.Types where

import           Control.Lens                   ( makeLenses
                                                , makePrisms
                                                )
import           Data.Eq                        ( Eq )
import           Data.Functor                   ( Functor )
import           Data.Ord                       ( Ord )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Prelude                        ( Integer )
import           Text.Show                      ( Show )
import           Aftok.Currency.Zcash           ( ZAddr )


newtype UserId = UserId UUID deriving (Show, Eq, Ord)
makePrisms ''UserId

newtype UserName = UserName Text deriving (Show, Eq)
makePrisms ''UserName

newtype Email = Email Text deriving (Show, Eq)
makePrisms ''Email

data AccountRecovery z
  = RecoverByEmail Email
  | RecoverByZAddr z
makePrisms ''AccountRecovery

data User = User
  { _username             :: !UserName
  , _userAccountRecovery  :: !(AccountRecovery ZAddr)
  }
makeLenses ''User

newtype ProjectId = ProjectId UUID deriving (Show, Eq, Ord)
makePrisms ''ProjectId

data CreditTo a
  -- payouts are made directly via a cryptocurrency network
  = CreditToCurrency !a
  -- payouts are distributed as requested by the specified contributor
  | CreditToUser !UserId
  -- payouts are distributed to this project's contributors
  | CreditToProject !ProjectId
  deriving (Show, Eq, Ord, Functor)
makePrisms ''CreditTo

creditToName :: CreditTo a -> Text
creditToName (CreditToCurrency _) = "credit_via_net"
creditToName (CreditToUser     _) = "credit_to_user"
creditToName (CreditToProject  _) = "credit_to_project"

data DepreciationFunction = LinearDepreciation Months Months
  deriving (Eq, Show)

newtype Months = Months Integer
  deriving (Eq, Show)
