{-# LANGUAGE TemplateHaskell #-}

module Aftok.Billables where

import           ClassyPrelude

import           Control.Lens               (makeLenses)

import           Data.UUID
import           Aftok.Time (Days(..))

newtype BillableId = BillableId UUID deriving (Show, Eq)


data BillingFrequency 
  = Annually
  | Monthly Int
  | SemiMonthly
  | Weekly Int
makeLenses ''BillingFrequency

data Recurrence
  = Recurring { _frequency :: BillingFrequency } 
  | OneTime   
makeLenses ''Recurrence

data Billable (p :: *) (c :: *) = Billable
  { _project :: p
  , _name :: Text
  , _description :: Text
  , _recurrence :: Recurrence
  , _amount :: c
  , _gracePeriod :: Days
  }
makeLenses ''Billable

monthly :: BillingFrequency
monthly = Monthly 1

bimonthly :: BillingFrequency
bimonthly = Monthly 2

quarterly :: BillingFrequency
quarterly = Monthly 3

seminannually :: BillingFrequency
seminannually = Monthly 6

annually :: BillingFrequency
annually = Annually

