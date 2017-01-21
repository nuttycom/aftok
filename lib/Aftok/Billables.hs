{-# LANGUAGE TemplateHaskell #-}

module Aftok.Billables where

import           ClassyPrelude

import           Control.Lens  (makeLenses, makePrisms)

import           Aftok         (UserId)
import           Aftok.Project (ProjectId)
import           Aftok.Time    (Days (..))
import           Aftok.Types   (Satoshi)
import           Data.UUID

newtype BillableId = BillableId UUID deriving (Show, Eq)
makePrisms ''BillableId

data Recurrence
  = Annually
  | Monthly Int
  | SemiMonthly
  | Weekly Int
  | OneTime
makeLenses ''Recurrence

recurrenceName :: Recurrence -> Text
recurrenceName Annually    = "annually"
recurrenceName (Monthly _) = "monthly"
recurrenceName SemiMonthly = "semimonthly"
recurrenceName (Weekly _)  = "weekly"
recurrenceName OneTime     = "onetime"

recurrenceCount :: Recurrence -> Maybe Int
recurrenceCount Annually    = Nothing
recurrenceCount (Monthly i) = Just i
recurrenceCount SemiMonthly = Nothing
recurrenceCount (Weekly i)  = Just i
recurrenceCount OneTime     = Nothing

data Billable' (p :: *) (u :: *) (c :: *) = Billable
  { _project     :: p
  , _creator     :: u
  , _name        :: Text
  , _description :: Text
  , _recurrence  :: Recurrence
  , _amount      :: c
  , _gracePeriod :: Days
  }
makeLenses ''Billable'

type Billable = Billable' ProjectId UserId Satoshi

monthly :: Recurrence
monthly = Monthly 1

bimonthly :: Recurrence
bimonthly = Monthly 2

quarterly :: Recurrence
quarterly = Monthly 3

seminannually :: Recurrence
seminannually = Monthly 6

annually :: Recurrence
annually = Annually

newtype SubscriptionId = SubscriptionId UUID deriving (Show, Eq)
makePrisms ''SubscriptionId

