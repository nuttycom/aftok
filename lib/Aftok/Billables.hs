{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}

module Aftok.Billables where

import           ClassyPrelude

import           Control.Lens     (makeLenses, makePrisms, preview, view, _Just)
import           Data.List        (unfoldr)
import           Data.Thyme.Clock as C
import           Data.Thyme.Time  as T
import           Data.UUID

import           Aftok            (UserId)
import           Aftok.Project    (ProjectId)
import           Aftok.Types      (Satoshi)

newtype BillableId = BillableId UUID deriving (Show, Eq)
makePrisms ''BillableId

data Recurrence
  = Annually
  | Monthly T.Months
--  | SemiMonthly
  | Weekly Int
  | OneTime
makeLenses ''Recurrence

recurrenceName :: Recurrence -> Text
recurrenceName Annually    = "annually"
recurrenceName (Monthly _) = "monthly"
--recurrenceName SemiMonthly = "semimonthly"
recurrenceName (Weekly _)  = "weekly"
recurrenceName OneTime     = "onetime"

recurrenceCount :: Recurrence -> Maybe Int
recurrenceCount Annually    = Nothing
recurrenceCount (Monthly i) = Just i
--recurrenceCount SemiMonthly = Nothing
recurrenceCount (Weekly i)  = Just i
recurrenceCount OneTime     = Nothing

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

data Billable' p u c = Billable
  { _project             :: p
  , _creator             :: u
  , _name                :: Text
  , _description         :: Text
  , _recurrence          :: Recurrence
  , _amount              :: c
  , _gracePeriod         :: Days
  , _requestExpiryPeriod :: Maybe C.NominalDiffTime
  }
makeLenses ''Billable'

type Billable = Billable' ProjectId UserId Satoshi

newtype SubscriptionId = SubscriptionId UUID deriving (Show, Eq)
makePrisms ''SubscriptionId

data Subscription' b = Subscription
  { _billable  :: b
  , _startTime :: C.UTCTime
  , _endTime   :: Maybe C.UTCTime
  } deriving (Functor, Foldable, Traversable)
makeLenses ''Subscription'

type Subscription = Subscription' BillableId

nextRecurrence :: Recurrence -> T.Day -> Maybe T.Day
nextRecurrence r = case r of
  Annually  -> Just . T.addGregorianYearsClip 1
  Monthly m -> Just . T.addGregorianMonthsClip m
  Weekly w  -> Just . T.addDays (w * 7)
  OneTime   -> const Nothing

{-
 - A stream of dates upon which the specified subscription
 - should be billed, beginning with the first day of the
 - subscription.
 -}
billingSchedule :: Subscription' Billable -> [T.Day]
billingSchedule s =
  let rec = view (billable . recurrence) s
      subEndDay = preview (endTime . _Just . C._utctDay) s

      next :: Maybe T.Day -> Maybe (T.Day, Maybe T.Day)
      next d = do
        d' <- d
        if (all (d' <) subEndDay) then Just (d', nextRecurrence rec d') else Nothing

  in  unfoldr next (Just $ view (startTime . C._utctDay) s)



