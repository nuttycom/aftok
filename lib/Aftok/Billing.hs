{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Aftok.Billing where

import Aftok.Types (Email, ProjectId, UserId)
import Control.Lens (_Just, makeLenses, makePrisms, preview, view)
import Data.Thyme.Clock as C
import Data.Thyme.Time as T
import Data.UUID

data Recurrence
  = Annually
  | Monthly T.Months
  | --  | SemiMonthly
    Weekly Int
  | OneTime

makeLenses ''Recurrence

recurrenceName :: Recurrence -> Text
recurrenceName Annually = "annually"
recurrenceName (Monthly _) = "monthly"
--recurrenceName SemiMonthly = "semimonthly"
recurrenceName (Weekly _) = "weekly"
recurrenceName OneTime = "onetime"

recurrenceCount :: Recurrence -> Maybe Int
recurrenceCount Annually = Nothing
recurrenceCount (Monthly i) = Just i
--recurrenceCount SemiMonthly = Nothing
recurrenceCount (Weekly i) = Just i
recurrenceCount OneTime = Nothing

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

-- | A potentially recurring billable amount.
data Billable' p u currency
  = Billable
      { _project :: p,
        _creator :: u,
        _name :: Text,
        _description :: Text,
        _messageText :: Text,
        _recurrence :: Recurrence,
        _amount :: currency,
        _gracePeriod :: Days,
        _requestExpiryPeriod :: NominalDiffTime,
        _paymentRequestEmailTemplate :: Maybe Text,
        _paymentRequestMemoTemplate :: Maybe Text
      }

makeLenses ''Billable'

type Billable amt = Billable' ProjectId UserId amt

newtype BillableId = BillableId UUID deriving (Show, Eq)

makePrisms ''BillableId

data ContactChannel
  = EmailChannel Email

-- | An association between a customer and a (potentially recurring) billable amount.
--
-- For one-time billing events, the end date should be the same as the start date.
data Subscription' u b
  = Subscription
      { _customer :: u,
        _billable :: b,
        _contactChannel :: ContactChannel,
        _startTime :: UTCTime,
        _endTime :: Maybe UTCTime
      }

makeLenses ''Subscription'

type Subscription = Subscription' UserId BillableId

newtype SubscriptionId = SubscriptionId UUID deriving (Show, Eq)

makePrisms ''SubscriptionId

nextRecurrence :: Recurrence -> T.Day -> Maybe T.Day
nextRecurrence r = case r of
  Annually -> Just . T.addGregorianYearsClip 1
  Monthly m -> Just . T.addGregorianMonthsClip m
  Weekly w -> Just . T.addDays (w * 7)
  OneTime -> const Nothing

-- | A stream of dates upon which the specified subscription
-- should be billed, beginning with the first day of the
-- subscription.
billingSchedule :: forall u a. Subscription' u (Billable a) -> [T.Day]
billingSchedule s =
  unfoldr next (Just $ view (startTime . C._utctDay) s)
  where
    rec = view (billable . recurrence) s
    subEndDay = preview (endTime . _Just . C._utctDay) s
    next :: Maybe T.Day -> Maybe (T.Day, Maybe T.Day)
    next d = do
      d' <- d
      if (all (d' <) subEndDay) then Just (d', nextRecurrence rec d') else Nothing
