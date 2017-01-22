module Aftok.Snaplet.Payments where

import           ClassyPrelude

import           Network.Bippy
import           Network.Bippy.Types

import           Snap.Core
import           Snap.Snaplet

import           Aftok.QConfig

requestPaymentHandler :: QConfig -> Handler App App
requestPaymentHandler cfg = do
  -- get payout percentages from payouts handler
  uid <- requireUserId
  pid <- requireProjectId
  ptime <- liftIO $ C.getCurrentTime
  widx <- snapEval $ fc (ReadWorkIndex pid)
  -- look up outstanding subscriptions the user has for this project
  -- determine which subscriptions need to be paid
  -- create a payment request for each subscription
  undefined

  


