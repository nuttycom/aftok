module Aftok.Snaplet.Payments where

import           ClassyPrelude

import           Network.Bippy
import           Network.Bippy.Types

import           Snap.Core
import           Snap.Snaplet

requestPaymentHandler :: Handler App App
requestPaymentHandler = do
  -- get payout percentages from payouts handler
  uid <- requireUserId
  pid <- requireProjectId
  ptime <- liftIO $ C.getCurrentTime
  payouts <- snapEval $ fc (ReadWorkIndex pid)
  pure $ payouts (toDepF $ project ^. depf) ptime widx
  -- look up the outstanding
  undefined

