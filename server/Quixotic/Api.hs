{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Api where

import ClassyPrelude 

import Control.Lens
import Control.Monad.State
import qualified Data.Aeson as A
import Data.Map

import Quixotic
import Quixotic.Database
import Quixotic.Json
import Quixotic.TimeLog

import Quixotic.Api.Types

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

logWorkHandler :: EventType -> Handler App App ()
logWorkHandler evType = requireUserId $ \uid -> do 
  QDB{..} <- view qdb <$> with qm get
  pid <- getParam "projectId"
  checkProjectAccess pid uid

  addrBytes <- getParam "btcAddr"
  timestamp <- liftIO getCurrentTime
  let workEvent = WorkEvent evType timestamp
      storeEv addr = runReaderT . recordEvent pid uid $ LogEntry addr workEvent
  case fmap decodeUtf8 addrBytes >>= parseBtcAddr of
    Nothing -> snapError 400 $ "Unable to parse bitcoin address from " <> (tshow addrBytes)
    Just addr -> liftPG $ storeEv addr

loggedIntervalsHandler :: Handler App App ()
loggedIntervalsHandler = requireLogin $ do
  QDB{..} <- view qdb <$> with qm get
  widx <- liftPG $ runReaderT readWorkIndex
  modifyResponse $ addHeader "content-type" "application/json"
  writeLBS . A.encode $ mapKeys (^. address) widx

payoutsHandler :: Handler App App ()
payoutsHandler = requireLogin $ do 
  (QModules QDB{..} df) <- with qm get
  ptime <- liftIO $ getCurrentTime
  widx <- liftPG $ runReaderT readWorkIndex
  modifyResponse $ addHeader "content-type" "application/json"
  writeLBS . A.encode . PayoutsJ $ payouts df ptime widx
