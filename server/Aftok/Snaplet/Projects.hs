{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Projects where

import ClassyPrelude 

import Data.Aeson as A
import Aftok
import Aftok.Database
import Aftok.Snaplet
import Aftok.Snaplet.Auth

import Snap.Core
import Snap.Snaplet

data CProject = CP { cpn :: Text, cpdepf :: DepreciationFunction }

instance FromJSON CProject where
  parseJSON (Object v) = CP <$> v .: "projectName" <*> v .: "depf"
  parseJSON _ = mzero

projectCreateHandler :: Handler App App ProjectId
projectCreateHandler = do
  uid <- requireUserId
  requestBody <- readRequestBody 4096
  cp <- maybe (snapError 400 "Could not parse project data") pure $ A.decode requestBody
  timestamp <- liftIO getCurrentTime
  snapEval . createProject $ Project (cpn cp) timestamp uid (cpdepf cp)

projectListHandler :: Handler App App [KeyedProject]
projectListHandler = do
  uid <- requireUserId
  snapEval $ findUserProjects uid

projectGetHandler :: Handler App App Project
projectGetHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  mp <- snapEval $ findProject pid uid
  maybe (snapError 404 $ "Project not found for id " <> tshow pid) pure mp

