{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Projects where

import ClassyPrelude 

import Control.Lens
import Data.Aeson as A
import Data.Attoparsec.ByteString (takeByteString)
import Data.Thyme.Clock as C
import qualified Network.Sendgrid.Api as Sendgrid
import System.IO (FilePath)
import Text.StringTemplate

import Aftok
import Aftok.Database
import Aftok.QConfig
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
  t <- liftIO C.getCurrentTime
  snapEval . createProject $ Project (cpn cp) t uid (cpdepf cp)

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

projectInviteHandler :: QConfig -> Handler App App ()
projectInviteHandler cfg = do
  uid <- requireUserId
  pid <- requireProjectId
  toEmail <- parseParam "email" (fmap (Email . decodeUtf8) takeByteString)
  t <- liftIO C.getCurrentTime
  (Just u, Just p, invCode) <- snapEval $ 
    (,,) <$> findUser uid
         <*> findProject pid uid
         <*> createInvitation pid uid toEmail t
  inviteEmail <- liftIO $ 
    projectInviteEmail (templatePath cfg) (p ^. projectName) (u ^. userEmail) toEmail invCode
  maybeSuccess <- liftIO $ Sendgrid.sendEmail (sendgridAuth cfg) inviteEmail
  maybe
    (snapError 500 "The invitation record was created successfully, but the introductory email could not be sent.")
    (const $ pure ())
    maybeSuccess

projectInviteEmail :: System.IO.FilePath 
                   -> ProjectName 
                   -> Email -> Email 
                   -> InvitationCode 
                   -> IO Sendgrid.EmailMessage 
projectInviteEmail templatePath pn from' to' invCode = do
  templates <- directoryGroup templatePath 
  template <- maybe (fail "Could not find template for invitation email") pure $ 
    getStringTemplate "invitation_email" templates
  let setAttrs = setAttribute "from_email" (from' ^. _Email) .
                 setAttribute "project_name" pn .
                 setAttribute "to_email" (to' ^. _Email) .
                 setAttribute "inv_code" (renderInvCode invCode) 
  return $ Sendgrid.EmailMessage 
    { from = "invitations@aftok.com"
    , to = unpack $ to' ^. _Email
    , subject = unpack $ "Welcome to the "<>pn<>" Aftok!"
    , text = render $ setAttrs template
    }


