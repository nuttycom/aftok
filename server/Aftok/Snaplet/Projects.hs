{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Projects 
  ( projectCreateHandler
  , projectListHandler
  , projectGetHandler
  , projectInviteHandler
  ) where

import           ClassyPrelude

import           Control.Lens
import           Data.Aeson                 as A
import           Data.Attoparsec.ByteString (takeByteString)
import           Data.Thyme.Clock           as C
import           Network.Mail.Mime
import           Network.Mail.SMTP          as SMTP
import           System.IO                  (FilePath)
import           Text.StringTemplate

import           Aftok
import           Aftok.Database
import           Aftok.Project
import           Aftok.QConfig
import           Aftok.Snaplet
import           Aftok.Snaplet.Auth

import           Snap.Core
import           Snap.Snaplet

data ProjectCreateRequest = CP { cpn :: Text, cpdepf :: DepreciationFunction }

instance FromJSON ProjectCreateRequest where
  parseJSON (Object v) = CP <$> v .: "projectName" <*> v .: "depf"
  parseJSON _ = mzero

projectCreateHandler :: Handler App App ProjectId
projectCreateHandler = do
  uid <- requireUserId
  requestBody <- readRequestBody 4096
  cp <- either (snapError 400 . tshow) pure $ A.eitherDecode requestBody
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
  liftIO $ sendProjectInviteEmail cfg (p ^. projectName) (u ^. userEmail) toEmail invCode


sendProjectInviteEmail :: QConfig
                       -> ProjectName
                       -> Email       -- Inviting user's email address
                       -> Email       -- Invitee's email address
                       -> InvitationCode
                       -> IO ()
sendProjectInviteEmail cfg pn fromEmail toEmail invCode =
  let SmtpConfig{..} = smtpConfig cfg
      mailer = maybe (sendMailWithLogin smtpHost) (sendMailWithLogin' smtpHost) smtpPort
  in  buildProjectInviteEmail (templatePath cfg) pn fromEmail toEmail invCode >>=
      (mailer smtpUser smtpPass)


buildProjectInviteEmail :: System.IO.FilePath
                        -> ProjectName
                        -> Email       -- Inviting user's email address
                        -> Email       -- Invitee's email address
                        -> InvitationCode
                        -> IO Mail
buildProjectInviteEmail templatePath pn fromEmail toEmail invCode = do
  templates <- directoryGroup templatePath
  case getStringTemplate "invitation_email" templates of
    Nothing -> fail "Could not find template for invitation email"
    Just template ->
      let setAttrs = setAttribute "from_email" (fromEmail ^. _Email) .
                     setAttribute "project_name" pn .
                     setAttribute "to_email" (toEmail ^. _Email) .
                     setAttribute "inv_code" (renderInvCode invCode)
          fromAddr = Address Nothing ("invitations@aftok.com")
          toAddr   = Address Nothing (toEmail ^. _Email)
          subject  = "Welcome to the "<>pn<>" Aftok!"
          body     = plainTextPart . render $ setAttrs template
      in  pure $ SMTP.simpleMail fromAddr [toAddr] [] [] subject [body]


