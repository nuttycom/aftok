{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Aftok.Snaplet.Projects
  ( projectCreateHandler
  , projectListHandler
  , projectGetHandler
  , projectInviteHandler
  )
where


import           Control.Lens
import           Control.Monad.Trans.Maybe      ( mapMaybeT )
import           Data.Aeson                    as A
import           Data.Attoparsec.ByteString     ( takeByteString )
import           Data.Thyme.Clock              as C
import           Filesystem.Path.CurrentOS      ( encodeString )
import qualified Filesystem.Path.CurrentOS     as F
import           Network.Mail.Mime
import           Network.Mail.SMTP             as SMTP
import           Text.StringTemplate

import           Aftok.Types
import           Aftok.Config
import           Aftok.Database
import           Aftok.Project
import           Aftok.QConfig                 as QC
import           Aftok.Snaplet
import           Aftok.Snaplet.Auth
import           Aftok.TimeLog.Serialization    ( depfFromJSON )
import           Aftok.Util                     ( fromMaybeT )

import           Snap.Core
import           Snap.Snaplet                  as S

data ProjectCreateRequest = CP { cpn :: Text, cpdepf :: DepreciationFunction }

instance FromJSON ProjectCreateRequest where
  parseJSON (Object v) =
    CP <$> v .: "projectName" <*> (depfFromJSON =<< v .: "depf")
  parseJSON _ = mzero

projectCreateHandler :: S.Handler App App ProjectId
projectCreateHandler = do
  uid         <- requireUserId
  requestBody <- readRequestBody 4096
  cp          <- either (snapError 400 . show) pure $ A.eitherDecode requestBody
  t           <- liftIO C.getCurrentTime
  snapEval . createProject $ Project (cpn cp) t uid (cpdepf cp)

projectListHandler :: S.Handler App App [(ProjectId, Project)]
projectListHandler = do
  uid <- requireUserId
  snapEval $ findUserProjects uid

projectGetHandler :: S.Handler App App Project
projectGetHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  fromMaybeT (snapError 404 $ "Project not found for id " <> show pid)
             (mapMaybeT snapEval $ findUserProject uid pid)

projectInviteHandler :: QConfig -> S.Handler App App ()
projectInviteHandler cfg = do
  uid     <- requireUserId
  pid     <- requireProjectId
  toEmail <- parseParam "email" (fmap (Email . decodeUtf8) takeByteString)
  t       <- liftIO C.getCurrentTime
  (Just u, Just p, invCode) <-
    snapEval
    $   (,,)
    <$> (runMaybeT $ findUser uid)
    <*> (runMaybeT $ findUserProject uid pid)
    <*> createInvitation pid uid toEmail t
  liftIO $ sendProjectInviteEmail cfg
                                  (p ^. projectName)
                                  (u ^. userEmail)
                                  toEmail
                                  invCode


sendProjectInviteEmail
  :: QConfig
  -> ProjectName
  -> Email       -- Inviting user's email address
  -> Email       -- Invitee's email address
  -> InvitationCode
  -> IO ()
sendProjectInviteEmail cfg pn fromEmail toEmail invCode =
  let SmtpConfig {..} = cfg ^. QC.smtpConfig
      mailer          = maybe (sendMailWithLogin _smtpHost)
                              (sendMailWithLogin' _smtpHost)
                              _smtpPort
  in  buildProjectInviteEmail (cfg ^. templatePath) pn fromEmail toEmail invCode
        >>= (mailer _smtpUser _smtpPass)


buildProjectInviteEmail
  :: F.FilePath
  -> ProjectName
  -> Email       -- Inviting user's email address
  -> Email       -- Invitee's email address
  -> InvitationCode
  -> IO Mail
buildProjectInviteEmail tpath pn fromEmail toEmail invCode = do
  templates <- directoryGroup $ encodeString tpath
  case getStringTemplate "invitation_email" templates of
    Nothing -> fail "Could not find template for invitation email"
    Just template ->
      let setAttrs =
              setAttribute "from_email" (fromEmail ^. _Email)
                . setAttribute "project_name" pn
                . setAttribute "to_email"     (toEmail ^. _Email)
                . setAttribute "inv_code"     (renderInvCode invCode)
          fromAddr = Address Nothing ("invitations@aftok.com")
          toAddr   = Address Nothing (toEmail ^. _Email)
          subject  = "Welcome to the " <> pn <> " Aftok!"
          body     = plainPart . render $ setAttrs template
      in  pure $ SMTP.simpleMail fromAddr [toAddr] [] [] subject [body]


