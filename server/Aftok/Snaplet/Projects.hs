{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Aftok.Snaplet.Projects
  ( projectCreateHandler,
    projectListHandler,
    projectGetHandler,
    projectInviteHandler,
    projectDetailGetHandler,
    contributorJSON,
    projectJSON,
    payoutsHandler,
    payoutsJSON,
    qdbProjectJSON,
    projectDetailJSON,
  )
where

import Aftok.Config
import Aftok.Database
import Aftok.Json (creditToJSON, idValue, identifiedJSON, obj, v1)
import Aftok.Project
import Aftok.QConfig as QC
import Aftok.Snaplet
import Aftok.Snaplet.Auth
import Aftok.TimeLog
  ( WorkShare,
    WorkShares,
    creditToShares,
    payouts,
    toDepF,
    wsLogged,
    wsShare,
  )
import Aftok.TimeLog.Serialization (depfFromJSON)
import Aftok.Types
import Aftok.Util (fromMaybeT)
import Control.Lens ((^.), _1, _2, makeLenses, to)
import Control.Monad.Trans.Maybe (mapMaybeT)
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.=), Value (..), object)
import Data.Attoparsec.ByteString (takeByteString)
import qualified Data.Map.Strict as M
import qualified Data.Thyme.Clock as C
import Filesystem.Path.CurrentOS (encodeString)
import qualified Filesystem.Path.CurrentOS as F
import Network.Mail.Mime
import qualified Network.Mail.SMTP as SMTP
import Snap.Core
import Snap.Snaplet as S
import Text.StringTemplate
import Time.Types (Hours (..))

data ProjectCreateRequest = CP {cpn :: Text, cpdepf :: DepreciationFunction}

instance A.FromJSON ProjectCreateRequest where
  parseJSON (A.Object v) =
    CP <$> v .: "projectName" <*> (depfFromJSON =<< v .: "depf")
  parseJSON _ = mzero

data Contributor
  = Contributor
      { _userId :: UserId,
        _handle :: UserName,
        _joinedOn :: C.UTCTime,
        _timeDevoted :: Hours,
        _revenueShare :: Rational
      }

makeLenses ''Contributor

data ProjectDetail
  = ProjectDetail
      { _pdProject :: Project,
        _pdContributors :: M.Map UserId Contributor
      }

makeLenses ''ProjectDetail

projectCreateHandler :: S.Handler App App ProjectId
projectCreateHandler = do
  uid <- requireUserId
  requestBody <- readRequestBody 4096
  cp <- either (snapError 400 . show) pure $ A.eitherDecode requestBody
  t <- liftIO C.getCurrentTime
  snapEval . createProject $ Project (cpn cp) t uid (DepreciationRules (cpdepf cp) Nothing)

projectListHandler :: S.Handler App App [(ProjectId, Project)]
projectListHandler = do
  uid <- requireUserId
  snapEval $ findUserProjects uid

projectGetHandler :: S.Handler App App Project
projectGetHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  fromMaybeT
    (snapError 404 $ "Project not found for id " <> show pid)
    (mapMaybeT snapEval $ findUserProject uid pid)

projectDetailGetHandler :: S.Handler App App ProjectDetail
projectDetailGetHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  project <-
    fromMaybeT
      (snapError 404 $ "Project not found for id " <> show pid)
      (mapMaybeT snapEval $ findUserProject uid pid)
  widx <- snapEval $ readWorkIndex pid uid
  ptime <- liftIO $ C.getCurrentTime
  let p = payouts (toDepF $ project ^. depRules) ptime widx
      toContributorRecord = \case
        (CreditToUser uid', ws) -> do
          (user, joinedOn') <-
            fromMaybeT
              (snapError 500 $ "No user record found for logged-in user.")
              (mapMaybeT snapEval $ findUserProjectDetail uid' pid)
          pure . Just . (uid',) $
            Contributor
              { _userId = uid',
                _handle = user ^. username,
                _joinedOn = joinedOn',
                _timeDevoted = Hours . (`div` 3600) . round . C.toSeconds' $ ws ^. wsLogged,
                _revenueShare = ws ^. wsShare
              }
        _ -> pure Nothing
  contributorRecords <-
    fmap (M.fromList . catMaybes)
      . traverse toContributorRecord
      $ M.assocs (p ^. creditToShares)
  pure $
    ProjectDetail
      { _pdProject = project,
        _pdContributors = contributorRecords
      }

payoutsHandler :: S.Handler App App WorkShares
payoutsHandler = do
  uid <- requireUserId
  pid <- requireProjectId
  project <-
    fromMaybeT
      (snapError 400 $ "Project not found for id " <> show pid)
      (mapMaybeT snapEval $ findUserProject uid pid)
  widx <- snapEval $ readWorkIndex pid uid
  ptime <- liftIO $ C.getCurrentTime
  pure $ payouts (toDepF $ project ^. depRules) ptime widx

projectInviteHandler :: QConfig -> S.Handler App App ()
projectInviteHandler cfg = do
  uid <- requireUserId
  pid <- requireProjectId
  toEmail <- parseParam "email" (fmap (Email . decodeUtf8) takeByteString)
  t <- liftIO C.getCurrentTime
  (Just p, invCode) <-
    snapEval $
      (,)
        <$> (runMaybeT $ findUserProject uid pid)
        <*> createInvitation pid uid toEmail t
  liftIO $
    sendProjectInviteEmail
      cfg
      (p ^. projectName)
      (Email "noreply@aftok.com")
      toEmail
      invCode

sendProjectInviteEmail ::
  QConfig ->
  ProjectName ->
  Email -> -- Inviting user's email address
  Email -> -- Invitee's email address
  InvitationCode ->
  IO ()
sendProjectInviteEmail cfg pn fromEmail toEmail invCode =
  let SmtpConfig {..} = cfg ^. QC.smtpConfig
      mailer =
        maybe
          (SMTP.sendMailWithLogin _smtpHost)
          (SMTP.sendMailWithLogin' _smtpHost)
          _smtpPort
   in buildProjectInviteEmail (cfg ^. templatePath) pn fromEmail toEmail invCode
        >>= (mailer _smtpUser _smtpPass)

buildProjectInviteEmail ::
  F.FilePath ->
  ProjectName ->
  Email -> -- Inviting user's email address
  Email -> -- Invitee's email address
  InvitationCode ->
  IO Mail
buildProjectInviteEmail tpath pn fromEmail toEmail invCode = do
  templates <- directoryGroup $ encodeString tpath
  case getStringTemplate "invitation_email" templates of
    Nothing -> fail "Could not find template for invitation email"
    Just template ->
      let setAttrs =
            setAttribute "from_email" (fromEmail ^. _Email)
              . setAttribute "project_name" pn
              . setAttribute "to_email" (toEmail ^. _Email)
              . setAttribute "inv_code" (renderInvCode invCode)
          fromAddr = Address Nothing ("invitations@aftok.com")
          toAddr = Address Nothing (toEmail ^. _Email)
          subject = "Welcome to the " <> pn <> " Aftok!"
          body = plainPart . render $ setAttrs template
       in pure $ SMTP.simpleMail fromAddr [toAddr] [] [] subject [body]

depfToJSON :: DepreciationFunction -> Value
depfToJSON = \case
  LinearDepreciation undep dep ->
    object
      [ "type" .= ("LinearDepreciation" :: Text),
        "arguments" .= object ["undep" .= undep, "dep" .= dep]
      ]

projectJSON :: Project -> A.Object
projectJSON p =
  obj
    [ "projectName" .= (p ^. projectName),
      "inceptionDate" .= (p ^. inceptionDate),
      "initiator" .= (p ^. initiator . _UserId),
      "depf" .= depfToJSON (p ^. depRules . depf)
    ]

qdbProjectJSON :: (ProjectId, Project) -> Value
qdbProjectJSON = identifiedJSON "project" (_1 . _ProjectId) (_2 . to (v1 . projectJSON))

contributorJSON :: Contributor -> Value
contributorJSON c =
  object
    [ "userId" .= idValue _UserId (c ^. userId),
      "username" .= (c ^. handle . _UserName),
      "joinedOn" .= (c ^. joinedOn),
      "timeDevoted" .= (c ^. timeDevoted . (to fromEnum)),
      "revenureShare"
        .= object
          [ "numerator" .= (c ^. revenueShare . (to numerator)),
            "denominator" .= (c ^. revenueShare . (to denominator))
          ]
    ]

projectDetailJSON :: ProjectDetail -> A.Object
projectDetailJSON detail =
  obj
    [ "project" .= Object (projectJSON $ detail ^. pdProject),
      "contributors" .= (M.elems $ fmap contributorJSON (detail ^. pdContributors))
    ]

payoutsJSON :: WorkShares -> A.Object
payoutsJSON ws =
  let payoutsRec :: (CreditTo, WorkShare Rational) -> Value
      payoutsRec (c, r) =
        object
          [ "creditTo" .= creditToJSON c,
            "payoutRatio" .= (r ^. wsShare),
            "payoutPercentage" .= (fromRational @Double (r ^. wsShare) * 100)
          ]
   in obj $ ["payouts" .= fmap payoutsRec (M.assocs (ws ^. creditToShares))]
