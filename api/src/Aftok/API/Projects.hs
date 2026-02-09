{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Projects API types for the Aftok API.
module Aftok.API.Projects
  ( -- * API Types
    ProjectsAPI,
    ProtectedProjectsAPI,
    SingleProjectAPI,

    -- * Request/Response Types
    ProjectCreateRequest (..),
    ProjectCreateResponse (..),
    ProjectSummary (..),
    ProjectResponse (..),
    ProjectDetailResponse (..),
    ProjectDetail (..),
    Contributor (..),
    ProjectInviteRequest (..),
    ProjectInviteResponse (..),
    CommsAddress (..),

    -- * Lenses
    cUserId,
    cHandle,
    cJoinedOn,
    cLoggedHours,
    cDepreciatedHours,
    cRevenueShare,
    pdProject,
    pdContributors,
  )
where

import Aftok.API.Auctions (ProjectAuctionsAPI)
import Aftok.API.Billing (ProjectBillablesAPI)
import Aftok.API.Codec ()
import Aftok.API.Types ()
import qualified Aftok.Currency.Zcash.Zip321 as Zip321
import Aftok.Project (Project (..))
import Aftok.Types
  ( DepreciationFunction (..),
    DepreciationRules (..),
    ProjectId,
    UserId (..),
    UserName (..),
  )
import qualified Autodocodec as AC
import Autodocodec (HasCodec (..), object, optionalField', requiredField')
import Autodocodec.Aeson (toJSONViaCodec, parseJSONViaCodec)
import Control.Lens (makeLenses)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    (.=),
  )
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.Ratio ((%))

import qualified Data.Thyme.Clock as C

import Servant.API
import Time.Types (Hours (..))

--------------------------------------------------------------------------------
-- Data Types (defined before TH splices)
--------------------------------------------------------------------------------

-- | Project creation request
data ProjectCreateRequest = ProjectCreateRequest
  { cpn :: Text,
    cpdepf :: DepreciationFunction
  }

instance FromJSON ProjectCreateRequest where parseJSON = parseJSONViaCodec

instance HasCodec ProjectCreateRequest where
  codec =
    object "ProjectCreateRequest" $
      ProjectCreateRequest
        <$> requiredField' "projectName" AC..= cpn
        <*> requiredField' "depf" AC..= cpdepf

-- | Contributor record for project detail
data Contributor = Contributor
  { _cUserId :: UserId,
    _cHandle :: UserName,
    _cJoinedOn :: C.UTCTime,
    _cLoggedHours :: Hours,
    _cDepreciatedHours :: Hours,
    _cRevenueShare :: Rational
  }

makeLenses ''Contributor

-- | Project detail with contributors
data ProjectDetail = ProjectDetail
  { _pdProject :: Project,
    _pdContributors :: M.Map UserId Contributor
  }

makeLenses ''ProjectDetail

-- | Communications address for invitations
data CommsAddress
  = EmailComms Text
  | ZcashComms Text

instance HasCodec CommsAddress where
  codec =
    AC.dimapCodec fromEither toEither $
      AC.disjointEitherCodec emailCodec zAddrCodec
    where
      emailCodec = object "EmailComms" $ requiredField' "email" AC..= id
      zAddrCodec = object "ZcashComms" $ requiredField' "zaddr" AC..= id
      fromEither :: Either Text Text -> CommsAddress
      fromEither = \case
        Left e -> EmailComms e
        Right z -> ZcashComms z
      toEither :: CommsAddress -> Either Text Text
      toEither = \case
        EmailComms e -> Left e
        ZcashComms z -> Right z

-- | Project invitation request
data ProjectInviteRequest = ProjectInviteRequest
  { greetName :: Text,
    pirMessage :: Maybe Text,
    inviteBy :: CommsAddress
  }

instance FromJSON ProjectInviteRequest where parseJSON = parseJSONViaCodec

instance HasCodec ProjectInviteRequest where
  codec =
    object "ProjectInviteRequest" $
      ProjectInviteRequest
        <$> requiredField' "greetName" AC..= greetName
        <*> optionalField' "message" AC..= pirMessage
        <*> requiredField' "inviteBy" AC..= inviteBy

-- | Project invitation response
data ProjectInviteResponse = ProjectInviteResponse
  { zip321URI :: Maybe Zip321.PaymentRequest
  }
  deriving (Generic)

instance ToJSON ProjectInviteResponse where
  toJSON (ProjectInviteResponse Nothing) = A.object []
  toJSON (ProjectInviteResponse (Just r)) =
    A.object ["zip321_request" .= (A.toJSON . Zip321.toURI $ r)]

--------------------------------------------------------------------------------
-- Response Types (defined after TH splices)
--------------------------------------------------------------------------------

-- | Project creation response
data ProjectCreateResponse = ProjectCreateResponse
  { pcrProjectId :: ProjectId
  }
  deriving (Generic)

instance HasCodec ProjectCreateResponse where
  codec =
    object "ProjectCreateResponse" $
      ProjectCreateResponse
        <$> requiredField' "projectId" AC..= pcrProjectId

instance ToJSON ProjectCreateResponse where toJSON = toJSONViaCodec

-- | Helper codec for serializing a Project to a flat JSON object
projectCodec :: AC.JSONObjectCodec ProjectFields
projectCodec =
  ProjectFields
    <$> requiredField' "projectName" AC..= pfName
    <*> requiredField' "inceptionDate" AC..= pfInceptionDate
    <*> requiredField' "initiator" AC..= pfInitiator
    <*> requiredField' "depf" AC..= pfDepf

data ProjectFields = ProjectFields
  { pfName :: Text,
    pfInceptionDate :: C.UTCTime,
    pfInitiator :: UserId,
    pfDepf :: DepreciationFunction
  }

instance HasCodec ProjectFields where
  codec = object "Project" projectCodec

projectToFields :: Project -> ProjectFields
projectToFields p =
  ProjectFields
    { pfName = _projectName p,
      pfInceptionDate = _inceptionDate p,
      pfInitiator = _initiator p,
      pfDepf = _depf (_depRules p)
    }

-- | Project response for GET /projects/:pid (flat project fields)
data ProjectResponse = ProjectResponse
  { prProject :: Project
  }

instance HasCodec ProjectResponse where
  codec =
    AC.dimapCodec (ProjectResponse . fieldsToProject) (projectToFields . prProject) $
      codec @ProjectFields
    where
      fieldsToProject pf =
        Project
          { _projectName = pfName pf,
            _inceptionDate = pfInceptionDate pf,
            _initiator = pfInitiator pf,
            _depRules = DepreciationRules (pfDepf pf) Nothing
          }

instance ToJSON ProjectResponse where toJSON = toJSONViaCodec

-- | Project summary for GET /projects list (with projectId)
data ProjectSummary = ProjectSummary
  { psProjectId :: ProjectId,
    psProject :: Project
  }

instance HasCodec ProjectSummary where
  codec =
    object "ProjectSummary" $
      (\pid pf -> ProjectSummary pid (fieldsToProject pf))
        <$> requiredField' "projectId" AC..= psProjectId
        <*> requiredField' "project" AC..= (projectToFields . psProject)
    where
      fieldsToProject pf =
        Project
          { _projectName = pfName pf,
            _inceptionDate = pfInceptionDate pf,
            _initiator = pfInitiator pf,
            _depRules = DepreciationRules (pfDepf pf) Nothing
          }

instance ToJSON ProjectSummary where toJSON = toJSONViaCodec

-- | Contributor serialization helper
data ContributorFields = ContributorFields
  { cfUserId :: UserId,
    cfUsername :: Text,
    cfJoinedOn :: C.UTCTime,
    cfLoggedHours :: Int64,
    cfDepreciatedHours :: Int64,
    cfRevenueShare :: RationalFields
  }

data RationalFields = RationalFields
  { rfNumerator :: Integer,
    rfDenominator :: Integer
  }

instance HasCodec RationalFields where
  codec =
    object "RationalFields" $
      RationalFields
        <$> requiredField' "numerator" AC..= rfNumerator
        <*> requiredField' "denominator" AC..= rfDenominator

instance HasCodec ContributorFields where
  codec =
    object "Contributor" $
      ContributorFields
        <$> requiredField' "userId" AC..= cfUserId
        <*> requiredField' "username" AC..= cfUsername
        <*> requiredField' "joinedOn" AC..= cfJoinedOn
        <*> requiredField' "loggedHours" AC..= cfLoggedHours
        <*> requiredField' "depreciatedHours" AC..= cfDepreciatedHours
        <*> requiredField' "revenueShare" AC..= cfRevenueShare

contributorToFields :: Contributor -> ContributorFields
contributorToFields c =
  ContributorFields
    { cfUserId = _cUserId c,
      cfUsername = let UserName n = _cHandle c in n,
      cfJoinedOn = _cJoinedOn c,
      cfLoggedHours = let Hours h = _cLoggedHours c in h,
      cfDepreciatedHours = let Hours h = _cDepreciatedHours c in h,
      cfRevenueShare = RationalFields (numerator $ _cRevenueShare c) (denominator $ _cRevenueShare c)
    }

-- | Project detail response for GET /projects/:pid/detail
data ProjectDetailResponse = ProjectDetailResponse
  { pdrDetail :: ProjectDetail
  }

instance HasCodec ProjectDetailResponse where
  codec =
    object "ProjectDetailResponse" $
      (\pf cs -> ProjectDetailResponse (ProjectDetail (fieldsToProject pf) (rebuildContributorMap cs)))
        <$> requiredField' "project" AC..= (projectToFields . _pdProject . pdrDetail)
        <*> requiredField' "contributors" AC..= (fmap contributorToFields . M.elems . _pdContributors . pdrDetail)
    where
      fieldsToProject pf =
        Project
          { _projectName = pfName pf,
            _inceptionDate = pfInceptionDate pf,
            _initiator = pfInitiator pf,
            _depRules = DepreciationRules (pfDepf pf) Nothing
          }
      rebuildContributorMap :: [ContributorFields] -> M.Map UserId Contributor
      rebuildContributorMap =
        M.fromList . fmap (\cf -> (cfUserId cf, fieldsToContributor cf))
      fieldsToContributor cf =
        Contributor
          { _cUserId = cfUserId cf,
            _cHandle = UserName (cfUsername cf),
            _cJoinedOn = cfJoinedOn cf,
            _cLoggedHours = Hours (cfLoggedHours cf),
            _cDepreciatedHours = Hours (cfDepreciatedHours cf),
            _cRevenueShare = rfNumerator (cfRevenueShare cf) % rfDenominator (cfRevenueShare cf)
          }

instance ToJSON ProjectDetailResponse where toJSON = toJSONViaCodec

--------------------------------------------------------------------------------
-- API Types
--------------------------------------------------------------------------------

-- | Public Projects API (none currently)
type ProjectsAPI = EmptyAPI

-- | Protected Projects API
type ProtectedProjectsAPI =
  "projects"
    :> ( -- GET /projects - List user's projects
         Get '[JSON] [ProjectSummary]
           -- POST /projects - Create project
           :<|> ReqBody '[JSON] ProjectCreateRequest :> Post '[JSON] ProjectCreateResponse
           -- Project-specific routes
           :<|> Capture "projectId" ProjectId :> SingleProjectAPI
       )

-- | Single project operations
type SingleProjectAPI =
  -- GET /projects/:projectId
  Get '[JSON] ProjectResponse
    -- GET /projects/:projectId/detail
    :<|> "detail" :> Get '[JSON] ProjectDetailResponse
    -- GET /projects/:projectId/payouts
    :<|> "payouts" :> Get '[JSON] Value
    -- GET /projects/:projectId/workIndex
    :<|> "workIndex" :> Get '[JSON] Value
    -- POST /projects/:projectId/invite
    :<|> "invite" :> ReqBody '[JSON] ProjectInviteRequest :> Post '[JSON] ProjectInviteResponse
    -- GET/POST /projects/:projectId/auctions
    :<|> "auctions" :> ProjectAuctionsAPI
    -- GET/POST /projects/:projectId/billables
    :<|> "billables" :> ProjectBillablesAPI
