{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

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
import Aftok.API.Types ()
import qualified Aftok.Currency.Zcash.Zip321 as Zip321
import Aftok.Project (Project (..))
import Aftok.TimeLog.Serialization (depfFromJSON)
import Aftok.Types
  ( DepreciationFunction (..),
    DepreciationRules (..),
    ProjectId,
    UserId (..),
    UserName (..),
  )
import Control.Lens (makeLenses)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
    object,
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.Thyme.Clock as C
import qualified Data.UUID as UUID
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

instance FromJSON ProjectCreateRequest where
  parseJSON (A.Object v) =
    ProjectCreateRequest <$> v .: "projectName" <*> (depfFromJSON =<< v .: "depf")
  parseJSON _ = mzero

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

-- | Project invitation request
data ProjectInviteRequest = ProjectInviteRequest
  { greetName :: Text,
    pirMessage :: Maybe Text,
    inviteBy :: CommsAddress
  }

instance FromJSON ProjectInviteRequest where
  parseJSON (A.Object v) = do
    name <- v .: "greetName"
    msg <- v .:? "message"
    comms <- v .: "inviteBy"
    emailComms <- fmap EmailComms <$> (comms .:? "email")
    zcashComms <- fmap ZcashComms <$> (comms .:? "zaddr")
    case emailComms <|> zcashComms of
      Nothing -> mzero
      Just addr -> pure $ ProjectInviteRequest name msg addr
  parseJSON _ = mzero

-- | Project invitation response
data ProjectInviteResponse = ProjectInviteResponse
  { zip321URI :: Maybe Zip321.PaymentRequest
  }
  deriving (Generic)

instance ToJSON ProjectInviteResponse where
  toJSON (ProjectInviteResponse Nothing) = object []
  toJSON (ProjectInviteResponse (Just r)) =
    object ["zip321_request" .= (A.toJSON . Zip321.toURI $ r)]

--------------------------------------------------------------------------------
-- Response Types (defined after TH splices)
--------------------------------------------------------------------------------

-- | Project creation response
data ProjectCreateResponse = ProjectCreateResponse
  { projectId :: ProjectId
  }
  deriving (Generic)

instance ToJSON ProjectCreateResponse

-- | Project response for GET /projects/:pid (flat project fields)
data ProjectResponse = ProjectResponse
  { prProject :: Project
  }

instance ToJSON ProjectResponse where
  toJSON (ProjectResponse p) = projectToJSON p

-- | Project summary for GET /projects list (with projectId)
data ProjectSummary = ProjectSummary
  { psProjectId :: ProjectId,
    psProject :: Project
  }

instance ToJSON ProjectSummary where
  toJSON (ProjectSummary pid p) =
    object
      [ "projectId" .= pid,
        "project" .= projectToJSON p
      ]

-- | Project detail response for GET /projects/:pid/detail
data ProjectDetailResponse = ProjectDetailResponse
  { pdrDetail :: ProjectDetail
  }

instance ToJSON ProjectDetailResponse where
  toJSON (ProjectDetailResponse detail) =
    object
      [ "project" .= projectToJSON (_pdProject detail),
        "contributors" .= (M.elems $ fmap contributorToJSON (_pdContributors detail))
      ]

--------------------------------------------------------------------------------
-- JSON serialization helpers
--------------------------------------------------------------------------------

-- | Serialize a Project to JSON
projectToJSON :: Project -> Value
projectToJSON p =
  object
    [ "projectName" .= _projectName p,
      "inceptionDate" .= _inceptionDate p,
      "initiator" .= (let UserId u = _initiator p in UUID.toText u),
      "depf" .= depfToJSON (_depf $ _depRules p)
    ]

-- | Serialize a DepreciationFunction to JSON
depfToJSON :: DepreciationFunction -> Value
depfToJSON = \case
  LinearDepreciation undep dep ->
    object
      [ "type" .= ("LinearDepreciation" :: Text),
        "arguments" .= object ["undep" .= undep, "dep" .= dep]
      ]

-- | Serialize a Contributor to JSON
contributorToJSON :: Contributor -> Value
contributorToJSON c =
  object
    [ "userId" .= _cUserId c,
      "username" .= (let UserName n = _cHandle c in n),
      "joinedOn" .= _cJoinedOn c,
      "loggedHours" .= (let Hours h = _cLoggedHours c in h),
      "depreciatedHours" .= (let Hours h = _cDepreciatedHours c in h),
      "revenueShare"
        .= object
          [ "numerator" .= numerator (_cRevenueShare c),
            "denominator" .= denominator (_cRevenueShare c)
          ]
    ]

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
