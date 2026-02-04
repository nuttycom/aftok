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
import qualified Aftok.Currency.Zcash.Zip321 as Zip321
import Aftok.Project (Project)
import Aftok.TimeLog.Serialization (depfFromJSON)
import Aftok.Types
  ( DepreciationFunction (..),
    ProjectId,
    UserId,
    UserName,
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
import Aftok.Json (obj, v1)
import qualified Data.Map.Strict as M
import qualified Data.Thyme.Clock as C
import Servant.API
import Time.Types (Hours (..))

--------------------------------------------------------------------------------
-- Data Types
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
    v1 . obj $ ["zip321_request" .= (A.toJSON . Zip321.toURI $ r)]

--------------------------------------------------------------------------------
-- API Types
--------------------------------------------------------------------------------

-- | Public Projects API (none currently)
type ProjectsAPI = EmptyAPI

-- | Protected Projects API
type ProtectedProjectsAPI =
  "projects"
    :> ( -- GET /projects - List user's projects
         Get '[JSON] Value
           -- POST /projects - Create project
           :<|> ReqBody '[JSON] ProjectCreateRequest :> Post '[JSON] ProjectId
           -- Project-specific routes
           :<|> Capture "projectId" ProjectId :> SingleProjectAPI
       )

-- | Single project operations
type SingleProjectAPI =
  -- GET /projects/:projectId
  Get '[JSON] Value
    -- GET /projects/:projectId/detail
    :<|> "detail" :> Get '[JSON] Value
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
