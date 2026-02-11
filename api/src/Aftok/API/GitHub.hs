{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- | GitHub integration API types for the Aftok API.
module Aftok.API.GitHub
  ( -- * API Types
    GitHubWebhookAPI,
    GitHubProjectAPI,
    GitHubUserAPI,

    -- * Request/Response Types
    LinkRepoRequest (..),
    LinkRepoResponse (..),
    RepoLinkInfo (..),
    LinkGitHubUsernameRequest (..),
    GitHubUsernameResponse (..),
  )
where

import Aftok.API.Codec ()
import Aftok.Types (GitHubRepoLinkId (..))
import qualified Autodocodec as AC
import Autodocodec (HasCodec (..), object, optionalField', requiredField')
import Autodocodec.Aeson (parseJSONViaCodec, toJSONViaCodec)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    Value (..),
  )
import qualified Data.Thyme.Clock as C
import Data.UUID (UUID)
import Servant.API

--------------------------------------------------------------------------------
-- Request/Response Types
--------------------------------------------------------------------------------

-- | Request to link a GitHub repository to a project
data LinkRepoRequest = LinkRepoRequest
  { lrrOwner :: Text,
    lrrRepo :: Text
  }
  deriving (Show, Eq, Generic)

instance HasCodec LinkRepoRequest where
  codec =
    object "LinkRepoRequest" $
      LinkRepoRequest
        <$> requiredField' "owner" AC..= lrrOwner
        <*> requiredField' "repo" AC..= lrrRepo

instance FromJSON LinkRepoRequest where parseJSON = parseJSONViaCodec

-- | Response when linking a repository
data LinkRepoResponse = LinkRepoResponse
  { lrLinkId :: GitHubRepoLinkId,
    lrWebhookSecret :: Text
  }
  deriving (Show, Eq, Generic)

instance HasCodec LinkRepoResponse where
  codec =
    object "LinkRepoResponse" $
      LinkRepoResponse
        <$> requiredField' "linkId" AC..= lrLinkId
        <*> requiredField' "webhookSecret" AC..= lrWebhookSecret

instance ToJSON LinkRepoResponse where toJSON = toJSONViaCodec

-- | Information about a linked repository
data RepoLinkInfo = RepoLinkInfo
  { rliId :: GitHubRepoLinkId,
    rliOwner :: Text,
    rliRepo :: Text,
    rliCreatedAt :: C.UTCTime
  }
  deriving (Show, Eq, Generic)

instance HasCodec RepoLinkInfo where
  codec =
    object "RepoLinkInfo" $
      RepoLinkInfo
        <$> requiredField' "id" AC..= rliId
        <*> requiredField' "owner" AC..= rliOwner
        <*> requiredField' "repo" AC..= rliRepo
        <*> requiredField' "createdAt" AC..= rliCreatedAt

instance ToJSON RepoLinkInfo where toJSON = toJSONViaCodec

-- | Request to link a GitHub username to user account
data LinkGitHubUsernameRequest = LinkGitHubUsernameRequest
  { lgurUsername :: Text
  }
  deriving (Show, Eq, Generic)

instance HasCodec LinkGitHubUsernameRequest where
  codec =
    object "LinkGitHubUsernameRequest" $
      LinkGitHubUsernameRequest
        <$> requiredField' "username" AC..= lgurUsername

instance FromJSON LinkGitHubUsernameRequest where parseJSON = parseJSONViaCodec

-- | Response with GitHub username info
data GitHubUsernameResponse = GitHubUsernameResponse
  { ghurUsername :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance HasCodec GitHubUsernameResponse where
  codec =
    object "GitHubUsernameResponse" $
      GitHubUsernameResponse
        <$> optionalField' "username" AC..= ghurUsername

instance ToJSON GitHubUsernameResponse where toJSON = toJSONViaCodec

--------------------------------------------------------------------------------
-- API Types
--------------------------------------------------------------------------------

-- | Public GitHub webhook endpoint (no auth, uses signature verification)
type GitHubWebhookAPI =
  "webhooks"
    :> "github"
    :> Header "X-GitHub-Event" Text
    :> Header "X-GitHub-Delivery" Text
    :> Header "X-Hub-Signature-256" Text
    :> ReqBody '[JSON] Value
    :> Post '[JSON] NoContent

-- | Project-level GitHub management API (protected)
type GitHubProjectAPI =
  "github"
    :> "repos"
    :> ( -- GET /projects/:projectId/github/repos - List linked repos
         Get '[JSON] [RepoLinkInfo]
           -- POST /projects/:projectId/github/repos - Link a repo
           :<|> ReqBody '[JSON] LinkRepoRequest :> Post '[JSON] LinkRepoResponse
           -- DELETE /projects/:projectId/github/repos/:id - Unlink a repo
           :<|> Capture "linkId" UUID :> Delete '[JSON] NoContent
       )

-- | User-level GitHub username linking API (protected)
type GitHubUserAPI =
  "github"
    :> ( -- GET /user/github - Get linked GitHub username
         Get '[JSON] GitHubUsernameResponse
           -- PUT /user/github - Link GitHub username
           :<|> ReqBody '[JSON] LinkGitHubUsernameRequest :> Put '[JSON] NoContent
           -- DELETE /user/github - Unlink GitHub username
           :<|> Delete '[JSON] NoContent
       )
