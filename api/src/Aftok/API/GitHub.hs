{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | GitHub integration API types for the Aftok API.
module Aftok.API.GitHub
  ( -- * API Types
    GitHubWebhookAPI,
    GitHubProjectAPI,
    GitHubUserAPI,
    GitHubOAuthCallbackAPI,

    -- * Request/Response Types
    LinkRepoRequest (..),
    LinkRepoResponse (..),
    RepoLinkInfo (..),
    GitHubOAuthInitResponse (..),
    GitHubUsernameResponse (..),
    GitHubWebhookPayload (..),
  )
where

import Aftok.API.Codec ()
import Aftok.Types (GitHubRepoLinkId (..))
import Autodocodec (HasCodec (..), object, optionalField', requiredField')
import qualified Autodocodec as AC
import Autodocodec.Aeson (parseJSONViaCodec, toJSONViaCodec)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
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

-- | Response containing the GitHub OAuth authorization URL
data GitHubOAuthInitResponse = GitHubOAuthInitResponse
  { goirAuthUrl :: Text
  }
  deriving (Show, Eq, Generic)

instance HasCodec GitHubOAuthInitResponse where
  codec =
    object "GitHubOAuthInitResponse" $
      GitHubOAuthInitResponse
        <$> requiredField' "authUrl" AC..= goirAuthUrl

instance ToJSON GitHubOAuthInitResponse where toJSON = toJSONViaCodec

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

-- | Raw webhook request body. Wrapped to permit per-newtype OpenAPI
-- 'ToSchema' (openapi3 cannot derive a schema for bare 'ByteString'). The
-- HMAC verification depends on byte-identical comparison with what
-- GitHub signed, so the handler MUST treat this body as opaque bytes
-- (not parsed-and-re-encoded JSON) when computing the signature.
newtype GitHubWebhookPayload = GitHubWebhookPayload {unGitHubWebhookPayload :: ByteString}
  deriving (MimeRender OctetStream, MimeUnrender OctetStream)

--------------------------------------------------------------------------------
-- API Types
--------------------------------------------------------------------------------

-- | Public GitHub webhook endpoint (no auth, uses signature verification)
--
-- The request body is consumed as raw bytes; HMAC verification depends on
-- byte-identical comparison with what GitHub signed. JSON parsing happens
-- in the handler after signature verification succeeds.
type GitHubWebhookAPI =
  "webhooks"
    :> "github"
    :> Header "Content-Type" Text
    :> Header "X-GitHub-Event" Text
    :> Header "X-GitHub-Delivery" Text
    :> Header "X-Hub-Signature-256" Text
    :> ReqBody '[OctetStream] GitHubWebhookPayload
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
           -- POST /user/github/link - Initiate GitHub OAuth flow
           :<|> "link" :> Post '[JSON] GitHubOAuthInitResponse
           -- DELETE /user/github - Unlink GitHub username
           :<|> Delete '[JSON] NoContent
       )

-- | GitHub OAuth callback endpoint (public, auth via state JWT)
type GitHubOAuthCallbackAPI =
  "user"
    :> "github"
    :> "callback"
    :> QueryParam "code" Text
    :> QueryParam "state" Text
    :> QueryParam "error" Text
    :> Verb 'GET 302 '[PlainText] (Headers '[Header "Location" Text] NoContent)
