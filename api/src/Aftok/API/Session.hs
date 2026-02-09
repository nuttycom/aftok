{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Session API types for the Aftok API.
module Aftok.API.Session
  ( -- * API Types
    SessionAPI,
    ProtectedSessionAPI,

    -- * Response Types
    LoginCheckResponse (..),
  )
where

import Aftok.API.Auth (AuthenticatedUser, LoginRequest)
import Aftok.API.Codec ()
import Autodocodec (HasCodec (..), object, requiredField', (.=))
import Autodocodec.Aeson (toJSONViaCodec)
import Data.Aeson (ToJSON (..))
import Servant.API
import Servant.Auth.Server (SetCookie)

-- | Session API (public endpoints)
type SessionAPI =
  -- POST /login - authenticate and get session cookie
  "login"
    :> ReqBody '[JSON] LoginRequest
    :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    -- GET /logout - clear session
    :<|> "logout" :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

-- | Protected Session API (requires auth)
type ProtectedSessionAPI =
  -- GET /login/check - verify current session
  "login" :> "check" :> Get '[JSON] LoginCheckResponse

-- | Login check response
data LoginCheckResponse = LoginCheckResponse
  { loggedIn :: Bool,
    loginUser :: Maybe AuthenticatedUser
  }
  deriving (Show, Eq, Generic)

instance HasCodec LoginCheckResponse where
  codec =
    object "LoginCheckResponse" $
      LoginCheckResponse
        <$> requiredField' "loggedIn" .= loggedIn
        <*> requiredField' "user" .= loginUser

instance ToJSON LoginCheckResponse where toJSON = toJSONViaCodec
