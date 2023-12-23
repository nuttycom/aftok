{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Aftok.Server.Auth where

import Aftok.Currency.Zcash (RPCError (..), ZValidateAddressErr (..))
import qualified Aftok.Currency.Zcash.Types as Zcash
import Aftok.Database (InvitationError (..))
import Aftok.Project (Invitation, InvitationCode, parseInvCode)
import Aftok.Types
  ( Email (..),
    RecoverBy (..),
    UserId (..),
    UserName (..),
    _UserId,
  )
import Control.Lens
  ( makeLenses,
    (^.),
  )
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import qualified Data.Aeson as A
-- import Data.Aeson.Types (typeMismatch)
-- import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Thyme.Clock as C
import Network.HTTP.Client
  ( httpLbs,
    parseRequest,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Client.MultipartFormData
  ( formDataBody,
    partBS,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import Servant.Auth.Server
  ( FromJWT (..),
    ToJWT (..),
  )

data Login = Login
  { _loginUsername :: UserName,
    _loginPassword :: Text
  }

makeLenses ''Login

instance FromJSON Login where
  parseJSON = withObject "Login" $
    \v -> Login <$> (UserName <$> v .: "username") <*> v .: "password"

data UserJWT = UserJWT
  { _userId :: UserId
  }

makeLenses ''UserJWT

instance ToJSON UserJWT where
  toJSON u = object ["user_id" .= (u ^. (userId . _UserId))]

instance FromJSON UserJWT where
  parseJSON = withObject "UserJWT" $ \v -> UserJWT <$> v .: "user_id"

instance FromJWT UserJWT

instance ToJWT UserJWT
