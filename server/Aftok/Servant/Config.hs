{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.Config
  ( -- * API
    ConfigAPI,

    -- * Response Types
    ClientConfig (..),

    -- * Server
    configServer,
  )
where

import Aftok.ServerConfig (CaptchaConfig, captchaSiteKey)
import Control.Lens ((^.))
import Data.Aeson (ToJSON)
import Servant

-- | Client configuration returned by the server
data ClientConfig = ClientConfig
  { recaptchaSiteKey :: Text
  }
  deriving (Show, Generic)

instance ToJSON ClientConfig

-- | Configuration API - public endpoint for client config
type ConfigAPI =
  "config" :> Get '[JSON] ClientConfig

-- | Config server implementation
configServer :: Applicative m => CaptchaConfig -> ServerT ConfigAPI m
configServer captchaCfg =
  pure $ ClientConfig (captchaCfg ^. captchaSiteKey)
