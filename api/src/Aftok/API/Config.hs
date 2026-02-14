{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | Config API types for the Aftok API.
module Aftok.API.Config
  ( -- * API Types
    ConfigAPI,

    -- * Response Types
    ClientConfig (..),
  )
where

import Data.Aeson (ToJSON)
import Servant.API

-- | Client configuration returned by the server
data ClientConfig = ClientConfig
  { recaptchaSiteKey :: Text
  }
  deriving (Show, Generic)

instance ToJSON ClientConfig

-- | Configuration API - public endpoint for client config
type ConfigAPI =
  "config" :> Get '[JSON] ClientConfig
