{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Config API types for the Aftok API.
module Aftok.API.Config
  ( -- * API Types
    ConfigAPI,

    -- * Response Types
    ClientConfig (..),
  )
where

import qualified Autodocodec as AC
import Autodocodec (HasCodec (..), object, requiredField')
import Autodocodec.Aeson (toJSONViaCodec)
import Data.Aeson (ToJSON (..))
import Servant.API

-- | Client configuration returned by the server
data ClientConfig = ClientConfig
  { recaptchaSiteKey :: Text
  }
  deriving (Show, Generic)

instance HasCodec ClientConfig where
  codec =
    object "ClientConfig" $
      ClientConfig
        <$> requiredField' "recaptchaSiteKey" AC..= recaptchaSiteKey

instance ToJSON ClientConfig where toJSON = toJSONViaCodec

-- | Configuration API - public endpoint for client config
type ConfigAPI =
  "config" :> Get '[JSON] ClientConfig
