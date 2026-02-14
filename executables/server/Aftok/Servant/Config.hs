{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Aftok.Servant.Config
  ( -- * API Types (re-exported from aftok-api)
    ConfigAPI,
    ClientConfig (..),

    -- * Server
    configServer,
  )
where

import Aftok.API.Config (ClientConfig (..), ConfigAPI)
import Aftok.ServerConfig (CaptchaConfig, captchaSiteKey)
import Control.Lens ((^.))
import Servant (ServerT)

-- | Config server implementation
configServer :: (Applicative m) => CaptchaConfig -> ServerT ConfigAPI m
configServer captchaCfg =
  pure $ ClientConfig (captchaCfg ^. captchaSiteKey)
