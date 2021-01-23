module Aftok.Snaplet.Json
  ( idJSON,
  )
where

import Aftok.Json (idValue, obj, v1)
import Control.Lens (Getter)
import Data.Aeson ((.=), Value)
import Data.UUID (UUID)

idJSON :: forall a. Text -> Getter a UUID -> a -> Value
idJSON t l a = v1 $ obj [t .= idValue l a]
