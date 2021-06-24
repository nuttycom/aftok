module Aftok.Snaplet.Json
  ( idJSON,
    zip321PaymentRequestJSON,
  )
where

import Aftok.Json (idValue, obj, v1)
import qualified Aftok.Payments.Zcash.Zip321 as Zip321
import Control.Lens (Getter)
import Data.Aeson (Value, toJSON, (.=))
import Data.Aeson.Key (fromText)
import Data.UUID (UUID)

idJSON :: forall a. Text -> Getter a UUID -> a -> Value
idJSON t l a = v1 $ obj [fromText t .= idValue l a]

zip321PaymentRequestJSON :: Zip321.PaymentRequest -> Value
zip321PaymentRequestJSON r =
  v1 . obj $
    ["zip321_request" .= (toJSON . Zip321.toURI $ r)]
