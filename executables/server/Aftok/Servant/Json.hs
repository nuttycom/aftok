module Aftok.Servant.Json
  ( idJSON,
    zip321PaymentRequestJSON,
  )
where

import qualified Aftok.Currency.Zcash.Zip321 as Zip321
import Aftok.Json (idValue)
import Control.Lens (Getter)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson.Key (fromText)
import Data.UUID (UUID)

idJSON :: forall a. Text -> Getter a UUID -> a -> Value
idJSON t l a = object [fromText t .= idValue l a]

zip321PaymentRequestJSON :: Zip321.PaymentRequest -> Value
zip321PaymentRequestJSON r =
  object
    ["zip321_request" .= (toJSON . Zip321.toURI $ r)]
