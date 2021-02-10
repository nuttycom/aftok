module Aftok.Snaplet.Json
  ( idJSON,
    zip321PaymentRequestJSON,
  )
where

import qualified Aftok.Currency.Zcash.Zip321 as Zip321
import Aftok.Json (idValue, obj, v1)
import Control.Lens (Getter)
import Data.Aeson ((.=), Value, toJSON)
import Data.UUID (UUID)

idJSON :: forall a. Text -> Getter a UUID -> a -> Value
idJSON t l a = v1 $ obj [t .= idValue l a]

zip321PaymentRequestJSON :: Zip321.PaymentRequest -> Value
zip321PaymentRequestJSON r =
  v1 . obj $
    ["zip321_request" .= (toJSON . Zip321.toURI $ r)]
