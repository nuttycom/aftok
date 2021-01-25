module Aftok.Api.Recaptcha
  ( getRecaptchaResponse
  ) where

import Prelude (bind, (==), ($), pure)
import Data.Maybe (Maybe(..))
import Effect (Effect)

getRecaptchaResponse :: Maybe String -> Effect (Maybe String)
getRecaptchaResponse elemId = do
  resp <- case elemId of
    Just eid -> getRecaptchaResponseInternal true eid
    Nothing -> getRecaptchaResponseInternal false ""
  pure $ if resp == "" then Nothing else Just resp

foreign import getRecaptchaResponseInternal :: Boolean -> String -> Effect String
