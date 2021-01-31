module Aftok.Api.Recaptcha
  ( getRecaptchaResponse
  , recaptchaRender
  ) where

import Prelude (bind, (==), ($), pure, Unit)
import Data.Maybe (Maybe(..))
import Effect (Effect)

getRecaptchaResponse :: Maybe String -> Effect (Maybe String)
getRecaptchaResponse elemId = do
  resp <- case elemId of
    Just eid -> getRecaptchaResponseInternal true eid
    Nothing -> getRecaptchaResponseInternal false ""
  pure $ if resp == "" then Nothing else Just resp

recaptchaRender :: String -> String -> Effect Unit
recaptchaRender = recaptchaRenderInternal

foreign import getRecaptchaResponseInternal :: Boolean -> String -> Effect String

foreign import recaptchaRenderInternal :: String -> String -> Effect Unit
