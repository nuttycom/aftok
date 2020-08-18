module Aftok.Project where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Except.Trans (ExceptT, runExceptT, except)
import Control.Monad.Error.Class (throwError)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.JSDate as JSDate
import Data.Either (Either(..), note)
-- import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.UUID (UUID, parseUUID)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Affjax (get, printError)
import Affjax.StatusCode (StatusCode(..))
import Affjax.ResponseFormat as RF

-- import Halogen as H
-- import Halogen.HTML.Core (ClassName(..))
-- import Halogen.HTML as HH
-- import Halogen.HTML.CSS as CSS
-- import Halogen.HTML.Events as E
-- import Web.Event.Event as WE
-- import Halogen.HTML.Properties as P

newtype Project' date = Project'
  { projectName :: String
  , inceptionDate :: date
  , initiator :: UUID
  }

type Project = Project' DateTime

type Capability m =
  { listProjects :: m (Either APIError (Array Project))
  }

data APIError 
  = Forbidden
  | ParseFailure Json String
  | Error { status :: Maybe StatusCode, message :: String }

instance decodeJsonProject :: DecodeJson (Project' String) where
  decodeJson json = do
    x <- decodeJson json
    projectName <- x .: "projectName" 
    inceptionDate <- x .: "inceptionDate" 
    initiatorStr <- x .: "initiator" 
    initiator <- note "Failed to decode initiator UUID" $ parseUUID initiatorStr
    pure $ Project' { projectName, inceptionDate, initiator }


listProjects :: Aff (Either APIError (Array Project))
listProjects = do
  result <- get RF.json "/api/projects" 
  liftEffect <<< runExceptT $ case result of
    Left err -> throwError $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> 
        throwError Forbidden
      StatusCode 200 -> do
        records <- except $ lmap (ParseFailure r.body) (decodeJson r.body)
        traverse parseProject records
      other -> 
        throwError $ Error { status: Just other, message: r.statusText }

parseProject :: Json -> ExceptT APIError Effect Project
parseProject json = do
  Project' p <- except <<< lmap (ParseFailure json) $ decodeJson json
  jsDate <- lift $ JSDate.parse p.inceptionDate
  pdate <- except $ note (ParseFailure json "Could not parse inception date") 
                         (JSDate.toDateTime jsDate)
  pure $ Project' (p { inceptionDate = pdate })

apiCapability :: Capability Aff
apiCapability = { listProjects }

mockCapability :: forall m. Applicative m => Capability m
mockCapability = { listProjects: pure (Right []) }

