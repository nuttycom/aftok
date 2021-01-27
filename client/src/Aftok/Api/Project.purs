module Aftok.Api.Project where

import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT, except, withExceptT)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Map as M
import Data.Newtype (class Newtype)
import Data.Rational (Rational)
import Data.Time.Duration (Hours, Days)
import Data.Traversable (traverse)
import Data.UUID (parseUUID)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Affjax (get, printError)
import Affjax.StatusCode (StatusCode(..))
import Affjax.ResponseFormat as RF
import Aftok.Types
  ( UserId(..)
  , ProjectId(..)
  )
import Aftok.Api.Types
  ( APIError(..) )
import Aftok.Api.Json (parseDate)

newtype Project' date
  = Project'
  { projectId :: ProjectId
  , projectName :: String
  , inceptionDate :: date
  , initiator :: UserId
  }

derive instance newtypeProject :: Newtype (Project' a) _

type Project
  = Project' DateTime

data DepreciationFn
  = LinearDepreciation { undep :: Days, dep :: Days }

newtype ProjectUserData' date 
  = ProjectUserData'
  { userName :: String
  , joinedOn :: date
  , totalContribution :: Hours
  , currentPayoutRatio :: Rational
  }

newtype ProjectDetail' date
  = ProjectDetail'
  { project :: Project' date
  , depreciation :: DepreciationFn
  , contributors :: M.Map UserId (ProjectUserData' date)
  }

type ProjectDetail = ProjectDetail' DateTime

data ProjectEvent
  = ProjectChange Project

instance decodeJsonProject :: DecodeJson (Project' String) where
  decodeJson json = do
    x <- decodeJson json
    project <- x .: "project"
    projectIdStr <- x .: "projectId"
    projectId <- ProjectId <$> (note "Failed to decode project UUID" $ parseUUID projectIdStr)
    projectName <- project .: "projectName"
    inceptionDate <- project .: "inceptionDate"
    initiatorStr <- project .: "initiator"
    initiator <- UserId <$> (note "Failed to decode initiator UUID" $ parseUUID initiatorStr)
    pure $ Project' { projectId, projectName, inceptionDate, initiator }

newtype Member' date
  = Member'
  { userId :: UserId
  , handle :: String
  , joinedOn :: date
  , timeDevoted :: Hours
  , revShareFrac :: Rational
  }

listProjects :: Aff (Either APIError (Array Project))
listProjects = do
  result <- get RF.json "/api/projects"
  EC.liftEffect <<< runExceptT
    $ case result of
        Left err -> throwError $ Error { status: Nothing, message: printError err }
        Right r -> case r.status of
          StatusCode 403 -> throwError Forbidden
          StatusCode 200 -> do
            records <- except $ lmap (ParseFailure r.body) (decodeJson r.body)
            traverse parseProject records
          other -> throwError $ Error { status: Just other, message: r.statusText }

parseProject :: Json -> ExceptT APIError Effect Project
parseProject json = do
  Project' p <- except <<< lmap (ParseFailure json) $ decodeJson json
  pdate <- withExceptT (ParseFailure json) $ parseDate p.inceptionDate
  pure $ Project' (p { inceptionDate = pdate })

