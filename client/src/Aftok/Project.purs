module Aftok.Project where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Except.Trans (ExceptT, runExceptT, except, withExceptT)
import Control.Monad.Error.Class (throwError)

import Data.Array (index)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (traverse, traverse_)
import Data.UUID (UUID, parseUUID, toString)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Affjax (get, printError)
import Affjax.StatusCode (StatusCode(..))
import Affjax.ResponseFormat as RF

import Aftok.Types (APIError(..), System, parseDate)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

newtype ProjectId = ProjectId UUID
derive instance projectIdEq :: Eq ProjectId
derive instance projectIdNewtype :: Newtype ProjectId _

pidStr :: ProjectId -> String
pidStr (ProjectId uuid) = toString uuid

newtype Project' date = Project'
  { projectId :: ProjectId
  , projectName :: String
  , inceptionDate :: date
  , initiator :: UUID
  }
derive instance newtypeProject :: Newtype (Project' a) _

type Project = Project' DateTime

type ProjectCState =
  { projects :: Array Project
  }

data ProjectAction 
  = Initialize
  | Select Int

type ProjectListSlot id = forall query. H.Slot query Project id

type Capability m =
  { listProjects :: m (Either APIError (Array Project))
  }

projectListComponent
  :: forall query input m
  .  Monad m
  => System m
  -> Capability m 
  -> H.Component HH.HTML query input Project m
projectListComponent console caps = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = eval
      , initialize = Just Initialize 
      }
  } where

    initialState :: input -> ProjectCState
    initialState _ = { projects: [] }

    render :: forall slots. ProjectCState -> H.ComponentHTML ProjectAction slots m
    render st = 
     let renderOption (Project' p) =
           HH.option [P.value $ pidStr p.projectId] [HH.text p.projectName]
      in HH.div
           [P.classes (ClassName <$> ["form-group"])]
           [HH.label
             [ P.classes (ClassName <$> ["sr-only"])
             , P.for "projectSelect"
             ]
             [ HH.text "Project" ]
           ,HH.select
             [P.classes (ClassName <$> ["form-control"])
             ,P.id_ "projectSelect"
             ,E.onSelectedIndexChange (Just <<< Select)
             ]
             ([HH.option [P.selected true, P.disabled true] [HH.text "Select a project"]] <> map renderOption st.projects)
           ]


    eval :: ProjectAction -> H.HalogenM ProjectCState ProjectAction () Project m Unit
    eval = case _ of
      Initialize -> do
        res <- lift caps.listProjects
        case res of 
            Left _ -> lift <<< console.error $ "Could not retrieve project list."
            Right projects -> H.modify_ (_ { projects = projects })

      Select i -> do
        projects <- H.gets (_.projects)
        lift <<< console.log $ "Selected project index " <> show i
        traverse_ H.raise (index projects (i - 1))

instance decodeJsonProject :: DecodeJson (Project' String) where
  decodeJson json = do
    x <- decodeJson json
    project <- x .: "project"

    projectIdStr <- x .: "projectId"
    projectId    <- ProjectId <$> (note "Failed to decode project UUID" $ parseUUID projectIdStr)

    projectName   <- project .: "projectName"
    inceptionDate <- project .: "inceptionDate"
    initiatorStr  <- project .: "initiator"
    initiator     <- note "Failed to decode initiator UUID" $ parseUUID initiatorStr
    pure $ Project' { projectId, projectName, inceptionDate, initiator }

listProjects :: Aff (Either APIError (Array Project))
listProjects = do
  result <- get RF.json "/api/projects" 
  EC.liftEffect <<< runExceptT $ case result of
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
  pdate <- withExceptT (ParseFailure json) $ parseDate p.inceptionDate
  pure $ Project' (p { inceptionDate = pdate })

apiCapability :: Capability Aff
apiCapability = { listProjects }

mockCapability :: forall m. Applicative m => Capability m
mockCapability = { listProjects: pure (Right []) }

