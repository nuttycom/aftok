module Aftok.Project where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Except.Trans (ExceptT, runExceptT, except, withExceptT)
import Control.Monad.Error.Class (throwError)

import Data.Array (index)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..), isNothing)
import Data.Traversable (traverse, traverse_)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class as EC
import Affjax (get, printError)
import Affjax.StatusCode (StatusCode(..))
import Affjax.ResponseFormat as RF

import Aftok.Types 
  ( APIError(..)
  , System
  , parseDate
  , pidStr
  , Project'(..)
  , Project
  )

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

type ProjectInput = Maybe Project

type ProjectCState =
  { selectedProject :: Maybe Project
  , projects :: Array Project
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
  -> H.Component HH.HTML query ProjectInput Project m
projectListComponent console caps = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = eval
      , initialize = Just Initialize 
      }
  } where

    initialState :: ProjectInput -> ProjectCState
    initialState input = { selectedProject: input, projects: [] }

    render :: forall slots. ProjectCState -> H.ComponentHTML ProjectAction slots m
    render st = 
      HH.div
        [P.classes (ClassName <$> ["form-group"])]
        [ HH.label
          [ P.classes (ClassName <$> ["sr-only"])
          , P.for "projectSelect"
          ]
          [ HH.text "Project" ]
        , HH.select
          [P.classes (ClassName <$> ["form-control"])
          ,P.id_ "projectSelect"
          ,E.onSelectedIndexChange (Just <<< Select)
          ]
          ( [HH.option [P.selected (isNothing st.selectedProject), P.disabled true] [HH.text "Select a project"]] 
            <> map renderOption st.projects
          )
        ]
      where
        renderOption (Project' p) =
          HH.option 
            [ P.selected (any (\(Project' p') -> p'.projectId == p.projectId) st.selectedProject)
            , P.value $ pidStr p.projectId
            ]
            [HH.text p.projectName]


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

