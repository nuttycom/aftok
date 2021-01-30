module Aftok.ProjectList where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Array (index)
-- import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..), isNothing)
import Data.Traversable (traverse_)
import Effect.Aff (Aff)
import Aftok.Types
  ( System
  , pidStr
  )
import Aftok.Api.Types
  ( APIError
  )
import Aftok.Api.Project
  ( Project'(..)
  , Project
  , listProjects
  )
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

type Input
  = Maybe Project

data Event
  = ProjectChange Project

type Slot id
  = forall query. H.Slot query Event id

type CState
  = { selectedProject :: Maybe Project
    , projects :: Array Project
    }

data Action
  = Initialize
  | Select Int

type Capability m
  = { listProjects :: m (Either APIError (Array Project))
    }

component ::
  forall query m.
  Monad m =>
  System m ->
  Capability m ->
  H.Component HH.HTML query Input Event m
component console caps =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = eval
              , initialize = Just Initialize
              }
    }
  where
  initialState :: Input -> CState
  initialState input = { selectedProject: input, projects: [] }

  render :: forall slots. CState -> H.ComponentHTML Action slots m
  render st =
    HH.div
      [ P.classes (ClassName <$> [ "form-group" ]) ]
      [ HH.label
          [ P.classes (ClassName <$> [ "sr-only" ])
          , P.for "projectSelect"
          ]
          [ HH.text "Project" ]
      , HH.select
          [ P.classes (ClassName <$> [ "form-control" ])
          , P.id_ "projectSelect"
          , E.onSelectedIndexChange (Just <<< Select)
          ]
          ( [ HH.option [ P.selected (isNothing st.selectedProject), P.disabled true ] [ HH.text "Select a project" ] ]
              <> map renderOption st.projects
          )
      ]
    where
    renderOption (Project' p) =
      HH.option
        [ P.selected (any (\(Project' p') -> p'.projectId == p.projectId) st.selectedProject)
        , P.value $ pidStr p.projectId
        ]
        [ HH.text p.projectName ]

  eval :: Action -> H.HalogenM CState Action () Event m Unit
  eval = case _ of
    Initialize -> do
      res <- lift caps.listProjects
      case res of
        Left _ -> lift <<< console.error $ "Could not retrieve project list."
        Right projects -> H.modify_ (_ { projects = projects })
    Select i -> do
      projects <- H.gets (_.projects)
      lift <<< console.log $ "Selected project index " <> show i
      traverse_ (H.raise <<< ProjectChange) (index projects (i - 1))

apiCapability :: Capability Aff
apiCapability = { listProjects }

mockCapability :: forall m. Applicative m => Capability m
mockCapability = { listProjects: pure (Right []) }
