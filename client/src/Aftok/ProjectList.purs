module Aftok.ProjectList where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Array (index)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Effect.Aff (Aff)
import Aftok.Types
  ( System
  , ProjectId
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
  = Maybe ProjectId

data Query a
  = ProjectCreated ProjectId a

data Output
  = ProjectChange ProjectId

type Slot id
  = H.Slot Query Output id

type CState
  = { selectedPid :: Maybe ProjectId
    , projects :: Array Project
    }

data Action
  = Initialize (Maybe ProjectId)
  | Select Int

type Capability m
  = { listProjects :: m (Either APIError (Array Project))
    }

component ::
  forall m.
  Monad m =>
  System m ->
  Capability m ->
  H.Component HH.HTML Query Input Output m
component console caps =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , initialize = Just (Initialize Nothing)
              , receive = Just <<< Initialize
              }
    }
  where
  initialState :: Input -> CState
  initialState input = { selectedPid: input, projects: [] }

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
          ( [ HH.option [ P.selected (isNothing st.selectedPid), P.disabled true ] [ HH.text "Select a project" ] ]
              <> map renderOption st.projects
          )
      ]
    where
    renderOption (Project' p) =
      HH.option
        [ P.selected (any (p.projectId == _) st.selectedPid)
        , P.value $ pidStr p.projectId
        ]
        [ HH.text p.projectName ]

  handleQuery :: forall slots a. Query a -> H.HalogenM CState Action slots Output m (Maybe a)
  handleQuery = case _ of
    ProjectCreated pid a -> do
      handleAction (Initialize (Just pid))
      pure (Just a)

  handleAction :: forall slots. Action -> H.HalogenM CState Action slots Output m Unit
  handleAction = case _ of
    Initialize pidMay -> do
      res <- lift caps.listProjects
      case res of
        Left _ -> lift <<< console.error $ "Could not retrieve project list."
        Right projects -> H.modify_ (_ { projects = projects, selectedPid = pidMay })
    Select i -> do
      projects <- H.gets (_.projects)
      traverse_ projectSelected (index projects (i - 1))
    where
      projectSelected p = do
        let pid = (unwrap p).projectId
        H.modify_ (_ { selectedPid = Just pid })
        H.raise $ ProjectChange pid

apiCapability :: Capability Aff
apiCapability = { listProjects }

mockCapability :: forall m. Applicative m => Capability m
mockCapability = { listProjects: pure (Right []) }
