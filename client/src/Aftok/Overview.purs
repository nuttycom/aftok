module Aftok.Overview where

import Prelude
-- import Control.Alt ((<|>))
-- import Control.Monad.Rec.Class (forever)
-- import Control.Monad.State (State, put, get, evalState)
-- import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
-- 
-- import Data.Array (reverse, cons)
-- import Data.Date (Date, year, month, day)
-- import Data.DateTime as DT
-- import Data.DateTime (DateTime(..), date)
-- import Data.DateTime.Instant (Instant, unInstant, fromDateTime, toDateTime)
-- import Data.Either (Either(..))
-- import Data.Enum (fromEnum)
import Data.Foldable (all)
-- import Data.Map as M
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
-- import Data.Time.Duration (Milliseconds(..), Hours(..), Days(..))
-- import Data.Traversable (traverse_, traverse)
-- import Data.Tuple (Tuple(..))
-- import Data.Unfoldable as U
-- -- import Text.Format as F -- (format, zeroFill, width)
-- import Effect.Aff as Aff
import Effect.Aff (Aff)
-- import Effect.Class (liftEffect)
-- import Effect.Exception (error)
-- import Effect.Now (now)
import Halogen as H
-- import Halogen.Query.EventSource (EventSource)
-- import Halogen.Query.EventSource as EventSource
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
-- import Halogen.HTML.CSS as CSS
-- import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
-- import CSS (backgroundColor, clear, clearBoth, border, rgb, solid, borderRadius, marginLeft)
-- import CSS.Display (display, flex)
-- import CSS.Geometry (width, height)
-- import CSS.Size (px, pct)
-- import Aftok.Api.Overview as TL
-- import Aftok.Api.Overview 
--   ( OverviewError, 
--     Event(..),
--     Interval(..), 
--     TimeInterval, 
--     KeyedEvent,
--     TimeSpan, 
--     start, end, interval,
--     event, eventTime, keyedEvent
--     )
import Aftok.Project as Project
-- import Aftok.Project (Project, Project'(..), ProjectId) --, pidStr)
import Aftok.Types (System, Project, ProjectEvent(..))

type OverviewInput
  = Maybe Project

type OverviewState
  = { selectedProject :: Maybe Project
    }

data Invitation
  = InviteByEmail String
  | InviteByZaddr String

data OverviewAction
  = Initialize
  | ProjectSelected Project
  | Invite Invitation

type Slot id
  = forall query. H.Slot query ProjectEvent id

type Slots
  = ( projectList :: Project.ProjectListSlot Unit
    )

_projectList = SProxy :: SProxy "projectList"

type Capability (m :: Type -> Type)
  = {
    }

component ::
  forall query m.
  Monad m =>
  System m ->
  Capability m ->
  Project.Capability m ->
  H.Component HH.HTML query OverviewInput ProjectEvent m
component system caps pcaps =
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
  initialState :: OverviewInput -> OverviewState
  initialState input =
    { selectedProject: input
    }

  render :: OverviewState -> H.ComponentHTML OverviewAction Slots m
  render st =
    HH.section
      [ P.classes (ClassName <$> [ "section-border", "border-primary" ]) ]
      [ HH.div
          [ P.classes (ClassName <$> [ "container", "pt-6" ]) ]
          [ HH.h1
              [ P.classes (ClassName <$> [ "mb-0", "font-weight-bold", "text-center" ]) ]
              [ HH.text "Project Overview" ]
          , HH.p
              [ P.classes (ClassName <$> [ "col-md-5", "text-muted", "text-center", "mx-auto" ]) ]
              [ HH.text "Your project timeline" ]
          , HH.div_
              [ HH.slot _projectList unit (Project.projectListComponent system pcaps) st.selectedProject (Just <<< ProjectSelected) ]
          , HH.div
              [ P.classes (ClassName <$> if isNothing st.selectedProject then [ "collapse" ] else []) ]
              []
          ]
      ]

  eval :: OverviewAction -> H.HalogenM OverviewState OverviewAction Slots ProjectEvent m Unit
  eval action = do
    case action of
      Initialize -> do
        pure unit
      Invite _ -> do
        pure unit
      ProjectSelected p -> do
        currentProject <- H.gets (_.selectedProject)
        when (all (\p' -> (unwrap p').projectId /= (unwrap p).projectId) currentProject)
          $ do
              H.raise (ProjectChange p)
              H.modify_ (_ { selectedProject = Just p })

apiCapability :: Capability Aff
apiCapability = {}

mockCapability :: Capability Aff
mockCapability = {}
