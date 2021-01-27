module Aftok.Overview where

import Prelude
-- import Control.Alt ((<|>))
-- import Control.Monad.Rec.Class (forever)
-- import Control.Monad.State (State, put, get, evalState)
-- import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
--
-- import Data.Array (reverse, cons)
import Data.Enum (toEnum)
import Data.Date (canonicalDate)
-- import Data.Date.Component (Year(..), Month(..), Day(..))
import Data.List as L
import Data.DateTime (DateTime(..), date)
import Data.DateTime.Instant (Instant)
import Data.Time.Duration (Hours(..), Days(..))
-- import Data.Either (Either(..))
-- import Data.Enum (fromEnum)
import Data.Fixed as F
import Data.Foldable (all)
-- import Data.Formatters.DateTime
import Data.Rational as R
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isNothing, fromMaybe)
import Data.Unfoldable as U
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
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
import Aftok.ProjectList as ProjectList
-- import Aftok.Project (Project, Project'(..), ProjectId) --, pidStr)
import Aftok.Types (System, ProjectId, dateStr)
import Aftok.Api.Project 
  (Project, Project'(..), ProjectEvent(..), Member', ProjectDetail, ProjectDetail'(..)
  , DepreciationFn(..)
  , ProjectUserData'(..)
  , ProjectUserData
  )

type OverviewInput
  = Maybe Project

type OverviewState
  = { selectedProject :: Maybe Project
    , projectDetail :: Maybe ProjectDetail
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
  = ( projectList :: ProjectList.Slot Unit
    )

_projectList = SProxy :: SProxy "projectList"

type Capability (m :: Type -> Type)
  = { getProjectMembers :: ProjectId -> m (Array (Member' Instant))
    }

component ::
  forall query m.
  Monad m =>
  System m ->
  Capability m ->
  ProjectList.Capability m ->
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
    , projectDetail: case input of
        Nothing -> Nothing
        Just p -> Just $ ProjectDetail' 
          { project: p
          , depreciation: LinearDepreciation { undep: Days 30.0, dep: Days 300.0 }
          , contributors: M.singleton (unwrap p).initiator $ ProjectUserData' 
            { userName: "Joe"
            , joinedOn: DateTime (fromMaybe bottom $ canonicalDate <$> (toEnum 2021) <*>  (toEnum 1) <*>  (toEnum 26)) bottom
            , totalContribution: Hours 100.0
            , currentPayoutRatio: 55 R.% 100
            }
          
          }
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
          [ HH.text "Your project details" ]
        , HH.div_
          [ HH.slot _projectList unit (ProjectList.component system pcaps) st.selectedProject (Just <<< ProjectSelected) ]
        , HH.div
          [ P.classes (ClassName <$> if isNothing st.selectedProject then [ "collapse" ] else []) ]
          (U.fromMaybe $ projectDetail <$> st.projectDetail)
        ]
      ]

  projectDetail :: ProjectDetail -> H.ComponentHTML OverviewAction Slots m
  projectDetail (ProjectDetail' detail) = do
    let (Project' project) = detail.project
    HH.div
      [ P.classes (ClassName <$> [ "container-fluid" ]) ]
      [ HH.section
        [ P.id_ "projectOverview", P.classes (ClassName <$> ["pt-3"]) ]
        [ HH.div
          -- header
          [ P.classes (ClassName <$> ["row", "pt-3", "font-weight-bold" ]) ]
          [ colmd2 (Just "Project Name")
          , colmd2 (Just "Undepreciated Period")
          , colmd2 (Just "Depreciation Duration")
          , colmd2 (Just "Originator")
          , colmd2 (Just "Origination Date")
          ]
        , HH.div
          [ P.classes (ClassName <$> ["row", "pt-3"]) ]
          ([ colmd2 (Just project.projectName) ] <>
           depreciationCols detail.depreciation <>
           [ colmd2 ((\(ProjectUserData' p) -> p.userName) <$> M.lookup project.initiator detail.contributors)
           , colmd2 (Just $ dateStr (date project.inceptionDate))
           ])
        ]
      , HH.section
        [ P.id_ "contributors" ]
        ([ HH.div
          -- header
          [ P.classes (ClassName <$> ["row", "pt-3", "font-weight-bold" ]) ]
          [ colmd2 (Just "Contributor")
          , colmd2 (Just "Joined")
          , colmd2 (Just "Contributed Hours")
          , colmd2 (Just "Current Revenue Share")
          ]
        ] <> 
          (contributorCols <$> (L.toUnfoldable $ M.values detail.contributors))
        )
      ]

  depreciationCols :: DepreciationFn -> Array (H.ComponentHTML OverviewAction Slots m)
  depreciationCols = case _ of
    LinearDepreciation obj ->
      [ colmd2 (Just $ show (unwrap obj.undep) <> " days")
      , colmd2 (Just $ show (unwrap obj.dep) <> " days")
      ]

  contributorCols :: ProjectUserData -> H.ComponentHTML OverviewAction Slots m
  contributorCols (ProjectUserData' pud) = 
    let pct = maybe "N/A" (\f -> F.toString (f * F.fromInt 100)) (F.fromNumber (R.toNumber pud.currentPayoutRatio) :: Maybe (F.Fixed F.P10000)) 
     in HH.div
      [ P.classes (ClassName <$> ["row", "pt-3", "pb-2" ]) ]
      [ colmd2 (Just pud.userName)
      , colmd2 (Just $ dateStr (date pud.joinedOn))
      , colmd2 (Just $ show (unwrap pud.totalContribution))
      , colmd2 (Just $ pct <> "%")
      ]

  colmd2 :: Maybe String -> H.ComponentHTML OverviewAction Slots m
  colmd2 xs = HH.div [ P.classes (ClassName <$> ["col-md-2"]) ] (U.fromMaybe $ HH.text <$> xs)

--         </section>
--              <!-- Map payouts -->
--              <div class="row font-weight-bold">
--                 <div class="col-md-2">
--                 </div>
--                 <div class="col-md-4">
--                     Payments
--                 </div>
--                 <div class="col-md-6">
--
--                 </div>
--              </div>
--              <div class="row">
--                 <div class="col-md-2">
--                 </div>
--                 <div class="col-md-2">
--                     Oct 20 2020
--                 </div>
--                 <div class="col-md-2">
--                     100 zec
--                 </div>
--                 <div class="col-md-2">
--                     Acme PaidUsRight
--                 </div>
--                 <div class="col-md-4">
--                 </div>
--             </div>
--             <!-- map payout creditTos-->
--             <div class="row pt-3">
--                 <div class="col-md-4">
--                 </div>
--                 <div class="col-md-2">
--                     Freuline Fred
--                 </div>
--                 <div class="col-md-2">
--                     2.4 zec
--                 </div>
--                 <div class="col-md-2">
--                     2.4 %
--                 </div>
--                 <div class="col-md-2">
--                 </div>
--             </div>
--             <div class="row pt-3">
--                 <div class="col-md-4">
--                 </div>
--                 <div class="col-md-2">
--                     Goobie Works A Lot
--                 </div>
--                 <div class="col-md-2">
--                     50 zec
--                 </div>
--                 <div class="col-md-2">
--                     50 %
--                 </div>
--                 <div class="col-md-2">
--                 </div>
--             </div> <div class="row pt-3">
--                 <div class="col-md-4">
--                 </div>
--                 <div class="col-md-2">
--                     Average Fella
--                 </div>
--                 <div class="col-md-2">
--                     25 zec
--                 </div>
--                 <div class="col-md-2">
--                     25 %
--                 </div>
--                 <div class="col-md-2">
--                 </div>
--             </div> <div class="row pt-3">
--                 <div class="col-md-4">
--                 </div>
--                 <div class="col-md-2">
--                     Cool Kid
--                 </div>
--                 <div class="col-md-2">
--                     24.6 zec
--                 </div>
--                 <div class="col-md-2">
--                     24.6 %
--                 </div>
--                 <div class="col-md-2">
--                 </div>
--             </div>
--
--         </section>
--
--
--         <!-- New Project form-->
--         <section id="addProject">
--
--             <div class="row pt-3">
--                 <div class="col-md-4">
--                     <span class="float-right">Project Name</span>
--                 </div>
--                 <div class="col-md-4">
--                     <input type="text" id="projectName" name="projectName" />
--                 </div>
--             </div>
--
--             <div class="row pt-3">
--                 <div class="col-md-4">
--                     <span class="float-right">Undepreciated Period ( Months )</span>
--                 </div>
--                 <div class="col-md-4">
--                     <input type="text" id="undepreciatedPeriod" name="undepreciatedPeriod" />
--                 </div>
--             </div>
--
--             <div class="row pt-3">
--                 <div class="col-md-4">
--                     <span class="float-right">Depreciation Duration ( Months )</span>
--                 </div>
--                 <div class="col-md-4">
--                     <input type="text" id="depreciationDuration" name="depreciationDuration" />
--                 </div>
--             </div>
--
--             <div class="row pt-3 pb-3">
--                 <div class="col-md-2">
--                 </div>
--                 <div class="col-md-10">
--                     <button class="btn btn-sm btn-primary lift ml-auto">Add Project</button>
--                 </div>
--             </div>
--
--         </section>

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
apiCapability =
  { getProjectMembers: \_ -> pure []
  }

mockCapability :: Capability Aff
mockCapability =
  { getProjectMembers: \_ -> pure []
  }
