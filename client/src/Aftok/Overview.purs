module Aftok.Overview where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Array (reverse, sortWith)
import Data.List as L
import Data.DateTime (DateTime, date)
import Data.Time.Duration (Hours(..), Days(..))
import Data.Either (Either(..))
import Data.Fixed as F
import Data.Foldable (all)
import Data.Ratio as R
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, isNothing)
import Data.Unfoldable as U
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.UUID (genUUID)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Aftok.HTML.Classes as C
import Aftok.ProjectList as ProjectList
import Aftok.Projects.Invite as Invite
import Aftok.Projects.Create as Create
import Aftok.Types (System, ProjectId, UserId(..), dateStr)
import Aftok.Api.Types (APIError)
import Aftok.Api.Project
  ( Project'(..)
  , ProjectDetail
  , ProjectDetail'(..)
  , DepreciationFn(..)
  , Contributor'(..)
  , getProjectDetail
  )

type OverviewInput
  = Maybe ProjectId

type OverviewState
  = { selectedProject :: Maybe ProjectId
    , projectDetail :: Maybe ProjectDetail
    }

data OverviewAction
  = Initialize
  | ProjectSelected ProjectId
  | OpenCreateModal
  | OpenInviteModal ProjectId

type Slot id
  = forall query. H.Slot query ProjectList.Output id

type Slots
  = ( projectList :: ProjectList.Slot Unit
    , projectCreateModal :: Create.Slot Unit
    , invitationModal :: Invite.Slot Unit
    )

_projectList = SProxy :: SProxy "projectList"
_projectCreateModal = SProxy :: SProxy "projectCreateModal"
_invitationModal = SProxy :: SProxy "invitationModal"

type Capability (m :: Type -> Type)
  = { getProjectDetail :: ProjectId -> m (Either APIError (Maybe ProjectDetail))
    , invitationCaps :: Invite.Capability m
    , createCaps :: Create.Capability m
    }

component ::
  forall query m.
  Monad m =>
  System m ->
  Capability m ->
  ProjectList.Capability m ->
  H.Component HH.HTML query OverviewInput ProjectList.Output m
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
    , projectDetail: Nothing
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
              [ P.classes (ClassName <$> [ "text-muted", "text-center", "mx-auto" ]) ]
              [ HH.text "Your project details" ]
          , HH.div_
              [ HH.slot
                  _projectList
                  unit
                  (ProjectList.component system pcaps)
                  st.selectedProject
                  (Just <<< (\(ProjectList.ProjectChange p) -> ProjectSelected p))
              , system.portal
                  _projectCreateModal
                  unit
                  (Create.component system caps.createCaps)
                  unit
                  Nothing
                  (Just <<< (\(Create.ProjectCreated p) -> ProjectSelected p))
              ]
          , HH.div
              [ P.classes (ClassName <$> if isNothing st.selectedProject then [ "collapse" ] else []) ]
              (U.fromMaybe $ projectDetail <$> st.projectDetail)
          , HH.div 
              [ P.classes (ClassName <$> [ "pt-6", "mx-auto" ]) ] 
              [ HH.button
                [ P.classes [ C.btn, C.btnPrimary ]
                , P.type_ ButtonButton
                , E.onClick (\_ -> Just OpenCreateModal)
                ]
                [ HH.text "Create a new project" ]
              ]
          ]
      ]

  projectDetail :: ProjectDetail -> H.ComponentHTML OverviewAction Slots m
  projectDetail (ProjectDetail' detail) = do
    let
      (Project' project) = detail.project
    HH.div
      [ P.classes (ClassName <$> [ "container-fluid" ]) ]
      [ HH.section
          [ P.id_ "projectOverview", P.classes (ClassName <$> [ "pt-3" ]) ]
          [ HH.div
              -- header
              [ P.classes (ClassName <$> [ "row", "pt-3", "font-weight-bold" ]) ]
              [ colmd2 (Just "Project Name")
              , colmd3 (Just "Undepreciated Period")
              , colmd3 (Just "Depreciation Duration")
              , colmd2 (Just "Originator")
              , colmd2 (Just "Origination Date")
              ]
          , HH.div
              [ P.classes (ClassName <$> [ "row", "pt-3" ]) ]
              ( [ colmd2 (Just project.projectName) ]
                  <> depreciationCols project.depf
                  <> [ colmd2 ((\(Contributor' p) -> p.handle) <$> M.lookup project.initiator detail.contributors)
                    , colmd2 (Just $ dateStr (date project.inceptionDate))
                    ]
              )
          ]
      , HH.section
          [ P.id_ "contributors" ]
          ( [ HH.div
                -- header
                [ P.classes (ClassName <$> [ "row", "pt-3", "font-weight-bold" ]) ]
                [ colmd2 (Just "Contributor")
                , colmd2 (Just "Joined")
                , colmd3 (Just "Contributed Hours")
                , colmd3 (Just "Current Revenue Share")
                ]
            ]
            <> (contributorCols <$> (
                 reverse 
                 <<< sortWith ((_.revShare) <<< unwrap) 
                 <<< L.toUnfoldable 
                 $ M.values detail.contributors
               ))
            <>
            [ HH.div 
              [ P.classes (ClassName <$> [ "row", "pt-3", "font-weight-bold" ]) ]
              [ HH.div 
                  [ P.classes (ClassName <$> [ "col-md-2" ]) ] 
                  [ HH.button
                    [ P.classes [ C.btn, C.btnPrimary ]
                    , P.type_ ButtonButton
                    , E.onClick (\_ -> Just (OpenInviteModal project.projectId))
                    ]
                    [ HH.text "Invite a collaborator" ]
                  ]
                , system.portal
                    _invitationModal
                    unit
                    (Invite.component system caps.invitationCaps)
                    unit
                    Nothing
                    (const Nothing)
              ]
            ]
          )
      ]

  depreciationCols :: DepreciationFn -> Array (H.ComponentHTML OverviewAction Slots m)
  depreciationCols = case _ of
    LinearDepreciation obj ->
      [ colmd3 (Just $ show (unwrap obj.undep) <> " days")
      , colmd3 (Just $ show (unwrap obj.dep) <> " days")
      ]

  contributorCols :: Contributor' DateTime -> H.ComponentHTML OverviewAction Slots m
  contributorCols (Contributor' pud) =
    let
      shareFrac = R.numerator pud.revShare `div` R.denominator pud.revShare

      pct = maybe "N/A" (\f -> F.toString (f * F.fromInt 100)) (F.fromNumber shareFrac :: Maybe (F.Fixed F.P10000))
    in
      HH.div
        [ P.classes (ClassName <$> [ "row", "pt-3", "pb-2" ]) ]
        [ colmd2 (Just pud.handle)
        , colmd2 (Just $ dateStr (date pud.joinedOn))
        , colmd3 (Just $ show (unwrap pud.timeDevoted))
        , colmd3 (Just $ pct <> "%")
        ]

  colmd2 :: Maybe String -> H.ComponentHTML OverviewAction Slots m
  colmd2 xs = HH.div [ P.classes (ClassName <$> [ "col-md-2" ]) ] (U.fromMaybe $ HH.text <$> xs)


  colmd3 :: Maybe String -> H.ComponentHTML OverviewAction Slots m
  colmd3 xs = HH.div [ P.classes (ClassName <$> [ "col-md-3" ]) ] (U.fromMaybe $ HH.text <$> xs)

  eval :: OverviewAction -> H.HalogenM OverviewState OverviewAction Slots ProjectList.Output m Unit
  eval action = do
    case action of
      Initialize -> do
        currentProject <- H.gets (_.selectedProject)
        traverse_ setProjectDetail currentProject
      OpenCreateModal -> do
        void <<< H.query _projectCreateModal unit $ H.tell (Create.OpenModal)
      OpenInviteModal pid -> do
        void <<< H.query _invitationModal unit $ H.tell (Invite.OpenModal pid)
      ProjectSelected pid -> do
        currentProject <- H.gets (_.selectedProject)
        when (all (_ /= pid) currentProject)
          $ do
              H.raise (ProjectList.ProjectChange pid)
              H.modify_ (_ { selectedProject = Just pid })
              setProjectDetail pid

  setProjectDetail :: ProjectId -> H.HalogenM OverviewState OverviewAction Slots ProjectList.Output m Unit
  setProjectDetail pid = do
    detail <- lift $ caps.getProjectDetail pid
    case detail of
      Left err -> lift $ system.error (show err)
      Right d -> H.modify_ (_ { projectDetail = d })

apiCapability :: Capability Aff
apiCapability =
  { getProjectDetail: getProjectDetail
  , invitationCaps: Invite.apiCapability
  , createCaps: Create.apiCapability
  }

mockCapability :: Capability Aff
mockCapability =
  { getProjectDetail:
      \pid -> do
        t <- liftEffect nowDateTime
        uid <- UserId <$> liftEffect genUUID
        pure <<< Right <<< Just
          $ ProjectDetail'
              { project:
                  Project'
                    { projectId: pid
                    , projectName: "Fake Project"
                    , inceptionDate: t
                    , initiator: uid
                    , depf: LinearDepreciation { undep: Days 30.0, dep: Days 300.0 }
                    }
              , contributors:
                  M.singleton uid
                    $ Contributor'
                        { userId: uid
                        , handle: "Joe"
                        , joinedOn: t
                        , timeDevoted: Hours 100.0
                        , revShare: 55.0 R.% 100.0
                        }
              }
  , invitationCaps: Invite.apiCapability
  , createCaps: Create.apiCapability
  }
