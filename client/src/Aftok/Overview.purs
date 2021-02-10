module Aftok.Overview where

import Prelude
import Control.Monad.Trans.Class (lift)
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
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as P
import Aftok.Billing.PaymentRequest as PaymentRequest
import Aftok.Modals as Modals
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.ProjectList as ProjectList
import Aftok.Projects.Invite as Invite
import Aftok.Types (System, ProjectId, UserId(..), dateStr)
import Aftok.Api.Types (APIError, Zip321Request)
import Aftok.Api.Project
  ( Project
  , Project'(..)
  , ProjectDetail
  , ProjectDetail'(..)
  , DepreciationFn(..)
  , Contributor'(..)
  , getProjectDetail
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
  | InvitationCreated (Maybe Zip321Request)

type Slot id
  = forall query. H.Slot query ProjectList.Event id

type Slots
  = ( projectList :: ProjectList.Slot Unit
    , invitationModal :: Invite.Slot Unit
    , inviteQRModal :: PaymentRequest.QrSlot Unit
    )

_projectList = SProxy :: SProxy "projectList"
_invitationModal = SProxy :: SProxy "invitationModal"
_inviteQRModal = SProxy :: SProxy "inviteQRModal"

type Capability (m :: Type -> Type)
  = { getProjectDetail :: ProjectId -> m (Either APIError (Maybe ProjectDetail))
    , invitationCaps :: Invite.Capability m
    }

component ::
  forall query m.
  Monad m =>
  System m ->
  Capability m ->
  ProjectList.Capability m ->
  H.Component HH.HTML query OverviewInput ProjectList.Event m
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
              [ P.classes (ClassName <$> [ "col-md-5", "text-muted", "text-center", "mx-auto" ]) ]
              [ HH.text "Your project details" ]
          , HH.div_
              [ HH.slot
                  _projectList
                  unit
                  (ProjectList.component system pcaps)
                  st.selectedProject
                  (Just <<< (\(ProjectList.ProjectChange p) -> ProjectSelected p))
              ]
          , HH.div
              [ P.classes (ClassName <$> if isNothing st.selectedProject then [ "collapse" ] else []) ]
              (U.fromMaybe $ projectDetail <$> st.projectDetail)
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
              <> (contributorCols <$> (L.toUnfoldable $ M.values detail.contributors))
              <>
            [ HH.div 
              [ P.classes (ClassName <$> [ "row", "pt-3", "font-weight-bold" ]) ]
              [ HH.div 
                  [ P.classes (ClassName <$> [ "col-md-2" ]) ] 
                  [ Modals.modalButton Invite.modalId "Invite a collaborator" Nothing]
                , system.portal
                    _invitationModal
                    unit
                    (Invite.component system caps.invitationCaps)
                    project.projectId
                    Nothing
                    (Just <<< InvitationCreated)
               , system.portal
                   _inviteQRModal
                   unit
                   (PaymentRequest.qrcomponent system)
                   Nothing
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

  eval :: OverviewAction -> H.HalogenM OverviewState OverviewAction Slots ProjectList.Event m Unit
  eval action = do
    case action of
      Initialize -> do
        currentProject <- H.gets (_.selectedProject)
        traverse_ (setProjectDetail <<< (\p -> (unwrap p).projectId)) currentProject
      Invite _ -> do
        pure unit
      ProjectSelected p -> do
        currentProject <- H.gets (_.selectedProject)
        when (all (\p' -> (unwrap p').projectId /= (unwrap p).projectId) currentProject)
          $ do
              H.raise (ProjectList.ProjectChange p)
              H.modify_ (_ { selectedProject = Just p })
              setProjectDetail (unwrap p).projectId
      InvitationCreated req -> do
        lift $ system.toggleModal Invite.modalId ModalFFI.HideModal
        lift $ system.toggleModal PaymentRequest.qrModalId ModalFFI.ShowModal
        traverse_ (\r -> H.query _inviteQRModal unit $ H.tell (PaymentRequest.QrRender r)) req
        pure unit

  setProjectDetail :: ProjectId -> H.HalogenM OverviewState OverviewAction Slots ProjectList.Event m Unit
  setProjectDetail pid = do
    detail <- lift $ caps.getProjectDetail pid
    case detail of
      Left err -> lift $ system.error (show err)
      Right d -> H.modify_ (_ { projectDetail = d })

apiCapability :: Capability Aff
apiCapability =
  { getProjectDetail: getProjectDetail
  , invitationCaps: Invite.apiCapability
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
  }
