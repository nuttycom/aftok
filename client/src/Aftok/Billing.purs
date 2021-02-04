module Aftok.Billing where

import Prelude
import Control.Monad.Trans.Class (lift)
-- import Data.DateTime (DateTime, date)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Maybe (Maybe(..), isNothing)
-- import Data.Unfoldable as U
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
-- import Data.Time.Duration (Hours(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
-- import Effect.Class (liftEffect)
-- import Effect.Now (nowDateTime)
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
-- import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Aftok.Billing.Create as Create
import Aftok.ProjectList as ProjectList
import Aftok.Types (System, ProjectId)
import Aftok.Api.Types (APIError(..))
import Aftok.Api.Project (Project)
import Aftok.Api.Billing
  ( BillableId
  , Billable
  , PaymentRequestId
  , PaymentRequest
  , createBillable
  , listProjectBillables
  , listUnpaidPaymentRequests
  )
import Aftok.Modals as Modals

type BillingInput
  = Maybe Project

type BillingState
  = { selectedProject :: Maybe Project
    , billables :: Array (Tuple BillableId Billable)
    , selectedBillable :: Maybe (Tuple BillableId Billable)
    , paymentRequests :: Array (Tuple PaymentRequestId PaymentRequest)
    }

data BillingAction
  = Initialize
  | ProjectSelected Project
  | BillableCreated (Tuple BillableId Billable)

type Slot id
  = forall query. H.Slot query ProjectList.Event id

type Slots
  = ( projectList :: ProjectList.Slot Unit
    , createBillable :: Create.Slot Unit
    )

_projectList = SProxy :: SProxy "projectList"
_createBillable = SProxy :: SProxy "createBillable"

type Capability (m :: Type -> Type)
  = { createBillable :: Create.Capability m
    , listProjectBillables :: ProjectId -> m (Either APIError (Array (Tuple BillableId Billable)))
    , listUnpaidPaymentRequests :: BillableId -> m (Either APIError (Array (Tuple PaymentRequestId PaymentRequest)))
    }


component ::
  forall query m.
  Monad m =>
  System m ->
  Capability m ->
  ProjectList.Capability m ->
  H.Component HH.HTML query BillingInput ProjectList.Event m
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
  initialState :: BillingInput -> BillingState
  initialState input =
    { selectedProject: input
    , billables: []
    , selectedBillable: Nothing
    , paymentRequests: []
    }

  render :: BillingState -> H.ComponentHTML BillingAction Slots m
  render st =
    HH.section
      [ P.classes (ClassName <$> [ "section-border", "border-primary" ]) ]
      [ HH.div
          [ P.classes (ClassName <$> [ "container", "pt-6" ]) ]
          [ HH.h1
              [ P.classes (ClassName <$> [ "mb-0", "font-weight-bold", "text-center" ]) ]
              [ HH.text "Billing" ]
          , HH.p
              [ P.classes (ClassName <$> [ "col-md-5", "text-muted", "text-center", "mx-auto" ]) ]
              [ HH.text "Your project's payment requests & payments" ]
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
              (case st.selectedProject of
                Just p -> 
                  [ Modals.modalButton "createBillable" "Create billable"
                  , system.portal
                      _createBillable
                      unit
                      (Create.component system caps.createBillable)
                      (unwrap p).projectId
                      Nothing
                      (Just <<< BillableCreated)
                  ]
                Nothing -> []
              )
          ]
      ]

  eval :: BillingAction -> H.HalogenM BillingState BillingAction Slots ProjectList.Event m Unit
  eval action = do
    case action of
      Initialize -> do
        currentProject <- H.gets (_.selectedProject)
        billables <- lift $ traverse (caps.listProjectBillables <<< (_.projectId) <<< unwrap) currentProject
        case billables of
          Nothing -> pure unit
          Just (Left err) -> lift $ system.error (show err)
          Just (Right b) -> H.modify_ (_ { billables = b })

      ProjectSelected p -> do
        currentProject <- H.gets (_.selectedProject)
        when (all (\p' -> (unwrap p').projectId /= (unwrap p).projectId) currentProject)
          $ do
              H.raise (ProjectList.ProjectChange p)
              H.modify_ (_ { selectedProject = Just p })
      BillableCreated _ ->
        pure unit

apiCapability :: Capability Aff
apiCapability =
  { createBillable: { createBillable: createBillable }
  , listProjectBillables: listProjectBillables
  , listUnpaidPaymentRequests: listUnpaidPaymentRequests
  }

mockCapability :: Capability Aff
mockCapability =
  { createBillable: { createBillable: \_ _ -> pure $ Left Forbidden }
  , listProjectBillables: \_ -> pure $ Left Forbidden
  , listUnpaidPaymentRequests: \_ -> pure $ Left Forbidden
  }
