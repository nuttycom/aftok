module Aftok.Billing where

import Prelude
import Control.Monad.Trans.Class (lift)
-- import Data.DateTime (DateTime, date)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Maybe (Maybe(..), isNothing)
import Data.Unfoldable as U
import Data.Newtype (unwrap)
import Data.Symbol (SProxy(..))
-- import Data.Time.Duration (Hours(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Effect.Aff (Aff)
-- import Effect.Class (liftEffect)
-- import Effect.Now (nowDateTime)
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Aftok.Billing.Create as Create
import Aftok.Billing.PaymentRequest as PaymentRequest
import Aftok.ProjectList as ProjectList
import Aftok.Types (System, ProjectId)
import Aftok.Api.Types (APIError(..), Zip321Request(..))
import Aftok.Api.Project (Project)
import Aftok.Api.Billing
  ( BillableId
  , Billable
  , PaymentRequestId
  , PaymentRequest
  , PaymentRequest'(..)
  , listProjectBillables
  , listUnpaidPaymentRequests
  , recurrenceStr
  )
import Aftok.HTML.Classes as C
import Aftok.Modals as Modals
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.Zcash (toZEC, zecString)

type BillingInput
  = Maybe Project

type BillingState
  = { selectedProject :: Maybe Project
    , billables :: Array (Tuple BillableId Billable)
    , paymentRequests :: Array (Tuple PaymentRequestId PaymentRequest)
    }

data BillingAction
  = Initialize
  | ProjectSelected Project
  | BillableCreated (Tuple BillableId Billable)
  | CreatePaymentRequest BillableId
  | PaymentRequestCreated (PaymentRequest)

type Slot id
  = forall query. H.Slot query ProjectList.Event id

type Slots
  = ( projectList :: ProjectList.Slot Unit
    , createBillable :: Create.Slot Unit
    , createPaymentRequest :: PaymentRequest.Slot Unit
    , showPaymentRequest :: PaymentRequest.QrSlot Unit
    )

_projectList = SProxy :: SProxy "projectList"
_createBillable = SProxy :: SProxy "createBillable"
_createPaymentRequest = SProxy :: SProxy "createPaymentRequest"
_showPaymentRequest = SProxy :: SProxy "showPaymentRequest"

type Capability (m :: Type -> Type)
  = { createBillable :: Create.Capability m
    , createPaymentRequest :: PaymentRequest.Capability m
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
                  [ renderBillableList st.billables
                  , HH.div 
                    [ P.classes (ClassName <$> [ "col-md-2" ]) ] 
                    [ Modals.modalButton "createBillable" "Create billable" Nothing]
                  , system.portal
                      _createBillable
                      unit
                      (Create.component system caps.createBillable)
                      (unwrap p).projectId
                      Nothing
                      (Just <<< BillableCreated)
                  , system.portal
                      _createPaymentRequest
                      unit
                      (PaymentRequest.component system caps.createPaymentRequest)
                      (unwrap p).projectId
                      Nothing
                      (Just <<< PaymentRequestCreated)
                  , system.portal
                      _showPaymentRequest
                      unit
                      (PaymentRequest.qrcomponent system)
                      Nothing
                      Nothing
                      (const Nothing)
                  ]
                Nothing -> []
              )
          ]
      ]

  renderBillableList :: Array (Tuple BillableId Billable) -> H.ComponentHTML BillingAction Slots m
  renderBillableList billables = 
    HH.div
      [ P.classes (ClassName <$> [ "container-fluid" ]) ]
      [ HH.section
          [ P.id_ "projectOverview", P.classes (ClassName <$> [ "pt-3" ]) ]
          ([ HH.div
              -- header
              [ P.classes (ClassName <$> [ "row", "pt-3", "font-weight-bold" ]) ]
              [ colmd2 (Just "Billable Name")
              , colmd2 (Just "Description")
              , colmd2 (Just "Amount")
              , colmd3 (Just "Recurrence")
              , colmd2 Nothing
              ] 
          ] <> (billableRow <$> billables))
      ]
    where
      billableRow (Tuple bid b) = 
        HH.div
          [ P.classes (ClassName <$> [ "row", "border-top" ]) ]
          [ colmd2 (Just b.name)
          , colmd2 (Just b.description)
          , colmd2 (Just (zecString <<< toZEC $ b.amount))
          , colmd3 (Just (recurrenceStr b.recurrence))
          , HH.div 
            [ P.classes (ClassName <$> [ "col-md-2" ]) ] 
            [ HH.button
              [ P.classes [ C.btn, C.btnPrimary, C.btnSmall ]
              , P.type_ ButtonButton
              , E.onClick (\_ -> Just $ CreatePaymentRequest bid)
              ]
              [ HH.text "New Payment Request" ]
            ]
          ]

  colmd2 :: forall i w. Maybe String -> HH.HTML i  w
  colmd2 xs = HH.div [ P.classes (ClassName <$> [ "col-md-2"]) ] (U.fromMaybe $ HH.text <$> xs)


  colmd3 :: forall i w. Maybe String -> HH.HTML i  w
  colmd3 xs = HH.div [ P.classes (ClassName <$> [ "col-md-3" ]) ] (U.fromMaybe $ HH.text <$> xs)

  eval :: BillingAction -> H.HalogenM BillingState BillingAction Slots ProjectList.Event m Unit
  eval action = do
    case action of
      Initialize -> do
        currentProject <- H.gets (_.selectedProject)
        traverse_ refreshBillables currentProject

      ProjectSelected p -> do
        currentProject <- H.gets (_.selectedProject)
        refreshBillables p
        when (all (\p' -> (unwrap p').projectId /= (unwrap p).projectId) currentProject)
          $ do
              H.raise (ProjectList.ProjectChange p)
              H.modify_ (_ { selectedProject = Just p })

      BillableCreated _ -> do
        currentProject <- H.gets (_.selectedProject)
        traverse_ refreshBillables currentProject

      CreatePaymentRequest bid -> do
        _ <- H.query _createPaymentRequest unit $ H.tell (PaymentRequest.SetBillableId bid)
        lift $ system.toggleModal PaymentRequest.modalId ModalFFI.ShowModal

      PaymentRequestCreated (PaymentRequest req) -> do
        lift $ system.toggleModal PaymentRequest.modalId ModalFFI.HideModal
        lift $ system.toggleModal PaymentRequest.qrModalId ModalFFI.ShowModal
        let req' = Zip321Request req.native_request.zip321_request
        _ <- H.query _showPaymentRequest unit $ H.tell (PaymentRequest.QrRender req')
        pure unit

    where 
      refreshBillables currentProject = do
        billables <- lift $ caps.listProjectBillables (unwrap currentProject).projectId
        case billables of
          Left err -> lift $ system.error (show err)
          Right b -> H.modify_ (_ { billables = b })

apiCapability :: Capability Aff
apiCapability =
  { createBillable: Create.apiCapability
  , createPaymentRequest: PaymentRequest.apiCapability
  , listProjectBillables: listProjectBillables
  , listUnpaidPaymentRequests: listUnpaidPaymentRequests
  }

mockCapability :: Capability Aff
mockCapability =
  { createBillable: Create.mockCapability
  , createPaymentRequest: PaymentRequest.mockCapability
  , listProjectBillables: \_ -> pure $ Left Forbidden
  , listUnpaidPaymentRequests: \_ -> pure $ Left Forbidden
  }
