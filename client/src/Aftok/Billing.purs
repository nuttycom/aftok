module Aftok.Billing where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..), isNothing)
import Data.Unfoldable as U
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Map (Map)
import Data.Map as Map
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Aftok.Billing.Create as Create
import Aftok.Billing.PaymentRequest as PaymentRequest
import Aftok.ProjectList as ProjectList
import Aftok.Types (System, ProjectId)
import Aftok.Api.Types (APIError(..))
import Aftok.Api.Billing
  ( BillableId
  , Billable
  , PaymentRequestId
  , PaymentRequest
  , listProjectBillables
  , listUnpaidPaymentRequests
  , recurrenceStr
  )
import Aftok.HTML.Classes as C
import Aftok.Zcash (toZEC, zecString)

type BillingInput
  = Maybe ProjectId

type BillingState
  = { selectedProject :: Maybe ProjectId
    , billables :: Array (Tuple BillableId Billable)
    , activeRequests :: Map BillableId (Array (Tuple PaymentRequestId PaymentRequest))
    , selectedBillable :: Maybe BillableId
    }

data BillingAction
  = Initialize
  | ProjectSelected (Maybe ProjectId)
  | OpenBillableModal ProjectId
  | BillableCreated BillableId
  | OpenPaymentRequestModal ProjectId BillableId

type Slot id
  = forall query. H.Slot query ProjectList.Output id

type Slots
  = ( projectList :: ProjectList.Slot Unit
    , createBillable :: Create.Slot Unit
    , createPaymentRequest :: PaymentRequest.Slot Unit
    )

_projectList = SProxy :: SProxy "projectList"
_createBillable = SProxy :: SProxy "createBillable"
_createPaymentRequest = SProxy :: SProxy "createPaymentRequest"

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
  H.Component HH.HTML query BillingInput ProjectList.Output m
component system caps pcaps =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              , receive = Just <<< ProjectSelected
              }
    }
  where
  initialState :: BillingInput -> BillingState
  initialState input =
    { selectedProject: input
    , billables: []
    , activeRequests: Map.empty
    , selectedBillable: Nothing
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
                  (Just <<< (\(ProjectList.ProjectChange p) -> ProjectSelected (Just p)))
              ]
          , HH.div
              [ P.classes (ClassName <$> if isNothing st.selectedProject then [ "collapse" ] else []) ]
              (case st.selectedProject of
                Just pid ->
                  [ renderBillableList pid st
                  , HH.div
                    [ P.classes (ClassName <$> [ "col-md-2" ]) ]
                    [ HH.button
                      [ P.classes [ C.btn, C.btnPrimary ]
                      , P.type_ ButtonButton
                      , E.onClick (\_ -> Just (OpenBillableModal pid))
                      ]
                      [ HH.text "Create billable" ]
                    ]
                  , system.portal
                      _createBillable
                      unit
                      (Create.component system caps.createBillable)
                      unit
                      Nothing
                      (\(Create.BillableCreated bid) -> Just (BillableCreated bid))
                  , system.portal
                      _createPaymentRequest
                      unit
                      (PaymentRequest.component system caps.createPaymentRequest)
                      unit
                      Nothing
                      (const Nothing)
                  ]
                Nothing -> []
              )
          ]
      ]

  renderBillableList ::
    ProjectId ->
    BillingState ->
    H.ComponentHTML BillingAction Slots m
  renderBillableList pid st =
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
          ] <> (billableRows =<< st.billables)
          )
      ]
    where
      billableRows (Tuple bid b) =
        let paymentRequestRow (Tuple prid preq) =
              HH.div
                [ P.classes (ClassName <$> [ "row", "border-top" ]) ]
                [ colmd2 Nothing
                , colmd2 (Just b.description)
                , colmd2 (Just (zecString <<< toZEC $ b.amount))
                , colmd3 (Just (recurrenceStr b.recurrence))
                ]
         in [ HH.div
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
              , E.onClick (\_ -> Just $ OpenPaymentRequestModal pid bid)
              ]
              [ HH.text "New payment request" ]
            ]
          ]
        ] <>
        ( if any ((==) bid) st.selectedBillable
            then paymentRequestRow <$> (join $ U.fromMaybe (Map.lookup bid st.activeRequests))
            else
              []
        )

  colmd2 :: forall i w. Maybe String -> HH.HTML i  w
  colmd2 xs = HH.div [ P.classes (ClassName <$> [ "col-md-2"]) ] (U.fromMaybe $ HH.text <$> xs)


  colmd3 :: forall i w. Maybe String -> HH.HTML i  w
  colmd3 xs = HH.div [ P.classes (ClassName <$> [ "col-md-3" ]) ] (U.fromMaybe $ HH.text <$> xs)

  handleAction :: BillingAction -> H.HalogenM BillingState BillingAction Slots ProjectList.Output m Unit
  handleAction action = do
    case action of
      Initialize -> do
        currentPid <- H.gets (_.selectedProject)
        traverse_ refreshBillables currentPid

      ProjectSelected pidMay -> do
        currentPid <- H.gets (_.selectedProject)
        traverse_ refreshBillables pidMay
        when (currentPid /= pidMay)
          $ traverse_ projectSelected pidMay

      OpenBillableModal pid -> do
        void $ H.query _createBillable unit $ H.tell (Create.OpenModal pid)

      BillableCreated _ -> do
        currentPid <- H.gets (_.selectedProject)
        traverse_ refreshBillables currentPid

      OpenPaymentRequestModal pid bid -> do
        void $ H.query _createPaymentRequest unit $ H.tell (PaymentRequest.OpenModal pid bid)

    where
      projectSelected pid = do
        H.modify_ (_ { selectedProject = Just pid })
        H.raise (ProjectList.ProjectChange pid)

      refreshBillables pid = do
        billables <- lift $ caps.listProjectBillables pid
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
