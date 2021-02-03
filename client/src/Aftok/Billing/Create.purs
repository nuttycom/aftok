module Aftok.Billing.Create where

import Prelude
import Control.Monad.Trans.Class (lift)
-- import Data.DateTime (DateTime, date)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Maybe (Maybe(..))
-- import Data.Unfoldable as U
import Data.Time.Duration (Hours(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
-- import Effect.Class (liftEffect)
-- import Effect.Now (nowDateTime)
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
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

data RType
  = RTAnnual
  | RTMonthly
  | RTWeekly
  | RTOneTime

type CState =
  { projectId :: ProjectId
  , name :: Maybe String
  , description :: Maybe String
  , message :: Maybe String
  , recurrenceType :: Maybe RType
  , recurrenceValue :: Maybe Int
  , amount :: Maybe Number
  , gracePeriod :: Maybe Hours
  , requestExpiry :: Maybe Hours
  }

data Query a
  = Tell a

type Input = ProjectId

type Output = Tuple BillableId Billable

data Action
  = ProjectChanged ProjectId
  | SetName String
  | SetDesc String
  | SetMessage String
  | SetRecurrenceType RType
  | SetRecurrenceDuration Number
  | SetBillingAmount Number

type Slot id
  = H.Slot Query Output id

type Capability (m :: Type -> Type)
  = { createBillable :: ProjectId -> Billable -> m (Either APIError BillableId)
    }

component ::
  forall m.
  Monad m =>
  System m ->
  Capability m ->
  H.Component HH.HTML Query Input Output m
component system caps =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = eval
              , receive = Just <<< ProjectChanged
              }
    }
  where
  initialState :: Input -> CState
  initialState input =
    { projectId: input
    , name : Nothing
    , description : Nothing
    , message : Nothing
    , recurrenceType : Nothing
    , recurrenceValue : Nothing
    , amount : Nothing
    , gracePeriod : Nothing
    , requestExpiry : Nothing
    }

  render :: forall slots. CState -> H.ComponentHTML Action slots m
  render st =
    HH.div
      [ P.classes (ClassName <$> ["card-body"]) ]
      [ HH.form_
        [
          HH.div
            [ P.classes (ClassName <$> ["form-group"]) ]
            [ HH.label [ P.for "billableName" ] [ HH.text "Bill Name:" ]
            , HH.input
              [ P.type_ P.InputText
              , P.classes (ClassName <$> [ "form-control" ])
              , P.id_ "billableName"
              , P.placeholder "A name for the product or service you want to bill for"
              , P.required true
              , P.autofocus true
              , E.onValueInput (Just <<< SetName)
              ]
            , HH.label [ P.for "billableDesc" ] [ HH.text "Bill Description:" ]
            , HH.input
              [ P.type_ P.InputText
              , P.classes (ClassName <$> [ "form-control" ])
              , P.id_ "billableDesc"
              , P.placeholder "Description of the product or service"
              , P.required true
              , P.autofocus true
              , E.onValueInput (Just <<< SetDesc)
              ]
            ]
        ]
      ]

  eval :: forall slots. Action -> H.HalogenM CState Action slots Output m Unit
  eval = case _ of
      ProjectChanged pid ->
        H.modify_ (_ { projectId = pid })
      SetName name -> pure unit
      SetDesc desc -> pure unit
      SetMessage msg -> pure unit
      SetRecurrenceType rtype -> pure unit
      SetRecurrenceDuration dur -> pure unit
      SetBillingAmount amt -> pure unit

