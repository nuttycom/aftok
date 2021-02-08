module Aftok.Billing.PaymentRequest where

import Prelude
import Control.Monad.Trans.Class (lift)
-- import Control.Monad.State.Class (get)
import Data.Either (Either(..), note)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse_)
import Data.Unfoldable as U
import Data.Validation.Semigroup (V(..), toEither)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Aftok.Types (System, ProjectId)
import Aftok.HTML.Classes as C
import Aftok.Modals as Modals
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.Api.Types (APIError(..))
import Aftok.Api.Billing
  ( BillableId
  , PaymentRequest'(..)
  , PaymentRequest
  , PaymentRequestMeta
  , createPaymentRequest
  )

data FieldError
  = NameRequired
  | BillableIdNotSet

derive instance fieldEq :: Eq FieldError
derive instance fieldOrd :: Ord FieldError

type CState =
  { projectId :: ProjectId
  , billableId :: Maybe BillableId
  , name :: Maybe String
  , description :: Maybe String
  , fieldErrors :: Array FieldError
  }

type Input = ProjectId

data Query a 
  = SetProjectId ProjectId a
  | SetBillableId BillableId a

type Output = PaymentRequest

data Action
  = ProjectChanged ProjectId
  | SetName String
  | SetDesc String
  | SavePaymentRequest

type Slot id
  = H.Slot Query Output id

type Capability (m :: Type -> Type)
  = { createPaymentRequest ::
        ProjectId ->
        BillableId ->
        PaymentRequestMeta ->
        m (Either APIError PaymentRequest)
    }

modalId :: String
modalId = "createPaymentRequest"

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
              { handleAction = handleAction
              , handleQuery = handleQuery
              , receive = Just <<< ProjectChanged
              }
    }
  where
  initialState :: Input -> CState
  initialState input =
    { projectId: input
    , billableId: Nothing
    , name : Nothing
    , description : Nothing
    , fieldErrors : []
    }

  render :: forall slots. CState -> H.ComponentHTML Action slots m
  render st =
    Modals.modalWithSave modalId "Create Payment Request" SavePaymentRequest
      [ HH.form_
        [ formGroup st
          [ NameRequired ]
          [ HH.label
            [ P.for "requestName"]
            [ HH.text "Request Name" ]
          , HH.input
            [ P.type_ P.InputText
            , P.classes [ C.formControl, C.formControlSm ]
            , P.id_ "requestName"
            , P.placeholder "A name for the payment request"
            , P.value (fromMaybe "" st.name)
            , E.onValueInput (Just <<< SetName)
            ]
          ]
        , formGroup st
          [ ]
          [ HH.label
              [ P.for "requestDesc"]
              [ HH.text "Request Description" ]
          , HH.input
              [ P.type_ P.InputText
              , P.classes [ C.formControl, C.formControlSm ]
              , P.id_ "requestDesc"
              , P.placeholder "Additional descriptive information"
              , P.value (fromMaybe "" st.description)
              , E.onValueInput (Just <<< SetDesc)
              ]
          ]
        ]
      ]

  formGroup :: forall i a. CState -> Array FieldError -> Array (HH.HTML i a) -> HH.HTML i a
  formGroup st fields body =
    HH.div
     [ P.classes [C.formGroup] ]
     (body <> (fieldError st =<< fields))

  fieldError :: forall i a. CState -> FieldError -> Array (HH.HTML i a)
  fieldError st field =
    if any (_ == field) st.fieldErrors
       then case field of
            NameRequired -> err "The name field is required"
            BillableIdNotSet -> err "The billable id is missing. Close this dialog and try again."
       else []
    where
    err str = [ HH.div_ [ HH.span [ P.classes (ClassName <$> [ "badge", "badge-danger-soft" ]) ] [ HH.text str ] ] ]

  handleQuery :: forall slots a. Query a -> H.HalogenM CState Action slots Output m (Maybe a)
  handleQuery = case _ of
    SetProjectId pid a -> do
      H.modify_ (_ { projectId = pid, billableId = Nothing, name = Nothing, description = Nothing })
      pure (Just a)

    SetBillableId bid a -> do
      H.modify_ (_ { billableId = Just bid })
      pure (Just a)

  handleAction :: forall slots. Action -> H.HalogenM CState Action slots Output m Unit
  handleAction = case _ of
    ProjectChanged pid ->
      H.modify_ (_ { projectId = pid, billableId = Nothing, name = Nothing, description = Nothing })
    SetName name ->
      H.modify_ (_ { name = Just name })
    SetDesc desc ->
      H.modify_ (_ { description = Just desc })
    SavePaymentRequest -> do
      bidV <- V <<< note [BillableIdNotSet] <$> H.gets (_.billableId)
      nameV <- V <<< note [NameRequired] <$> H.gets (_.name)
      descV <- H.gets (_.description)
      let reqV = { requestName: _, requestDesc: _ } <$> nameV <*> pure descV
          breqV = Tuple <$> bidV <*> reqV
      case toEither breqV of
        Left errors -> do
          H.modify_ (_ { fieldErrors = errors })
        Right (Tuple bid reqMeta) -> do
          pid <- H.gets (_.projectId)
          res <- lift $ caps.createPaymentRequest pid bid reqMeta
          case res of
            Right content -> do
              H.raise content
              H.modify_ (_ { billableId = Nothing, name = Nothing, description = Nothing, fieldErrors = [] })
              lift $ system.toggleModal "createPaymentRequest" ModalFFI.HideModal
            Left errs ->
              lift $ system.error (show errs)

apiCapability :: Capability Aff
apiCapability =
  { createPaymentRequest: createPaymentRequest
  }

mockCapability :: Capability Aff
mockCapability =
  { createPaymentRequest: \_ _ _ -> pure $ Left Forbidden }


type QrInput = Maybe PaymentRequest

type QrState = 
  { req :: Maybe PaymentRequest
  , dataUrl :: Maybe String
  }

data QrQuery a
  = QrRender PaymentRequest a

data QrAction 
  = QrInit
  | QrClose

type QrSlot id
  = H.Slot QrQuery Unit id

qrModalId :: String
qrModalId = "paymentRequestQR"

qrcomponent ::
  forall m output.
  Monad m =>
  System m ->
  H.Component HH.HTML QrQuery QrInput output m
qrcomponent system =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval 
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , initialize = Just QrInit
              }
    }
  where
  initialState :: QrInput -> QrState
  initialState input = 
    { req: input, dataUrl: Nothing }

  render :: forall slots. QrState -> H.ComponentHTML QrAction slots m
  render st =
    Modals.modalWithClose qrModalId "Payment Request" QrClose
      [ HH.div_
        ((\url -> HH.img [P.src url]) <$> U.fromMaybe st.dataUrl)
      ]

  handleQuery :: forall slots a. QrQuery a -> H.HalogenM QrState QrAction slots output m (Maybe a)
  handleQuery = case _ of
    QrRender r a -> do
      dataUrl <- lift $ renderQR r
      H.modify_ (_ { dataUrl = Just dataUrl })
      pure (Just a)

  handleAction :: forall slots. QrAction -> H.HalogenM QrState QrAction slots output m Unit
  handleAction = case _ of
    QrInit -> do
      req <- H.gets (_.req)
      lift $ traverse_ renderQR req
    QrClose -> 
      H.modify_ (_ { req = Nothing, dataUrl = Nothing })

  renderQR :: PaymentRequest -> m String
  renderQR (PaymentRequest r) = 
    system.renderQR { value: r.native_request.zip321_request, size: 300 }
