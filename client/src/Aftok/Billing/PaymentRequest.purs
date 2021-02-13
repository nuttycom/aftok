module Aftok.Billing.PaymentRequest where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..), note)
import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..), toEither)
import Effect.Aff (Aff)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties.ARIA as ARIA

import Aftok.Api.Types (APIError(..), Zip321Request(..))
import Aftok.Api.Billing
  ( BillableId
  , PaymentRequest'(..)
  , PaymentRequest
  , PaymentRequestMeta
  , createPaymentRequest
  )
import Aftok.Components.Zip321QR as Zip321QR
import Aftok.HTML.Classes as C
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.Types (System, ProjectId)

data FieldError
  = PidFieldNotSet
  | BillableIdNotSet
  | NameRequired

derive instance fieldEq :: Eq FieldError
derive instance fieldOrd :: Ord FieldError

type CState =
  { projectId :: Maybe ProjectId
  , billableId :: Maybe BillableId
  , name :: Maybe String
  , description :: Maybe String
  , fieldErrors :: Array FieldError
  , mode :: Mode
  }

data Mode 
  = Form
  | QrScan Zip321Request

data Query a 
  = OpenModal ProjectId BillableId a

data Action
  = SetName String
  | SetDesc String
  | SavePaymentRequest
  | Close

type Slot id
  = forall output. H.Slot Query output id

type Slots
  = ( requestQR :: Zip321QR.Slot Unit
    )

_requestQR = SProxy :: SProxy "requestQR"

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
  forall input output m.
  Monad m =>
  System m ->
  Capability m ->
  H.Component HH.HTML Query input output m
component system caps =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              }
    }
  where
  initialState :: CState
  initialState =
    { projectId: Nothing
    , billableId: Nothing
    , name: Nothing
    , description: Nothing
    , fieldErrors: []
    , mode: Form
    }

  render :: CState -> H.ComponentHTML Action Slots m
  render st =
    HH.div
      [ P.classes [ C.modal ]
      , P.id_ modalId
      , P.tabIndex (negate 1)
      , ARIA.role "dialog"
      , ARIA.labelledBy (modalId <> "Title")
      , ARIA.hidden "true"
      ]
      [ HH.div
        [ P.classes [C.modalDialog], ARIA.role "document" ]
        [ HH.div
          [ P.classes [C.modalContent] ]
          [ HH.div
            [ P.classes [C.modalHeader] ]
            [ HH.h5 [P.classes [C.modalTitle], P.id_ (modalId <>"Title") ] [HH.text "Request a payment"]
            , HH.button
              [ P.classes [ C.close ]
              , ARIA.label "Close"
              , P.type_ ButtonButton
              , E.onClick (\_ -> Just Close)
              ]
              [ HH.span [ARIA.hidden "true"] [HH.text "×"]]
            ]
          , HH.div
            [ P.classes [C.modalBody] ]
            case st.mode of
              Form -> 
                [ requestForm st ]
              QrScan req -> 
                [ HH.slot _requestQR unit (Zip321QR.component system) req (const Nothing) ]
          , HH.div
            [ P.classes [C.modalFooter] ] $
            case st.mode of 
              Form -> 
                [ HH.button
                  [ P.type_ ButtonButton
                  , P.classes [ C.btn, C.btnSecondary]
                  , E.onClick (\_ -> Just Close)
                  ]
                  [ HH.text "Close" ]
                , HH.button
                  [ P.type_ ButtonButton
                  , P.classes [ C.btn, C.btnPrimary ]
                  , E.onClick (\_ -> Just SavePaymentRequest)
                  ]
                  [ HH.text "Create Request" ]
                ]

              QrScan _ -> 
                [ HH.button
                  [ P.type_ ButtonButton
                  , P.classes [ C.btn, C.btnPrimary]
                  , E.onClick (\_ -> Just Close)
                  ]
                  [ HH.text "Close" ]
                ]

          ]
        ]
      ]

      
  requestForm st = 
    HH.form_
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

  formGroup :: forall i a. CState -> Array FieldError -> Array (HH.HTML i a) -> HH.HTML i a
  formGroup st fields body =
    HH.div
     [ P.classes [C.formGroup] ]
     (body <> (fieldError st =<< fields))

  fieldError :: forall i a. CState -> FieldError -> Array (HH.HTML i a)
  fieldError st field =
    if any (_ == field) st.fieldErrors
       then case field of
            PidFieldNotSet -> err "The project id is missing. Close this dialog and try again."
            BillableIdNotSet -> err "The billable id is missing. Close this dialog and try again."
            NameRequired -> err "The name field is required"
       else []
    where
    err str = [ HH.div_ [ HH.span [ P.classes (ClassName <$> [ "badge", "badge-danger-soft" ]) ] [ HH.text str ] ] ]

  handleQuery :: forall slots a. Query a -> H.HalogenM CState Action slots output m (Maybe a)
  handleQuery = case _ of
    OpenModal pid bid a -> do
      H.modify_ (\_ -> initialState { projectId = Just pid, billableId = Just bid } )
      lift $ system.toggleModal modalId ModalFFI.ShowModal
      pure (Just a)

  handleAction :: forall slots. Action -> H.HalogenM CState Action slots output m Unit
  handleAction = case _ of
    SetName name ->
      H.modify_ (_ { name = Just name })
    SetDesc desc ->
      H.modify_ (_ { description = Just desc })
    SavePaymentRequest -> do
      pidV <- V <<< note [PidFieldNotSet] <$> H.gets (_.projectId)
      bidV <- V <<< note [BillableIdNotSet] <$> H.gets (_.billableId)
      nameV <- V <<< note [NameRequired] <$> H.gets (_.name)
      descV <- H.gets (_.description)
      let reqV = { requestName: _, requestDesc: _ } <$> nameV <*> pure descV
      case toEither (Tuple <$> pidV <*> (Tuple <$> bidV <*> reqV)) of
        Right (Tuple pid (Tuple bid reqMeta)) -> do
          res <- lift $ caps.createPaymentRequest pid bid reqMeta
          case res of
            Right (PaymentRequest req) -> do
              H.modify_ (_ { mode = QrScan $ Zip321Request req.native_request.zip321_request })
            Left errs ->
              lift $ system.error (show errs)
        Left errors -> do
          H.modify_ (_ { fieldErrors = errors })
    Close -> do 
      H.modify_ (const initialState) -- wipe the state for safety
      lift $ system.toggleModal "createPaymentRequest" ModalFFI.HideModal

apiCapability :: Capability Aff
apiCapability =
  { createPaymentRequest: createPaymentRequest
  }

mockCapability :: Capability Aff
mockCapability =
  { createPaymentRequest: \_ _ _ -> pure $ Left Forbidden }

