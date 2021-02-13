module Aftok.Projects.Invite where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array (filter)
import Data.Either (Either(..), note)
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..), toEither)
import Effect.Aff (Aff)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties.ARIA as ARIA
import Aftok.Api.Account as Acc
import Aftok.Api.Project as Project
import Aftok.Api.Project (Invitation')
import Aftok.Api.Types (APIError, CommsType(..), CommsAddress(..), Zip321Request)
import Aftok.Components.Zip321QR as Zip321QR
import Aftok.HTML.Forms (commsSwitch, commsField)
import Aftok.HTML.Classes as C
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.Types (System, ProjectId)

data Field
  = PidField
  | NameField
  | EmailField
  | ZAddrField

derive instance fieldEq :: Eq Field
derive instance fieldOrd :: Ord Field

data Query a 
  = OpenModal ProjectId a

data Mode 
  = Form
  | QrScan Zip321Request

type CState =
  { projectId :: Maybe ProjectId
  , greetName :: Maybe String
  , message :: Maybe String
  , channel :: CommsType
  , email :: Maybe String
  , zaddr :: Maybe String
  , fieldErrors :: Array Field
  , mode :: Mode
  }

data Action
  = SetGreetName String
  | SetMessage String
  | SetCommsType CommsType
  | SetEmail String
  | SetZAddr String
  | CreateInvitation
  | Close

type Slot id
  = forall output. H.Slot Query output id

type Slots
  = ( inviteQR :: Zip321QR.Slot Unit
    )

_inviteQR = SProxy :: SProxy "inviteQR"

type Capability (m :: Type -> Type)
  = { createInvitation :: ProjectId -> Invitation' CommsAddress -> m (Either APIError (Maybe Zip321Request))
    , checkZAddr :: String -> m Acc.ZAddrCheckResponse
    }

modalId :: String
modalId = "createInvitation"

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
    , greetName : Nothing
    , message : Nothing
    , channel: ZcashComms
    , email: Nothing
    , zaddr: Nothing
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
            [ HH.h5 [P.classes [C.modalTitle], P.id_ (modalId <>"Title") ] [HH.text "Invite a collaborator"]
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
                [ inviteForm st ]
              QrScan req -> 
                [ HH.slot _inviteQR unit (Zip321QR.component system) req (const Nothing) ]
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
                  , E.onClick (\_ -> Just CreateInvitation)
                  ]
                  [ HH.text "Send invitation" ]
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

  inviteForm st = 
    HH.form_
      [ formGroup st
        [ NameField ]
        [ HH.label
          [ P.for "greetName"]
          [ HH.text "Name" ]
        , HH.input
          [ P.type_ P.InputText
          , P.classes [ C.formControl, C.formControlSm ]
          , P.id_ "greetName"
          , P.placeholder "Who are you inviting?"
          , E.onValueInput (Just <<< SetGreetName)
          ]
        ]
      , formGroup st
        [ ]
        [ HH.label
            [ P.for "message"]
            [ HH.text "Message" ]
        , HH.input
            [ P.type_ P.InputText
            , P.classes [C.formControl, C.formControlSm]
            , P.id_ "message"
            , P.placeholder "Enter your message here"
            , E.onValueInput (Just <<< SetMessage)
            ]
        ]
      , commsSwitch SetCommsType st.channel
      , commsField SetEmail SetZAddr st $ case _ of
          EmailComms -> fieldError st EmailField    
          ZcashComms -> fieldError st ZAddrField
      ]

  formGroup :: forall i a. CState -> Array Field -> Array (HH.HTML i a) -> HH.HTML i a
  formGroup st fields body =
    HH.div
     [ P.classes [C.formGroup] ]
     (body <> (fieldError st =<< fields))

  fieldError :: forall i a. CState -> Field -> Array (HH.HTML i a)
  fieldError st field =
    if any (_ == field) st.fieldErrors
       then case field of
            PidField -> err "No project id found; please report an error"
            NameField -> err "The name field is required"
            EmailField -> err "An email value is required when email comms are selected"
            ZAddrField -> err "Not a valid Zcash shielded address"
       else []
    where
    err str = [ HH.div_ [ HH.span [ P.classes (ClassName <$> [ "badge", "badge-danger-soft" ]) ] [ HH.text str ] ] ]

  -- we use a query to initialize, since this is a modal that doesn't actually get unloaded.
  handleQuery :: forall slots a. Query a -> H.HalogenM CState Action slots output m (Maybe a)
  handleQuery = case _ of
    OpenModal pid a -> do
      H.modify_ (\_ -> initialState { projectId = Just pid })
      lift $ system.toggleModal modalId ModalFFI.ShowModal
      pure (Just a)

  handleAction :: forall slots. Action -> H.HalogenM CState Action slots output m Unit
  handleAction = case _ of
    SetGreetName name ->
      H.modify_ (_ { greetName = Just name })
    SetMessage msg ->
      H.modify_ (_ { message = Just msg })
    SetCommsType t -> 
      H.modify_ (_ { channel = t })
    SetEmail email -> 
      H.modify_ (_ { email = Just email })
    SetZAddr addr -> do
      let setZAddr addr' = do
            zres <- lift $ caps.checkZAddr addr'
            H.modify_ (_ { zaddr = Just addr' })
            case zres of
              Acc.ZAddrCheckValid -> 
                H.modify_ (\st -> st { fieldErrors = filter (_ /= ZAddrField) st.fieldErrors
                                     , channel = ZcashComms 
                                     })
              Acc.ZAddrCheckInvalid -> 
                H.modify_ (\st -> st { fieldErrors = st.fieldErrors <> [ZAddrField] })
      when (addr /= "") (setZAddr addr)
    CreateInvitation -> do
      pidV <- V <<< note [PidField] <$> H.gets (_.projectId)
      nameV <- V <<< note [NameField] <$> H.gets (_.greetName)
      message <- H.gets (_.message)
      channel <- H.gets (_.channel)
      addrV <- 
        case channel of
             EmailComms -> map EmailCommsAddr <<< V <<< note [EmailField] <$> H.gets (_.email)
             ZcashComms -> map ZcashCommsAddr <<< V <<< note [ZAddrField] <$> H.gets (_.zaddr)
      let reqV :: V (Array Field) (Invitation' CommsAddress)
          reqV = { greetName: _, message: _, inviteBy: _ }
            <$> nameV
            <*> pure message
            <*> addrV
      case toEither (Tuple <$> pidV <*> reqV) of
        Right (Tuple pid invitation) -> do
          res <- lift $ caps.createInvitation pid invitation
          case res of
            Right (Just req) -> H.modify_ (_ { mode = QrScan req })
            Right Nothing    -> handleAction Close
            Left errs        -> lift $ system.error (show errs)
        Left errors -> do
          H.modify_ (_ { fieldErrors = errors })
    Close -> do
      H.modify_ (const initialState) -- wipe the state for safety
      lift $ system.toggleModal modalId ModalFFI.HideModal

apiCapability :: Capability Aff
apiCapability 
  = { createInvitation: Project.invite
    , checkZAddr: Acc.checkZAddr
    }
