module Aftok.Projects.Invite where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array (filter)
import Data.Either (Either(..), note)
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V(..), toEither)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Aftok.Api.Account as Acc
import Aftok.Api.Project as Project
import Aftok.Api.Project (Invitation')
import Aftok.Api.Types (APIError, CommsType(..), CommsAddress(..), Zip321Request)
import Aftok.HTML.Forms (commsSwitch, commsField)
import Aftok.HTML.Classes as C
import Aftok.Modals as Modals
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.Types (System, ProjectId)

data Field
  = NameField
  | EmailField
  | ZAddrField

derive instance fieldEq :: Eq Field
derive instance fieldOrd :: Ord Field

type CState =
  { projectId :: ProjectId
  , greetName :: Maybe String
  , message :: Maybe String
  , recoveryType :: CommsType
  , recoveryEmail :: Maybe String
  , recoveryZAddr :: Maybe String
  , fieldErrors :: Array Field
  }

type Input = ProjectId

type Output = Maybe Zip321Request

data Action
  = ProjectChanged ProjectId
  | SetGreetName String
  | SetMessage String
  | SetCommsType CommsType
  | SetEmail String
  | SetZAddr String
  | CreateInvitation

type Slot id
  = forall query. H.Slot query Output id

type Capability (m :: Type -> Type)
  = { createInvitation :: ProjectId -> Invitation' CommsAddress -> m (Either APIError (Maybe Zip321Request))
    , checkZAddr :: String -> m Acc.ZAddrCheckResponse
    }

modalId :: String
modalId = "createInvitation"

component ::
  forall query m.
  Monad m =>
  System m ->
  Capability m ->
  H.Component HH.HTML query Input Output m
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
    , greetName : Nothing
    , message : Nothing
    , recoveryType: EmailComms
    , recoveryEmail: Nothing
    , recoveryZAddr: Nothing
    , fieldErrors : []
    }

  render :: forall slots. CState -> H.ComponentHTML Action slots m
  render st =
    Modals.modalWithSave modalId "Invite a collaborator" CreateInvitation
      [ HH.form_
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
        , commsSwitch SetCommsType st.recoveryType
        , commsField SetEmail SetZAddr st $ case _ of
            EmailComms -> fieldError st EmailField    
            ZcashComms -> fieldError st ZAddrField
        ]
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
            NameField -> err "The name field is required"
            EmailField -> err "The email field is when email comms are selected"
            ZAddrField -> err "Not a valid Zcash shielded address."
       else []
    where
    err str = [ HH.div_ [ HH.span [ P.classes (ClassName <$> [ "badge", "badge-danger-soft" ]) ] [ HH.text str ] ] ]

  setZAddr addr = do
    zres <- lift $ caps.checkZAddr addr
    H.modify_ (_ { recoveryZAddr = Just addr })
    case zres of
      Acc.ZAddrCheckValid -> 
        H.modify_ (\st -> st { fieldErrors = filter (_ /= ZAddrField) st.fieldErrors, recoveryType = ZcashComms })
      Acc.ZAddrCheckInvalid -> 
        H.modify_ (\st -> st { fieldErrors = st.fieldErrors <> [ZAddrField] })

  -- eval :: forall slots. Action -> H.HalogenM CState Action slots Output m Unit
  eval = case _ of
      ProjectChanged pid ->
        H.modify_ (_ { projectId = pid })
      SetGreetName name ->
        H.modify_ (_ { greetName = Just name })
      SetMessage msg ->
        H.modify_ (_ { message = Just msg })
      SetCommsType t -> 
        H.modify_ (_ { recoveryType = t })
      SetEmail email -> 
        H.modify_ (_ { recoveryEmail = Just email })
      SetZAddr addr ->
        when (addr /= "") (setZAddr addr)
      CreateInvitation -> do
        nameV <- V <<< note [NameField] <$> H.gets (_.greetName)
        message <- H.gets (_.message)
        addrType <- H.gets (_.recoveryType)
        addrV <- 
          case addrType of
            EmailComms -> map EmailCommsAddr <<< V <<< note [EmailField] <$> H.gets (_.recoveryEmail)
            ZcashComms -> map ZcashCommsAddr <<< V <<< note [ZAddrField] <$> H.gets (_.recoveryZAddr)
        let reqV :: V (Array Field) (Invitation' CommsAddress)
            reqV = { greetName: _, message: _, inviteBy: _ }
              <$> nameV
              <*> pure message
              <*> addrV
        case toEither reqV of
          Left errors -> do
            H.modify_ (_ { fieldErrors = errors })
          Right invitation -> do
            pid <- H.gets (_.projectId)
            res <- lift $ caps.createInvitation pid invitation
            case res of
              Right result -> do
                H.raise result
                lift $ system.toggleModal modalId ModalFFI.HideModal
              Left errs ->
                lift $ system.error (show errs)

apiCapability :: Capability Aff
apiCapability 
  = { createInvitation: Project.invite
    , checkZAddr: Acc.checkZAddr
    }
