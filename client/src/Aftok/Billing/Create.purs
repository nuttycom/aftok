module Aftok.Billing.Create where

import Prelude
import Control.Monad.Trans.Class (lift)
-- import Data.DateTime (DateTime, date)
import Data.Either (Either(..), note)
import Data.Fixed as Fixed
import Data.Foldable (any)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Number (fromString) as Number
import Data.Number.Format (toString) as Number
-- import Data.Unfoldable as U
import Data.Validation.Semigroup (V(..), toEither)
import Data.Time.Duration (Hours(..))
-- import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
-- import Effect.Class (liftEffect)
-- import Effect.Now (nowDateTime)
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
  , Billable
  , Recurrence(..)
  , createBillable
  )
import Aftok.Zcash (ZEC(..), toZatoshi)

data Field
  = NameField
  | DescField
  | MessageField
  | MonthlyRecurrenceField
  | WeeklyRecurrenceField
  | AmountField
  | GracePeriodField
  | RequestExpiryField

derive instance fieldEq :: Eq Field
derive instance fieldOrd :: Ord Field

data RType
  = RTAnnual
  | RTMonthly
  | RTWeekly
  | RTOneTime

derive instance rtypeEq :: Eq RType

type CState =
  { projectId :: ProjectId
  , name :: Maybe String
  , description :: Maybe String
  , message :: Maybe String
  , recurrenceType :: RType
  , recurrenceValue :: Maybe Int
  , amount :: Maybe ZEC
  , gracePeriod :: Maybe Hours
  , requestExpiry :: Maybe Hours
  , fieldErrors :: Array Field
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
  | SetRecurrenceMonths String
  | SetRecurrenceWeeks String
  | SetBillingAmount String
  | SetGracePeriod String
  | SetRequestExpiry String
  | SaveBillable

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
    , recurrenceType : RTOneTime
    , recurrenceValue : Nothing
    , amount : Nothing
    , gracePeriod : Nothing
    , requestExpiry : Nothing
    , fieldErrors : []
    }

  render :: forall slots. CState -> H.ComponentHTML Action slots m
  render st =
    Modals.modalWithSave "createBillable" "Create Billable" SaveBillable
      [ HH.form_
        [ formGroup st
          [ NameField ]
          [ HH.label 
            [ P.for "billableName"]
            [ HH.text "Product Name" ] 
          , HH.input
            [ P.type_ P.InputText
            , P.classes [ C.formControlSm ]
            , P.id_ "billableName"
            , P.placeholder "A name for the product or service you want to bill for"
            , E.onValueInput (Just <<< SetName)
            ]
          ] 
        , formGroup st
          [ DescField ]
          [ HH.label 
              [ P.for "billableDesc"]
              [ HH.text "Product Description" ] 
          , HH.input
              [ P.type_ P.InputText
              , P.classes [ C.formControlSm ]
              , P.id_ "billableDesc"
              , P.placeholder "Description of the product or service"
              , E.onValueInput (Just <<< SetDesc)
              ]
          ]
        , formGroup st
          [ MessageField ]
          [ HH.label 
              [ P.for "billableMsg"]
              [ HH.text "Message to be included with bill" ] 
          , HH.input
              [ P.type_ P.InputText
              , P.id_ "billableMsg"
              , P.placeholder "Enter your message here"
              , E.onValueInput (Just <<< SetMessage)
              ]
          ]
        , formGroup st
          [MonthlyRecurrenceField, WeeklyRecurrenceField]
          [ HH.label_
            [ HH.input 
                ([ P.type_ P.InputRadio 
                , P.name "recurType" 
                , E.onClick \_ -> Just (SetRecurrenceType RTAnnual)
                ] <> (if st.recurrenceType == RTAnnual then [P.checked true] else []))
            , HH.text " Annual"
            ]
          , HH.label_
            [ HH.input 
                ([ P.type_ P.InputRadio 
                , P.name "recurType" 
                , E.onClick \_ -> Just (SetRecurrenceType RTMonthly)
                ] <> (if st.recurrenceType == RTMonthly then [P.checked true] else []))
            , HH.text " every "
            , HH.input
                [ P.type_ P.InputNumber
                , P.classes [ C.formControlSm ]
                , P.value (if st.recurrenceType == RTMonthly
                              then maybe "" show st.recurrenceValue
                              else "")
                , P.min 1.0
                , P.max 12.0
                , E.onValueInput (Just <<< SetRecurrenceMonths)
                ] 
            , HH.text " Months"
            ]
          , HH.label_
            [ HH.input 
                ([ P.type_ P.InputRadio 
                , P.name "recurType" 
                , E.onClick \_ -> Just (SetRecurrenceType RTWeekly)
                ] <> (if st.recurrenceType == RTWeekly then [P.checked true] else []))
            , HH.text " every "
            , HH.input
                [ P.type_ P.InputNumber
                , P.classes [ C.formControlSm ]
                , P.value (if st.recurrenceType == RTWeekly
                              then maybe "" show st.recurrenceValue
                              else "")
                , P.min 1.0
                , P.max 12.0
                , E.onValueInput (Just <<< SetRecurrenceWeeks)
                ] 
            , HH.text " Weeks"
            ]
          , HH.label_
            [ HH.input 
                ([ P.type_ P.InputRadio 
                , P.name "recurType" 
                , E.onClick \_ -> Just (SetRecurrenceType RTOneTime)
                ] <> (if st.recurrenceType == RTOneTime then [P.checked true] else []))
            , HH.text " One-Time"
            ]
          ]
        , formGroup st
          [AmountField]
          [ HH.label 
              [ P.for "billableAmount"]
              [ HH.text "Amount" ] 
          , HH.input
              [ P.type_ P.InputNumber
              , P.classes [ C.formControlSm ]
              , P.id_ "billableAmount"
              , P.value (maybe "" (Fixed.toString <<< unwrap) st.amount)
              , P.placeholder "1.0"
              , P.min 0.0
              , E.onValueInput (Just <<< SetBillingAmount)
              ]
          , HH.div
              [ P.classes [ ClassName "input-group-append" ] ]
              [ HH.span [ P.classes [ ClassName "input-group-text" ] ] [ HH.text "ZEC" ] ]
          ]
        , formGroup  st
          [GracePeriodField]
          [ HH.label 
              [ P.for "gracePeriod"]
              [ HH.text "Grace Period (Hours)" ] 
          , HH.input
              [ P.type_ P.InputNumber
              , P.id_ "gracePeriod"
              , P.classes [ C.formControlSm ]
              , P.value (maybe "" (Number.toString <<< unwrap) st.gracePeriod)
              , P.placeholder "Hours until a bill is considered overdue"
              , P.min 0.0
              , E.onValueInput (Just <<< SetGracePeriod)
              ] 
          ]
        , formGroup st
          [RequestExpiryField]
          [ HH.label 
              [ P.for "requestExpiry"]
              [ HH.text "Request Expiry Period (Hours)" ] 
          , HH.input
              [ P.type_ P.InputNumber
              , P.id_ "gracePeriod"
              , P.classes [ C.formControlSm ]
              , P.value (maybe "" (Number.toString <<< unwrap) st.requestExpiry)
              , P.placeholder "Hours until a payment request expires"
              , P.min 0.0
              , E.onValueInput (Just <<< SetRequestExpiry)
              ] 
          ]
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
            DescField -> err "The description field is required"
            MessageField -> err "The message field is required"
            MonthlyRecurrenceField -> err "You must enter a valid number of months."
            WeeklyRecurrenceField -> err "You must enter a valid number of weeks."
            AmountField -> err "You must enter a valid amount of ZEC"
            GracePeriodField -> err "You must enter a valid number of hours."
            RequestExpiryField -> err "You must enter a valid number of hours."
       else []
    where
    err str = [ HH.div_ [ HH.span [ P.classes (ClassName <$> [ "badge", "badge-danger-soft" ]) ] [ HH.text str ] ] ]

  eval :: forall slots. Action -> H.HalogenM CState Action slots Output m Unit
  eval = case _ of
      ProjectChanged pid ->
        H.modify_ (_ { projectId = pid })
      SetName name -> 
        H.modify_ (_ { name = Just name })
      SetDesc desc -> 
        H.modify_ (_ { description = Just desc })
      SetMessage msg -> 
        H.modify_ (_ { message = Just msg })
      SetRecurrenceType rtype -> do
        curRecurType <- H.gets _.recurrenceType
        curDuration <- H.gets _.recurrenceValue
        let rdur = case curRecurType of
              RTMonthly | rtype == RTMonthly -> curDuration
              RTWeekly  | rtype == RTWeekly  -> curDuration
              _ -> Nothing
        H.modify_ (_ { recurrenceType = rtype, recurrenceValue = rdur  })
      SetRecurrenceMonths dur -> 
        case Int.fromString dur of
             (Just n) -> H.modify_ (_ { recurrenceType = RTMonthly, recurrenceValue = Just n })
             (Nothing) -> pure unit
      SetRecurrenceWeeks dur -> 
        case Int.fromString dur of
             (Just n) -> H.modify_ (_ { recurrenceType = RTWeekly, recurrenceValue = Just n })
             (Nothing) -> pure unit
      SetBillingAmount amt -> 
        case Fixed.fromString amt of
             (Just zec) -> H.modify_ (_ { amount = Just (ZEC zec) })
             (Nothing) -> pure unit
      SetGracePeriod dur -> 
        case Number.fromString dur of
             (Just n) -> H.modify_ (_ { gracePeriod = Just (Hours n) })
             (Nothing) -> pure unit
      SetRequestExpiry dur -> 
        case Number.fromString dur of
             (Just n) -> H.modify_ (_ { requestExpiry = Just (Hours n) })
             (Nothing) -> pure unit
      SaveBillable -> do
        nameV <- V <<< note [NameField] <$> H.gets (_.name)
        descV <- V <<< note [DescField] <$> H.gets (_.description)
        msgV <- V <<< note [MessageField] <$> H.gets (_.message)
        rtype <- H.gets (_.recurrenceType)
        rvalueV <- case rtype of
          RTAnnual  -> pure $ V (Right Annually)
          RTMonthly -> V <<< maybe (Left [MonthlyRecurrenceField]) (Right <<< Monthly) <$> H.gets (_.recurrenceValue)
          RTWeekly  -> V <<< maybe (Left [WeeklyRecurrenceField]) (Right <<< Weekly) <$> H.gets (_.recurrenceValue)
          RTOneTime -> pure $ V (Right OneTime)
        zatsV <- V <<< maybe (Left [AmountField]) (Right <<< toZatoshi) <$> H.gets (_.amount)
        gperV <- V <<< note [GracePeriodField] <$> H.gets (_.gracePeriod)
        expiryV <- V <<< note [RequestExpiryField] <$> H.gets (_.requestExpiry)
        let toBillable = { name: _
                         , description: _
                         , message: _
                         , recurrence: _
                         , amount: _
                         , gracePeriod: _
                         , expiryPeriod: _
                         }
        
            reqV :: V (Array Field) Billable
            reqV = 
              toBillable <$> nameV
                         <*> descV
                         <*> msgV
                         <*> rvalueV
                         <*> zatsV
                         <*> gperV
                         <*> expiryV

        case toEither reqV of
          Left errors -> do
            H.modify_ (_ { fieldErrors = errors })
          Right billable -> do
            lift $ system.log "BILLABLE OK"
            pid <- H.gets (_.projectId)
            res <- lift $ caps.createBillable pid billable
            case res of
              Right bid -> do
                H.raise (Tuple bid billable)
                lift $ system.toggleModal "createBillable" ModalFFI.HideModal
              Left errs -> 
                lift $ system.error (show errs)

apiCapability :: Capability Aff
apiCapability =
  { createBillable: createBillable
  }

mockCapability :: Capability Aff
mockCapability =
  { createBillable: \_ _ -> pure $ Left Forbidden }

