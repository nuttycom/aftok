module Aftok.Billing.Create where

import Prelude

import Aftok.Api.Billing (BillableId, Billable, Recurrence(..), createBillable)
import Aftok.Api.Types (APIError(..))
import Aftok.HTML.Classes as C
import Aftok.Modals as Modals
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.Types (System, ProjectId)
import Aftok.Zcash (ZEC(..), toZatoshi)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..), note)
import Data.Fixed as Fixed
import Data.Foldable (any)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Number (fromString) as Number
import Data.Number.Format (toString) as Number
import Data.Time.Duration (Hours(..), Days(..))
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..), toEither)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

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
  , gracePeriod :: Maybe Days
  , requestExpiry :: Maybe Hours
  , fieldErrors :: Array Field
  }

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
  = forall query. H.Slot query Output id

type Capability (m :: Type -> Type)
  = { createBillable :: ProjectId -> Billable -> m (Either APIError BillableId)
    }

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
            , P.classes [ C.formControl, C.formControlSm ]
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
              , P.classes [ C.formControl, C.formControlSm ]
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
              , P.classes [C.formControl, C.formControlSm]
              , P.id_ "billableMsg"
              , P.placeholder "Enter your message here"
              , E.onValueInput (Just <<< SetMessage)
              ]
          ]
        , formGroup st
          [MonthlyRecurrenceField, WeeklyRecurrenceField]
          [ formCheckGroup
              { id: "recurAnnual"
              , checked: (st.recurrenceType == RTAnnual)
              , labelClasses: []
              }
              (\_ -> Just (SetRecurrenceType RTAnnual))
              [ HH.text "Annual" ]
          , formCheckGroup
              { id: "recurMonthly"
              , checked: (st.recurrenceType == RTMonthly)
              , labelClasses: [C.formInline]
              }
              (\_ -> Just (SetRecurrenceType RTMonthly))
              [ HH.text "Every"
              , HH.input
                  [ P.type_ P.InputNumber
                  , P.classes [ C.formControl, C.formControlXs, C.formControlFlush, C.marginX2 ]
                  , P.value (if st.recurrenceType == RTMonthly
                                then maybe "" show st.recurrenceValue
                                else "")
                  , P.min 1.0
                  , P.max 12.0
                  , E.onValueInput (Just <<< SetRecurrenceMonths)
                  ]
              , HH.text "Months"]
          , formCheckGroup
            { id: "recurWeekly"
            , checked: (st.recurrenceType == RTWeekly)
            , labelClasses: [C.formInline]
            }
            (\_ -> Just (SetRecurrenceType RTWeekly))
            [ HH.text "Every"
            , HH.input
                [ P.type_ P.InputNumber
                , P.classes [ C.formControl, C.formControlXs, C.formControlFlush, C.marginX2 ]
                , P.value (if st.recurrenceType == RTWeekly
                              then maybe "" show st.recurrenceValue
                              else "")
                , P.min 1.0
                , P.max 12.0
                , E.onValueInput (Just <<< SetRecurrenceWeeks)
                ]
            , HH.text "Weeks"
            ]
          , formCheckGroup
            { id: "oneTime"
            , checked: st.recurrenceType == RTOneTime
            , labelClasses: []
            }
            (\_ -> Just (SetRecurrenceType RTOneTime))
            [ HH.text "One-Time" ]
          ]
        , formGroup st
          [AmountField]
          [ HH.label
              [ P.for "billableAmount"]
              [ HH.text "Amount" ]
          , HH.div
          [ P.classes [ ClassName "input-group", ClassName "input-group-sm" ] ]
              [ HH.input
                  [ P.type_ P.InputNumber
                  , P.classes [ C.formControl ]
                  , P.id_ "billableAmount"
                  , P.value (maybe "" (Fixed.toString <<< unwrap) st.amount)
                  , P.placeholder "1.0"
                  , P.min 0.0
                  , E.onValueInput (Just <<< SetBillingAmount)
                  ]
              , HH.div
                [ P.classes [ ClassName "input-group-append"] ]
                [ HH.span
                    [ P.classes [ ClassName "input-group-text" ]
                    , P.style "height: auto;" -- fix bad calculated height from LandKit
                    ]
                    [ HH.text "ZEC" ] ]
              ]
          ]
        , formGroup  st
          [GracePeriodField]
          [ HH.label
              [ P.for "gracePeriod"]
              [ HH.text "Grace Period (Days)" ]
          , HH.input
              [ P.type_ P.InputNumber
              , P.id_ "gracePeriod"
              , P.classes [ C.formControl, C.formControlSm ]
              , P.value (maybe "" (Number.toString <<< unwrap) st.gracePeriod)
              , P.placeholder "Days until a bill is considered overdue"
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
              , P.classes [ C.formControl, C.formControlSm ]
              , P.value (maybe "" (Number.toString <<< unwrap) st.requestExpiry)
              , P.placeholder "Hours until a payment request expires"
              , P.min 0.0
              , E.onValueInput (Just <<< SetRequestExpiry)
              ]
          ]
        ]
      ]

  formCheckGroup :: forall i a.
    { id :: String
    , checked :: Boolean
    , labelClasses :: Array ClassName
    }
    -> (Unit -> Maybe a)
    -> Array (HH.HTML i a)
    -> HH.HTML i a
  formCheckGroup { id, checked, labelClasses } onChange children  =
    HH.div
      [ P.classes [C.formCheck] ]
      [ HH.input
          ([ P.type_ P.InputRadio
          , P.name "recurType"
          , P.classes [C.formCheckInput]
          , P.id_ id
          , E.onClick \_ -> onChange unit
          ] <> (if checked then [P.checked true] else []))
       , HH.label
           [ P.classes ([C.formCheckLabel ] <> labelClasses)
           , P.for id]
           children
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
             (Just n) -> H.modify_ (_ { gracePeriod = Just (Days n) })
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

