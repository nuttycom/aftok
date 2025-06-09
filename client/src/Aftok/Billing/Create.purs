module Aftok.Billing.Create where

import Prelude

import Aftok.Api.Billing (BillableId, Billable, Recurrence(..), createBillable)
import Aftok.Api.Types (APIError(..))
import Aftok.HTML.Classes as C
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.Types (System, ProjectId)
import Aftok.Zcash (ZEC(..), toZatoshi, ZPrec)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..), note)
import Data.Fixed as Fixed
import Data.Foldable (any)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Number (fromString) as Number
import Data.Number.Format (toString) as Number
import Data.Time.Duration (Hours(..), Days(..))
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

data Field
  = PidField
  | NameField
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
  { projectId :: Maybe ProjectId
  , name :: Maybe String
  , description :: Maybe String
  , message :: Maybe String
  , recurrenceType :: RType
  , recurrenceValue :: Maybe Int
  , amount :: Maybe String
  , gracePeriod :: Maybe Days
  , requestExpiry :: Maybe Hours
  , fieldErrors :: Array Field
  }

data Query a = OpenModal ProjectId a

data Output = BillableCreated BillableId

data Action
  = SetName String
  | SetDesc String
  | SetMessage String
  | SetRecurrenceType RType
  | SetRecurrenceMonths String
  | SetRecurrenceWeeks String
  | SetBillingAmount String
  | SetGracePeriod String
  | SetRequestExpiry String
  | Save
  | Close

type Slot id = H.Slot Query Output id

type Capability (m :: Type -> Type) =
  { createBillable :: ProjectId -> Billable -> m (Either APIError BillableId)
  }

modalId :: String
modalId = "createBillable"

component
  :: forall input m
   . Monad m
  => System m
  -> Capability m
  -> H.Component Query input Output m
component system caps =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            }
    }
  where
  initialState :: CState
  initialState =
    { projectId: Nothing
    , name: Nothing
    , description: Nothing
    , message: Nothing
    , recurrenceType: RTOneTime
    , recurrenceValue: Nothing
    , amount: Nothing
    , gracePeriod: Nothing
    , requestExpiry: Nothing
    , fieldErrors: []
    }

  render :: forall slots. CState -> H.ComponentHTML Action slots m
  render st =
    HH.div
      [ P.classes [ C.modal ]
      , P.id modalId
      , P.tabIndex (negate 1)
      , ARIA.role "dialog"
      , ARIA.labelledBy (modalId <> "Title")
      , ARIA.hidden "true"
      ]
      [ HH.div
          [ P.classes [ C.modalDialog ], ARIA.role "document" ]
          [ HH.div
              [ P.classes [ C.modalContent ] ]
              [ HH.div
                  [ P.classes [ C.modalHeader ] ]
                  [ HH.h5 [ P.classes [ C.modalTitle ], P.id (modalId <> "Title") ] [ HH.text "Create a new billable item" ]
                  , HH.button
                      [ P.classes [ C.close ]
                      , ARIA.label "Close"
                      , P.type_ ButtonButton
                      , E.onClick (\_ -> Close)
                      ]
                      [ HH.span [ ARIA.hidden "true" ] [ HH.text "×" ] ]
                  ]
              , HH.div
                  [ P.classes [ C.modalBody ] ]
                  [ HH.form_
                      [ formGroup st
                          [ NameField ]
                          [ HH.label
                              [ P.for "billableName" ]
                              [ HH.text "Product Name" ]
                          , HH.input
                              [ P.type_ P.InputText
                              , P.classes [ C.formControl, C.formControlSm ]
                              , P.id "billableName"
                              , P.placeholder "A name for the product or service you want to bill for"
                              , E.onValueInput SetName
                              ]
                          ]
                      , formGroup st
                          [ DescField ]
                          [ HH.label
                              [ P.for "billableDesc" ]
                              [ HH.text "Product Description" ]
                          , HH.input
                              [ P.type_ P.InputText
                              , P.classes [ C.formControl, C.formControlSm ]
                              , P.id "billableDesc"
                              , P.placeholder "Description of the product or service"
                              , E.onValueInput SetDesc
                              ]
                          ]
                      , formGroup st
                          [ MessageField ]
                          [ HH.label
                              [ P.for "billableMsg" ]
                              [ HH.text "Message to be included with bill" ]
                          , HH.input
                              [ P.type_ P.InputText
                              , P.classes [ C.formControl, C.formControlSm ]
                              , P.id "billableMsg"
                              , P.placeholder "Enter your message here"
                              , E.onValueInput SetMessage
                              ]
                          ]
                      , formGroup st
                          [ MonthlyRecurrenceField, WeeklyRecurrenceField ]
                          [ formCheckGroup
                              { id: "recurAnnual"
                              , checked: (st.recurrenceType == RTAnnual)
                              , labelClasses: []
                              }
                              (\_ -> SetRecurrenceType RTAnnual)
                              [ HH.text "Annual" ]
                          , formCheckGroup
                              { id: "recurMonthly"
                              , checked: (st.recurrenceType == RTMonthly)
                              , labelClasses: [ C.formInline ]
                              }
                              (\_ -> SetRecurrenceType RTMonthly)
                              [ HH.text "Every"
                              , HH.input
                                  [ P.type_ P.InputNumber
                                  , P.classes [ C.formControl, C.formControlXs, C.formControlFlush, C.marginX2 ]
                                  , P.value
                                      ( if st.recurrenceType == RTMonthly then maybe "" show st.recurrenceValue
                                        else ""
                                      )
                                  , P.min 1.0
                                  , P.max 12.0
                                  , E.onValueInput SetRecurrenceMonths
                                  ]
                              , HH.text "Months"
                              ]
                          , formCheckGroup
                              { id: "recurWeekly"
                              , checked: (st.recurrenceType == RTWeekly)
                              , labelClasses: [ C.formInline ]
                              }
                              (\_ -> SetRecurrenceType RTWeekly)
                              [ HH.text "Every"
                              , HH.input
                                  [ P.type_ P.InputNumber
                                  , P.classes [ C.formControl, C.formControlXs, C.formControlFlush, C.marginX2 ]
                                  , P.value
                                      ( if st.recurrenceType == RTWeekly then maybe "" show st.recurrenceValue
                                        else ""
                                      )
                                  , P.min 1.0
                                  , P.max 12.0
                                  , E.onValueInput SetRecurrenceWeeks
                                  ]
                              , HH.text "Weeks"
                              ]
                          , formCheckGroup
                              { id: "oneTime"
                              , checked: st.recurrenceType == RTOneTime
                              , labelClasses: []
                              }
                              (\_ -> SetRecurrenceType RTOneTime)
                              [ HH.text "One-Time" ]
                          ]
                      , formGroup st
                          [ AmountField ]
                          [ HH.label
                              [ P.for "billableAmount" ]
                              [ HH.text "Amount" ]
                          , HH.div
                              [ P.classes [ ClassName "input-group", ClassName "input-group-sm" ] ]
                              [ HH.input
                                  [ P.type_ P.InputNumber
                                  , P.classes [ C.formControl ]
                                  , P.id "billableAmount"
                                  , P.value (fromMaybe "" st.amount)
                                  , P.placeholder "1.0"
                                  , P.min 0.0
                                  , E.onValueInput SetBillingAmount
                                  ]
                              , HH.div
                                  [ P.classes [ ClassName "input-group-append" ] ]
                                  [ HH.span
                                      [ P.classes [ ClassName "input-group-text" ]
                                      , P.style "height: auto;" -- fix bad calculated height from LandKit
                                      ]
                                      [ HH.text "ZEC" ]
                                  ]
                              ]
                          ]
                      , formGroup st
                          [ GracePeriodField ]
                          [ HH.label
                              [ P.for "gracePeriod" ]
                              [ HH.text "Grace Period (Days)" ]
                          , HH.input
                              [ P.type_ P.InputNumber
                              , P.id "gracePeriod"
                              , P.classes [ C.formControl, C.formControlSm ]
                              , P.value (maybe "" (Number.toString <<< unwrap) st.gracePeriod)
                              , P.placeholder "Days until a bill is considered overdue"
                              , P.min 0.0
                              , E.onValueInput SetGracePeriod
                              ]
                          ]
                      , formGroup st
                          [ RequestExpiryField ]
                          [ HH.label
                              [ P.for "requestExpiry" ]
                              [ HH.text "Request Expiry Period (Hours)" ]
                          , HH.input
                              [ P.type_ P.InputNumber
                              , P.id "gracePeriod"
                              , P.classes [ C.formControl, C.formControlSm ]
                              , P.value (maybe "" (Number.toString <<< unwrap) st.requestExpiry)
                              , P.placeholder "Hours until a payment request expires"
                              , P.min 0.0
                              , E.onValueInput SetRequestExpiry
                              ]
                          ]
                      ]
                  , formGroup st [ PidField ] []
                  ]
              , HH.div
                  [ P.classes [ C.modalFooter ] ]
                  [ HH.button
                      [ P.type_ ButtonButton
                      , P.classes [ C.btn, C.btnSecondary ]
                      , E.onClick (\_ -> Close)
                      ]
                      [ HH.text "Close" ]
                  , HH.button
                      [ P.type_ ButtonButton
                      , P.classes [ C.btn, C.btnPrimary ]
                      , E.onClick (\_ -> Save)
                      ]
                      [ HH.text "Create billable" ]
                  ]
              ]
          ]
      ]

  formGroup :: forall i a. CState -> Array Field -> Array (HH.HTML i a) -> HH.HTML i a
  formGroup st fields body =
    HH.div
      [ P.classes [ C.formGroup ] ]
      (body <> (fieldError st =<< fields))

  formCheckGroup
    :: forall i a
     . { id :: String
       , checked :: Boolean
       , labelClasses :: Array ClassName
       }
    -> (Unit -> a)
    -> Array (HH.HTML i a)
    -> HH.HTML i a
  formCheckGroup { id, checked, labelClasses } onChange children =
    HH.div
      [ P.classes [ C.formCheck ] ]
      [ HH.input
          ( [ P.type_ P.InputRadio
            , P.name "recurType"
            , P.classes [ C.formCheckInput ]
            , P.id id
            , E.onClick \_ -> onChange unit
            ] <> (if checked then [ P.checked true ] else [])
          )
      , HH.label
          [ P.classes ([ C.formCheckLabel ] <> labelClasses)
          , P.for id
          ]
          children
      ]

  fieldError :: forall i a. CState -> Field -> Array (HH.HTML i a)
  fieldError st field =
    if any (_ == field) st.fieldErrors then case field of
      PidField -> err "No project id found; please report an error"
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
    err str =
      [ HH.div_
          [ HH.span
              [ P.classes (ClassName <$> [ "badge", "badge-danger-soft" ]) ]
              [ HH.text str ]
          ]
      ]

  -- we use a query to initialize, since this is a modal that doesn't actually get unloaded.
  handleQuery :: forall slots a. Query a -> H.HalogenM CState Action slots Output m (Maybe a)
  handleQuery = case _ of
    OpenModal pid a -> do
      H.modify_ (\_ -> initialState { projectId = Just pid })
      lift $ system.toggleModal modalId ModalFFI.ShowModal
      pure (Just a)

  handleAction :: forall slots. Action -> H.HalogenM CState Action slots Output m Unit
  handleAction = case _ of
    SetName name ->
      H.modify_ (_ { name = Just name })
    SetDesc desc ->
      H.modify_ (_ { description = Just desc })
    SetMessage msg ->
      H.modify_ (_ { message = Just msg })
    SetRecurrenceType rtype -> do
      curRecurType <- H.gets _.recurrenceType
      curDuration <- H.gets _.recurrenceValue
      let
        rdur = case curRecurType of
          RTMonthly | rtype == RTMonthly -> curDuration
          RTWeekly | rtype == RTWeekly -> curDuration
          _ -> Nothing
      H.modify_ (_ { recurrenceType = rtype, recurrenceValue = rdur })
    SetRecurrenceMonths dur ->
      case Int.fromString dur of
        (Just n) -> H.modify_ (_ { recurrenceType = RTMonthly, recurrenceValue = Just n })
        (Nothing) -> pure unit
    SetRecurrenceWeeks dur ->
      case Int.fromString dur of
        (Just n) -> H.modify_ (_ { recurrenceType = RTWeekly, recurrenceValue = Just n })
        (Nothing) -> pure unit
    SetBillingAmount amt -> do
      curAmount <- H.gets (_.amount)
      case Fixed.fromString amt of
        (Just (_ :: Fixed.Fixed ZPrec)) ->
          H.modify_ (_ { amount = Just amt })
        (Nothing) ->
          H.modify_ (_ { amount = curAmount })
    SetGracePeriod dur ->
      case Number.fromString dur of
        (Just n) -> H.modify_ (_ { gracePeriod = Just (Days n) })
        (Nothing) -> pure unit
    SetRequestExpiry dur ->
      case Number.fromString dur of
        (Just n) -> H.modify_ (_ { requestExpiry = Just (Hours n) })
        (Nothing) -> pure unit

    Save -> do
      pidV <- V <<< note [ PidField ] <$> H.gets (_.projectId)
      nameV <- V <<< note [ NameField ] <$> H.gets (_.name)
      descV <- V <<< note [ DescField ] <$> H.gets (_.description)
      msgV <- V <<< note [ MessageField ] <$> H.gets (_.message)
      rtype <- H.gets (_.recurrenceType)
      rvalueV <- case rtype of
        RTAnnual -> pure $ V (Right Annually)
        RTMonthly -> V <<< maybe (Left [ MonthlyRecurrenceField ]) (Right <<< Monthly) <$> H.gets (_.recurrenceValue)
        RTWeekly -> V <<< maybe (Left [ WeeklyRecurrenceField ]) (Right <<< Weekly) <$> H.gets (_.recurrenceValue)
        RTOneTime -> pure $ V (Right OneTime)
      zecStr <- (Fixed.fromString =<< _) <$> H.gets (_.amount)
      zatsV <- pure $ V (maybe (Left [ AmountField ]) (Right <<< toZatoshi <<< ZEC) zecStr)
      gperV <- V <<< note [ GracePeriodField ] <$> H.gets (_.gracePeriod)
      expiryV <- V <<< note [ RequestExpiryField ] <$> H.gets (_.requestExpiry)
      let
        toBillable =
          { name: _
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

      case toEither (Tuple <$> pidV <*> reqV) of
        Right (Tuple pid billable) -> do
          res <- lift $ caps.createBillable pid billable
          case res of
            Right bid -> do
              H.raise (BillableCreated bid)
              handleAction Close
            Left errs -> do
              lift $ system.error (show errs)
        Left errors -> do
          H.modify_ (_ { fieldErrors = errors })

    Close -> do
      H.modify_ (const initialState) -- wipe the state for safety
      lift $ system.toggleModal modalId ModalFFI.HideModal

apiCapability :: Capability Aff
apiCapability =
  { createBillable: createBillable
  }

mockCapability :: Capability Aff
mockCapability =
  { createBillable: \_ _ -> pure $ Left Forbidden }

