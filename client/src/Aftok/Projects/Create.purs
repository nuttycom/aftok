module Aftok.Projects.Create where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..), note)
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.Time.Duration (Days(..))
import Data.Validation.Semigroup (V(..), toEither)
import Effect.Aff (Aff)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties.ARIA as ARIA
import Aftok.Api.Project (ProjectCreateRequest, DepreciationFn(..), createProject)
import Aftok.Api.Types (APIError(..))
import Aftok.HTML.Classes as C
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.Types (System, ProjectId)

data Field
  = NameField
  | UndepField
  | DepField

derive instance fieldEq :: Eq Field
derive instance fieldOrd :: Ord Field

type CState =
  { projectName :: Maybe String
  , undep :: Maybe Days
  , dep :: Maybe Days
  , fieldErrors :: Array Field
  }

data Query a = OpenModal a

data Output = ProjectCreated ProjectId

data Action
  = SetName String
  | SetUndepDays String
  | SetDepDays String
  | Save
  | Close

type Slot id = H.Slot Query Output id

type Capability (m :: Type -> Type) =
  { createProject :: ProjectCreateRequest -> m (Either APIError ProjectId)
  }

modalId :: String
modalId = "createProject"

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
    { projectName: Nothing
    , undep: Nothing
    , dep: Nothing
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
                  [ HH.h5 [ P.classes [ C.modalTitle ], P.id (modalId <> "Title") ] [ HH.text "Create a new project" ]
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
                              [ P.for "projectName" ]
                              [ HH.text "Project Name" ]
                          , HH.input
                              [ P.type_ P.InputText
                              , P.classes [ C.formControl, C.formControlSm ]
                              , P.id "projectName"
                              , P.placeholder "My awesome new project!!!"
                              , E.onValueInput SetName
                              ]
                          ]
                      , formGroup st
                          [ UndepField ]
                          [ HH.label
                              [ P.for "undepDays" ]
                              [ HH.text "Number of days before a share begins to depreciate" ]
                          , HH.input
                              [ P.type_ P.InputNumber
                              , P.classes [ C.formControl, C.formControlXs, C.formControlFlush, C.marginX2 ]
                              , P.id "undepDays"
                              , P.placeholder "180"
                              , E.onValueInput SetUndepDays
                              ]
                          ]
                      , formGroup st
                          [ DepField ]
                          [ HH.label
                              [ P.for "undepDays" ]
                              [ HH.text "Number of days over which a share depreciates" ]
                          , HH.input
                              [ P.type_ P.InputNumber
                              , P.classes [ C.formControl, C.formControlXs, C.formControlFlush, C.marginX2 ]
                              , P.id "undepDays"
                              , P.placeholder "1800"
                              , E.onValueInput SetDepDays
                              ]
                          ]
                      ]
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
                      [ HH.text "Create project" ]
                  ]
              ]
          ]
      ]

  formGroup :: forall i a. CState -> Array Field -> Array (HH.HTML i a) -> HH.HTML i a
  formGroup st fields body =
    HH.div
      [ P.classes [ C.formGroup ] ]
      (body <> (fieldError st =<< fields))

  fieldError :: forall i a. CState -> Field -> Array (HH.HTML i a)
  fieldError st field =
    if any (_ == field) st.fieldErrors then case field of
      NameField -> err "The name field is required"
      UndepField -> err "A number of days before depreciation starts is required"
      DepField -> err "The number of days over which a share depreciates is required"
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
    OpenModal a -> do
      H.modify_ (const initialState)
      lift $ system.toggleModal modalId ModalFFI.ShowModal
      pure (Just a)

  handleAction :: forall slots. Action -> H.HalogenM CState Action slots Output m Unit
  handleAction = case _ of
    SetName name ->
      H.modify_ (_ { projectName = Just name })
    SetUndepDays days ->
      case Number.fromString days of
        (Just n) -> H.modify_ (_ { undep = Just $ Days n })
        (Nothing) -> pure unit
    SetDepDays days ->
      case Number.fromString days of
        (Just n) -> H.modify_ (_ { dep = Just $ Days n })
        (Nothing) -> pure unit
    Save -> do
      nameV <- V <<< note [ NameField ] <$> H.gets (_.projectName)
      undepV <- V <<< note [ UndepField ] <$> H.gets (_.undep)
      depV <- V <<< note [ DepField ] <$> H.gets (_.dep)
      let
        req =
          { projectName: _
          , depf: _
          }

        reqV =
          req <$> nameV
            <*> (LinearDepreciation <$> ({ undep: _, dep: _ } <$> undepV <*> depV))

      case toEither reqV of
        Right req' -> do
          res <- lift $ caps.createProject req'
          case res of
            Right pid -> do
              H.raise (ProjectCreated pid)
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
  { createProject: createProject
  }

mockCapability :: Capability Aff
mockCapability =
  { createProject: \_ -> pure $ Left Forbidden }

