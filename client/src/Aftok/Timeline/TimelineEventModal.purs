module Aftok.Timeline.TimelineEventModal where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties.ARIA as ARIA

import Aftok.Api.Types (Zip321Request)
import Aftok.Components.Zip321QR as Zip321QR
import Aftok.HTML.Classes as C
import Aftok.Modals.ModalFFI as ModalFFI
import Aftok.Types (System)

data Mode 
  = LogStart Zip321Request
  | LogEnd Zip321Request

type CState = 
  { mode :: Maybe Mode }

data Query a 
  = OpenModal Mode a

data Action
  = Close

type Slot id
  = forall output. H.Slot Query output id

type Slots
  = ( requestQR :: Zip321QR.Slot Unit
    )

_requestQR = SProxy :: SProxy "requestQR"

modalId :: String
modalId = "logEventModal"

modalTitle :: CState -> String
modalTitle s = case s.mode of 
  Just (LogStart _) -> "Scan & submit to log work start"
  Just (LogEnd _) -> "Scan & submit to log end of work period"
  Nothing -> "Awaiting ZIP321 Request"

zip321Request :: Mode -> Zip321Request
zip321Request mode = case mode of
  LogStart req -> req
  LogEnd req -> req

component ::
  forall input output m.
  Monad m =>
  System m ->
  H.Component HH.HTML Query input output m
component system =
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
    { mode: Nothing
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
            [ HH.h5 [P.classes [C.modalTitle], P.id_ (modalId <>"Title") ] [HH.text (modalTitle st)]
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
              Just req -> 
                [ HH.slot _requestQR unit (Zip321QR.component system) (zip321Request req) (const Nothing) ]
              Nothing -> []
          , HH.div
            [ P.classes [C.modalFooter] ]
            [ HH.button
              [ P.type_ ButtonButton
              , P.classes [ C.btn, C.btnSecondary]
              , E.onClick (\_ -> Just Close)
              ]
              [ HH.text "Close" ]
            ]
          ]
        ]
      ]
      
  handleQuery :: forall slots a. Query a -> H.HalogenM CState Action slots output m (Maybe a)
  handleQuery = case _ of
    OpenModal mode a -> do
      H.modify_ (\_ -> initialState { mode = Just mode } )
      lift $ system.toggleModal modalId ModalFFI.ShowModal
      pure (Just a)

  handleAction :: forall slots. Action -> H.HalogenM CState Action slots output m Unit
  handleAction = case _ of
    Close -> do 
      H.modify_ (const initialState) -- wipe the state for safety
      lift $ system.toggleModal modalId ModalFFI.HideModal
