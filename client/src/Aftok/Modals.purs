module Aftok.Modals where

import Prelude ((<>), negate)
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties.ARIA as ARIA

import Aftok.HTML.Classes as C
import Aftok.HTML.Properties as AP

modalButton :: forall action slots m. String -> String -> Maybe action -> H.ComponentHTML action slots m
modalButton target text action =
  HH.button
    [ P.classes [ C.btn, C.btnPrimary ]
    , AP.dataToggle "modal"
    , AP.dataTarget ("#" <> target)
    , P.type_ ButtonButton
    , E.onClick (\_ -> action)
    ]
    [ HH.text text ]

modalWithSave ::
  forall action slots m.
  String ->
  String ->
  action ->
  Array (H.ComponentHTML action slots m) ->
  H.ComponentHTML action slots m
modalWithSave modalId title submit contents =
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
          [ HH.h5 [P.classes [C.modalTitle], P.id_ (modalId <>"Title") ] [HH.text title]
          , HH.button
            [ P.classes [ C.close ]
            , AP.dataDismiss "modal"
            , ARIA.label "Close"
            , P.type_ ButtonButton
            ]
            [ HH.span [ARIA.hidden "true"] [HH.text "×"]]
          ]
        , HH.div
          [ P.classes [C.modalBody] ]
          contents
        , HH.div
          [ P.classes [C.modalFooter] ]
          [ HH.button
            [ P.type_ ButtonButton
            , P.classes [ C.btn, C.btnSecondary]
            , AP.dataDismiss "modal"
            ]
            [ HH.text "Close" ]
          , HH.button
            [ P.type_ ButtonButton
            , P.classes [ C.btn, C.btnPrimary ]
            , E.onClick (\_ -> Just submit)
            ]
            [ HH.text "Save changes"]
          ]
        ]
      ]
    ]

modalWithClose ::
  forall action slots m.
  String ->
  String ->
  action ->
  Array (H.ComponentHTML action slots m) ->
  H.ComponentHTML action slots m
modalWithClose modalId title action contents =
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
          [ HH.h5 [P.classes [C.modalTitle], P.id_ (modalId <>"Title") ] [HH.text title]
          , HH.button
            [ P.classes [ C.close ]
            , AP.dataDismiss "modal"
            , ARIA.label "Close"
            , P.type_ ButtonButton
            ]
            [ HH.span [ARIA.hidden "true"] [HH.text "×"]]
          ]
        , HH.div
          [ P.classes [C.modalBody] ]
          contents
        , HH.div
          [ P.classes [C.modalFooter] ]
          [ HH.button
            [ P.type_ ButtonButton
            , P.classes [ C.btn, C.btnSecondary]
            , AP.dataDismiss "modal"
            , E.onClick (\_ -> Just action)
            ]
            [ HH.text "Close" ]
          ]
        ]
      ]
    ]
