module Aftok.Modals where

import Prelude ((<>), negate)
import DOM.HTML.Indexed.ButtonType (ButtonType(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as P
import Halogen.HTML.Properties.ARIA as ARIA

import Aftok.HTML.Classes as C
import Aftok.HTML.Properties as AP

modalButton :: forall w i. String -> String -> HH.HTML w i
modalButton target text =
  HH.button
    [ P.classes [ C.btn, C.btnPrimary ]
    , AP.dataToggle "modal"
    , AP.dataTarget ("#" <> target)
    , P.type_ ButtonButton
    ]
    [ HH.text text ]

modal :: forall w i. String -> String -> Array (HH.HTML w i) -> HH.HTML w i
modal modalId title contents =
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
            [ HH.span [ARIA.hidden "true"] [HH.text "&times;"]]
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
            ]
            [ HH.text "Save changes"]
          ]
        ]
      ]
    ]
