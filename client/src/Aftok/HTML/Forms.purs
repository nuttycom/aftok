module Aftok.HTML.Forms where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.HTML.Core (AttrName(..), ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import CSS.Display (display, flex)
import CSS.Flexbox (flexFlow, row, nowrap)
import Aftok.Api.Types (CommsType(..))

type CommsState r = 
  { channel :: CommsType
  , email :: Maybe String
  , zaddr :: Maybe String
  | r }

type SetCommsType action = CommsType -> action
type SetEmail action = String -> action
type SetZaddr action = String -> action

commsSwitch :: forall i a. SetCommsType a -> CommsType -> HH.HTML i a
commsSwitch setCommsType rt =
  HH.div
    [ P.classes (ClassName <$> [ "form-group", "mb-3" ]) ]
    [ HH.label
        [ P.for "commsSwitch" ]
        [ HH.text "Choose a communications method" ]
    , HH.div
        [ P.classes (ClassName <$> [ "form-group", "mb-3" ])
        , CSS.style do
            display flex
            flexFlow row nowrap
        ]
        [ HH.span
            [ P.classes (ClassName <$> [ if rt == EmailComms then "text-success" else "text-muted" ]) ]
            $ [ HH.text "Email" ]
        , HH.div
            [ P.classes (ClassName <$> [ "custom-control", "custom-switch", "custom-switch-light", "mx-3" ]) ]
            [ HH.input
                [ P.type_ P.InputCheckbox
                , P.classes (ClassName <$> [ "custom-control-input" ])
                , P.id_ "commsSwitch"
                , P.checked (rt == ZcashComms)
                , E.onChecked (\b -> Just <<< setCommsType $ if b then ZcashComms else EmailComms)
                ]
            , HH.label [ P.classes (ClassName <$> [ "custom-control-label" ]), P.for "commsSwitch" ] []
            ]
        , HH.span
            [ P.classes (ClassName <$> [ if rt == ZcashComms then "text-success" else "text-muted" ]) ]
            [ HH.text "Z-Address" ]
        ]
    ]

type CommsErrors i a = CommsType -> Array (HH.HTML i a)

commsField :: 
  forall i a r. 
  SetEmail a -> 
  SetZaddr a -> 
  CommsState r -> 
  CommsErrors i a -> 
  HH.HTML i a
commsField setEmail setZAddr st errs = case st.channel of
  EmailComms ->
    HH.div_ $ 
      [ HH.label [ P.for "email" ] [ HH.text "Email Address" ]
      , HH.input
          [ P.type_ P.InputEmail
          , P.classes (ClassName <$> [ "form-control" ])
          , P.id_ "email"
          , P.placeholder "name@address.com"
          , P.value (fromMaybe "" st.email)
          , E.onValueInput (Just <<< setEmail)
          ]
      ]
      <> errs EmailComms 
  ZcashComms ->
    HH.div_ $
      [ HH.label
          [ P.for "zaddr" ]
          [ HH.text "Zcash Shielded Address"
          , HH.a
              [ P.attr (AttrName "data-toggle") "modal"
              , P.href "#modalAboutZAddr"
              ]
              [ HH.img [ P.src "/assets/img/icons/duotone-icons/Code/Info-circle.svg" ]
              ]
          ]
      , HH.input
          [ P.type_ P.InputText
          , P.classes (ClassName <$> [ "form-control" ])
          , P.id_ "email"
          , P.placeholder "Enter a Zcash shielded address"
          , P.value (fromMaybe "" st.zaddr)
          , E.onValueInput (Just <<< setZAddr)
          ]
      ]
      <> errs ZcashComms
