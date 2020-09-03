module Aftok.Signup where

import Prelude

-- import Control.Monad.Trans.Class (lift)

import Data.Maybe (Maybe(..), fromMaybe)

-- import Affjax (post, get, printError)
import Affjax.StatusCode (StatusCode)
-- import Affjax.RequestBody as RB
-- import Affjax.ResponseFormat as RF

import Halogen as H
import Halogen.HTML.Core (AttrName(..), ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Web.UIEvent.MouseEvent as ME
import Web.Event.Event as WE
import Halogen.HTML.Properties as P

-- import CSS (backgroundImage, url)
import CSS.Display (display, flex)
import CSS.Flexbox (flexFlow, row, nowrap)

import Aftok.Types (System)

data SignupResponse
  = OK
  | Error { status :: Maybe StatusCode, message :: String }

data RecoveryType
  = RecoveryEmail
  | RecoveryZAddr

derive instance recoveryTypeEq :: Eq RecoveryType

type SignupState =
  { username :: Maybe String
  , password :: Maybe String
  , passwordConfirm :: Maybe String
  , recoveryType :: RecoveryType
  , recoveryEmail :: Maybe String
  , recoveryZAddr :: Maybe String
  , loginResponse :: Maybe SignupResponse
  }

data SignupAction
  = SetUsername String
  | SetPassword String
  | ConfirmPassword String
  | SetRecoveryType RecoveryType
  | SetRecoveryEmail String
  | SetRecoveryZAddr String
  | Signin ME.MouseEvent
  | Signup WE.Event

data SignupResult 
  = SignupComplete { username :: String }

type Slot id = forall query. H.Slot query SignupResult id

type Capability m = 
  { signup :: String -> String -> m SignupResponse
  }

type Config =
  { recaptchaKey :: String
  }

component 
  :: forall query input m
  .  Monad m
  => System m
  -> Capability m 
  -> Config
  -> H.Component HH.HTML query input SignupResult m
component system caps conf = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = eval }
  } where

    initialState :: input -> SignupState
    initialState _ = 
      { username: Nothing
      , password: Nothing
      , passwordConfirm: Nothing
      , recoveryType: RecoveryEmail
      , recoveryEmail: Nothing
      , recoveryZAddr: Nothing
      , loginResponse: Nothing
      }

    render :: forall slots. SignupState -> H.ComponentHTML SignupAction slots m
    render st = 
      HH.section
        [ P.classes (ClassName <$> ["section-border", "border-primary"]) ]
        [ HH.div
          [ P.classes (ClassName <$> ["container", "d-flex", "flex-column"]) ]
          [ HH.div
            [ P.classes (ClassName <$> ["align-items-center", "pt-6"]) ]
            [ HH.h1 
              [ P.classes (ClassName <$> ["mb-0", "font-weight-bold", "text-center"]) ]
              [ HH.text "Sign up" ]
            , HH.p 
              [ P.classes (ClassName <$> ["text-center", "text-muted", "col-md-5", "mx-auto"]) ]
              [ HH.text "You can use either an email address or zcash payment address for account recovery." ]
            ]  
          , HH.div
            [ P.classes (ClassName <$> ["row", "align-items-center", "justify-content-center", "no-gutters"]) ]
            [ HH.div
              [ P.classes (ClassName <$> ["col-12", "col-lg-4", "py-8", "py-md-0"]) ]
              [ HH.form 
                [ P.classes (ClassName <$> ["mb-6"]) ]
                [ HH.div
                  [ P.classes (ClassName <$> ["form-group"]) ]
                  [ HH.label [ P.for "username" ] [ HH.text "Username" ]
                  , HH.input 
                    [ P.type_ P.InputText
                    , P.classes (ClassName <$> ["form-control"])
                    , P.id_ "username"
                    , P.placeholder "Choose a handle (username)"
                    , P.required true
                    , P.autofocus true
                    , P.value (fromMaybe "" st.username)
                    , E.onValueInput (Just <<< SetUsername)
                    ]
                  ]
                , HH.div
                  [ P.classes (ClassName <$> ["form-group"]) ]
                  [ HH.label [ P.for "password" ] [ HH.text "Password" ]
                  , HH.input 
                    [ P.type_ P.InputPassword
                    , P.classes (ClassName <$> ["form-control"])
                    , P.id_ "password"
                    , P.placeholder "Enter a unique password"
                    , P.required true
                    , P.value (fromMaybe "" st.password)
                    , E.onValueInput (Just <<< SetPassword)
                    ]
                  , HH.input 
                    [ P.type_ P.InputPassword
                    , P.classes (ClassName <$> ["form-control"])
                    , P.id_ "password"
                    , P.placeholder "Enter a unique password"
                    , P.required true
                    , P.value (fromMaybe "" st.passwordConfirm)
                    , E.onValueInput (Just <<< ConfirmPassword)
                    ]
                  ]
                , recoverySwitch st.recoveryType
                , recoveryField st
                , HH.div
                  [ P.classes (ClassName <$> ["form-group", "mb-3"]) ]
                  [ HH.div
                    [ P.classes (ClassName <$> ["form-group", "mb-3"]) 
                    , P.attr (AttrName "data-sitekey") conf.recaptchaKey
                    ] []
                  ]
                , HH.button
                  [ P.classes (ClassName <$> ["btn", "btn-block", "btn-primary"]) ]
                  [ HH.text "Sign up" ]
                ]
              , HH.p  
                [ P.classes (ClassName <$> ["mb-0", "font-size-sm", "text-center", "text-muted"]) ]
                [ HH.text "Alreay have an account? " 
                , HH.a 
                  [ P.href "#", E.onClick (Just <<< Signin) ]
                  [ HH.text "Sign in" ]
                ]
              ]
            ]
          ]
        ]

    eval :: SignupAction -> H.HalogenM SignupState SignupAction () SignupResult m Unit
    eval = case _ of
      SetUsername user -> H.modify_ (_ { username = Just user })
      SetPassword pass -> H.modify_ (_ { password = Just pass })
      SetRecoveryType t -> H.modify_ (_ { recoveryType = t })
      ConfirmPassword pass -> H.modify_ (_ { passwordConfirm = Just pass })
      _ -> pure unit


recoverySwitch :: forall i. RecoveryType -> HH.HTML i SignupAction
recoverySwitch rt = 
  HH.div
    [ P.classes (ClassName <$> ["form-group", "mb-3"]) ]
    [ HH.label 
      [ P.for "recoverySwitch" ] 
      [ HH.text "Choose a recovery method" ]
    , HH.div
      [ P.classes (ClassName <$> ["form-group", "mb-3"]) 
      , CSS.style do
          display flex
          flexFlow row nowrap
      ]
      [ HH.span 
        [ P.classes (ClassName <$> [ if rt == RecoveryEmail then "text-success" else "text-muted"]) ] 
        [ HH.text "Email" ]
      , HH.div
        [ P.classes (ClassName <$> ["custom-control", "custom-switch", "custom-switch-light", "mx-3"]) ]
        [ HH.input 
          [ P.type_ P.InputCheckbox
          , P.classes (ClassName <$> ["custom-control-input"])
          , P.id_ "recoverySwitch"
          , E.onChecked (\b -> Just <<< SetRecoveryType $ if b then RecoveryZAddr else RecoveryEmail)
          ]
        , HH.label [ P.classes (ClassName <$> [ "custom-control-label" ]), P.for "recoverySwitch" ] []
        ]
      , HH.span 
        [ P.classes (ClassName <$> [if rt == RecoveryZAddr then "text-success" else "text-muted"]) ] 
        [ HH.text "Z-Address" ]
      ]
    ]

recoveryField :: forall i. SignupState -> HH.HTML i SignupAction
recoveryField st = case st.recoveryType of
  RecoveryEmail -> 
    HH.div 
      [ P.id_ "recoveryEmail" ]
      [ HH.label [ P.for "email" ] [ HH.text "Email Address" ]
      , HH.input 
        [ P.type_ P.InputEmail
        , P.classes (ClassName <$> ["form-control"])
        , P.id_ "email"
        , P.placeholder "name@address.com"
        , P.value (fromMaybe "" st.recoveryEmail)
        , E.onValueInput (Just <<< SetRecoveryEmail)
        ]
      ]
  RecoveryZAddr ->
    HH.div 
      [ P.id_ "recoveryZAddr" ]
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
        , P.classes (ClassName <$> ["form-control"])
        , P.id_ "email"
        , P.placeholder "Enter a Zcash shielded address"
        , P.value (fromMaybe "" st.recoveryZAddr)
        , E.onValueInput (Just <<< SetRecoveryZAddr)
        ]
      ]

mockCapability :: forall m. Applicative m => Capability m
mockCapability = 
  { signup: \_ _ -> pure OK 
  }
