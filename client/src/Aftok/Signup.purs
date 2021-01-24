module Aftok.Signup where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Foldable (any)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), note)
import Data.Validation.Semigroup (V(..), toEither, andThen, invalid)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
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
import Aftok.Api.Account as Acc
import Aftok.Api.Account (SignupRequest, SignupResponse, signupRequest)
import Aftok.Api.Recaptcha (getRecaptchaResponse)

data SignupError
  = UsernameRequired
  | UsernameTaken
  | PasswordRequired
  | ConfirmRequired
  | PasswordMismatch
  | EmailRequired
  | ZAddrRequired
  | ZAddrInvalid
  | CaptchaError
  | APIError { status :: Maybe StatusCode, message :: String }

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
  , signupErrors :: Array SignupError
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
  = SignupComplete String 
  | SigninNav

type Slot id = forall query. H.Slot query SignupResult id

type Capability m = 
  { checkUsername :: String -> m Acc.UsernameCheckResponse
  , checkZAddr :: String -> m Acc.ZAddrCheckResponse
  , signup :: SignupRequest -> m SignupResponse
  , getRecaptchaResponse :: Maybe String -> m (Maybe String)
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
      , signupErrors: []
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
                [ P.classes (ClassName <$> ["mb-6"]) 
                , E.onSubmit (Just <<< Signup)
                ]
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
                    , P.id_ "passwordConfirm"
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
                    [ P.classes (ClassName <$> ["g-recaptcha", "mx-auto"]) 
                    , P.attr (AttrName "data-sitekey") conf.recaptchaKey
                    ] []
                  ]
                , HH.button
                  [ P.classes (ClassName <$> ["btn", "btn-block", "btn-primary"]) ]
                  [ HH.text "Sign up" ]
                ]
              , HH.p  
                [ P.classes (ClassName <$> ["mb-0", "font-size-sm", "text-center", "text-muted"]) ]
                [ HH.text "Already have an account? " 
                , HH.a 
                  [ P.href "#login" ]
                  [ HH.text "Sign in" ]
                ]
              ]
            ]
          ]
        ]

    eval :: SignupAction -> H.HalogenM SignupState SignupAction () SignupResult m Unit
    eval = case _ of
      SetUsername user -> do
        ures <- lift $ caps.checkUsername user
        H.modify_ (_ { username = Just user })
        case ures of
          Acc.UsernameCheckOK    -> pure unit
          Acc.UsernameCheckTaken -> H.modify_ (_ { signupErrors = [UsernameTaken] })

      SetPassword pass -> do
        H.modify_ (_ { password = Just pass })
        confirm <- H.gets (_.passwordConfirm)
        when (any (notEq pass) confirm) (H.modify_ (_ { signupErrors = [PasswordMismatch] }))
           
      ConfirmPassword confirm -> do
        H.modify_ (_ { passwordConfirm = Just confirm })
        password <- H.gets (_.password)
        when (any (notEq confirm) password) (H.modify_ (_ { signupErrors = [PasswordMismatch] }))

      SetRecoveryType t -> H.modify_ (_ { recoveryType = t })
      SetRecoveryEmail email -> H.modify_ (_ { recoveryEmail = Just email })
      SetRecoveryZAddr addr -> do
        lift $ system.log "Switching to signin..."
        zres <- lift $ caps.checkZAddr addr
        H.modify_ (_ { recoveryZAddr = Just addr })
        case zres of
          Acc.ZAddrCheckValid -> pure unit
          Acc.ZAddrCheckInvalid -> H.modify_ (_ { signupErrors = [ZAddrInvalid] })

      Signin ev -> do 
        lift $ system.log "Switching to signin..."
        lift $ system.preventDefault (ME.toEvent ev)
        H.raise SigninNav

      Signup ev -> do
        lift $ system.preventDefault ev
        recType <- H.gets (_.recoveryType)
        usernameV <- V <<< note [UsernameRequired] <$> H.gets (_.username)
        pwdFormV  <- V <<< note [PasswordRequired] <$> H.gets (_.password)
        pwdConfV  <- V <<< note [ConfirmRequired ] <$> H.gets (_.passwordConfirm)
        recoveryType <- H.gets (_.recoveryType)
        recoveryV <- case recoveryType of
          RecoveryEmail -> 
            V <<< note [EmailRequired] <<< map Acc.RecoverByEmail <$> H.gets (_.recoveryEmail)
          RecoveryZAddr -> 
            V <<< note [ZAddrRequired] <<< map Acc.RecoverByZAddr <$> H.gets (_.recoveryZAddr)
        recapV <- lift $ V <<< note [CaptchaError] <$> caps.getRecaptchaResponse Nothing
        lift $ system.log "Sending signup request..."
        let reqV :: V (Array SignupError) Acc.SignupRequest
            reqV = signupRequest <$> usernameV
                                 <*> ((eq <$> pwdFormV <*> pwdConfV) `andThen` 
                                      (if _ then pwdFormV else invalid [PasswordMismatch]))
                                 <*> recoveryV
                                 <*> recapV
        case toEither reqV of
          Left errors -> do
            lift $ system.log "Got signup HTTP error."
            H.modify_ (_ { signupErrors = errors })
          Right req -> do
            response <- lift (caps.signup req)
            lift <<< system.log $ "Got signup response " <> show response
            case response of
              Acc.SignupOK         -> H.raise (SignupComplete $ req.username)
              Acc.CaptchaInvalid   -> H.modify_ (_ { signupErrors = [CaptchaError] })
              Acc.ZAddrInvalid     -> H.modify_ (_ { signupErrors = [ZAddrInvalid] })
              Acc.UsernameTaken    -> H.modify_ (_ { signupErrors = [UsernameTaken] })
              Acc.ServiceError c m -> H.modify_ (_ { signupErrors = [APIError { status: c, message: m }]})

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

apiCapability :: Capability Aff
apiCapability = 
  { checkUsername: \_ -> pure Acc.UsernameCheckOK
  , checkZAddr: Acc.checkZAddr
  , signup: Acc.signup
  , getRecaptchaResponse: liftEffect <<< getRecaptchaResponse 
  }

mockCapability :: Capability Aff
mockCapability = 
  { checkUsername: \_ -> pure Acc.UsernameCheckOK
  , checkZAddr: \_ -> pure Acc.ZAddrCheckValid
  , signup: \_ -> pure Acc.SignupOK
  , getRecaptchaResponse: liftEffect <<< getRecaptchaResponse 
  }
