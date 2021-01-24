module Aftok.Login where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Maybe (Maybe(..))

import Effect.Aff (Aff)

import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Web.Event.Event as WE
import Halogen.HTML.Properties as P

import CSS (backgroundImage, url)

import Landkit.Card as Card

import Aftok.Types (System)
import Aftok.Api.Account (LoginResponse(..), login, checkLogin, logout)

data LoginError
  = Forbidden 
  | ServerError

type LoginState =
  { username :: String
  , password :: String
  , loginError :: Maybe LoginError
  }

data LoginAction
  = SetUsername String
  | SetPassword String
  | Login WE.Event

data LoginResult 
  = LoginComplete { username :: String }

type Slot id = forall query. H.Slot query LoginResult id

type Capability m = 
  { login :: String -> String -> m LoginResponse
  , checkLogin :: m LoginResponse
  , logout :: m Unit
  }

component 
  :: forall query input m
  .  Monad m
  => System m
  -> Capability m 
  -> H.Component HH.HTML query input LoginResult m
component system caps = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = eval }
  } where

    initialState :: input -> LoginState
    initialState _ = { username: "", password: "", loginError: Nothing }

    render :: forall slots. LoginState -> H.ComponentHTML LoginAction slots m
    render st =
      Card.component $
        HH.div
          [ P.classes (ClassName <$> ["row", "no-gutters", "container"]) ]
          [ HH.div
            [ P.classes (ClassName <$> ["col-12", "col-md-6", "bg-cover", "card-img-left"])
            , CSS.style $ backgroundImage (url "/assets/img/photos/latch.jpg")
            ]
            [
              HH.div
                [ P.classes (ClassName <$> ["shape", "shape-right", "shape-fluid-y", "svg-shim", "text-white", "d-none", "d-md-block"])]
                [ HH.img [ P.src "/assets/img/shapes/curves/curve-4.svg" ]]
            ]
          , HH.div
            [ P.classes (ClassName <$> ["col-12", "col-md-6"]) ]
            [ HH.div
              [ P.classes (ClassName <$> ["card-body"]) ]
              [ HH.h2
                [ P.classes (ClassName <$> ["mb-0", "font-weight-bold", "text-center"])]
                [ HH.text "Sign In"]

              , HH.form
                [ P.classes (ClassName <$> ["mb-6"])
                , E.onSubmit (Just <<< Login)
                ]
                [ HH.div
                  [ P.classes (ClassName <$> ["form-group"])]
                  [ HH.label
                    [ P.classes (ClassName <$> ["sr-only"])
                    , P.for "modalSigninHorizontalUsername"
                    ]
                    [ HH.text "Username" ]
                  , HH.input
                    [ P.type_ P.InputText
                    , P.classes (ClassName <$> ["form-control"])
                    , P.id_ "modalSigninHorizontalUsername"
                    , P.placeholder "Username"
                    , P.required true
                    , P.autofocus true
                    , P.value st.username
                    , E.onValueInput (Just <<< SetUsername)
                    ]
                  ]
                , HH.div
                  [ P.classes (ClassName <$> ["form-group"])]
                  [ HH.label
                    [ P.classes (ClassName <$> ["sr-only"])
                    , P.for "modalSigninHorizontalPassword"
                    ]
                    [ HH.text "Password" ]
                  , HH.input
                    [ P.type_ P.InputPassword
                    , P.classes (ClassName <$> ["form-control"])
                    , P.id_ "modalSigninHorizontalPassword"
                    , P.placeholder "Password"
                    , P.required true
                    , P.value st.password
                    , E.onValueInput (Just <<< SetPassword)
                    ]
                  ]
                , case st.loginError of
                    Nothing ->
                      HH.div_ []
                    Just err -> 
                      let message = case err of
                            Forbidden -> "Login failed. Check your username and password."
                            ServerError -> "Login failed due to an internal error. Please contact support."
                       in HH.div
                            [ P.classes (ClassName <$> ["alert alert-danger"]) ]
                            [ HH.text message ]

                , HH.button
                  [ P.classes (ClassName <$> ["btn", "btn-block", "btn-primary"]) ]
                  [ HH.text "Sign in" ]
                ]
              ]
              , HH.p  
                [ P.classes (ClassName <$> ["mb-0", "font-size-sm", "text-center", "text-muted"]) ]
                [ HH.text "Need an account? " 
                , HH.a 
                  [ P.href "#signup" ]
                  [ HH.text "Sign up" ]
                ]
            ]
          ]

    eval :: LoginAction -> H.HalogenM LoginState LoginAction () LoginResult m Unit
    eval = case _ of
      SetUsername user -> H.modify_ (_ { username = user })
      SetPassword pass -> H.modify_ (_ { password = pass })
      Login ev -> do
        lift $ system.preventDefault ev
        user <- H.gets (_.username)
        pass <- H.gets (_.password)
        response <- lift (caps.login user pass)
        case response of
          LoginOK        -> H.raise (LoginComplete { username: user })
          LoginForbidden -> H.modify_ (_ { loginError = Just Forbidden })
          LoginError _   -> H.modify_ (_ { loginError = Just ServerError })

apiCapability :: Capability Aff
apiCapability = { login, checkLogin, logout }

mockCapability :: forall m. Applicative m => Capability m
mockCapability = 
  { login: \_ _ -> pure LoginOK 
  , checkLogin: pure LoginOK
  , logout: pure unit
  }
