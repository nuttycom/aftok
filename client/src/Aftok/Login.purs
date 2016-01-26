module Aftok.Login where

import Prelude

import Control.Monad.Aff (Aff())

import Data.Maybe (Maybe(..))
import Data.Functor (($>))

import Halogen
import Halogen.HTML.Core (className)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P

import Network.HTTP.Affjax (AJAX(), affjax)
import Network.HTTP.Method 
import Network.HTTP.StatusCode 

type LoginState = { username :: String, password :: String }

initialState :: LoginState
initialState = { username: "", password: "" }

-- | The component query algebra.
data LoginAction a
  = SetUsername String a
  | SetPassword String a
  | Login String String a

-- | The effects used in the login component.
type LoginEffects eff = HalogenEffects (ajax :: AJAX | eff)

-- | The definition for the app's main UI component.
ui :: forall eff. Component LoginState LoginAction (Aff (LoginEffects eff))
ui = component render eval
  where

  render :: LoginState -> ComponentHTML LoginAction
  render st =
    H.div 
      [ P.classes (className <$> ["container"]) ]
      [ H.form 
        [ P.classes [ className "form-signin" ] ]
        [ H.h2    [ P.classes [ className "form-signin-heading" ]] [ H.text "Aftok Login" ] 
        , H.label [ P.for "inputUsername", P.classes [ className "sr-only" ]] [ H.text "username" ]
        , H.input
          [ P.inputType P.InputText
          , P.id_ "inputUsername"
          , P.classes [ className "form-control" ]
          , P.placeholder "username"
          , P.required true
          , P.autofocus true
          , P.value st.username
          , E.onValueInput (E.input SetUsername)
          ]
        , H.label [ P.for "inputPassword", P.classes [ className "sr-only" ]] [ H.text "username" ]
        , H.input
          [ P.inputType P.InputPassword
          , P.id_ "inputPassword"
          , P.classes [ className "form-control" ]
          , P.placeholder "password"
          , P.required true
          , P.value st.password
          , E.onValueInput (E.input SetPassword)
          ]
        , H.button
          [ P.classes (className <$> ["btn", "btn-primary"])
          , E.onClick (E.input_ (Login st.username st.password)) 
          ]
          [ H.text "Sign in" ]
        ]
      ]

  eval :: Natural LoginAction (ComponentDSL LoginState LoginAction (Aff (LoginEffects eff)))
  eval (SetUsername user next) = modify (_ { username = user }) $> next
  eval (SetPassword pass next) = modify (_ { password = pass }) $> next
  eval (Login user pass next) = do
    result <- liftAff' (login user pass)
    pure next

data LoginResponse 
  = OK 
  | Forbidden 
  | Error { status :: StatusCode, message :: String }

-- | Post credentials to the login service and interpret the response
login :: forall eff. String -> String -> Aff (ajax :: AJAX | eff) LoginResponse
login user pass = do
  result <- affjax $ { method: POST
                     , url: "/login"
                     , headers: []
                     , content: Nothing :: Maybe String
                     , username: Just user
                     , password: Just pass
                     }
  pure $ case result.status of 
    StatusCode 403 -> Forbidden
    StatusCode 200 -> OK
    other -> Error { status: other , message: result.response }


