module Aftok.Login where

import Prelude (type (~>), Void, bind, pure, ($), ($>), (<$>), const)


import Control.Monad.Aff (Aff())

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff (HalogenEffects)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

import Network.HTTP.Affjax (AJAX(), affjax)
import Data.HTTP.Method (Method(..))
import Network.HTTP.StatusCode (StatusCode(..))

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
ui :: forall eff. H.Component HH.HTML LoginAction LoginState Void (Aff (LoginEffects eff))
ui = H.component 
  { initialState: const initialState
  , render 
  , eval
  , receiver: const Nothing
  } where

  render :: LoginState -> H.ComponentHTML LoginAction
  render st =
    HH.div 
      [ P.classes (ClassName <$> ["container"]) ]
      [ HH.form 
        [ P.classes [ ClassName "form-signin" ] ]
        [ HH.h2    [ P.classes [ ClassName "form-signin-heading" ]] [ HH.text "Aftok Login" ] 
        , HH.label [ P.for "inputUsername", P.classes [ ClassName "sr-only" ]] [ HH.text "username" ]
        , HH.input
          [ P.type_ P.InputText
          , P.id_ "inputUsername"
          , P.classes [ ClassName "form-control" ]
          , P.placeholder "username"
          , P.required true
          , P.autofocus true
          , P.value st.username
          , E.onValueInput (E.input SetUsername)
          ]
        , HH.label [ P.for "inputPassword", P.classes [ ClassName "sr-only" ]] [ HH.text "username" ]
        , HH.input
          [ P.type_ P.InputPassword
          , P.id_ "inputPassword"
          , P.classes [ ClassName "form-control" ]
          , P.placeholder "password"
          , P.required true
          , P.value st.password
          , E.onValueInput (E.input SetPassword)
          ]
        , HH.button
          [ P.classes (ClassName <$> ["btn", "btn-primary"])
          , E.onClick (E.input_ (Login st.username st.password)) 
          ]
          [ HH.text "Sign in" ]
        ]
      ]

  eval :: LoginAction ~> H.ComponentDSL LoginState LoginAction Void (Aff (LoginEffects eff))
  eval (SetUsername user next) = H.modify (_ { username = user }) $> next
  eval (SetPassword pass next) = H.modify (_ { password = pass }) $> next
  eval (Login user pass next) = do
    result <- H.liftAff (login user pass)
    pure next

data LoginResponse 
  = OK 
  | Forbidden 
  | Error { status :: StatusCode, message :: String }

-- | Post credentials to the login service and interpret the response
login :: forall eff. String -> String -> Aff (ajax :: AJAX | eff) LoginResponse
login user pass = do
  result <- affjax $ { method: Left POST
                     , url: "/login"
                     , headers: []
                     , content: Nothing :: Maybe String
                     , username: Just user
                     , password: Just pass
                     , withCredentials: true
                     }
  pure $ case result.status of 
    StatusCode 403 -> Forbidden
    StatusCode 200 -> OK
    other -> Error { status: other , message: result.response }


