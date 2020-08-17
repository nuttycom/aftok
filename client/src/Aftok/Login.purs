module Aftok.Login where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe(..))

import Effect.Aff (Aff)
import Affjax (request, defaultRequest, printError)
import Affjax.StatusCode (StatusCode(..))
import Affjax.RequestBody as RB

import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

import CSS (backgroundImage, url)

import Landkit.Card as Card

import Effect.Class.Console (log)

type LoginRequest = { username :: String, password :: String }

data LoginResponse
  = OK
  | Forbidden
  | Error { status :: Maybe StatusCode, message :: String }

type LoginState =
  { username :: String
  , password :: String
  , loginResponse :: Maybe LoginResponse
  }

data LoginAction
  = SetUsername String
  | SetPassword String
  | Login

data LoginComplete = LoginComplete

type Slot id = forall query. H.Slot query LoginComplete id

type Capability m = 
  { login :: String -> String -> m LoginResponse
  }

component 
  :: forall query input m
  .  Monad m 
  => Capability m 
  -> H.Component HH.HTML query input LoginComplete m
component caps = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = eval }
  } where

    initialState :: input -> LoginState
    initialState _ = { username: "", password: "", loginResponse: Nothing }


    render :: forall slots. LoginState -> H.ComponentHTML LoginAction slots m
    render st =
      Card.component $
        HH.div
          [ P.classes (ClassName <$> ["row", "no-gutters"]) ]
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
                , E.onSubmit (\_ -> Just Login)
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
                , case st.loginResponse of
                    Nothing ->
                      HH.div_ []
                    Just OK ->
                      HH.div
                      [ P.classes (ClassName <$> ["alert alert-warning"]) ]
                      [ HH.text "Login ok, but you should have been redirected. Why are you still here?" ]
                    Just Forbidden ->
                      HH.div
                      [ P.classes (ClassName <$> ["alert alert-danger"]) ]
                      [ HH.text "Login failed. Check your username and password." ]
                    Just (Error e) ->
                      HH.div
                      [ P.classes (ClassName <$> ["alert alert-danger"]) ]
                      [ HH.text ("Login failed: " <> e.message) ]

                , HH.button
                  [ P.classes (ClassName <$> ["btn", "btn-block", "btn-primary"])
                  ]
                  [ HH.text "Sign in" ]
                ]
              ]
            ]
          ]

    eval :: LoginAction -> H.HalogenM LoginState LoginAction () LoginComplete m Unit
    eval = case _ of
      SetUsername user -> H.modify_ (_ { username = user })
      SetPassword pass -> H.modify_ (_ { password = pass })
      Login -> do
        user <- H.gets (_.username)
        pass <- H.gets (_.password)
        response <- lift (caps.login user pass)
        H.modify_ (_ { loginResponse = Just response })
        case response of
          OK -> H.raise LoginComplete 
          _  -> pure unit

-- | Post credentials to the login service and interpret the response
login :: String -> String -> Aff LoginResponse
login user pass = do
  log "Sending login request to /api/login ..."
  result <- request $ 
    defaultRequest { method = Left POST
                   , url = "/api/login"
                   , content = Just <<< RB.Json <<< encodeJson $ { username: user, password : pass }
                   }
  case result of
       Left err -> log (printError err)
       Right r -> log ("Got status: " <> show r.status)
  pure $ case result of
    Left err -> Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> Forbidden
      StatusCode 200 -> OK
      other -> Error { status: Just other, message: r.statusText }

apiCapability :: Capability Aff
apiCapability = { login }

mockCapability :: forall m. Applicative m => Capability m
mockCapability = { login: \_ _ -> pure OK }
