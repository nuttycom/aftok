module Aftok.Login where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import Effect.Aff (Aff)
import Effect.Class as EC
import Affjax (post, get, printError)
import Affjax.StatusCode (StatusCode(..))
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF

import Halogen as H
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Web.Event.Event as WE
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
  | Login WE.Event

newtype LoginComplete = LoginComplete { username :: String }

type Slot id = forall query. H.Slot query LoginComplete id

type Capability m = 
  { login :: String -> String -> m LoginResponse
  , checkLogin :: m LoginResponse
  , logout :: m Unit
  }

component 
  :: forall query input m
  .  EC.MonadEffect m 
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
      Login ev -> do
        EC.liftEffect $ WE.preventDefault ev
        user <- H.gets (_.username)
        pass <- H.gets (_.password)
        response <- lift (caps.login user pass)
        H.modify_ (_ { loginResponse = Just response })
        case response of
          OK -> H.raise (LoginComplete { username: user })
          _  -> pure unit

-- | Post credentials to the login service and interpret the response
login :: String -> String -> Aff LoginResponse
login user pass = do
  log "Sending login request to /api/login ..."
  result <- post RF.ignore "/api/login" (Just <<< RB.Json <<< encodeJson $ { username: user, password : pass })
  case result of
       Left err -> log ("Login failed: " <> printError err)
       Right r  -> log ("Login status: " <> show r.status)
  pure $ case result of
    Left err -> Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> Forbidden
      StatusCode 200 -> OK
      other -> Error { status: Just other, message: r.statusText }

checkLogin :: Aff LoginResponse
checkLogin = do
  log "Sending login check to /api/login/check ..."
  result <- get RF.ignore "/api/login/check"
  case result of
    Left err -> do
      log ("Login failed: " <> printError err)
      pure $ Error { status: Nothing, message: printError err }
    Right r -> do
      log ("Login status: " <> show r.status)
      pure $ case r.status of
        StatusCode 200 -> OK
        StatusCode _   -> Forbidden

logout :: Aff Unit
logout = do
  log "Logging out on server with  /api/logout ..."
  result <- get RF.ignore "/api/logout"
  case result of
    Left err -> log ("Logout failed: " <> printError err)
    Right r ->  log ("Logout status: " <> show r.status)

apiCapability :: Capability Aff
apiCapability = { login, checkLogin, logout }

mockCapability :: forall m. Applicative m => Capability m
mockCapability = 
  { login: \_ _ -> pure OK 
  , checkLogin: pure OK
  , logout: pure unit
  }
