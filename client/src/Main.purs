module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Data.Either (Either(..))
--import Data.Foldable (foldMap)
import Data.Foreign.Class (readProp)
import Data.Functor (($>))
--import Data.Maybe (Maybe(..))

import Halogen
import Halogen.Util (appendToBody, onLoad)
import Halogen.HTML.Core (className)
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Properties.Indexed as P

import Network.HTTP.Affjax (AJAX(), post)

-- | The state of the component.
type LoginState = { username :: String, password :: String }

initialState :: LoginState
initialState = { username: "", password: "" }

-- | The component query algebra.
data LoginAction a
  = SetUsername String a
  | SetPassword String a
  | Login String String a

-- | The effects used in the app.
type AppEffects eff = HalogenEffects (ajax :: AJAX | eff)

-- | The definition for the app's main UI component.
ui :: forall eff. Component LoginState LoginAction (Aff (AppEffects eff))
ui = component render eval
  where

  render :: LoginState -> ComponentHTML LoginAction
  render st =
    H.div 
      [ P.classes (className <$> ["panel", "panel-primary"]) ]
      [ H.div 
        [ P.classes [ className "panel-heading" ] ]
        [ H.h3 [ P.classes [ className "panel-title" ]] [ H.text "Aftok Login" ] ]
      , H.div 
        [ P.classes [ className "panel-body" ] ]
        [
          H.h2_
            [ H.text "username:" ]
        , H.p_
            [ H.input
                [ P.value st.username
                , P.inputType P.InputText
                , E.onValueInput (E.input SetUsername)
                ]
            ]
        , H.h2_
            [ H.text "password:" ]
        , H.p_
            [ H.input
                [ P.value st.password
                , P.inputType P.InputPassword
                , E.onValueInput (E.input SetPassword)
                ]
            ]
        , H.p_
            [ H.button
                [ P.classes (className <$> ["btn", "btn-primary"])
                , E.onClick (E.input_ (Login st.username st.password)) 
                ]
                [ H.text "Login" ]
            ]
        ]
      ]

  eval :: Natural LoginAction (ComponentDSL LoginState LoginAction (Aff (AppEffects eff)))
  eval (SetUsername user next) = modify (_ { username = user }) $> next
  eval (SetPassword pass next) = modify (_ { password = pass }) $> next
  eval (Login user pass next) = do
    _ <- liftAff' (fetchJS user pass)
    pure next

-- | Post some PureScript code to the trypurescript API and fetch the JS result.
fetchJS :: forall eff. String -> String -> Aff (ajax :: AJAX | eff) String
fetchJS user pass = do
  result <- post "https://aftok.com/login" user
  let response = result.response
  return case readProp "js" response <|> readProp "error" response of
    Right js -> js
    Left _ -> "Invalid response"

-- | Run the app.
main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui initialState
  onLoad $ appendToBody app.node
