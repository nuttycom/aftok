module Main where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))

import Effect (Effect)
import Effect.Aff (Aff)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

import Aftok.Login as Login
import Aftok.Timeline as Timeline
import Aftok.Project as Project
-- import Effect.Class.Console (info)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let --login = Login.mockCapability
      login = Login.apiCapability
      timeline = Timeline.mockCapability
      project = Project.apiCapability
      mainc = component login timeline project
  runUI mainc unit body

data MainState 
  = LoggedIn 
  | LoggedOut

data MainAction
  = Initialize
  | LoginComplete Login.LoginComplete

type Slots = 
  ( login :: Login.Slot Unit
  , timeline :: Timeline.Slot Unit
  )

_login = SProxy :: SProxy "login"
_timeline = SProxy :: SProxy "timeline"

component 
  :: forall query input output
  .  Login.Capability Aff
  -> Timeline.Capability Aff
  -> Project.Capability Aff
  -> H.Component HH.HTML query input output Aff
component loginCap tlCap pCap = H.mkComponent 
  { initialState
  , render 
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = eval 
      , initialize = Just Initialize 
      }
  } where
    initialState :: input -> MainState
    initialState _ = LoggedOut

    render :: MainState -> H.ComponentHTML MainAction Slots Aff
    render s = case s of
      LoggedOut -> 
        HH.div_ [ HH.slot _login unit (Login.component loginCap) unit (Just <<< LoginComplete) ]
      LoggedIn -> 
        HH.div_ [ HH.slot _timeline unit (Timeline.component tlCap { width: 600.0 }) unit absurd ]

    eval :: MainAction -> H.HalogenM MainState MainAction Slots output Aff Unit
    eval = case _ of
      Initialize -> do
        projects <- lift pCap.listProjects
        case projects of 
          Left err -> H.put LoggedOut
          Right _ -> H.put LoggedIn

      LoginComplete (Login.LoginComplete xs) -> 
        H.put LoggedIn
        
