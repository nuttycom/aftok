module Main where

import Prelude

import Data.Int (toNumber)
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
-- import Effect.Class.Console (info)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let --login = Login.mockCapability
      login = Login.apiCapability
      timeline = Timeline.mockCapability
  let c = component login timeline
  runUI c unit body

data MainState 
  = LoggedIn 
  | LoggedOut

data MainAction
  = LoginComplete Login.LoginComplete

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
  -> H.Component HH.HTML query input output Aff
component loginCap tlCap = H.mkComponent 
  { initialState
  , render 
  , eval: H.mkEval $ H.defaultEval { handleAction = eval }
  } where

    initialState :: input -> MainState
    initialState _ = LoggedOut

    render :: MainState -> H.ComponentHTML MainAction Slots Aff
    render s = case s of
      LoggedOut -> 
        HH.div_ [ HH.slot _login unit (Login.component loginCap) unit (Just <<< LoginComplete) ]
      LoggedIn -> 
        HH.div_ [ HH.slot _timeline unit (Timeline.component tlCap { width: toNumber 600 }) unit absurd ]

    eval :: MainAction -> H.HalogenM MainState MainAction Slots output Aff Unit
    eval = case _ of
      LoginComplete _ -> H.put LoggedIn
