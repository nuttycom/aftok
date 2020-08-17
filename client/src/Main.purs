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
  --let c = component Login.mockCapability
  let c = component Login.apiCapability
  runUI c unit body

data MainState 
  = LoggedIn 
  | LoggedOut

data MainAction
  = LoginComplete Login.LoginComplete

type Slots = 
  ( login :: Login.Slot Int
  , timeline :: Timeline.Slot Int
  )

_login = SProxy :: SProxy "login"
_timeline = SProxy :: SProxy "timeline"

component 
  :: forall query input output
  .  Login.Capability Aff
  -> H.Component HH.HTML query input output Aff
component loginCap = H.mkComponent 
  { initialState
  , render 
  , eval: H.mkEval $ H.defaultEval { handleAction = eval }
  } where

    initialState :: input -> MainState
    initialState _ = LoggedOut

    render :: MainState -> H.ComponentHTML MainAction Slots Aff
    render s = case s of
      LoggedOut -> 
        HH.div_ [ HH.slot _login 0 (Login.component loginCap) unit (Just <<< LoginComplete) ]
      LoggedIn -> 
        HH.div_ [ HH.slot _timeline 1 (Timeline.component { width: toNumber 600 }) unit absurd ]

    eval :: MainAction -> H.HalogenM MainState MainAction Slots output Aff Unit
    eval = case _ of
      LoginComplete _ -> H.put LoggedIn
