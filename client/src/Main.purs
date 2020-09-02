module Main where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))

import Effect (Effect)

import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
-- import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P

import Aftok.Types (System, liveSystem)
import Aftok.Login as Login
import Aftok.Timeline as Timeline
import Aftok.Project as Project

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let --login = Login.mockCapability
      login = Login.apiCapability
      timeline = Timeline.apiCapability
      project = Project.apiCapability
      mainComponent = component liveSystem login timeline project
  runUI mainComponent unit body

data MainState 
  = Loading
  | LoggedOut
  | LoggedIn 

data MainAction
  = Initialize
  | LoginComplete Login.LoginComplete
  | Logout

type Slots = 
  ( login :: Login.Slot Unit
  , timeline :: Timeline.Slot Unit
  )

_login = SProxy :: SProxy "login"
_timeline = SProxy :: SProxy "timeline"

component 
  :: forall query input output m
  .  Monad m
  => System m
  -> Login.Capability m
  -> Timeline.Capability m
  -> Project.Capability m
  -> H.Component HH.HTML query input output m
component system loginCap tlCap pCap = H.mkComponent 
  { initialState
  , render 
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = eval 
      , initialize = Just Initialize 
      }
  } where
    initialState :: input -> MainState
    initialState _ = Loading

    render :: MainState -> H.ComponentHTML MainAction Slots m
    render = case _ of
      Loading ->
        HH.div [P.classes [ClassName "loader"]] [HH.text "Loading..."]

      LoggedOut -> 
        HH.div_ 
          [ HH.slot _login unit (Login.component system loginCap) unit (Just <<< LoginComplete) ]

      LoggedIn -> 
        withNavBar $ HH.div_ 
          [ HH.slot _timeline unit (Timeline.component system tlCap pCap) unit absurd ]

    eval :: MainAction -> H.HalogenM MainState MainAction Slots output m Unit
    eval = case _ of
      Initialize -> do
        result <- lift loginCap.checkLogin
        case result of 
          Login.Forbidden -> H.put LoggedOut
          _ -> H.put LoggedIn

      LoginComplete (Login.LoginComplete _) -> 
        H.put LoggedIn

      Logout -> do
        lift loginCap.logout
        H.put LoggedOut

withNavBar :: forall s m. H.ComponentHTML MainAction s m -> H.ComponentHTML MainAction s m
withNavBar body =
  HH.div_ 
    [HH.nav
      [P.classes (ClassName <$> ["navbar", "navbar-expand-lg", "navbar-light", "bg-white"])]
      [HH.div
        [P.classes (ClassName <$> ["container-fluid"])]
        [brand, logout]
      ]
    ,body 
    ]      

brand :: forall a s m. H.ComponentHTML a s m
brand = HH.div
  [P.classes (ClassName <$> ["navbar-brand"])]
  [HH.h4
    [P.classes (ClassName <$> ["font-weight-bold"])]
    [HH.text "Aftok"]
  ]

logout :: forall s m. H.ComponentHTML MainAction s m
logout = HH.button
  [P.classes (ClassName <$> ["btn", "navbar-btn", "btn-sm", "btn-primary", "lift", "ml-auto"])
  ,E.onClick \_ -> Just Logout 
  ]
  [HH.text "Logout"]

