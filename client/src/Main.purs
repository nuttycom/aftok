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
import Aftok.Api.Account as Acc
import Aftok.Signup as Signup
import Aftok.Timeline as Timeline
import Aftok.Project as Project

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let --login = Login.mockCapability
      login = Login.apiCapability
      signup = Signup.apiCapability
      timeline = Timeline.apiCapability
      project = Project.apiCapability
      mainComponent = component liveSystem login signup timeline project
  runUI mainComponent unit body

data MainView 
  = VLoading
  | VSignup
  | VLogin
  | VTimeline 

type MainState =
  { view :: MainView
  , config :: Signup.Config
  }

data MainAction
  = Initialize
  | LoginAction Login.LoginResult
  | SignupAction Signup.SignupResult
  | LogoutAction

type Slots = 
  ( login :: Login.Slot Unit
  , signup :: Signup.Slot Unit
  , timeline :: Timeline.Slot Unit
  )

_login = SProxy :: SProxy "login"
_signup = SProxy :: SProxy "signup"
_timeline = SProxy :: SProxy "timeline"

component 
  :: forall query input output m
  .  Monad m
  => System m
  -> Login.Capability m
  -> Signup.Capability m
  -> Timeline.Capability m
  -> Project.Capability m
  -> H.Component HH.HTML query input output m
component system loginCap signupCap tlCap pCap = H.mkComponent 
  { initialState
  , render 
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = eval 
      , initialize = Just Initialize 
      }
  } where
    initialState :: input -> MainState
    initialState _ = 
      { view: VLoading
      , config: { recaptchaKey: "6LdiA78ZAAAAAGGvDId_JmDbhalduIDZSqbuikfq" } 
      }

    render :: MainState -> H.ComponentHTML MainAction Slots m
    render st = case st.view of
      VLoading ->
        HH.div [P.classes [ClassName "loader"]] [HH.text "Loading..."]

      VSignup -> 
        HH.div_ 
          [ HH.slot _signup unit (Signup.component system signupCap st.config) unit (Just <<< SignupAction) ]

      VLogin -> 
        HH.div_ 
          [ HH.slot _login unit (Login.component system loginCap) unit (Just <<< LoginAction) ]

      VTimeline -> 
        withNavBar $ HH.div_ 
          [ HH.slot _timeline unit (Timeline.component system tlCap pCap) unit absurd ]

    eval :: MainAction -> H.HalogenM MainState MainAction Slots output m Unit
    eval action = case action of
      Initialize -> do
        route <- lift system.getHash
        nextView <- case route of 
          "login"  -> pure VLogin
          "signup" -> pure VSignup
          other -> do
            result <- lift loginCap.checkLogin
            case result of
                Acc.LoginForbidden -> pure VLogin
                Acc.LoginError _ -> pure VLogin
                _ -> pure VTimeline
        H.modify_ (_ { view = nextView })

      LoginAction (Login.LoginComplete _) -> 
        H.modify_ (_ { view = VTimeline })

      SignupAction (Signup.SignupComplete _) ->
        H.modify_ (_ { view = VTimeline })

      SignupAction (Signup.SigninNav) ->
        H.modify_ (_ { view = VLogin })

      LogoutAction -> do
        lift loginCap.logout
        H.modify_ (_ { view = VLogin })


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
  ,E.onClick \_ -> Just LogoutAction 
  ]
  [HH.text "Logout"]

