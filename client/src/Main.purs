module Main where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)
import Halogen as H
import Halogen.Aff (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML as HH
-- import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Routing (match)
import Routing.Hash (matchesWith)
import Routing.Match (Match, lit)
import Aftok.Types (System, liveSystem)
import Aftok.Login as Login
import Aftok.Api.Account as Acc
import Aftok.Api.Project (Project)
import Aftok.Billing as Billing
import Aftok.Signup as Signup
import Aftok.Timeline as Timeline
import Aftok.Overview as Overview
import Aftok.ProjectList as ProjectList

main :: Effect Unit
main =
  runHalogenAff do
    body <- awaitBody
    let --login = Login.mockCapability
      login = Login.apiCapability

      signup = Signup.apiCapability

      timeline = Timeline.apiCapability

      project = ProjectList.apiCapability

      overview = Overview.apiCapability

      billing = Billing.apiCapability

      mainComponent = component liveSystem login signup timeline project overview billing
    halogenIO <- runUI mainComponent unit body
    void $ liftEffect
      $ matchesWith (match mainRoute) \oldMay new ->
          when (oldMay /= Just new) do
            launchAff_ <<< halogenIO.query <<< H.tell $ Navigate new

data View
  = VLoading
  | VSignup
  | VLogin
  | VOverview
  | VTimeline
  | VBilling

mainRoute :: Match View
mainRoute =
  oneOf
    [ VSignup <$ lit "signup"
    , VLogin <$ lit "login"
    , VOverview <$ lit "overview"
    , VTimeline <$ lit "timeline"
    , VBilling <$ lit "billing"
    ]

routeHash :: View -> String
routeHash = case _ of
  VSignup -> "signup"
  VLogin -> "login"
  VTimeline -> "timeline"
  VOverview -> "overview"
  VBilling -> "billing"
  VLoading -> ""

-- derive instance genericView :: Generic View _
derive instance eqView :: Eq View

derive instance ordView :: Ord View

data MainQuery a
  = Navigate View a

type MainState
  = { view :: View
    , config :: Signup.Config
    , selectedProject :: Maybe Project
    }

data MainAction
  = Initialize
  | LoginAction Login.LoginResult
  | SignupAction Signup.SignupResult
  | ProjectAction ProjectList.Event
  | LogoutAction

type Slots
  = ( login :: Login.Slot Unit
    , signup :: Signup.Slot Unit
    , overview :: Overview.Slot Unit
    , timeline :: Timeline.Slot Unit
    , billing :: Billing.Slot Unit
    )

_login = SProxy :: SProxy "login"

_signup = SProxy :: SProxy "signup"

_overview = SProxy :: SProxy "overview"

_timeline = SProxy :: SProxy "timeline"

_billing = SProxy :: SProxy "billing"

component ::
  forall input output m.
  Monad m =>
  System m ->
  Login.Capability m ->
  Signup.Capability m ->
  Timeline.Capability m ->
  ProjectList.Capability m ->
  Overview.Capability m ->
  Billing.Capability m ->
  H.Component HH.HTML MainQuery input output m
component system loginCap signupCap tlCap pCap ovCap bcap =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , initialize = Just Initialize
              }
    }
  where
  initialState :: input -> MainState
  initialState _ =
    { view: VLoading
    , config: { recaptchaKey: "6LdiA78ZAAAAAGGvDId_JmDbhalduIDZSqbuikfq" }
    , selectedProject: Nothing
    }

  render :: MainState -> H.ComponentHTML MainAction Slots m
  render st = case st.view of
    VLoading -> HH.div [ P.classes [ ClassName "loader" ] ] [ HH.text "Loading..." ]
    VSignup ->
      HH.div_
        [ HH.slot _signup unit (Signup.component system signupCap st.config) unit (Just <<< SignupAction) ]
    VLogin ->
      HH.div_
        [ HH.slot _login unit (Login.component system loginCap) unit (Just <<< LoginAction) ]
    VOverview ->
      withNavBar
        $ HH.div_
            [ HH.slot _overview unit (Overview.component system ovCap pCap) st.selectedProject (Just <<< ProjectAction) ]
    VTimeline ->
      withNavBar
        $ HH.div_
            [ HH.slot _timeline unit (Timeline.component system tlCap pCap) st.selectedProject (Just <<< ProjectAction) ]
    VBilling ->
      withNavBar
        $ HH.div_
            [ HH.slot _billing unit (Billing.component system bcap pCap) st.selectedProject (Just <<< ProjectAction) ]

  handleAction :: MainAction -> H.HalogenM MainState MainAction Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      route <- lift system.getHash
      nextView <- case route of
        "login" -> pure VLogin
        "signup" -> pure VSignup
        other -> do
          result <- lift loginCap.checkLogin
          pure
            $ case result of
                Acc.LoginForbidden -> VLogin
                Acc.LoginError _ -> VLogin
                _ -> case other of
                  "timeline" -> VTimeline
                  "billing" -> VBilling
                  _ -> VOverview
      navigate nextView
    SignupAction (Signup.SignupComplete _) -> navigate VOverview
    SignupAction (Signup.SigninNav) -> navigate VLogin
    LoginAction (Login.LoginComplete _) -> navigate VOverview
    LogoutAction -> do
      lift loginCap.logout
      navigate VLogin
    ProjectAction (ProjectList.ProjectChange p) -> H.modify_ (_ { selectedProject = Just p })

  handleQuery :: forall a. MainQuery a -> H.HalogenM MainState MainAction Slots output m (Maybe a)
  handleQuery = case _ of
    Navigate view a -> do
      currentView <- H.gets _.view
      when (currentView /= view) $ navigate view
      pure (Just a)

  navigate :: View -> H.HalogenM MainState MainAction Slots output m Unit
  navigate view = do
    lift $ system.setHash (routeHash view)
    H.modify_ (_ { view = view })

withNavBar :: forall s m. H.ComponentHTML MainAction s m -> H.ComponentHTML MainAction s m
withNavBar body =
  HH.div_
    [ HH.nav
        [ P.classes (ClassName <$> [ "navbar", "navbar-expand-lg", "navbar-light", "bg-white" ]) ]
        [ HH.div
            [ P.classes (ClassName <$> [ "container-fluid" ]) ]
            [ brand
            , HH.ul [ P.classes (ClassName <$> [ "navbar-nav", "ml-auto" ]) ] (map navItem nav)
            , logout
            ]
        ]
    , body
    ]

nav :: Array NavItem
nav =
  [ { label: "Overview", path: "overview" }
  , { label: "Timeline", path: "timeline" }
  , { label: "Billing", path: "billing" }
  ]

brand :: forall a s m. H.ComponentHTML a s m
brand =
  HH.div
    [ P.classes (ClassName <$> [ "navbar-brand" ]) ]
    [ HH.h4
        [ P.classes (ClassName <$> [ "font-weight-bold" ]) ]
        [ HH.a [ P.href "/" ] [ HH.text "Aftok" ] ]
    ]

logout :: forall s m. H.ComponentHTML MainAction s m
logout =
  HH.button
    [ P.classes (ClassName <$> [ "btn", "navbar-btn", "btn-sm", "btn-primary", "lift", "ml-auto" ])
    , E.onClick \_ -> Just LogoutAction
    ]
    [ HH.text "Logout" ]

type NavTop
  = { label :: String
    , items :: Array NavItem
    }

type NavItem
  = { label :: String
    , path :: String
    }

navItem :: forall a s m. NavItem -> H.ComponentHTML a s m
navItem ni =
  HH.li
    [ P.classes (ClassName <$> [ "nav-item" ]) ]
    [ HH.a
        [ P.classes (ClassName <$> [ "nav-link" ])
        , P.href ("#" <> ni.path)
        ]
        [ HH.text ni.label ]
    ]
