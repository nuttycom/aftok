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

--     <!-- Navigation -->
--     <ul class="navbar-nav ml-auto">
--       <li class="nav-item dropdown">
--         <a class="nav-link dropdown-toggle" id="navbarAccount" data-toggle="dropdown" href="#" aria-haspopup="true" aria-expanded="false">
--           Guidebook
--         </a>
--         <ul class="dropdown-menu" aria-labelledby="navbarAccount">
--           <li class="dropdown-item dropright">
--             <a class="dropdown-link dropdown-toggle" data-toggle="dropdown" href="#">
--               Getting Started
--             </a>
--             <div class="dropdown-menu">
--               <a class="dropdown-item" href="@@webRoot/guide-foundation.html">
--                 Foundational Principles
--               </a>
--               <a class="dropdown-item" href="@@webRoot/guide-time.html">
--                 Measuring Contributions
--               </a>
--               <a class="dropdown-item" href="@@webRoot/guide-revenue.html">
--                 Revenue Sharing
--               </a>
--               <a class="dropdown-item" href="@@webRoot/guide-tithing.html">
--                 Varying Compensation
--               </a>
--             </div>
--           </li>
-- 					<!--
--           <li class="dropdown-item dropright">
--             <a class="dropdown-link dropdown-toggle" data-toggle="dropdown" href="#">
--               Up And Running
--             </a>
--             <div class="dropdown-menu">
--               <a class="dropdown-item" href="@@webRoot/guide-voting.html">
--                 Share-Weighted Voting
--               </a>
--               <a class="dropdown-item" href="@@webRoot/guide-auctions.html">
--                 Expense Auctions
--               </a>
--               <a class="dropdown-item" href="@@webRoot/guide-forks.html">
--                 Project Forks & Merges
--               </a>
--             </div>
--           </li>
--           <li class="dropdown-item dropright">
--             <a class="dropdown-link dropdown-toggle" data-toggle="dropdown" href="#">
--               Coming Soon!
--             </a>
--             <div class="dropdown-menu">
--               <a class="dropdown-item" href="@@webRoot/guide-debt-contracts.html">
--                 3rd-party Contracts
--               </a>
--               <a class="dropdown-item" href="@@webRoot/guide-delegative-voting.html">
--                 Delegative Voting
--               </a>
--             </div>
--           </li>
-- 					<li class="dropdown-item dropright">
--             <a class="dropdown-item" href="@@webRoot/blog.html">
--               Blog
--             </a>
--           </li>
-- 				  -->
--         </ul>
--       </li>
--       <li class="nav-item dropdown">
--         <a class="nav-link dropdown-toggle" id="navbarAccount" data-toggle="dropdown" href="#" aria-haspopup="true" aria-expanded="false">
--           My Account
--         </a>
--         <ul class="dropdown-menu" aria-labelledby="navbarAccount">
--           <li class="dropdown-item dropright">
--             <a class="dropdown-item" data-toggle="modal" href="#modalSigninHorizontal">
--               Sign In
--             </a>
--           </li>
-- 					<!--
--           <li class="dropdown-item dropright">
--             <a class="dropdown-item" href="@@webRoot/account-revenue.html">
--               Revenue Dashboard
--             </a>
--           </li>
--           <li class="dropdown-item dropright">
--             <a class="dropdown-item" href="@@webRoot/account-auctions.html">
--               Active Expense Auctions
--             </a>
--           </li>
--           <li class="dropdown-item dropright">
--             <a class="dropdown-item" href="@@webRoot/account-general.html">
--               Project List
--             </a>
--           </li>
--           <li class="dropdown-item dropright">
--             <a class="dropdown-item" href="@@webRoot/account-tithes.html">
--               My Tithes
--             </a>
--           </li>
--           <li class="dropdown-item dropright">
--             <a class="dropdown-item" href="@@webRoot/account-keys.html">
--               Manage Payment Keys
--             </a>
--           </li>
--           <li class="dropdown-item dropright">
--             <a class="dropdown-item" href="@@webRoot/account-billing.html">
--               Billing Center
--             </a>
--           </li>
-- 					-->
--         </ul>
--       </li>
--       <li class="nav-item dropdown">
--         <a class="nav-link dropdown-toggle" id="navbarDocumentation" data-toggle="dropdown" href="#" aria-haspopup="true" aria-expanded="false">
--           Documentation
--         </a>
--         <div class="dropdown-menu dropdown-menu-md" aria-labelledby="navbarDocumentation">
--           <div class="list-group list-group-flush">
--             <a class="list-group-item" href="@@webRoot/docs/index.html">
-- 
--               <!-- Icon -->
--               <div class="icon icon-sm text-primary">
--                 @@include("../assets/img/icons/duotone-icons/General/Clipboard.svg")
--               </div>
-- 
--               <!-- Content -->
--               <div class="ml-4">
-- 
--                 <!-- Heading -->
--                 <h6 class="font-weight-bold text-uppercase text-primary mb-0">
--                   Documentation
--                 </h6>
-- 
--                 <!-- Text -->
--                 <p class="font-size-sm text-gray-700 mb-0">
--                   CLI & API user guide
--                 </p>
-- 
--               </div>
-- 
--             </a>
--             <a class="list-group-item" href="@@webRoot/faq.html">
-- 
--               <!-- Icon -->
--               <div class="icon icon-sm text-primary">
--                 @@include("../assets/img/icons/duotone-icons/Code/Question-circle.svg")
--               </div>
-- 
--               <!-- Content -->
--               <div class="ml-4">
-- 
--                 <!-- Heading -->
--                 <h6 class="font-weight-bold text-uppercase text-primary mb-0">
--                   FAQ
--                 </h6>
-- 
--                 <!-- Text -->
--                 <p class="font-size-sm text-gray-700 mb-0">
--                   Common problems & solutions
--                 </p>
-- 
--               </div>
-- 
--             </a>
--             <a class="list-group-item" href="https://discord.gg/wbhCGjw" target="_blank">
-- 
--               <!-- Icon -->
--               <div class="icon icon-sm text-primary">
--                 @@include("../assets/img/icons/social/discord.svg")
--               </div>
-- 
--               <!-- Content -->
--               <div class="ml-4">
-- 
--                 <!-- Heading -->
--                 <h6 class="font-weight-bold text-uppercase text-primary mb-0">
--                   Community
--                 </h6>
-- 
--                 <!-- Text -->
--                 <p class="font-size-sm text-gray-700 mb-0">
--                   Join our Discord chat
--                 </p>
-- 
--               </div>
-- 
--             </a>
-- 						<!--
--             <a class="list-group-item" href="@@webRoot/docs/changelog.html">
-- 
--               <div class="icon icon-sm text-primary">
--                 @@include("../assets/img/icons/duotone-icons/Files/File.svg")
--               </div>
-- 
--               <div class="ml-4">
-- 
--                 <h6 class="font-weight-bold text-uppercase text-primary mb-0">
--                   Changelog
--                 </h6>
-- 
--                 <p class="font-size-sm text-gray-700 mb-0">
--                   Project history
--                 </p>
--               </div>
--             </a>
-- 						-->
--           </div>
--         </div>
--       </li>
--     </ul>
