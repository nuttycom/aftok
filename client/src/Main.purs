module Main where

import Prelude (Unit, bind, ($))

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())

import Halogen.Aff (HalogenEffects)
import Halogen.VDom.Driver (runUI)
import Halogen.Aff as HA
import Aftok.Login as L

import Network.HTTP.Affjax (AJAX())

type AppEffects eff = HalogenEffects (console :: CONSOLE, ajax :: AJAX | eff)

main :: Eff (AppEffects ()) Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  runUI L.ui L.initialState body
