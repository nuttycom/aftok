module Main where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Aff (runAff)

import Halogen
import Halogen.Util (appendToBody, onLoad)
import Halogen.Util (appendToBody, onLoad)
import qualified Aftok.Login as L

import Network.HTTP.Affjax (AJAX())

type AppEffects eff = HalogenEffects (console :: CONSOLE, ajax :: AJAX | eff)

main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI L.ui L.initialState
  onLoad $ appendToBody app.node
