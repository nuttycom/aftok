module Main where

import Prelude (Unit, bind, ($), (<$), (<$>), (*>), (<<<))

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE())

import Data.Maybe (maybe)
import Data.UUID (UUID, parseUUID)

import Routing.Match (Match)
import Routing.Match.Class (lit, str)

import Halogen.Aff (HalogenEffects)
import Halogen.VDom.Driver (runUI)
import Halogen.Aff as HA

import Network.HTTP.Affjax (AJAX())

import Aftok.Login as L

type AppEffects eff = HalogenEffects (console :: CONSOLE, ajax :: AJAX | eff)

main :: Eff (AppEffects ()) Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  runUI L.ui L.initialState body

data ARoute 
  = Home
  | Login
  | Project UUID
  | NotFound

routing :: Match ARoute
routing = Home <$ lit ""
      <|> Login <$ (lit "" *> lit "login")
      <|> (maybe NotFound Project <<< parseUUID) <$> (lit "" *> lit "project" *> str)
