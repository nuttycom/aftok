{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Users where

import ClassyPrelude
import Control.Lens
import Quixotic

newtype UserId = UserId Int64 deriving (Show, Eq)
makePrisms ''UserId

newtype UserName = UserName Text deriving (Show, Eq)
makePrisms ''UserName

data User = User
  { _username :: UserName
  , _userAddress :: BtcAddr
  , _userEmail :: Text
  }
makeLenses ''User

