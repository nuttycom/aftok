{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Quixotic.Users where

import ClassyPrelude
import Control.Lens
import Quixotic

newtype UserId = UserId Int64 deriving (Show, Eq)

data User = User
  { _userAddress :: BtcAddr
  , _userEmail :: Text
  }

makeLenses ''User
