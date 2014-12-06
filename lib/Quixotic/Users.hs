{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}

module Quixotic.Users where

import ClassyPrelude
import Quixotic

newtype UserId = UserId Int deriving (Show, Eq)

data User = User
  { userAddress :: BtcAddr
  , userEmail :: Text
  }
