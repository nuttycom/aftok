{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NoImplicitPrelude #-}

module Quixotic.Users where

import ClassyPrelude

newtype UserId = UserId Int

data User = User
  { userAddress :: BtcAddr
  , userEmail :: Text
  }
