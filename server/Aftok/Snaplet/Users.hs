{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Aftok.Snaplet.Users
  ( acceptInvitationHandler,
    checkZAddrHandler,
    checkUsernameHandler,
    registerHandler,
    CaptchaError (..),
    checkCaptcha,
  )
where

import qualified Aftok.Currency.Zcash as Zcash
import Aftok.Database (findUserByName)
import Aftok.Database.PostgreSQL (QDBM)
import Aftok.Http.Auth (RegisterCaps(..), register, AuthConfig, username, regUser, password, CaptchaError(..))
import Aftok.Project (parseInvCode)
import Aftok.Snaplet
import Aftok.Snaplet.Auth
import Aftok.Types
  (
    UserId,
    UserName (..),
    _UserName,
  )
import Aftok.Time (hoistClock, systemClock)
import Control.FromSum (fromMaybeM)
import Control.Lens ((^.))
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as T
import Data.Thyme.Clock as C
import qualified Snap.Core as S
import qualified Snap.Snaplet as S
import qualified Snap.Snaplet.Auth as AU

checkUsernameHandler :: S.Handler App App ()
checkUsernameHandler = do
  params <- S.getParams
  uname <-
    maybe
      (snapError 400 "username parameter is required")
      (either (const $ snapError 400 "username must be valid UTF-8") (pure . UserName) . decodeUtf8')
      (listToMaybe =<< M.lookup "username" params)
  found <- snapEval (runMaybeT $ findUserByName uname)
  case found of
    Nothing -> pure ()
    Just _ -> snapError 400 "username is already taken"

checkZAddrHandler :: RegisterCaps QDBM -> S.Handler App App Zcash.Address
checkZAddrHandler ops = do
  params <- S.getParams
  zaddrBytes <-
    maybe
      (snapError 400 "zaddr parameter is required")
      pure
      (listToMaybe =<< M.lookup "zaddr" params)
  zaddrEither <- qdbmEval $ validateZAddr ops (T.decodeUtf8 zaddrBytes)
  case zaddrEither of
    Left err ->
      snapError 400 $ "The Z-Address provided for account recovery was invalid: " <> show err
    Right zaddr ->
      pure zaddr

registerHandler :: RegisterCaps QDBM -> AuthConfig -> S.Handler App App UserId
registerHandler ops cfg = do
  rbody <- S.readRequestBody 4096
  userData <- fromMaybeM (snapError 400 "Could not parse user data") (A.decode rbody)
  regResult <- qdbmEval . runExceptT $ register (hoistClock liftIO systemClock) ops cfg userData
  case regResult of
    Left err ->
      snapErrorJS 400 "User registration failed" err
    Right uid -> do
      snapUser <- S.with auth $ AU.createUser (userData ^. regUser . username . _UserName) (userData ^. password)
      either throwDenied (const $ pure uid) snapUser

acceptInvitationHandler :: RegisterCaps QDBM -> S.Handler App App ()
acceptInvitationHandler caps = do
  uid <- requireUserId
  now <- liftIO C.getCurrentTime
  params <- S.getParams
  invCodes <-
    maybe
      (snapError 400 "invCode parameter is required")
      (pure . traverse (parseInvCode . T.decodeUtf8))
      (M.lookup "invCode" params)
  either
    (\e -> snapError 400 $ "Invitation code was rejected as invalid: " <> e)
    (\cx -> qdbmEval $ traverse_ (acceptInvitation caps uid now) cx)
    invCodes
