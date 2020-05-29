module Aftok.Snaplet.Auth where



import           Control.Lens
import           Control.Error.Util             ( maybeT )
import           Control.Monad.Trans.Maybe      ( mapMaybeT )
import           Data.Attoparsec.ByteString     ( parseOnly )

import           Aftok.Types
import           Aftok.Database
import           Aftok.Snaplet
import           Aftok.Util.Http                ( authHeaderParser )

import           Snap.Core
import           Snap.Snaplet                  as S
import qualified Snap.Snaplet.Auth             as AU

requireLogin :: S.Handler App App AU.AuthUser
requireLogin = do
  req          <- getRequest
  rawHeader    <- maybe throwChallenge pure $ getHeader "Authorization" req
  (uname, pwd) <- either (throwDenied . AU.AuthError) pure
    $ parseOnly authHeaderParser rawHeader
  authResult <- with auth $ AU.loginByUsername uname (AU.ClearText pwd) False
  either throwDenied pure authResult

requireUser :: S.Handler App App AU.AuthUser
requireUser = do
  currentUser <- with auth AU.currentUser
  maybe requireLogin pure currentUser

requireUserId :: S.Handler App App UserId
requireUserId = do
  currentUser <- UserName . AU.userLogin <$> requireUser
  maybeT
    (snapError 403 "Unable to retrieve user record for authenticated user")
    (pure . (^. _1))
    (mapMaybeT snapEval $ findUserByName currentUser)

throwChallenge :: MonadSnap m => m a
throwChallenge = do
  modifyResponse
    $ (setResponseStatus 401 "Unauthorized")
    . (setHeader "WWW-Authenticate" "Basic realm=aftok")
  getResponse >>= finishWith

throwDenied :: MonadSnap m => AU.AuthFailure -> m a
throwDenied failure = do
  modifyResponse $ setResponseStatus 403 "Access Denied"
  writeText $ "Access Denied: " <> show failure
  getResponse >>= finishWith

