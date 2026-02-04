{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Aftok.Database.PostgreSQL.PasswordReset
  ( createPasswordResetToken,
    findPasswordResetToken,
    markPasswordResetTokenUsed,
    deleteExpiredPasswordResetTokens,
    findUserByEmail,
  )
where

import Aftok.Database.PostgreSQL.Types
  ( DBM,
    idParser,
    pexec,
    pinsert,
    pquery,
    utcParser,
  )
import Aftok.Types
  ( Email (..),
    PasswordResetToken (..),
    PasswordResetTokenId (..),
    UserId (..),
    User (..),
    UserName (..),
    RecoverBy (..),
    _Email,
  )
import qualified Aftok.Currency.Zcash as Zcash
import Control.Lens ((^.))
import qualified Data.Thyme.Clock as C
import Data.Thyme.Time.Core (fromThyme, toThyme)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Safe (headMay)

-- | Create a new password reset token
createPasswordResetToken ::
  UserId ->
  Text ->
  C.UTCTime ->
  DBM PasswordResetTokenId
createPasswordResetToken (UserId uid) token expiresAt =
  pinsert
    PasswordResetTokenId
    [sql| INSERT INTO password_reset_tokens (user_id, token, expires_at)
          VALUES (?, ?, ?) RETURNING id |]
    (uid, token, fromThyme expiresAt)

-- | Find a password reset token by token string
findPasswordResetToken :: Text -> DBM (Maybe (PasswordResetTokenId, PasswordResetToken))
findPasswordResetToken token = do
  headMay
    <$> pquery
      tokenParser
      [sql| SELECT id, user_id, token, expires_at, created_at, used_at
            FROM password_reset_tokens
            WHERE token = ? |]
      (Only token)
  where
    tokenParser :: RowParser (PasswordResetTokenId, PasswordResetToken)
    tokenParser = do
      tokenId <- idParser PasswordResetTokenId
      userId <- idParser UserId
      tok <- field
      expiresAt <- utcParser
      createdAt <- utcParser
      usedAt <- fmap toThyme <$> field
      pure (tokenId, PasswordResetToken userId tok expiresAt createdAt usedAt)

-- | Mark a password reset token as used
markPasswordResetTokenUsed :: PasswordResetTokenId -> C.UTCTime -> DBM ()
markPasswordResetTokenUsed (PasswordResetTokenId tokenId) usedAt =
  void $
    pexec
      [sql| UPDATE password_reset_tokens
            SET used_at = ?
            WHERE id = ? |]
      (fromThyme usedAt, tokenId)

-- | Delete expired password reset tokens (cleanup)
deleteExpiredPasswordResetTokens :: C.UTCTime -> DBM Int64
deleteExpiredPasswordResetTokens now =
  pexec
    [sql| DELETE FROM password_reset_tokens
          WHERE expires_at < ? OR used_at IS NOT NULL |]
    (Only $ fromThyme now)

-- | Find a user by their recovery email address
findUserByEmail :: Email -> DBM (Maybe (UserId, User))
findUserByEmail email = do
  headMay
    <$> pquery
      userParser
      [sql| SELECT id, handle, recovery_email, recovery_zaddr
            FROM users
            WHERE recovery_email = ? |]
      (Only $ email ^. _Email)
  where
    userParser :: RowParser (UserId, User)
    userParser = do
      uid <- idParser UserId
      uname <- UserName <$> field
      remail <- fmap (RecoverByEmail . Email) <$> field
      rzaddr <- fmap (RecoverByZAddr . Zcash.Address) <$> field
      user <- User uname <$> maybe empty pure (remail <|> rzaddr)
      pure (uid, user)
