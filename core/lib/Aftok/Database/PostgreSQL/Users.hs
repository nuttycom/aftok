{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Aftok.Database.PostgreSQL.Users
  ( createUser,
    createUserWithPassword,
    findUser,
    findUserByName,
    findUserByNameWithPassword,
    findUserPaymentAddress,
    findUserProjectDetail,
    findAccountPaymentAddress,
    findAccountZcashIVK,
    updateUserPassword,
    setUserZcashAddress,
    findUserZcashAddress,
    -- GitHub username operations
    findUserByGitHubUsername,
    linkGitHubUsername,
    unlinkGitHubUsername,
    getUserGitHubUsername,
  )
where

import Aftok.Currency (Currency (..))
import qualified Aftok.Currency.Zcash as Zcash
import Aftok.Database ()
import Aftok.Password (PasswordHash (..))
import Aftok.Database.PostgreSQL.Types
  ( DBM,
    askNetworkMode,
    bitcoinAddressParser,
    idParser,
    pexec,
    pinsert,
    pquery,
    utcParser,
    zcashAddressParser,
    zcashIvkParser,
  )
import Aftok.Types
import Control.Lens
import qualified Data.Thyme.Clock as C
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
  ( sql,
  )
import Safe (headMay)
import Prelude hiding (null)

userParser :: RowParser User
userParser = do
  uname <- UserName <$> field
  remail <- fmap (RecoverByEmail . Email) <$> field
  rzaddr <- fmap (RecoverByZAddr . Zcash.Address) <$> field
  User uname <$> maybe empty pure (remail <|> rzaddr)

createUser :: User -> DBM UserId
createUser user' = do
  uid <-
    pinsert
      UserId
      [sql| INSERT INTO users (handle, recovery_email, recovery_zaddr)
          VALUES (?, ?, ?) RETURNING id |]
      ( user' ^. (username . _UserName),
        user' ^? userAccountRecovery . _RecoverByEmail . _Email,
        user' ^? userAccountRecovery . _RecoverByZAddr . Zcash._Address
      )
  case user' ^. userAccountRecovery of
    RecoverByZAddr addr -> linkZcashAccount uid addr
    RecoverByEmail _ -> pure ()
  pure uid

createUserWithPassword :: User -> PasswordHash -> DBM UserId
createUserWithPassword user' pwdHash = do
  uid <-
    pinsert
      UserId
      [sql| INSERT INTO users (handle, recovery_email, recovery_zaddr, password_hash)
          VALUES (?, ?, ?, ?) RETURNING id |]
      ( user' ^. (username . _UserName),
        user' ^? userAccountRecovery . _RecoverByEmail . _Email,
        user' ^? userAccountRecovery . _RecoverByZAddr . Zcash._Address,
        unPasswordHash pwdHash
      )
  case user' ^. userAccountRecovery of
    RecoverByZAddr addr -> linkZcashAccount uid addr
    RecoverByEmail _ -> pure ()
  pure uid

linkZcashAccount :: UserId -> Zcash.Address -> DBM ()
linkZcashAccount uid addr =
  void $
    pexec
      [sql| INSERT INTO cryptocurrency_accounts (user_id, is_primary, zcash_addr)
          VALUES (?, ?, ?) |]
      ( uid ^. _UserId,
        True,
        addr ^. Zcash._Address
      )

findUser :: UserId -> DBM (Maybe User)
findUser (UserId uid) = do
  headMay
    <$> pquery
      userParser
      [sql| SELECT handle, recovery_email, recovery_zaddr FROM users WHERE id = ? |]
      (Only uid)

findUserProjectDetail :: UserId -> ProjectId -> DBM (Maybe (User, C.UTCTime))
findUserProjectDetail (UserId uid) (ProjectId pid) = do
  headMay
    <$> pquery
      ((,) <$> userParser <*> utcParser)
      [sql| SELECT u.handle, u.recovery_email, u.recovery_zaddr, p.joined_at
            FROM users u
            JOIN project_companions p on p.user_id = u.id
            WHERE u.id = ? AND p.project_id = ? |]
      (uid, pid)

findUserByName :: UserName -> DBM (Maybe (UserId, User))
findUserByName (UserName h) = do
  headMay
    <$> pquery
      ((,) <$> idParser UserId <*> userParser)
      [sql| SELECT id, handle, recovery_email, recovery_zaddr FROM users WHERE handle = ? |]
      (Only h)

findUserByNameWithPassword :: UserName -> DBM (Maybe (UserId, User, Maybe PasswordHash))
findUserByNameWithPassword (UserName h) = do
  headMay
    <$> pquery
      ((\uid user pwdHash -> (uid, user, PasswordHash <$> pwdHash)) <$> idParser UserId <*> userParser <*> field)
      [sql| SELECT id, handle, recovery_email, recovery_zaddr, password_hash FROM users WHERE handle = ? |]
      (Only h)

findUserPaymentAddress :: UserId -> Currency a c -> DBM (Maybe (AccountId, a))
findUserPaymentAddress uid = \case
  BTC -> do
    mode <- askNetworkMode
    headMay
      <$> pquery
        ((,) <$> idParser AccountId <*> bitcoinAddressParser mode)
        [sql| SELECT id, btc_addr FROM cryptocurrency_accounts
            WHERE user_id = ?
            AND is_primary = true
            AND btc_addr IS NOT NULL |]
        (Only $ view _UserId uid)
  ZEC -> do
    headMay
      <$> pquery
        ((,) <$> idParser AccountId <*> zcashAddressParser)
        [sql| SELECT id, zcash_addr FROM cryptocurrency_accounts
            WHERE user_id = ?
            AND is_primary = true
            AND zcash_addr IS NOT NULL |]
        (Only $ view _UserId uid)

findAccountPaymentAddress :: AccountId -> Currency a c -> DBM (Maybe a)
findAccountPaymentAddress aid = \case
  BTC -> do
    mode <- askNetworkMode
    headMay
      <$> pquery
        (bitcoinAddressParser mode)
        [sql| SELECT btc_addr FROM cryptocurrency_accounts
            WHERE id = ?
            AND btc_addr IS NOT NULL |]
        (Only $ view _AccountId aid)
  ZEC -> do
    headMay
      <$> pquery
        (zcashAddressParser)
        [sql| SELECT zcash_addr FROM cryptocurrency_accounts
            WHERE id = ?
            AND zcash_addr IS NOT NULL |]
        (Only $ view _AccountId aid)

-- TODO: rework this for the case where someone wants to
-- use new diversified addresses for each purchase?

findAccountZcashIVK :: AccountId -> DBM (Maybe Zcash.IVK)
findAccountZcashIVK aid =
  headMay
    <$> pquery
      (zcashIvkParser)
      [sql| SELECT zcash_ivk FROM cryptocurrency_accounts
            WHERE id = ?
            AND zcash_ivk IS NOT NULL |]
      (Only $ view _AccountId aid)

-- | Update a user's password hash
updateUserPassword :: UserId -> PasswordHash -> DBM ()
updateUserPassword (UserId uid) pwdHash =
  void $
    pexec
      [sql| UPDATE users SET password_hash = ? WHERE id = ? |]
      (unPasswordHash pwdHash, uid)

-- | Find a user's current primary Zcash address
findUserZcashAddress :: UserId -> DBM (Maybe Zcash.Address)
findUserZcashAddress (UserId uid) =
  headMay
    <$> pquery
      zcashAddressParser
      [sql| SELECT zcash_addr FROM cryptocurrency_accounts
            WHERE user_id = ?
            AND is_primary = true
            AND zcash_addr IS NOT NULL |]
      (Only uid)

-- | Set a user's primary Zcash address (event-sourced)
setUserZcashAddress :: UserId -> Zcash.Address -> DBM ()
setUserZcashAddress uid addr = do
  -- Read current address for event log
  currentAddr <- findUserZcashAddress uid
  -- Record the change event
  void $
    pexec
      [sql| INSERT INTO address_change_events (user_id, zcash_addr, previous_addr)
            VALUES (?, ?, ?) |]
      ( uid ^. _UserId,
        addr ^. Zcash._Address,
        fmap (view Zcash._Address) currentAddr
      )
  -- Check if a primary row already exists
  existing <-
    headMay
      <$> pquery
        (idParser AccountId)
        [sql| SELECT id FROM cryptocurrency_accounts
              WHERE user_id = ? AND is_primary = true |]
        (Only $ uid ^. _UserId)
  case existing of
    Just _ ->
      -- Update the existing primary account row
      void $
        pexec
          [sql| UPDATE cryptocurrency_accounts
                SET zcash_addr = ?
                WHERE user_id = ? AND is_primary = true |]
          ( addr ^. Zcash._Address,
            uid ^. _UserId
          )
    Nothing ->
      -- Insert a new primary account row
      linkZcashAccount uid addr

-- | Find a user by their linked GitHub username
findUserByGitHubUsername :: GitHubUsername -> DBM (Maybe (UserId, User))
findUserByGitHubUsername (GitHubUsername ghUser) = do
  headMay
    <$> pquery
      ((,) <$> idParser UserId <*> userParser)
      [sql| SELECT id, handle, recovery_email, recovery_zaddr
            FROM users
            WHERE github_username = ? |]
      (Only ghUser)

-- | Link a GitHub username to a user account
linkGitHubUsername :: UserId -> GitHubUsername -> DBM ()
linkGitHubUsername (UserId uid) (GitHubUsername ghUser) =
  void $
    pexec
      [sql| UPDATE users SET github_username = ? WHERE id = ? |]
      (ghUser, uid)

-- | Unlink a GitHub username from a user account
unlinkGitHubUsername :: UserId -> DBM ()
unlinkGitHubUsername (UserId uid) =
  void $
    pexec
      [sql| UPDATE users SET github_username = NULL WHERE id = ? |]
      (Only uid)

-- | Get the GitHub username linked to a user account
getUserGitHubUsername :: UserId -> DBM (Maybe GitHubUsername)
getUserGitHubUsername (UserId uid) = do
  results <-
    pquery
      (fmap GitHubUsername <$> field)
      [sql| SELECT github_username FROM users WHERE id = ? |]
      (Only uid)
  pure $ join (headMay results)
