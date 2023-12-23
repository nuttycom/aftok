{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Aftok.Database.PostgreSQL.Users
  ( pgCreateUser,
    pgFindUser,
    pgFindUserByName,
    pgFindUserPaymentAddress,
    pgFindUserProjectDetail,
    pgFindAccountPaymentAddress,
    pgFindAccountZcashIVK,
    pgUserCaps,
  )
where

import Aftok.Currency (Currency (..))
import qualified Aftok.Currency.Bitcoin as Bitcoin
import qualified Aftok.Currency.Zcash as Zcash
import Aftok.Database (DBError (..))
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
import Aftok.Database.Users (UserCaps (..))
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
  User uname <$> maybe empty pure (remail <|> rzaddr) <*> field

pgUserCaps :: UserCaps (ReaderT (Bitcoin.NetworkMode, Connection) (ExceptT DBError IO))
pgUserCaps =
  UserCaps
    { createUser = pgCreateUser,
      findUser = pgFindUser,
      findUserProjectDetail = pgFindUserProjectDetail,
      findUserByName = pgFindUserByName,
      findUserPaymentAddress = pgFindUserPaymentAddress,
      findAccountPaymentAddress = pgFindAccountPaymentAddress,
      findAccountZcashIVK = pgFindAccountZcashIVK
    }

pgCreateUser :: User -> DBM UserId
pgCreateUser user' = do
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

pgFindUser :: UserId -> DBM (Maybe User)
pgFindUser (UserId uid) = do
  headMay
    <$> pquery
      userParser
      -- FIXME: password hash
      [sql| SELECT handle, recovery_email, recovery_zaddr FROM users WHERE id = ? |]
      (Only uid)

pgFindUserProjectDetail :: UserId -> ProjectId -> DBM (Maybe (User, C.UTCTime))
pgFindUserProjectDetail (UserId uid) (ProjectId pid) = do
  headMay
    <$> pquery
      ((,) <$> userParser <*> utcParser)
      -- FIXME: password hash
      [sql| SELECT u.handle, u.recovery_email, u.recovery_zaddr, p.joined_at
            FROM users u
            JOIN project_companions p on p.user_id = u.id
            WHERE u.id = ? AND p.project_id = ? |]
      (uid, pid)

pgFindUserByName :: UserName -> DBM (Maybe (UserId, User))
pgFindUserByName (UserName h) = do
  headMay
    <$> pquery
      ((,) <$> idParser UserId <*> userParser)
      -- FIXME: password hash
      [sql| SELECT id, handle, recovery_email, recovery_zaddr FROM users WHERE handle = ? |]
      (Only h)

pgFindUserPaymentAddress :: UserId -> Currency a c -> DBM (Maybe (AccountId, a))
pgFindUserPaymentAddress uid = \case
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

pgFindAccountPaymentAddress :: AccountId -> Currency a c -> DBM (Maybe a)
pgFindAccountPaymentAddress aid = \case
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

pgFindAccountZcashIVK :: AccountId -> DBM (Maybe Zcash.IVK)
pgFindAccountZcashIVK aid =
  headMay
    <$> pquery
      (zcashIvkParser)
      [sql| SELECT zcash_ivk FROM cryptocurrency_accounts
            WHERE id = ?
            AND zcash_ivk IS NOT NULL |]
      (Only $ view _AccountId aid)
