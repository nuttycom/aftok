{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Aftok.Database.PostgreSQL.Users
  ( createUser,
    findUser,
    findUserByName,
    findUserPaymentAddress,
    findAccountPaymentAddress,
    findAccountZcashIVK,
  )
where

import Aftok.Currency (Currency (..))
import qualified Aftok.Currency.Zcash as Zcash
import Aftok.Database ()
import Aftok.Database.PostgreSQL.Types
  ( DBM,
    askNetworkMode,
    bitcoinAddressParser,
    idParser,
    pinsert,
    pquery,
    zcashAddressParser,
    zcashIvkParser,
  )
import Aftok.Types
import Control.Lens
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.SqlQQ
  ( sql,
  )
import Safe (headMay)
import Prelude hiding (null)

userParser :: RowParser User
userParser =
  User
    <$> (UserName <$> field)
    <*> ( (maybe empty pure =<< fmap (RecoverByEmail . Email) <$> field)
            <|> (maybe empty pure =<< fmap (RecoverByZAddr . Zcash.Address) <$> field)
        )

createUser :: User -> DBM UserId
createUser user' = do
  pinsert
    UserId
    [sql| INSERT INTO users (handle, recovery_email, recovery_zaddr)
          VALUES (?, ?, ?) RETURNING id |]
    ( user' ^. (username . _UserName),
      user' ^? userAccountRecovery . _RecoverByEmail . _Email,
      user' ^? userAccountRecovery . _RecoverByZAddr . Zcash._Address
    )

findUser :: UserId -> DBM (Maybe User)
findUser (UserId uid) = do
  headMay
    <$> pquery
      userParser
      [sql| SELECT handle, recovery_email, recovery_zaddr FROM users WHERE id = ? |]
      (Only uid)

findUserByName :: UserName -> DBM (Maybe (UserId, User))
findUserByName (UserName h) = do
  headMay
    <$> pquery
      ((,) <$> idParser UserId <*> userParser)
      [sql| SELECT id, handle, recovery_email, recovery_zaddr FROM users WHERE handle = ? |]
      (Only h)

findUserPaymentAddress :: UserId -> Currency a c -> DBM (Maybe a)
findUserPaymentAddress uid = \case
  BTC -> do
    mode <- askNetworkMode
    headMay
      <$> pquery
        (bitcoinAddressParser mode)
        [sql| SELECT btc_addr FROM cryptocurrency_accounts
            WHERE user_id = ?
            AND currency = 'BTC'
            AND is_primary = true |]
        (Only $ view _UserId uid)
  ZEC -> do
    headMay
      <$> pquery
        (zcashAddressParser)
        [sql| SELECT zcash_addr FROM cryptocurrency_accounts
            WHERE user_id = ?
            AND currency = 'ZEC'
            AND is_primary = true |]
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
