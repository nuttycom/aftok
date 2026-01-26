{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aftok.Password
  ( PasswordHash (..),
    hashPassword,
    verifyPassword,
  )
where

import qualified Crypto.KDF.BCrypt as BCrypt

-- | A bcrypt password hash
newtype PasswordHash = PasswordHash {unPasswordHash :: ByteString}
  deriving (Show, Eq)

-- | Hash a password using bcrypt with a cost factor of 12
hashPassword :: ByteString -> IO PasswordHash
hashPassword pwd = PasswordHash <$> BCrypt.hashPassword 12 pwd

-- | Verify a password against a stored hash
verifyPassword :: ByteString -> PasswordHash -> Bool
verifyPassword pwd (PasswordHash hash) = BCrypt.validatePassword pwd hash
