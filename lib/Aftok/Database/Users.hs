{-# LANGUAGE LiberalTypeSynonyms #-}

module Aftok.Database.Users where

import Aftok.Currency (Currency)
import qualified Aftok.Currency.Zcash as Zcash
import Aftok.Types
  ( AccountId,
    ProjectId,
    User,
    UserId,
    UserName,
  )
import qualified Data.Thyme.Clock as C

data UserCaps (m :: Type -> Type) = UserCaps
  { createUser :: User -> m UserId,
    findUser :: UserId -> m (Maybe User),
    findUserProjectDetail :: UserId -> ProjectId -> m (Maybe (User, C.UTCTime)),
    findUserByName :: UserName -> m (Maybe (UserId, User)),
    findUserPaymentAddress :: forall a c. UserId -> Currency a c -> m (Maybe (AccountId, a)),
    findAccountPaymentAddress :: forall a c. AccountId -> Currency a c -> m (Maybe a),
    findAccountZcashIVK :: AccountId -> m (Maybe Zcash.IVK)
  }

hoistUserCaps :: (forall a. m a -> n a) -> UserCaps m -> UserCaps n
hoistUserCaps nt c =
  UserCaps
    { createUser = nt . createUser c,
      findUser = nt . findUser c,
      findUserProjectDetail = (nt .) . findUserProjectDetail c,
      findUserByName = nt . findUserByName c,
      findUserPaymentAddress = (nt .) . findUserPaymentAddress c,
      findAccountPaymentAddress = (nt .) . findAccountPaymentAddress c,
      findAccountZcashIVK = nt . findAccountZcashIVK c
    }
