{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Quixotic.Json where

import ClassyPrelude

import Control.Lens hiding ((.=))
import Data.Aeson 
import Data.Aeson.Types
import Data.Data
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as C

import Quixotic

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

data Version = Version { majorVersion :: Word8
                       , minorVersion :: Word8
                       , trivialVersion :: Word8 
                       } deriving (Typeable, Data)

printVersion :: Version -> Text
printVersion Version{..} = intercalate "." (fmap tshow [majorVersion, minorVersion, trivialVersion])

versionParser :: P.Parser Version
versionParser = Version <$> P.decimal <*> (P.char '.' >> P.decimal) <*> (P.char '.' >> P.decimal)

versioned :: Version -> Value -> Value
versioned ver v = object [ "schemaVersion" .= printVersion ver
                         , "value" .= v ]

version :: QuasiQuoter 
version = QuasiQuoter { quoteExp = quoteVersionExp
                      , quotePat = error "Pattern quasiquotation of versions not supported."
                      , quoteType = error "Type quasiquotation of versions not supported."
                      , quoteDec = error "Dec quasiquotation of versions not supported."
                      }


quoteVersionExp :: String -> TH.Q TH.Exp
quoteVersionExp s = do
  v <- either (fail . show) pure $ P.parseOnly versionParser (C.pack s)
  dataToExpQ (const Nothing) v

unversion :: (Version -> Value -> Parser a) -> Value -> Parser a
unversion f (Object v) = do
  vers <- v .: "schemaVersion"
  vers' <- either (\_ -> mzero) pure $ P.parseOnly versionParser (encodeUtf8 vers)
  value <- v .: "value"
  f vers' value
unversion _ _ = mzero

projectJSON :: Project -> Value
projectJSON p = 
  object [ "projectName"    .= (p ^. projectName)
         , "inceptionDate"  .= (p ^. inceptionDate)
         , "initiator"      .= (p ^. (initiator._UserId)) ]

