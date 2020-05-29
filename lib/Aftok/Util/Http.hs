module Aftok.Util.Http where



import           Data.Attoparsec.ByteString
import           Data.ByteString                ( split )
import qualified Data.ByteString.Base64        as B64

type AuthHeader = (Text, ByteString)

authHeaderParser :: Parser AuthHeader
authHeaderParser = do
  let isBase64Char w =
        (w >= 47 && w <= 57)
          || (w >= 64 && w <= 90)
          || (w >= 97 && w <= 122)
          || (w == 43 || w == 61)
  b64     <- string "Basic " *> takeWhile1 isBase64Char
  decoded <- either fail pure $ B64.decode b64
  case split 58 decoded of
    [uname, pwd] -> pure (decodeUtf8 uname, pwd)
    _ ->
      fail "Could not unpack auth header into username and password components"

