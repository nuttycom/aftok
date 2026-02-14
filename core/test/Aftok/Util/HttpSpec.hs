{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Aftok.Util.HttpSpec where

import Aftok.Util.Http
import Data.Attoparsec.ByteString
import Test.Hspec

spec :: Spec
spec = do
  describe "HTTP Basic header parsing" $ do
    it "parses the Basic auth header" $ do
      let rawHeader = "Basic bnV0dHljb206a2pudGVzdA=="
      (parseOnly authHeaderParser rawHeader)
        `shouldBe` (Right ("nuttycom", "kjntest"))

main :: IO ()
main = hspec spec
