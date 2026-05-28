{-# LANGUAGE OverloadedStrings #-}

module Aftok.GitHubSpec where

import Aftok.GitHub
  ( Duration (..),
    parseDuration,
    parseTimeSpent,
    verifySignature,
  )
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Test.Hspec

spec :: Spec
spec = do
  describe "parseDuration" $ do
    it "parses '2h30m'" $
      parseDuration "2h30m" `shouldBe` Just (Duration 2 30)
    it "parses '2h 30m' with internal whitespace" $
      parseDuration "2h 30m" `shouldBe` Just (Duration 2 30)
    it "parses decimal hours '2.5h'" $
      parseDuration "2.5h" `shouldBe` Just (Duration 2 30)
    it "parses total minutes '150m' as 2h30m" $
      parseDuration "150m" `shouldBe` Just (Duration 2 30)
    it "parses '2 hours' long form" $
      parseDuration "2 hours" `shouldBe` Just (Duration 2 0)
    it "parses '30 minutes' long form" $
      parseDuration "30 minutes" `shouldBe` Just (Duration 0 30)
    it "trims leading and trailing whitespace" $
      parseDuration "   2h30m   " `shouldBe` Just (Duration 2 30)
    it "is case-insensitive" $
      parseDuration "2H30M" `shouldBe` Just (Duration 2 30)
    it "rejects invalid input" $
      parseDuration "not a duration" `shouldBe` Nothing
    it "rejects empty input" $
      parseDuration "" `shouldBe` Nothing
    it "rejects zero duration" $
      parseDuration "0h0m" `shouldBe` Nothing

  describe "parseTimeSpent" $ do
    it "detects 'Time Spent:' (case-insensitive)" $
      parseTimeSpent "PR description\n\nTime Spent: 2h30m\n\nthanks"
        `shouldBe` Just (Duration 2 30)
    it "detects 'time-spent:'" $
      parseTimeSpent "time-spent: 2h" `shouldBe` Just (Duration 2 0)
    it "detects 'timespent:' compact form" $
      parseTimeSpent "timespent: 45m" `shouldBe` Just (Duration 0 45)
    it "matches case-insensitively" $
      parseTimeSpent "TIME SPENT: 1h" `shouldBe` Just (Duration 1 0)
    it "honors only the first 'Time Spent:' in a multi-line body" $
      parseTimeSpent
        "Initial draft.\n\nTime Spent: 2h30m\n\nLater addendum: Time Spent: 99h"
        `shouldBe` Just (Duration 2 30)
    it "returns Nothing when no marker is present" $
      parseTimeSpent "A perfectly normal PR body with no time tracking marker."
        `shouldBe` Nothing
    it "returns Nothing when the duration after the marker is unparseable" $
      parseTimeSpent "Time Spent: forever"
        `shouldBe` Nothing

  describe "verifySignature" $ do
    -- Fixture: HMAC-SHA256 of the exact ASCII bytes
    --   {"hello":"world"}
    -- under the secret "topsecret". Hex computed once via:
    --   python3 -c "import hmac, hashlib; \
    --     print(hmac.new(b'topsecret', b'{\"hello\":\"world\"}', \
    --                    hashlib.sha256).hexdigest())"
    let fixturePayload = "{\"hello\":\"world\"}" :: ByteString
        fixtureSecret = "topsecret"
        fixtureHex = "afd00617ceb8f63e65ea5c310f06bf78c3901e7a713db532e25da26ad63c7236"
        fixtureSig = "sha256=" <> fixtureHex

    it "accepts a valid GitHub-style sha256= signature" $
      verifySignature fixtureSecret fixtureSig fixturePayload
        `shouldBe` True

    it "rejects a signature whose hex is wrong" $
      let wrong = "sha256=" <> toText (replicate 64 '0')
       in verifySignature fixtureSecret wrong fixturePayload
            `shouldBe` False

    it "rejects a signature with the wrong prefix scheme" $
      verifySignature fixtureSecret ("sha1=" <> fixtureHex) fixturePayload
        `shouldBe` False

    it "rejects a malformed (non-hex) signature" $
      verifySignature fixtureSecret "sha256=nothex" fixturePayload
        `shouldBe` False

    -- Critical regression test for the "HMAC over raw body" fix.
    --
    -- The original handler computed the HMAC over re-encoded JSON
    -- (toStrict $ A.encode payload), which produces different bytes from
    -- whatever GitHub originally signed (different whitespace, key order,
    -- nested-array spacing, etc.). This test demonstrates the divergence:
    -- parsing the fixture as JSON and re-encoding it yields a byte
    -- sequence whose HMAC does NOT match the fixture signature, even
    -- though the JSON is logically identical.
    it "would reject a re-encoded payload (proves we must HMAC raw bytes)" $ do
      let rawPayload =
            "{\"b\": 2, \"a\": 1, \"nested\": {\"x\": [1,2,3]}}" :: ByteString
          -- Hex computed once over the raw bytes above with secret "topsecret":
          rawHex = "3e0fd72b4f31df65cca479603bf5bcb663bb620a387b55afbbc02c0a8007b9fe"
          rawSig = "sha256=" <> rawHex
      -- The raw bytes verify correctly.
      verifySignature fixtureSecret rawSig rawPayload `shouldBe` True
      -- Round-tripping through Aeson changes the byte sequence...
      let parsed = A.eitherDecodeStrict rawPayload :: Either String A.Value
      case parsed of
        Left e -> expectationFailure $ "fixture must be valid JSON: " <> e
        Right v -> do
          let reencoded = LBS.toStrict (A.encode v)
          reencoded `shouldNotBe` rawPayload
          -- ...so signing the re-encoded form would fail against the
          -- original signature. This is exactly the bug the raw-bytes fix
          -- prevents.
          verifySignature fixtureSecret rawSig reencoded `shouldBe` False

main :: IO ()
main = hspec spec
