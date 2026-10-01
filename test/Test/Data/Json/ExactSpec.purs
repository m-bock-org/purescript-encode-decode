module Test.Data.Json.ExactSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Either (isLeft) as Either
import Data.Bifunctor (lmap)
import Data.Json.Codec (JsonCodec, codecInt, codecNumberText, codecRefineMaybe, decoder, encoder)
import Data.Json.Codec.Record (codecRecord)
import Data.Json.Codec.Variant (codecEnumWith) as CodecVariant
import Data.Json.Decode
  ( JsonDecodeError(..)
  , parseKeepingNumbers
  , printJsonDecodeError
  , runDecode
  , runDecodeFromString
  )
import Data.Json.Encode (runEncodeToString)
import Data.Json.Sum.Encoding (snakeCase)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as V
import Type.Proxy (Proxy(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

type Fill = { qty :: String }

-- | A fill as an exchange sends it, read keeping its numbers' text.
readFill :: String -> Either String Fill
readFill raw = do
  json <- parseKeepingNumbers raw

  lmap printJsonDecodeError (runDecode (decoder codecFill) json)

codecFill :: JsonCodec Fill
codecFill = codecRecord { qty: codecNumberText }

type Status = Variant (pendingNew :: {}, trade :: {})

spec :: Spec Unit
spec = do
  describe "Data.Json.Decode.parseKeepingNumbers with Data.Json.Codec.codecNumberText" do
    it "reads a number as the digits it was written as, trailing zeros and all" do
      readFill "{\"qty\":0.00104800}" `shouldEqual` Right { qty: "0.00104800" }

    it "keeps digits a double would lose" do
      readFill "{\"qty\":0.1000000000000000055}"
        `shouldEqual` Right { qty: "0.1000000000000000055" }

    it "writes the digits back verbatim, as a JSON number" do
      runEncodeToString (encoder codecFill) { qty: "0.00104800" }
        `shouldEqual` "{\"qty\":0.00104800}"

    it "refuses a number parsed the ordinary way, whose text is already gone" do
      runDecodeFromString (decoder codecFill) "{\"qty\":0.5}"
        `shouldSatisfy` Either.isLeft

    it "refuses text that is not a number" do
      readFill "{\"qty\":\"half\"}" `shouldSatisfy` Either.isLeft

  describe "Data.Json.Codec.codecRefineMaybe" do
    it "says what was expected when the parse misses" do
      let
        codecPositive = codecRefineMaybe "a positive whole number"
          (\n -> if n > 0 then Just n else Nothing)
          identity
          codecInt

      runDecodeFromString (decoder codecPositive) "-1"
        `shouldEqual` Left (TypeMismatch "expected a positive whole number")
      runDecodeFromString (decoder codecPositive) "3" `shouldEqual` Right 3

  describe "Data.Json.Sum.Encoding.snakeCase" do
    it "writes a label as a snake-case API does" do
      map snakeCase [ "pendingNew", "trade", "icebergRefill" ]
        `shouldEqual` [ "pending_new", "trade", "iceberg_refill" ]

    it "gives codecEnumWith both directions from the one rewrite" do
      let
        codecStatus = CodecVariant.codecEnumWith snakeCase

      runEncodeToString (encoder codecStatus) (V.inj (Proxy @"pendingNew") {} :: Status)
        `shouldEqual` "\"pending_new\""
      let
        read :: Either JsonDecodeError Status
        read = runDecodeFromString (decoder codecStatus) "\"pending_new\""

      map (V.on (Proxy @"pendingNew") (const "pendingNew") (const "other")) read
        `shouldEqual` Right "pendingNew"

-- ## Context
--
-- `codecNumberText` is for numbers that must not round - money, above
-- all. Its two ends are `parseKeepingNumbers` on the way in and
-- `JSON.rawJSON` on the way out, and the specs pin both against the
-- text: `0.00104800` keeps its zeros, and `0.1000000000000000055` keeps
-- the digits a double cannot hold.
