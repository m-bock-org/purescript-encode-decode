module Test.Data.Json.RefineSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Json.Codec (codecInt, codecRefineMaybe, decoder, encoder)
import Data.Json.Codec.Variant (codecEnumWith) as CodecVariant
import Data.Json.Decode (JsonDecodeError(..), runDecodeFromString)
import Data.Json.Encode (runEncodeToString)
import Data.Json.Sum.Encoding (snakeCase)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as V
import Type.Proxy (Proxy(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Status = Variant (pendingNew :: {}, trade :: {})

spec :: Spec Unit
spec = do
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
