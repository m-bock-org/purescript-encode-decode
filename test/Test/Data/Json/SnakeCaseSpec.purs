module Test.Data.Json.SnakeCaseSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Json.Codec (decoder, encoder)
import Data.Json.Codec.Variant (codecEnumWith) as CodecVariant
import Data.Json.Decode (JsonDecodeError, runDecodeFromString)
import Data.Json.Encode (runEncodeToString)
import Data.Json.Sum.Encoding (snakeCase)
import Data.Variant (Variant)
import Data.Variant as V
import Type.Proxy (Proxy(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

type Status = Variant (pendingNew :: {}, trade :: {})

spec :: Spec Unit
spec = do
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
