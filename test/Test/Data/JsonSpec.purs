module Test.Data.JsonSpec (spec) where

import Prelude

import Data.Argonaut.Core (fromBoolean, fromNumber)
import Data.Either (Either(..), isLeft)
import Data.Json.Decode (decodeBoolean, decodeInt, decodeNumber, decodeString, decodeArray, decodeObject, decodeMapFromObject, decodeNativeTuple2)
import Data.Json.Encode (encodeInt, encodeString, encodeArray, encodeObject, encodeMapToObject, encodeNativeTuple2)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import Data.Json.Decode (JsonDecodeError(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- | A deliberately narrow key decoder for the Map tests - accepts only
-- | "a"/"b", so a key outside that set fails the whole decode.
decodeKnownKey :: String -> Either JsonDecodeError String
decodeKnownKey k
  | k == "a" || k == "b" = Right k
  | otherwise = Left (TypeMismatch ("unknown key: " <> k))

spec :: Spec Unit
spec = do
  describe "Data.Json" do
    describe "primitives" do
      it "round-trips a string" do
        decodeString (encodeString "hello") `shouldEqual` Right "hello"

      it "round-trips a whole-number Int" do
        decodeInt (encodeInt 42) `shouldEqual` Right 42

      it "rejects a non-integer JSON number as Int" do
        decodeInt (fromNumber 1.5) `shouldSatisfy` isLeft

      it "decodes numbers and booleans" do
        decodeNumber (fromNumber 3.5) `shouldEqual` Right 3.5
        decodeBoolean (fromBoolean true) `shouldEqual` Right true

    describe "array" do
      it "round-trips an array" do
        decodeArray decodeInt (encodeArray encodeInt [ 1, 2, 3 ]) `shouldEqual` Right [ 1, 2, 3 ]

    describe "object" do
      it "round-trips an object, keys unchanged" do
        let value = Object.fromFoldable [ "a" /\ 1, "b" /\ 2 ]
        decodeObject decodeInt (encodeObject encodeInt value) `shouldEqual` Right value

    describe "map" do
      it "round-trips a Map as a JSON object" do
        let value = Map.fromFoldable [ "a" /\ 1, "b" /\ 2 ]
        decodeMapFromObject pure decodeInt (encodeMapToObject identity encodeInt value)
          `shouldEqual` Right value

      -- Unlike decodeObject, a Map's keys go through a decoder of their
      -- own - so a bad key has to fail the whole decode, not just get
      -- passed through the way a raw object's keys are.
      it "fails when a key doesn't decode" do
        decodeMapFromObject decodeKnownKey decodeString
          (encodeMapToObject identity encodeString (Map.fromFoldable [ "nope" /\ "x" ]))
          `shouldSatisfy` isLeft

      it "succeeds when every key decodes" do
        let value = Map.fromFoldable [ "a" /\ "x", "b" /\ "y" ]
        decodeMapFromObject decodeKnownKey decodeString
          (encodeMapToObject identity encodeString value) `shouldEqual` Right value

    describe "tuple" do
      it "round-trips a 2-tuple as a JSON array" do
        decodeNativeTuple2 decodeString decodeInt (encodeNativeTuple2 encodeString encodeInt ("a" /\ 1)) `shouldEqual` Right ("a" /\ 1)

      it "rejects an array of the wrong length" do
        decodeNativeTuple2 decodeString decodeInt (encodeArray encodeInt [ 1, 2, 3 ]) `shouldSatisfy` isLeft
