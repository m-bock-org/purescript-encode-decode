module Test.Data.JsonSpec (spec) where

import Prelude

import Data.Argonaut.Core (fromBoolean, fromNumber)
import Data.Either (Either(..), isLeft)
import Data.Json.Decode (decodeBoolean, decodeInt, decodeNumber, decodeString, decodeArray, decodeObject, decodeNativeTuple2)
import Data.Json.Encode (encodeInt, encodeString, encodeArray, encodeObject, encodeNativeTuple2)
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

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

    describe "tuple" do
      it "round-trips a 2-tuple as a JSON array" do
        decodeNativeTuple2 decodeString decodeInt (encodeNativeTuple2 encodeString encodeInt ("a" /\ 1)) `shouldEqual` Right ("a" /\ 1)

      it "rejects an array of the wrong length" do
        decodeNativeTuple2 decodeString decodeInt (encodeArray encodeInt [ 1, 2, 3 ]) `shouldSatisfy` isLeft
