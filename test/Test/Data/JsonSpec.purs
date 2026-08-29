module Test.Data.JsonSpec (spec) where

import Prelude

import Data.Argonaut.Core (fromBoolean, fromNumber)
import Data.Either (Either(..), isLeft)
import Data.Json.Decode
  ( JsonDecodeError(..)
  , decodeArray
  , decodeBoolean
  , decodeInt
  , decodeMapFromObject
  , decodeNativeTuple2
  , decodeNumber
  , decodeObject
  , decodeString
  , decodeTupleArrayFromObject
  )
import Data.Json.Decode (toFn) as Decode
import Data.Json.Decode.Tuple (decodeTuple)
import Data.Json.Encode
  ( encodeArray
  , encodeBoolean
  , encodeInt
  , encodeMapToObject
  , encodeNativeTuple2
  , encodeObject
  , encodeString
  , encodeTupleArrayToObject
  , stringify
  )
import Data.Json.Encode (toFn) as Encode
import Data.Json.Encode.Tuple (encodeTuple)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
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
        Decode.toFn decodeString (Encode.toFn encodeString "hello") `shouldEqual` Right "hello"

      it "round-trips a whole-number Int" do
        Decode.toFn decodeInt (Encode.toFn encodeInt 42) `shouldEqual` Right 42

      it "rejects a non-integer JSON number as Int" do
        Decode.toFn decodeInt (fromNumber 1.5) `shouldSatisfy` isLeft

      it "decodes numbers and booleans" do
        Decode.toFn decodeNumber (fromNumber 3.5) `shouldEqual` Right 3.5
        Decode.toFn decodeBoolean (fromBoolean true) `shouldEqual` Right true

    describe "array" do
      it "round-trips an array" do
        Decode.toFn (decodeArray decodeInt) (Encode.toFn (encodeArray encodeInt) [ 1, 2, 3 ])
          `shouldEqual` Right [ 1, 2, 3 ]

    describe "object" do
      it "round-trips an object, keys unchanged" do
        let value = Object.fromFoldable [ "a" /\ 1, "b" /\ 2 ]
        Decode.toFn (decodeObject decodeInt) (Encode.toFn (encodeObject encodeInt) value)
          `shouldEqual` Right value

    describe "map" do
      it "round-trips a Map as a JSON object" do
        let value = Map.fromFoldable [ "a" /\ 1, "b" /\ 2 ]
        Decode.toFn (decodeMapFromObject pure decodeInt)
          (Encode.toFn (encodeMapToObject identity encodeInt) value)
          `shouldEqual` Right value

      -- Unlike decodeObject, a Map's keys go through a decoder of their
      -- own - so a bad key has to fail the whole decode, not just get
      -- passed through the way a raw object's keys are.
      it "fails when a key doesn't decode" do
        Decode.toFn (decodeMapFromObject decodeKnownKey decodeString)
          ( Encode.toFn (encodeMapToObject identity encodeString)
              (Map.fromFoldable [ "nope" /\ "x" ])
          )
          `shouldSatisfy` isLeft

      it "succeeds when every key decodes" do
        let value = Map.fromFoldable [ "a" /\ "x", "b" /\ "y" ]
        Decode.toFn (decodeMapFromObject decodeKnownKey decodeString)
          (Encode.toFn (encodeMapToObject identity encodeString) value)
          `shouldEqual` Right value

    -- The shape a Map reduces to, on its own: an object whose keys are
    -- data but whose value is an association list on both sides.
    describe "tuple-array object" do
      it "round-trips an association list as a JSON object" do
        let value = [ "a" /\ 1, "b" /\ 2 ]
        Decode.toFn (decodeTupleArrayFromObject pure decodeInt)
          (Encode.toFn (encodeTupleArrayToObject identity encodeInt) value)
          `shouldEqual` Right value

      it "fails when a key doesn't decode" do
        Decode.toFn (decodeTupleArrayFromObject decodeKnownKey decodeInt)
          (Encode.toFn (encodeTupleArrayToObject identity encodeInt) [ "nope" /\ 1 ])
          `shouldSatisfy` isLeft

    describe "tuple" do
      it "round-trips a 2-tuple as a JSON array" do
        Decode.toFn (decodeNativeTuple2 decodeString decodeInt)
          (Encode.toFn (encodeNativeTuple2 encodeString encodeInt) ("a" /\ 1))
          `shouldEqual` Right ("a" /\ 1)

      it "rejects an array of the wrong length" do
        Decode.toFn (decodeNativeTuple2 decodeString decodeInt)
          (Encode.toFn (encodeArray encodeInt) [ 1, 2, 3 ])
          `shouldSatisfy` isLeft

    describe "n-tuple" do
      it "round-trips a 2-tuple" do
        let value = "a" /\ 1
        Decode.toFn (decodeTuple (decodeString /\ decodeInt))
          (Encode.toFn (encodeTuple (encodeString /\ encodeInt)) value)
          `shouldEqual` Right value

      it "round-trips a 3-tuple" do
        let value = "a" /\ 1 /\ true
        Decode.toFn (decodeTuple (decodeString /\ decodeInt /\ decodeBoolean))
          (Encode.toFn (encodeTuple (encodeString /\ encodeInt /\ encodeBoolean)) value)
          `shouldEqual` Right value

      it "round-trips a 5-tuple" do
        let value = 1 /\ 2 /\ 3 /\ 4 /\ "five"
        Decode.toFn
          (decodeTuple (decodeInt /\ decodeInt /\ decodeInt /\ decodeInt /\ decodeString))
          ( Encode.toFn
              (encodeTuple (encodeInt /\ encodeInt /\ encodeInt /\ encodeInt /\ encodeString))
              value
          )
          `shouldEqual` Right value

      it "encodes to a flat array, not a nested one" do
        stringify (Encode.toFn (encodeTuple (encodeInt /\ encodeInt /\ encodeInt)) (1 /\ 2 /\ 3))
          `shouldEqual` "[1,2,3]"

      it "nests, since a tuple of encoders is itself encoder-shaped" do
        let value = 1 /\ ("a" /\ true)
        Decode.toFn (decodeTuple (decodeInt /\ decodeTuple (decodeString /\ decodeBoolean)))
          ( Encode.toFn
              (encodeTuple (encodeInt /\ encodeTuple (encodeString /\ encodeBoolean)))
              value
          )
          `shouldEqual` Right value

      it "rejects an array shorter than the tuple" do
        Decode.toFn (decodeTuple (decodeInt /\ decodeInt /\ decodeInt))
          (Encode.toFn (encodeArray encodeInt) [ 1, 2 ])
          `shouldSatisfy` isLeft

      it "rejects an array longer than the tuple, rather than dropping the surplus" do
        Decode.toFn (decodeTuple (decodeInt /\ decodeInt))
          (Encode.toFn (encodeArray encodeInt) [ 1, 2, 3 ])
          `shouldSatisfy` isLeft

      it "reports the failing position" do
        Decode.toFn (decodeTuple (decodeInt /\ decodeString /\ decodeInt))
          (Encode.toFn (encodeTuple (encodeInt /\ encodeInt /\ encodeInt)) (1 /\ 2 /\ 3))
          `shouldEqual` Left (AtIndex 1 (TypeMismatch "String"))
