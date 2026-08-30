module Test.Data.JsonSpec (spec) where

import Prelude

import Data.Argonaut.Core (fromBoolean, fromNumber)
import Data.Either (Either(..), isLeft)
import Data.Json.Decode
  ( DecodeJson
  , JsonDecodeError(..)
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
import Data.Json.Decode (decodeAttempt, decodeFail, decodeFromString, decodeObjectWithKey, decodeRefine) as D
import Data.Json.Decode.Record (decodeRecord)
import Data.Json.Decode.Tuple (decodeTuple)
import Data.Json.Encode
  ( EncodeJson
  , encodeArray
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
import Data.Json.Encode (encodeDispatch, encodeMaybe, encodeNull, encodeToString, encoded) as E
import Data.Json.Encode.Record (encodeRecord)
import Data.Json.Encode.Tuple (encodeTuple)
import Data.Map as Map
import Data.Maybe (Maybe(..))
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

    -- The encoder on its own, asserted against exact wire text rather than
    -- through a round-trip: a round-trip passes just as happily when both
    -- sides are wrong in the same way.
    describe "n-tuple encoder" do
      it "writes a 2-tuple as a flat 2-element array" do
        stringify (Encode.toFn (encodeTuple (encodeString /\ encodeInt)) ("a" /\ 1))
          `shouldEqual` """["a",1]"""

      it "writes a 3-tuple as a flat 3-element array, in order" do
        stringify
          ( Encode.toFn (encodeTuple (encodeString /\ encodeInt /\ encodeBoolean))
              ("a" /\ 1 /\ true)
          )
          `shouldEqual` """["a",1,true]"""

      it "writes a 5-tuple flat, not right-nested" do
        stringify
          ( Encode.toFn
              (encodeTuple (encodeInt /\ encodeInt /\ encodeInt /\ encodeInt /\ encodeString))
              (1 /\ 2 /\ 3 /\ 4 /\ "five")
          )
          `shouldEqual` """[1,2,3,4,"five"]"""

      it "nests only where a nested tuple codec says so" do
        stringify
          ( Encode.toFn
              (encodeTuple (encodeInt /\ encodeTuple (encodeString /\ encodeBoolean)))
              (1 /\ ("a" /\ true))
          )
          `shouldEqual` """[1,["a",true]]"""

      it "applies each position's own encoder, not one shared encoder" do
        stringify (Encode.toFn (encodeTuple (encodeString /\ encodeString)) ("1" /\ "2"))
          `shouldEqual` """["1","2"]"""
        stringify (Encode.toFn (encodeTuple (encodeInt /\ encodeInt)) (1 /\ 2))
          `shouldEqual` """[1,2]"""

      it "agrees with encodeNativeTuple2 at two positions" do
        let value = "a" /\ 1
        stringify (Encode.toFn (encodeTuple (encodeString /\ encodeInt)) value)
          `shouldEqual`
            stringify (Encode.toFn (encodeNativeTuple2 encodeString encodeInt) value)

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

  describe "decodeFromString" do
    it "parses and decodes in one step" do
      D.decodeFromString (decodeRecord { a: decodeInt }) """{"a":1}"""
        `shouldEqual` Right { a: 1 }

    it "reports malformed JSON as a decode error, not a crash" do
      (D.decodeFromString decodeInt "{not json" :: Either JsonDecodeError Int)
        `shouldSatisfy` isLeft

    it "still reports a decode failure on well-formed JSON" do
      (D.decodeFromString decodeInt """"nope"""" :: Either JsonDecodeError Int)
        `shouldEqual` Left (TypeMismatch "Number")

  describe "decodeFail" do
    it "fails with the error it is given" do
      Decode.toFn (D.decodeFail (TypeMismatch "on purpose") :: DecodeJson Int)
        (Encode.toFn encodeInt 1)
        `shouldEqual` Left (TypeMismatch "on purpose")

    it "lets a bind chain reject a tag it does not know" do
      let
        dec = decodeRecord { kind: decodeString } >>= case _ of
          { kind: "int" } -> decodeRecord { value: decodeInt }
          { kind } -> D.decodeFail (AtKey "kind" (TypeMismatch kind))
      D.decodeFromString dec """{"kind":"int","value":7}""" `shouldEqual` Right { value: 7 }
      (D.decodeFromString dec """{"kind":"other","value":7}""" :: Either JsonDecodeError { value :: Int })
        `shouldEqual` Left (AtKey "kind" (TypeMismatch "other"))

  describe "decodeRefine" do
    it "narrows a decoded value" do
      let dec = D.decodeRefine (\n -> if n > 0 then Right n else Left (TypeMismatch "positive")) decodeInt
      D.decodeFromString dec "3" `shouldEqual` Right 3

    it "fails when the narrowing rejects" do
      let dec = D.decodeRefine (\n -> if n > 0 then Right n else Left (TypeMismatch "positive")) decodeInt
      D.decodeFromString dec "-3" `shouldEqual` Left (TypeMismatch "positive")

    it "does not run the refinement when the decode itself failed" do
      let dec = D.decodeRefine (\_ -> Left (TypeMismatch "refinement ran")) decodeInt
      (D.decodeFromString dec """"nope"""" :: Either JsonDecodeError Int)
        `shouldEqual` Left (TypeMismatch "Number")

  describe "decodeAttempt" do
    it "reports a success as Right without failing" do
      D.decodeFromString (D.decodeAttempt decodeInt) "1" `shouldEqual` Right (Right 1)

    it "captures a failure as a value instead of aborting" do
      D.decodeFromString (D.decodeAttempt decodeInt) """"nope""""
        `shouldEqual` Right (Left (TypeMismatch "Number"))

    it "lets a sibling field survive an unreadable one" do
      let dec = decodeRecord { error: decodeString, result: D.decodeAttempt decodeInt }
      D.decodeFromString dec """{"error":"why","result":"nope"}"""
        `shouldEqual` Right { error: "why", result: Left (TypeMismatch "Number") }

  describe "decodeObjectWithKey" do
    it "gives each value a decoder chosen by its own key" do
      let dec = D.decodeObjectWithKey \k -> map (\n -> k <> "=" <> show n) decodeInt
      D.decodeFromString dec """{"a":1,"b":2}"""
        `shouldEqual` Right (Object.fromFoldable [ "a" /\ "a=1", "b" /\ "b=2" ])

    it "fails on the first value its own key's decoder rejects" do
      let dec = D.decodeObjectWithKey \k -> if k == "a" then decodeInt else D.decodeFail (TypeMismatch k)
      (D.decodeFromString dec """{"a":1,"b":2}""" :: Either JsonDecodeError (Object.Object Int))
        `shouldEqual` Left (TypeMismatch "b")

  describe "encodeDispatch" do
    it "lets each case of a sum choose its own shape" do
      let
        enc = E.encodeDispatch case _ of
          Left n -> E.encoded (encodeRecord { tag: encodeString, value: encodeInt })
            { tag: "ok", value: n }
          Right e -> E.encoded (encodeRecord { tag: encodeString, reason: encodeString })
            { tag: "err", reason: e }
      E.encodeToString enc (Left 1 :: Either Int String) `shouldEqual` """{"value":1,"tag":"ok"}"""
      E.encodeToString enc (Right "bad" :: Either Int String)
        `shouldEqual` """{"tag":"err","reason":"bad"}"""

  describe "encodeToString" do
    it "encodes straight to a document" do
      E.encodeToString (encodeRecord { a: encodeInt }) { a: 1 } `shouldEqual` """{"a":1}"""

    it "round-trips with decodeFromString" do
      let codec = { enc: encodeRecord { a: encodeInt }, dec: decodeRecord { a: decodeInt } }
      D.decodeFromString codec.dec (E.encodeToString codec.enc { a: 42 })
        `shouldEqual` Right { a: 42 }

  describe "encodeNull / encodeMaybe" do
    it "writes null for Nothing and the value for Just" do
      E.encodeToString (E.encodeMaybe encodeInt) (Just 1) `shouldEqual` "1"
      E.encodeToString (E.encodeMaybe encodeInt) (Nothing :: Maybe Int) `shouldEqual` "null"

    it "nests inside a record like any other field encoder" do
      E.encodeToString (encodeRecord { a: E.encodeMaybe encodeInt }) { a: Nothing }
        `shouldEqual` """{"a":null}"""

    it "encodeNull ignores whatever it is given" do
      E.encodeToString (E.encodeNull :: EncodeJson String) "anything" `shouldEqual` "null"
