module Test.Data.Json.RecordSpec (spec) where

import Prelude

import Data.Argonaut.Core (Json, fromObject, jsonNull)
import Data.Argonaut.Decode.Decoders (decodeMaybe)
import Data.Either (Either(..), isLeft)
import Data.Json.Decode (decodeInt, decodeString, fromFn)
import Data.Json.Decode (toFn) as Decode
import Data.Json.Decode.Record (decodeRecord, decodeRecordWithDefaults)
import Data.Json.Encode (encodeInt, encodeString)
import Data.Json.Encode (toFn) as Encode
import Data.Json.Encode.Record (encodeRecord)
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

emptyObject :: Json
emptyObject = fromObject Object.empty

objectWith :: String -> Json -> Json
objectWith key value = fromObject (Object.singleton key value)

-- | Argonaut's own `decodeMaybe` is still a plain function, so it needs
-- | wrapping to sit in a record of `DecodeJson`s - exactly the boundary
-- | `fromFn` exists for.
decodeMaybeString :: _
decodeMaybeString = fromFn (decodeMaybe (Decode.toFn decodeString))

spec :: Spec Unit
spec = do
  describe "Data.Json.Record" do
    describe "decodeRecord/encodeRecord" do
      it "round-trips a record field-by-field" do
        let
          codec = { name: decodeString, age: decodeInt }
          encoders = { name: encodeString, age: encodeInt }
          value = { name: "ada", age: 36 }
        Decode.toFn (decodeRecord codec) (Encode.toFn (encodeRecord encoders) value)
          `shouldEqual` Right value

      it "fails when a field is missing" do
        Decode.toFn (decodeRecord { name: decodeString }) emptyObject `shouldSatisfy` isLeft

    describe "decodeRecordWithDefaults" do
      it "falls back to the default when a key is missing entirely" do
        Decode.toFn (decodeRecordWithDefaults { name: "anonymous" } { name: decodeString })
          emptyObject
          `shouldEqual` Right { name: "anonymous" }

      it "does not fall back when the key is present" do
        Decode.toFn (decodeRecordWithDefaults { name: "anonymous" } { name: decodeString })
          (objectWith "name" (Encode.toFn encodeString "ada"))
          `shouldEqual` Right { name: "ada" }

      it "a present null is a real decode error, not the default, for a non-Maybe field" do
        Decode.toFn (decodeRecordWithDefaults { name: "anonymous" } { name: decodeString })
          (objectWith "name" jsonNull)
          `shouldSatisfy` isLeft

      it "a present null decodes as Nothing for a Maybe field, rather than falling back to the default" do
        Decode.toFn
          ( decodeRecordWithDefaults { nickname: Just "default" }
              { nickname: decodeMaybeString }
          )
          (objectWith "nickname" jsonNull)
          `shouldEqual` Right { nickname: Nothing }
