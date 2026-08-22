module Test.Data.Json.RecordSpec (spec) where

import Prelude

import Data.Argonaut.Core (Json, fromObject, jsonNull)
import Data.Argonaut.Decode.Decoders (decodeMaybe)
import Data.Either (Either(..), isLeft)
import Data.Json.Decode (decodeInt, decodeString)
import Data.Json.Decode.Record (decodeRecord, decodeRecordWithDefaults)
import Data.Json.Encode (encodeInt, encodeString)
import Data.Json.Encode.Record (encodeRecord)
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

emptyObject :: Json
emptyObject = fromObject Object.empty

objectWith :: String -> Json -> Json
objectWith key value = fromObject (Object.singleton key value)

spec :: Spec Unit
spec = do
  describe "Data.Json.Record" do
    describe "decodeRecord/encodeRecord" do
      it "round-trips a record field-by-field" do
        let
          codec = { name: decodeString, age: decodeInt }
          encoders = { name: encodeString, age: encodeInt }
          value = { name: "ada", age: 36 }
        decodeRecord codec (encodeRecord encoders value) `shouldEqual` Right value

      it "fails when a field is missing" do
        decodeRecord { name: decodeString } emptyObject `shouldSatisfy` isLeft

    describe "decodeRecordWithDefaults" do
      it "falls back to the default when a key is missing entirely" do
        decodeRecordWithDefaults { name: "anonymous" } { name: decodeString } emptyObject
          `shouldEqual` Right { name: "anonymous" }

      it "does not fall back when the key is present" do
        decodeRecordWithDefaults { name: "anonymous" } { name: decodeString } (objectWith "name" (encodeString "ada"))
          `shouldEqual` Right { name: "ada" }

      it "a present null is a real decode error, not the default, for a non-Maybe field" do
        decodeRecordWithDefaults { name: "anonymous" } { name: decodeString } (objectWith "name" jsonNull)
          `shouldSatisfy` isLeft

      it "a present null decodes as Nothing for a Maybe field, rather than falling back to the default" do
        decodeRecordWithDefaults { nickname: Just "default" } { nickname: decodeMaybe decodeString } (objectWith "nickname" jsonNull)
          `shouldEqual` Right { nickname: Nothing }
