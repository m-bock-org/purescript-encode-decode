module Test.Data.Json.RecordSpec (spec) where

import Prelude

import Data.Argonaut.Core (Json, fromObject, jsonNull)
import Data.Argonaut.Decode.Decoders (decodeMaybe)
import Data.Either (Either(..))
import Data.Either (isLeft) as Either
import Data.Json.Decode (DecodeJson, decodeInt, decodeString, fromFn)
import Data.Json.Decode (runDecode) as Decode
import Data.Json.Decode.Record (decodeRecord, decodeRecordWithDefaults)
import Data.Json.Encode (encodeInt, encodeString)
import Data.Json.Encode (runEncode) as Encode
import Data.Json.Encode.Record (encodeRecord)
import Data.Maybe (Maybe(..))
import Foreign.Object as Obj
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-- | Private.
emptyObject :: Json
emptyObject = fromObject Obj.empty

-- | Private. Used only by `spec`.
objectWith :: String -> Json -> Json
objectWith key value = fromObject (Obj.singleton key value)

-- | Argonaut's own `decodeMaybe` is still a plain function, so it needs
-- | wrapping to sit in a record of `DecodeJson`s - exactly the boundary
-- | Private.
decodeMaybeString :: DecodeJson (Maybe String)
decodeMaybeString = fromFn (decodeMaybe (Decode.runDecode decodeString))

-- | Uses `objectWith`.
spec :: Spec Unit
spec = do
  describe "Data.Json.Record" do
    describe "decodeRecord/encodeRecord" do
      it "round-trips a record field-by-field" do
        let
          codec = { name: decodeString, age: decodeInt }
          encoders = { name: encodeString, age: encodeInt }
          value = { name: "ada", age: 36 }
        Decode.runDecode (decodeRecord codec) (Encode.runEncode (encodeRecord encoders) value)
          `shouldEqual` Right value

      it "fails when a field is missing" do
        Decode.runDecode (decodeRecord { name: decodeString }) emptyObject `shouldSatisfy` Either.isLeft

    describe "decodeRecordWithDefaults" do
      it "falls back to the default when a key is missing entirely" do
        Decode.runDecode (decodeRecordWithDefaults { name: "anonymous" } { name: decodeString })
          emptyObject
          `shouldEqual` Right { name: "anonymous" }

      it "does not fall back when the key is present" do
        Decode.runDecode (decodeRecordWithDefaults { name: "anonymous" } { name: decodeString })
          (objectWith "name" (Encode.runEncode encodeString "ada"))
          `shouldEqual` Right { name: "ada" }

      it "a present null is a real decode error, not the default, for a non-Maybe field" do
        Decode.runDecode (decodeRecordWithDefaults { name: "anonymous" } { name: decodeString })
          (objectWith "name" jsonNull)
          `shouldSatisfy` Either.isLeft

      it "a present null decodes as Nothing for a Maybe field, rather than falling back to the default" do
        Decode.runDecode
          ( decodeRecordWithDefaults { nickname: Just "default" }
              { nickname: decodeMaybeString }
          )
          (objectWith "nickname" jsonNull)
          `shouldEqual` Right { nickname: Nothing }
