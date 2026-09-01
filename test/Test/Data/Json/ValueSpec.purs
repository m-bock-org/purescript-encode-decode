module Test.Data.Json.ValueSpec (spec) where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Json.Value (JsonData(..), fromJson, toJson)
import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Data.Json.Value" do
  it "reads every shape" do
    fromJson (Argonaut.jsonNull) `shouldEqual` JsonNull
    fromJson (Argonaut.fromBoolean true) `shouldEqual` JsonBoolean true
    fromJson (Argonaut.fromNumber 1.5) `shouldEqual` JsonNumber 1.5
    fromJson (Argonaut.fromString "a") `shouldEqual` JsonString "a"

  it "reads a nested payload" do
    case jsonParser """{"a":[1,"b"],"c":{"d":null}}""" of
      Left err -> shouldEqual err ""
      Right json ->
        fromJson json `shouldEqual`
          JsonObject
            ( Object.fromFoldable
                [ "a" /\ JsonArray [ JsonNumber 1.0, JsonString "b" ]
                , "c" /\ JsonObject (Object.fromFoldable [ "d" /\ JsonNull ])
                ]
            )

  it "writes an asserted Int as a plain number" do
    Argonaut.stringify (toJson (JsonInt 42)) `shouldEqual` "42"

  it "round-trips the grammar subset - all fromJson can produce" do
    case jsonParser """{"a":[1,"b"],"c":{"d":null},"e":true}""" of
      Left err -> shouldEqual err ""
      -- Json has no Show, so compare the rendered form: it is also
      -- the thing a round trip is supposed to preserve.
      Right json ->
        Argonaut.stringify (toJson (fromJson json)) `shouldEqual` Argonaut.stringify json
