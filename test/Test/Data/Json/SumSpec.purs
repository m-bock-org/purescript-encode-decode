module Test.Data.Json.SumSpec (spec) where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Generic.Rep (class Generic)
import Data.Json.Decode (Json, JsonDecodeError(..), decodeInt, decodeString, jsonParser)
import Data.Json.Decode.Sum (decodeEnum, decodeEnumWith, decodeSum, decodeSumWith)
import Data.Json.Encode (encodeInt, encodeString, stringify)
import Data.Json.Encode.Sum (encodeEnum, encodeEnumWith, encodeSum, encodeSumWith)
import Data.Json.Sum.Encoding (Encoding(..), lowerFirst)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

data Shape
  = Blob
  | Circle Int
  | Rect Int String

derive instance Generic Shape _
derive instance Eq Shape
instance Show Shape where
  show s = genericShow s

encodeShape :: Shape -> Json
encodeShape = encodeSum
  { "Blob": unit
  , "Circle": encodeInt
  , "Rect": encodeInt /\ encodeString
  }

decodeShape :: Json -> Either JsonDecodeError Shape
decodeShape = decodeSum
  { "Blob": unit
  , "Circle": decodeInt
  , "Rect": decodeInt /\ decodeString
  }

data Mode = Simulation | Realisation

derive instance Generic Mode _
derive instance Eq Mode
instance Show Mode where
  show m = genericShow m

-- | The wire format tick-duck's journal already uses on disk:
-- | `{"kind": "tradeTick", "entry": {...}}`.
journalEncoding :: Encoding
journalEncoding = EncodeTagged
  { tagKey: "kind"
  , valuesKey: "entry"
  , unwrapSingleArguments: true
  , omitEmptyArguments: true
  , mapTag: lowerFirst
  }

encodeShapeJournalStyle :: Shape -> Json
encodeShapeJournalStyle = encodeSumWith journalEncoding
  { "Blob": unit
  , "Circle": encodeInt
  , "Rect": encodeInt /\ encodeString
  }

decodeShapeJournalStyle :: Json -> Either JsonDecodeError Shape
decodeShapeJournalStyle = decodeSumWith journalEncoding
  { "Blob": unit
  , "Circle": decodeInt
  , "Rect": decodeInt /\ decodeString
  }

-- | What `decodeSum` reports when every constructor was tried and none
-- | claimed the tag - see `Data.Json.Decode.Sum.Err`.
noMatch :: JsonDecodeError
noMatch = TypeMismatch "no matching constructor"

unsafeParse :: String -> Json
unsafeParse s = case jsonParser s of
  Right json -> json
  Left _ -> encodeString ("SumSpec fixture: invalid JSON literal " <> s)

spec :: Spec Unit
spec = do
  describe "Data.Json.Sum" do
    describe "round-trip, default encoding" do
      it "round-trips a nullary constructor" do
        decodeShape (encodeShape Blob) `shouldEqual` Right Blob

      it "round-trips a single-argument constructor" do
        decodeShape (encodeShape (Circle 3)) `shouldEqual` Right (Circle 3)

      it "round-trips a multi-argument constructor" do
        decodeShape (encodeShape (Rect 2 "wide")) `shouldEqual` Right (Rect 2 "wide")

    describe "wire format" do
      it "tags with the constructor name and wraps arguments in an array" do
        stringify (encodeShape (Circle 3)) `shouldEqual` """{"tag":"Circle","values":[3]}"""

      it "encodes a nullary constructor with an empty values array" do
        stringify (encodeShape Blob) `shouldEqual` """{"tag":"Blob","values":[]}"""

    describe "errors" do
      -- The distinction that makes sum decoding correct: a tag that
      -- matched but whose payload is broken must report that payload
      -- error, not fall through and end up as "no constructor
      -- matched" - which would hide the real cause.
      it "reports the payload error when a matching case has a bad payload" do
        let json = unsafeParse """{"tag":"Circle","values":["not-an-int"]}"""
        -- Asserted as "not the no-match error" rather than against
        -- argonaut's exact wording for a bad Int, which this library
        -- passes through untouched and doesn't control.
        decodeShape json `shouldSatisfy` isLeft
        decodeShape json `shouldSatisfy` (_ /= Left noMatch)

      it "fails when no constructor matches the tag" do
        decodeShape (unsafeParse """{"tag":"Hexagon","values":[]}""") `shouldSatisfy` isLeft

    describe "custom encoding (tick-duck journal format)" do
      it "reproduces the existing on-disk shape exactly" do
        stringify (encodeShapeJournalStyle (Circle 3))
          `shouldEqual` """{"kind":"circle","entry":3}"""

      it "omits the values key entirely for a nullary constructor" do
        stringify (encodeShapeJournalStyle Blob) `shouldEqual` """{"kind":"blob"}"""

      it "round-trips through the custom encoding" do
        decodeShapeJournalStyle (encodeShapeJournalStyle (Rect 2 "wide"))
          `shouldEqual` Right (Rect 2 "wide")

    describe "enum" do
      it "encodes a nullary-only type as a plain string" do
        stringify (encodeEnum Simulation) `shouldEqual` """"Simulation""""

      it "round-trips" do
        decodeEnum (encodeEnum Realisation) `shouldEqual` Right Realisation

      it "applies mapTag in both directions" do
        stringify (encodeEnumWith lowerFirst Simulation) `shouldEqual` """"simulation""""
        decodeEnumWith lowerFirst (encodeEnumWith lowerFirst Realisation)
          `shouldEqual` Right Realisation

      it "rejects a string that isn't a constructor" do
        (decodeEnum (encodeString "Nonsense") :: Either JsonDecodeError Mode)
          `shouldSatisfy` isLeft
