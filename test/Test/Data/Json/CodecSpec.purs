module Test.Data.Json.CodecSpec (spec) where

import Prelude

import Data.Either (Either(..))

import Data.Either (isLeft) as Either
import Data.Json.Codec (JsonCodec, codecArray, codecBoolean, codecInt, codecMaybe, codecRefine, codecString, decoder, encoder)
import Data.Json.Codec.Record (codecOptional, codecRecord)
import Data.Json.Codec.Sum (codecEnum, codecSum, codecSumWith)
import Data.Json.Codec.Variant (codecVariant)
import Data.Variant (Variant)
import Data.Variant as V
import Type.Proxy (Proxy(..))
import Data.Json.Codec.Tuple (codecTuple)
import Data.Json.Sum.Encoding (Encoding(..), defaultEncoding)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Data.Json.Decode (JsonDecodeError(..), decodeArray, decodeInt, decodeMaybe, decodeString, runDecode, runDecodeFromString)
import Data.Json.Decode.Record (decodeRecord)
import Data.Json.Encode (encodeArray, encodeInt, encodeMaybe, encodeString, runEncode, runEncodeToString)
import Data.Json.Encode.Record (encodeRecord)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

type User =
  { name :: String
  , age :: Int
  , tags :: Array String
  , nickname :: Maybe String
  }

-- | Written once. Both directions come out of it, and the compiler
-- | holds them to the same record type.
-- | Private.
codecUser :: JsonCodec User
codecUser = codecRecord
  { name: codecString
  , age: codecInt
  , tags: codecArray codecString
  , nickname: codecMaybe codecString
  }

newtype Slug = Slug String

derive instance Newtype Slug _
derive newtype instance Eq Slug
derive newtype instance Show Slug

-- | A refinement that can reject in one direction only, which is every
-- | refinement.
-- | Private.
codecSlug :: JsonCodec Slug
codecSlug = codecRefine narrow unwrap codecString
  where
  narrow s
    | s == "" = Left (TypeMismatch "a non-empty slug")
    | otherwise = Right (wrap s)

-- | Private. Used only by `spec`.
roundTrip :: ∀ a. JsonCodec a -> a -> Either JsonDecodeError a
roundTrip c value = runDecode (decoder c) (runEncode (encoder c) value)

data Shape
  = Circle Int
  | Rect Int String
  | Blob

derive instance Generic Shape _
derive instance Eq Shape
instance Show Shape where
  show = genericShow

-- | Nullary, one argument and two, in one description.
-- | Private.
codecShape :: JsonCodec Shape
codecShape = codecSum
  { "Circle": codecInt
  , "Rect": codecInt /\ codecString
  , "Blob": unit
  }

data Mode = Simulation | Realisation

derive instance Generic Mode _
derive instance Eq Mode
instance Show Mode where
  show = genericShow

-- | Private.
codecMode :: JsonCodec Mode
codecMode = codecEnum

-- | that is a bijection, so the one a codec can carry.
-- | Private.
codecMaybeNick :: JsonCodec { name :: String, nickname :: Maybe String }
codecMaybeNick = codecRecord
  { name: codecString
  , nickname: codecOptional codecString
  }

type Msg = Variant (newState :: Int, note :: String)

-- | The shape the dashboard actually uses: a tagged case carrying one
-- | value.
-- | Private.
codecMsg :: JsonCodec Msg
codecMsg = codecVariant
  { newState: codecInt
  , note: codecString
  }

-- | Uses `roundTrip`, `encodingWith`.
spec :: Spec Unit
spec = do
  describe "Data.Json.Codec" do
    it "round-trips a record described once" do
      let user = { name: "ada", age: 36, tags: [ "fp", "ml" ], nickname: Just "the countess" }
      roundTrip codecUser user `shouldEqual` Right user

    it "round-trips the absent case of a Maybe field" do
      let user = { name: "ada", age: 36, tags: [], nickname: Nothing }
      roundTrip codecUser user `shouldEqual` Right user
    it "reads the field names a hand-written document uses" do
      runDecodeFromString (decoder codecUser)
        """{"name":"ada","age":36,"tags":["fp"],"nickname":null}"""
        `shouldEqual` Right { name: "ada", age: 36, tags: [ "fp" ], nickname: Nothing }
    it "agrees, byte for byte, with the two directions written apart" do
      let user = { name: "ada", age: 36, tags: [ "fp" ], nickname: Just "the countess" }
      let
        apart = encodeRecord
          { name: encodeString
          , age: encodeInt
          , tags: encodeArray encodeString
          , nickname: encodeMaybe encodeString
          }
      runEncodeToString (encoder codecUser) user
        `shouldEqual` runEncodeToString apart user

      let
        apartBack = decodeRecord
          { name: decodeString
          , age: decodeInt
          , tags: decodeArray decodeString
          , nickname: decodeMaybe decodeString
          }
      runDecode (decoder codecUser) (runEncode apart user)
        `shouldEqual` runDecode apartBack (runEncode apart user)

    it "round-trips an optional field through absence" do
      roundTrip codecMaybeNick { name: "ada", nickname: Nothing }
        `shouldEqual` Right { name: "ada", nickname: Nothing }
      roundTrip codecMaybeNick { name: "ada", nickname: Just "the countess" }
        `shouldEqual` Right { name: "ada", nickname: Just "the countess" }

    it "writes no key at all for an absent optional field" do
      runEncodeToString (encoder codecMaybeNick) { name: "ada", nickname: Nothing }
        `shouldEqual` """{"name":"ada"}"""

    it "round-trips a refinement" do
      roundTrip codecSlug (Slug "gig-pilot") `shouldEqual` Right (Slug "gig-pilot")

    it "rejects what the refinement rejects, on the decode side only" do
      runDecode (decoder codecSlug) (runEncode (encoder codecSlug) (Slug ""))
        `shouldSatisfy` Either.isLeft

  describe "Data.Json.Codec.Sum" do
    it "round-trips every constructor arity from one description" do
      roundTrip codecShape (Circle 1) `shouldEqual` Right (Circle 1)
      roundTrip codecShape (Rect 2 "wide") `shouldEqual` Right (Rect 2 "wide")
      roundTrip codecShape Blob `shouldEqual` Right Blob

    it "agrees with itself about the wire format" do
      runDecodeFromString (decoder codecShape)
        """{"tag":"Rect","values":[2,"wide"]}"""
        `shouldEqual` Right (Rect 2 "wide")
    it "moves both halves when the format changes" do
      let c = codecSumWith (encodingWith "kind") { "Circle": codecInt, "Rect": codecInt /\ codecString, "Blob": unit }
      runDecodeFromString (decoder (c :: JsonCodec Shape))
        """{"kind":"Circle","values":[1]}"""
        `shouldEqual` Right (Circle 1)
      roundTrip c (Circle 1) `shouldEqual` Right (Circle 1)

    it "round-trips a tuple as a flat array" do
      let c = codecTuple (codecString /\ codecInt /\ codecBoolean)
      roundTrip c ("a" /\ 1 /\ true) `shouldEqual` Right ("a" /\ 1 /\ true)
      runDecodeFromString (decoder c) """["a",1,true]"""
        `shouldEqual` Right ("a" /\ 1 /\ true)

    it "round-trips every case of a variant" do
      roundTrip codecMsg (V.inj (Proxy @"newState") 7)
        `shouldEqual` Right (V.inj (Proxy @"newState") 7)
      roundTrip codecMsg (V.inj (Proxy @"note") "hi")
        `shouldEqual` Right (V.inj (Proxy @"note") "hi")

    it "writes a variant as tag and value, unwrapped" do
      runEncodeToString (encoder codecMsg) (V.inj (Proxy @"newState") 7)
        `shouldEqual` """{"tag":"newState","value":7}"""

    it "reports a broken payload rather than falling through to the next case" do
      runDecodeFromString (decoder codecMsg) """{"tag":"newState","value":"seven"}"""
        `shouldSatisfy` Either.isLeft

    it "round-trips an all-nullary type as a plain string" do
      roundTrip codecMode Simulation `shouldEqual` Right Simulation
      runDecodeFromString (decoder codecMode) "\"Realisation\"" `shouldEqual` Right Realisation

-- | Private. Used only by `spec`.
encodingWith :: String -> Encoding
encodingWith tagKey = case defaultEncoding of
  EncodeTagged r -> EncodeTagged (r { tagKey = tagKey })
  other -> other
--
-- `spec`
-- A round trip cannot catch a codec that agrees with itself on the
-- wrong names, because both halves are wrong together. Only a
-- document written by hand pins them.
-- The codec layer is meant to be exactly the two directions,
-- assembled - not a third implementation that resembles them. This
-- is what says so, and what would break first if the split ever
-- started doing work of its own.
-- The `Encoding` is the argument two hand-written directions are
-- most likely to disagree about, so it is the one worth changing in
-- a test: one argument moves both halves.
