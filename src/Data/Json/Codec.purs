-- | Both directions at once, for the formats that can afford it.
-- |
-- | The rest of this library splits encode from decode deliberately, and
-- | that stays the default. A `JsonCodec` is the opt-in layer above it:
-- | one description that yields both, so the two cannot drift apart.
-- |
-- | **When to reach for it.** A format with no history - something this
-- | program writes and this program reads, where the only meaning of
-- | "correct" is that a round trip returns what went in. Then writing
-- | the two halves separately is duplication, and duplication that a
-- | compiler cannot check: nothing stops an encoder writing `dayRate`
-- | and a decoder reading `day_rate`, and no test catches it if both
-- | tests are written from the same wrong idea.
-- |
-- | **When not to.** Anything with readers or writers you do not deploy
-- | at the same moment: a persisted file, a public API, a queue. There
-- | the decoder has to be more tolerant than the encoder is generous -
-- | it must read what last year's version wrote, while the encoder only
-- | ever writes today's shape. A codec forces the two to be mirror
-- | images, so `decodeRecordWithDefaults` has no codec counterpart on
-- | purpose. That is not a gap to fill later.
-- |
-- | Nothing is trapped: `encoder` and `decoder` take one out at any
-- | point, so a record can be built as a codec and have its decode side
-- | replaced where the format grew a field.
module Data.Json.Codec
  ( JsonCodec
  , codec
  , encoder
  , decoder
  , codecInvmap
  , codecRefine
  , codecRawJson
  , codecString
  , codecNumber
  , codecInt
  , codecBoolean
  , codecArray
  , codecMaybe
  , codecObject
  , codecMapToObject
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Contravariant ((>$<))
import Data.Json.Decode (DecodeJson, JsonDecodeError)
import Data.Json.Decode as Decode
import Data.Json.Encode (EncodeJson, Json)
import Data.Json.Encode as Encode
import Data.Map (Map)
import Data.Maybe (Maybe)
import Foreign.Object (Object)

-- | An encoder and a decoder for the same type, kept together.
-- |
-- | Opaque, like `EncodeJson` and `DecodeJson`, and for the same reason:
-- | the pair is meant to be composed out of the combinators here. `codec`
-- | is the way in when the two halves already exist.
newtype JsonCodec a = JsonCodec
  { encode :: EncodeJson a
  , decode :: DecodeJson a
  }

-- | Pair two halves that already exist. The one way in from hand-written
-- | directions, and the place where "these two agree" stops being
-- | checked and starts being asserted.
codec :: forall a. EncodeJson a -> DecodeJson a -> JsonCodec a
codec encode decode = JsonCodec { encode, decode }

-- | The encoding half. Not an escape hatch - it is how a codec is
-- | *used*, since running one means picking a direction.
encoder :: forall a. JsonCodec a -> EncodeJson a
encoder (JsonCodec c) = c.encode

-- | The decoding half.
decoder :: forall a. JsonCodec a -> DecodeJson a
decoder (JsonCodec c) = c.decode

-- | Move a codec to another type across a pair of total conversions -
-- | how a newtype, or any type isomorphic to one already described,
-- | gets a codec without restating its shape.
-- |
-- | Two functions rather than one because a codec is invariant: it both
-- | consumes and produces its type, so neither `map` nor `>$<` can be
-- | written for it. That is not a limitation to work around; it is the
-- | reason the type is honest.
codecInvmap :: forall a b. (a -> b) -> (b -> a) -> JsonCodec a -> JsonCodec b
codecInvmap to from (JsonCodec c) =
  JsonCodec { encode: from >$< c.encode, decode: map to c.decode }

-- | `codecInvmap` where only one direction can fail - a parse into a
-- | smarter type, a range check, a lookup that can miss. The mirror of
-- | `decodeRefine`, and the shape every refinement has: narrowing can
-- | reject, widening cannot.
codecRefine
  :: forall a b
   . (a -> Either JsonDecodeError b)
  -> (b -> a)
  -> JsonCodec a
  -> JsonCodec b
codecRefine narrow widen (JsonCodec c) =
  JsonCodec { encode: widen >$< c.encode, decode: Decode.decodeRefine narrow c.decode }

-- | A subtree carried through untouched.
codecRawJson :: JsonCodec Json
codecRawJson = codec Encode.encodeRawJson Decode.decodeRawJson

codecString :: JsonCodec String
codecString = codec Encode.encodeString Decode.decodeString

codecNumber :: JsonCodec Number
codecNumber = codec Encode.encodeNumber Decode.decodeNumber

codecInt :: JsonCodec Int
codecInt = codec Encode.encodeInt Decode.decodeInt

codecBoolean :: JsonCodec Boolean
codecBoolean = codec Encode.encodeBoolean Decode.decodeBoolean

codecArray :: forall a. JsonCodec a -> JsonCodec (Array a)
codecArray c = codec (Encode.encodeArray (encoder c)) (Decode.decodeArray (decoder c))

-- | `null` on the wire for `Nothing`, and a present value otherwise.
-- |
-- | An *absent key* is a different thing and has no codec: leaving a key
-- | out on encode means the decoder needs a default when it is missing,
-- | and defaults are the tolerance that only belongs on the decode side.
codecMaybe :: forall a. JsonCodec a -> JsonCodec (Maybe a)
codecMaybe c = codec (Encode.encodeMaybe (encoder c)) (Decode.decodeMaybe (decoder c))

-- | An object whose keys are data and whose values all share a type.
codecObject :: forall a. JsonCodec a -> JsonCodec (Object a)
codecObject c = codec (Encode.encodeObject (encoder c)) (Decode.decodeObject (decoder c))

-- | A `Map` with `String` keys, as a JSON object.
-- |
-- | `String` keys only, and deliberately. Any other key type needs both
-- | a `k -> String` and a `String -> Either JsonDecodeError k`, which
-- | are not derivable from one another - so a codec would just be a pair
-- | of functions the caller passes anyway, and the honest way to write
-- | that is `Encode.encodeMapToObject` and `Decode.decodeMapFromObject`,
-- | paired with `codec`.
codecMapToObject :: forall a. JsonCodec a -> JsonCodec (Map String a)
codecMapToObject c = codec
  (Encode.encodeMapToObject identity (encoder c))
  (Decode.decodeMapFromObject Right (decoder c))
