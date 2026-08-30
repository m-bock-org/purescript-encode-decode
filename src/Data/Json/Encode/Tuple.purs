-- | Encode a native tuple of any length as a flat JSON array, from a
-- | matching tuple of per-position encoders - the same shape
-- | `Data.Json.Encode.Record` uses for records. See
-- | `Data.Json.Decode.Tuple` for the other direction.
-- |
-- | ```purescript
-- | encodeRow :: EncodeJson (String /\ Int /\ Boolean)
-- | encodeRow = encodeTuple (encodeString /\ encodeInt /\ encodeBoolean)
-- | ```
-- |
-- | encodes to `["ada", 3, true]`.
-- |
-- | Any length, including two - a pair needs no special case. And it
-- | nests, because a tuple of encoders is itself an encoder-shaped
-- | value.
module Data.Json.Encode.Tuple
  ( encodeTuple
  , class EncodeTupleParts
  , gEncodeTupleParts
  ) where

import Prelude

import Data.Array (cons) as Array
import Data.Functor.Contravariant (cmap)
import Data.Json.Encode (EncodeJson, Json, encodeArray, encodeRawJson, runEncode)
import Data.Tuple.Nested (type (/\), (/\))

-- | `encodeTuple (encodeString /\ encodeInt)` - the value tuple's type
-- | is determined by the encoder tuple's, so it never has to be written
-- | out.
encodeTuple :: forall e t. EncodeTupleParts e t => e -> EncodeJson t
encodeTuple encoders = cmap (gEncodeTupleParts encoders) (encodeArray encodeRawJson)

-- | Generic derivation for `encodeTuple`, one position at a time.
-- | Not meant to be used directly - go through `encodeTuple`.
-- |
-- | The two instances do not overlap, and that is only true because
-- | `EncodeJson` is a newtype rather than an alias for `a -> Json`:
-- | `EncodeJson a /\ rest` is headed by `Tuple` and a lone `EncodeJson
-- | a` by `EncodeJson`, so the compiler can tell "more positions
-- | follow" from "this is the last one" structurally. With the alias,
-- | both heads were functions and the induction had no base case the
-- | solver could pick out.
class EncodeTupleParts :: Type -> Type -> Constraint
class EncodeTupleParts e t | e -> t where
  gEncodeTupleParts :: e -> t -> Array Json

instance
  EncodeTupleParts erest trest =>
  EncodeTupleParts (EncodeJson a /\ erest) (a /\ trest) where
  gEncodeTupleParts (encodeHead /\ encodeRest) (head /\ rest) =
    Array.cons (runEncode encodeHead head) (gEncodeTupleParts encodeRest rest)

else instance EncodeTupleParts (EncodeJson a) a where
  gEncodeTupleParts encodeLast last = [ runEncode encodeLast last ]
