-- | Encoders for JSON primitives, arrays, objects, maps and 2-tuples,
-- | plus the `Json` type, `jsonNull` and `stringify` themselves - so a
-- | consumer never needs its own `import Data.Argonaut.*` line. See
-- | `Data.Json.Decode` for the other direction, and
-- | `Data.Json.Encode.Record`/`.Sum`/`.Tuple` for the composite shapes.
-- |
-- | `EncodeJson` is an opaque newtype rather than an alias for `a ->
-- | Json`. The point is that a business-logic type's encoder should be
-- | *composed* out of the combinators here, not written as a function
-- | that reaches into a raw `Json` tree by hand. `fromFn`/`toFn` are the
-- | way out when you genuinely need one; reaching for either is a signal
-- | that a combinator is missing.
module Data.Json.Encode
  ( Json
  , EncodeJson
  , fromFn
  , toFn
  , encodeRawJson
  , encodeString
  , encodeNumber
  , encodeInt
  , encodeBoolean
  , encodeArray
  , encodeObject
  , encodeNativeTuple2
  , encodeMapToObject
  , encodeTupleArrayToObject
  , jsonNull
  , stringify
  ) where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Encode.Encoders as Encoders
import Data.Functor.Contravariant (class Contravariant)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object)
import Foreign.Object as Obj

-- | Naming an imported type directly in an export list doesn't re-export
-- | it in PureScript (only `module X` does, which would re-export all of
-- | `Data.Argonaut.Core`, not just this one type) - a local alias is the
-- | workaround. Transparent: a `Json` here and one from `Data.Argonaut.
-- | Core` (or `Data.Json.Decode`'s own alias) are the same type as far as
-- | the compiler is concerned.
type Json = Argonaut.Json

-- | A JSON encoder for `a`. Opaque on purpose - see the module comment.
newtype EncodeJson a = EncodeJson (a -> Json)

-- | An encoder consumes its `a`, so it is contravariant: given a way to
-- | turn a `b` into an `a`, an encoder for `a` becomes one for `b`. This
-- | is the instance a newtype `Encoder` gets to have, and the reason
-- | `Functor`/`Applicative`/`Monad` are absent rather than forgotten -
-- | none of them can be written for a type whose parameter appears in
-- | argument position. `Divide`/`Divisible` are absent for a different
-- | reason: they would need a way to combine two `Json` values into one,
-- | and there is no canonical choice (an array? an object? merged?), so
-- | the combinators below name the choice instead.
instance Contravariant EncodeJson where
  cmap f (EncodeJson g) = EncodeJson (g <<< f)

-- | Wrap a raw encoding function. The escape hatch out of composition -
-- | needed at the boundary where a `newtype` is unwrapped, or for a
-- | shape no combinator covers yet.
fromFn :: forall a. (a -> Json) -> EncodeJson a
fromFn = EncodeJson

-- | Run an encoder. Also the escape hatch *in*: passing `toFn e` where a
-- | plain function is expected.
toFn :: forall a. EncodeJson a -> a -> Json
toFn (EncodeJson f) = f

-- | The encoder that does nothing: puts an already-built `Json` in
-- | place. Use it to hand a container combinator contents that are
-- | already encoded, e.g. `encodeArray encodeRawJson` for an
-- | `Array Json`. Mirror of `Data.Json.Decode.decodeRawJson`.
encodeRawJson :: EncodeJson Json
encodeRawJson = EncodeJson identity

----------------------------------------------------------------------------------------------------
-- Primitives
----------------------------------------------------------------------------------------------------

-- | Encode a `String` as JSON.
encodeString :: EncodeJson String
encodeString = EncodeJson Encoders.encodeString

-- | Encode a `Number` as JSON.
encodeNumber :: EncodeJson Number
encodeNumber = EncodeJson Encoders.encodeNumber

-- | Encode an `Int` as JSON.
encodeInt :: EncodeJson Int
encodeInt = EncodeJson Encoders.encodeInt

-- | Encode a `Boolean` as JSON.
encodeBoolean :: EncodeJson Boolean
encodeBoolean = EncodeJson Encoders.encodeBoolean

----------------------------------------------------------------------------------------------------
-- Array
----------------------------------------------------------------------------------------------------

-- | Encode an `Array a` as a JSON array, applying one encoder to every element.
encodeArray :: forall a. EncodeJson a -> EncodeJson (Array a)
encodeArray (EncodeJson f) = EncodeJson (Encoders.encodeArray f)

----------------------------------------------------------------------------------------------------
-- Object
----------------------------------------------------------------------------------------------------

-- | Encode an `Object a` as a JSON object, applying one encoder to every
-- | value (keys stay as-is).
encodeObject :: forall a. EncodeJson a -> EncodeJson (Object a)
encodeObject (EncodeJson f) = EncodeJson (Encoders.encodeForeignObject f)

----------------------------------------------------------------------------------------------------
-- Tuple
----------------------------------------------------------------------------------------------------

-- | Encode a native tuple `a /\ b` as a 2-element JSON array. See
-- | `Data.Json.Encode.Tuple.encodeTuple` for arbitrary lengths.
encodeNativeTuple2 :: forall a b. EncodeJson a -> EncodeJson b -> EncodeJson (a /\ b)
encodeNativeTuple2 (EncodeJson f) (EncodeJson g) = EncodeJson (Encoders.encodeTuple f g)

----------------------------------------------------------------------------------------------------
-- Map
----------------------------------------------------------------------------------------------------

-- | Encode a `Map k v` as a JSON object, rendering each key to a string.
encodeMapToObject :: forall k v. (k -> String) -> EncodeJson v -> EncodeJson (Map k v)
encodeMapToObject keyEncoder valueEncoder =
  EncodeJson (toFn (encodeTupleArrayToObject keyEncoder valueEncoder) <<< Map.toUnfoldable)

-- | Encode an association list as a JSON object. The shape `Map`
-- | reduces to, exposed on its own because a wire format is often an
-- | object whose keys are data (a ticker response keyed by pair, say)
-- | without the value being a `Map` on either side - and because the
-- | alternative is a hand-written `Json` fold, which is exactly what
-- | this library exists to avoid. Later entries win on a duplicate key,
-- | matching `Foreign.Object.fromFoldable`.
encodeTupleArrayToObject
  :: forall k v. (k -> String) -> EncodeJson v -> EncodeJson (Array (k /\ v))
encodeTupleArrayToObject keyEncoder (EncodeJson encodeValue) =
  EncodeJson \entries ->
    Argonaut.fromObject
      (Obj.fromFoldable (map (\(k /\ v) -> keyEncoder k /\ encodeValue v) entries))

----------------------------------------------------------------------------------------------------
-- Raw tree
----------------------------------------------------------------------------------------------------

-- | The JSON `null` value - for a field that's genuinely optional rather
-- | than modeled as `Maybe`, e.g. `fromFn (maybe jsonNull (toFn encodeString))`.
jsonNull :: Json
jsonNull = Argonaut.jsonNull

-- | Render a `Json` tree as wire text - the last step after building it
-- | up with the encoders above.
stringify :: Json -> String
stringify = Argonaut.stringify
