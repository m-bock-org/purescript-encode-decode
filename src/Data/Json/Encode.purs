-- | Encode functions for JSON primitives, arrays, and 2-tuples, plus the
-- | `Json` type, `jsonNull`, and `stringify` themselves - so a consumer
-- | never needs its own `import Data.Argonaut.*` line. See `Data.Json.
-- | Decode` for the other direction, `Data.Json.Encode.Record` for the
-- | one thing in this library that isn't trivial.
module Data.Json.Encode
  ( Json
  , encodeString
  , encodeNumber
  , encodeInt
  , encodeBoolean
  , encodeArray
  , encodeObject
  , encodeNativeTuple2
  , jsonNull
  , stringify
  ) where

import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Encode.Encoders as Encoders
import Data.Tuple.Nested (type (/\))
import Foreign.Object (Object)

-- | Naming an imported type directly in an export list doesn't re-export
-- | it in PureScript (only `module X` does, which would re-export all of
-- | `Data.Argonaut.Core`, not just this one type) - a local alias is the
-- | workaround. Transparent: a `Json` here and one from `Data.Argonaut.
-- | Core` (or `Data.Json.Decode`'s own alias) are the same type as far as
-- | the compiler is concerned.
type Json = Argonaut.Json

----------------------------------------------------------------------------------------------------
-- Primitives
----------------------------------------------------------------------------------------------------

-- | Encode a `String` as JSON.
encodeString :: String -> Json
encodeString = Encoders.encodeString

-- | Encode a `Number` as JSON.
encodeNumber :: Number -> Json
encodeNumber = Encoders.encodeNumber

-- | Encode an `Int` as JSON.
encodeInt :: Int -> Json
encodeInt = Encoders.encodeInt

-- | Encode a `Boolean` as JSON.
encodeBoolean :: Boolean -> Json
encodeBoolean = Encoders.encodeBoolean

----------------------------------------------------------------------------------------------------
-- Array
----------------------------------------------------------------------------------------------------

-- | Encode an `Array a` as a JSON array, applying one encoder to every element.
encodeArray :: forall a. (a -> Json) -> Array a -> Json
encodeArray = Encoders.encodeArray

----------------------------------------------------------------------------------------------------
-- Object
----------------------------------------------------------------------------------------------------

-- | Encode an `Object a` as a JSON object, applying one encoder to every
-- | value (keys stay as-is).
encodeObject :: forall a. (a -> Json) -> Object a -> Json
encodeObject = Encoders.encodeForeignObject

----------------------------------------------------------------------------------------------------
-- Tuple
----------------------------------------------------------------------------------------------------

-- | Encode a native tuple `a /\ b` as a 2-element JSON array.
encodeNativeTuple2 :: forall a b. (a -> Json) -> (b -> Json) -> (a /\ b) -> Json
encodeNativeTuple2 = Encoders.encodeTuple

----------------------------------------------------------------------------------------------------
-- Raw tree
----------------------------------------------------------------------------------------------------

-- | The JSON `null` value - for a field that's genuinely optional rather
-- | than modeled as `Maybe`, e.g. `maybe jsonNull encodeString`.
jsonNull :: Json
jsonNull = Argonaut.jsonNull

-- | Render a `Json` tree as wire text - the last step after building it
-- | up with the encoders above.
stringify :: Json -> String
stringify = Argonaut.stringify
