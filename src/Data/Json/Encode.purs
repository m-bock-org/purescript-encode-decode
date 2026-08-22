-- | Encode functions for JSON primitives, arrays, and 2-tuples - see
-- | `Data.Json.Decode` for the other direction, `Data.Json.Encode.Record`
-- | for the one thing in this library that isn't trivial.
module Data.Json.Encode
  ( encodeString
  , encodeNumber
  , encodeInt
  , encodeBoolean
  , encodeArray
  , encodeObject
  , encodeNativeTuple2
  ) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Encoders as Encoders
import Data.Tuple.Nested (type (/\))
import Foreign.Object (Object)

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
