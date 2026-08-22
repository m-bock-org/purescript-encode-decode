-- | Decode functions for JSON primitives, arrays, and 2-tuples - see
-- | `Data.Json.Encode` for the other direction, `Data.Json.Decode.Record`
-- | for the one thing in this library that isn't trivial.
module Data.Json.Decode
  ( module Data.Argonaut.Decode.Error
  , decodeString
  , decodeNumber
  , decodeInt
  , decodeBoolean
  , decodeArray
  , decodeNativeTuple2
  ) where

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Decoders as Decoders
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Either (Either)
import Data.Tuple.Nested (type (/\))

----------------------------------------------------------------------------------------------------
-- Primitives
----------------------------------------------------------------------------------------------------

-- | Decode a JSON string.
decodeString :: Json -> Either JsonDecodeError String
decodeString = Decoders.decodeString

-- | Decode a JSON number.
decodeNumber :: Json -> Either JsonDecodeError Number
decodeNumber = Decoders.decodeNumber

-- | Decode a JSON number, failing if it isn't a whole number.
decodeInt :: Json -> Either JsonDecodeError Int
decodeInt = Decoders.decodeInt

-- | Decode a JSON boolean.
decodeBoolean :: Json -> Either JsonDecodeError Boolean
decodeBoolean = Decoders.decodeBoolean

----------------------------------------------------------------------------------------------------
-- Array
----------------------------------------------------------------------------------------------------

-- | Decode a JSON array, applying one decoder to every element.
decodeArray :: forall a. (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (Array a)
decodeArray = Decoders.decodeArray

----------------------------------------------------------------------------------------------------
-- Tuple
----------------------------------------------------------------------------------------------------

-- | Decode a 2-element JSON array as a native tuple `a /\ b` - for a
-- | *fixed-length* array where each position has its own type, unlike
-- | `decodeArray`'s variable-length array of one uniform type.
decodeNativeTuple2
  :: forall a b
   . (Json -> Either JsonDecodeError a)
  -> (Json -> Either JsonDecodeError b)
  -> Json
  -> Either JsonDecodeError (a /\ b)
decodeNativeTuple2 = Decoders.decodeTuple
