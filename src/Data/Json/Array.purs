module Data.Json.Array
  ( decodeArray
  , encodeArray
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromArray, toArray)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

encodeArray :: forall a. (a -> Json) -> Array a -> Json
encodeArray encodeA = fromArray <<< map encodeA

decodeArray :: forall a. (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (Array a)
decodeArray decodeA json = case toArray json of
  Nothing -> Left (TypeMismatch "Array")
  Just arr -> traverse decodeA arr
