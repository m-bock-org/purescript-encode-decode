-- | Decode a fixed-length JSON array where each position has its own type
-- | (unlike `Data.Json.Array.decodeArray`, which is for a variable-length
-- | array of one uniform type).
module Data.Json.Tuple
  ( decodeNativeTuple2
  ) where

import Prelude

import Data.Argonaut.Core (Json, toArray)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))

decodeNativeTuple2
  :: forall a b
   . (Json -> Either JsonDecodeError a)
  -> (Json -> Either JsonDecodeError b)
  -> Json
  -> Either JsonDecodeError (a /\ b)
decodeNativeTuple2 decodeA decodeB json = case toArray json of
  Just [ a, b ] -> (/\) <$> decodeA a <*> decodeB b
  _ -> Left (TypeMismatch "2-element array")
