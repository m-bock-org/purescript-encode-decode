-- | Decode functions for JSON primitives, arrays, and 2-tuples, plus the
-- | `Json` type and `jsonParser` themselves - so a consumer never needs
-- | its own `import Data.Argonaut.*` line. See `Data.Json.Encode` for the
-- | other direction, `Data.Json.Decode.Record` for the one thing in this
-- | library that isn't trivial.
module Data.Json.Decode
  ( module Data.Argonaut.Decode.Error
  , Json
  , DecodeJson
  , decodeString
  , decodeNumber
  , decodeInt
  , decodeBoolean
  , decodeArray
  , decodeObject
  , decodeNativeTuple2
  , decodeMapFromObject
  , jsonParser
  ) where

import Prelude

import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Decode.Decoders as Decoders
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser) as Parser
import Data.Either (Either)
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object (Object)
import Foreign.Object as Obj

-- | Naming an imported type directly in an export list doesn't re-export
-- | it in PureScript (only `module X` does, which would re-export all of
-- | `Data.Argonaut.Core`, not just this one type) - a local alias is the
-- | workaround. Transparent: a `Json` here and one from `Data.Argonaut.
-- | Core` (or `Data.Json.Encode`'s own alias) are the same type as far as
-- | the compiler is concerned.
type Json = Argonaut.Json

-- | The shape every `decode{Type} :: DecodeJson {Type}` function in a
-- | consumer's own codebase is expected to have - naming it lets a
-- | signature say "this is a JSON decoder for `a`" instead of writing
-- | `Json -> Either JsonDecodeError a` out by hand at every call site.
type DecodeJson a = Json -> Either JsonDecodeError a

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
-- Object
----------------------------------------------------------------------------------------------------

-- | Decode a JSON object, applying one decoder to every value (keys stay
-- | as-is) - unlike `Data.Json.Decode.Record`, this is for an object
-- | whose *set* of keys isn't known ahead of time. Pass `Right` to get
-- | the raw, undecoded `Object Json` back.
decodeObject :: forall a. (Json -> Either JsonDecodeError a) -> Json -> Either JsonDecodeError (Object a)
decodeObject = Decoders.decodeForeignObject

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

----------------------------------------------------------------------------------------------------
-- Map
----------------------------------------------------------------------------------------------------

decodeMapFromObject
  :: forall k v
   . Ord k
  => (String -> Either JsonDecodeError k)
  -> (Json -> Either JsonDecodeError v)
  -> Json
  -> Either JsonDecodeError (Map k v)
decodeMapFromObject decodeK decodeV json = do
  obj <- decodeObject pure json
  entries :: Array _ <- for (Obj.toUnfoldable obj) \(kStr /\ vJson) -> do
    k <- decodeK kStr
    v <- decodeV vJson
    pure (k /\ v)
  pure $ Map.fromFoldable entries

----------------------------------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------------------------------

-- | Parse a JSON string into a `Json` tree - the one step before any of
-- | the decoders above can run.
jsonParser :: String -> Either String Json
jsonParser = Parser.jsonParser
