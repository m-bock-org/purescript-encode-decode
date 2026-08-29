-- | Decoders for JSON primitives, arrays, objects, maps and 2-tuples,
-- | plus the `Json` type and `jsonParser` themselves - so a consumer
-- | never needs its own `import Data.Argonaut.*` line. See
-- | `Data.Json.Encode` for the other direction, and
-- | `Data.Json.Decode.Record`/`.Sum`/`.Tuple` for the composite shapes.
-- |
-- | `DecodeJson` is an opaque newtype rather than an alias for `Json ->
-- | Either JsonDecodeError a`. The point is that a business-logic type's
-- | decoder should be *composed* out of the combinators here, not
-- | written as a function that walks a raw `Json` tree by hand.
-- | `fromFn`/`toFn` are the way out when you genuinely need one;
-- | reaching for either is a signal that a combinator is missing.
module Data.Json.Decode
  ( module Data.Argonaut.Decode.Error
  , Json
  , DecodeJson
  , fromFn
  , toFn
  , decodeRawJson
  , decodeString
  , decodeNumber
  , decodeInt
  , decodeBoolean
  , decodeArray
  , decodeObject
  , decodeNativeTuple2
  , decodeMapFromObject
  , decodeTupleArrayFromObject
  , jsonParser
  ) where

import Prelude

import Control.Alt (class Alt)
import Data.Argonaut.Core as Argonaut
import Data.Argonaut.Decode.Decoders as Decoders
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser) as Parser
import Data.Either (Either(..))
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

-- | A JSON decoder for `a`. Opaque on purpose - see the module comment.
newtype DecodeJson a = DecodeJson (Json -> Either JsonDecodeError a)

-- | A decoder produces its `a`, so unlike `EncodeJson` it is covariant
-- | and gets the whole tower. `Monad` matters in practice: it is what
-- | lets a later field's decoder depend on an earlier field's *value*
-- | (a version tag choosing a payload shape, say) without dropping to a
-- | raw `Json` function.
derive instance Functor DecodeJson

instance Apply DecodeJson where
  apply (DecodeJson f) (DecodeJson a) = DecodeJson \json -> f json <*> a json

instance Applicative DecodeJson where
  pure a = DecodeJson \_ -> Right a

instance Bind DecodeJson where
  bind (DecodeJson a) f = DecodeJson \json -> do
    a' <- a json
    toFn (f a') json

instance Monad DecodeJson

-- | First decoder that succeeds wins. For a wire format that admits more
-- | than one spelling of the same thing - a field that used to be a
-- | string and is now an object, an endpoint that renamed a key - where
-- | the alternative is an ad-hoc `case` over the raw tree. The right
-- | decoder's error is the one reported when both fail, since it is the
-- | one that ran last.
instance Alt DecodeJson where
  alt (DecodeJson a) (DecodeJson b) = DecodeJson \json -> case a json of
    Right ok -> Right ok
    Left _ -> b json

-- | Wrap a raw decoding function. The escape hatch out of composition -
-- | needed at the boundary where a `newtype` is built from a validated
-- | primitive, or for a shape no combinator covers yet.
fromFn :: forall a. (Json -> Either JsonDecodeError a) -> DecodeJson a
fromFn = DecodeJson

-- | Run a decoder. Also the escape hatch *in*: passing `toFn d` where a
-- | plain function is expected.
toFn :: forall a. DecodeJson a -> Json -> Either JsonDecodeError a
toFn (DecodeJson f) = f

-- | The decoder that does nothing: hands back the `Json` as it stands.
-- | Distinct from `pure`, which ignores its input and yields a constant.
-- | Use it to ask a container combinator for its raw contents, e.g.
-- | `decodeObject decodeRawJson` for an `Object Json`.
decodeRawJson :: DecodeJson Json
decodeRawJson = DecodeJson Right

----------------------------------------------------------------------------------------------------
-- Primitives
----------------------------------------------------------------------------------------------------

-- | Decode a JSON string.
decodeString :: DecodeJson String
decodeString = DecodeJson Decoders.decodeString

-- | Decode a JSON number.
decodeNumber :: DecodeJson Number
decodeNumber = DecodeJson Decoders.decodeNumber

-- | Decode a JSON number, failing if it isn't a whole number.
decodeInt :: DecodeJson Int
decodeInt = DecodeJson Decoders.decodeInt

-- | Decode a JSON boolean.
decodeBoolean :: DecodeJson Boolean
decodeBoolean = DecodeJson Decoders.decodeBoolean

----------------------------------------------------------------------------------------------------
-- Array
----------------------------------------------------------------------------------------------------

-- | Decode a JSON array, applying one decoder to every element.
decodeArray :: forall a. DecodeJson a -> DecodeJson (Array a)
decodeArray (DecodeJson f) = DecodeJson (Decoders.decodeArray f)

----------------------------------------------------------------------------------------------------
-- Object
----------------------------------------------------------------------------------------------------

-- | Decode a JSON object, applying one decoder to every value (keys stay
-- | as-is) - unlike `Data.Json.Decode.Record`, this is for an object
-- | whose *set* of keys isn't known ahead of time. Pass `decodeRawJson`
-- | to get the raw, undecoded `Object Json` back.
decodeObject :: forall a. DecodeJson a -> DecodeJson (Object a)
decodeObject (DecodeJson f) = DecodeJson (Decoders.decodeForeignObject f)

----------------------------------------------------------------------------------------------------
-- Tuple
----------------------------------------------------------------------------------------------------

-- | Decode a 2-element JSON array as a native tuple `a /\ b` - for a
-- | *fixed-length* array where each position has its own type, unlike
-- | `decodeArray`'s variable-length array of one uniform type. See
-- | `Data.Json.Decode.Tuple.decodeTuple` for arbitrary lengths.
decodeNativeTuple2 :: forall a b. DecodeJson a -> DecodeJson b -> DecodeJson (a /\ b)
decodeNativeTuple2 (DecodeJson f) (DecodeJson g) = DecodeJson (Decoders.decodeTuple f g)

----------------------------------------------------------------------------------------------------
-- Map
----------------------------------------------------------------------------------------------------

-- | Decode a JSON object as a `Map k v`, parsing each key from its string.
decodeMapFromObject
  :: forall k v
   . Ord k
  => (String -> Either JsonDecodeError k)
  -> DecodeJson v
  -> DecodeJson (Map k v)
decodeMapFromObject decodeK decodeV =
  map Map.fromFoldable (decodeTupleArrayFromObject decodeK decodeV)

-- | Decode a JSON object as an association list, parsing each key from
-- | its string. The shape `Map` reduces to, exposed on its own because a
-- | wire format is often an object whose keys are data (a ticker
-- | response keyed by pair, say) without the value being a `Map` on
-- | either side - and because the alternative is a hand-written walk
-- | over `Object Json`, which is exactly what this library exists to
-- | avoid. Key order follows `Foreign.Object.toUnfoldable`.
decodeTupleArrayFromObject
  :: forall k v
   . (String -> Either JsonDecodeError k)
  -> DecodeJson v
  -> DecodeJson (Array (k /\ v))
decodeTupleArrayFromObject decodeK decodeV = do
  obj <- decodeObject decodeRawJson
  fromFn \_ ->
    for (Obj.toUnfoldable obj) \(kStr /\ vJson) -> do
      k <- decodeK kStr
      v <- toFn decodeV vJson
      pure (k /\ v)

----------------------------------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------------------------------

-- | Parse a JSON string into a `Json` tree - the one step before any of
-- | the decoders above can run.
jsonParser :: String -> Either String Json
jsonParser = Parser.jsonParser
