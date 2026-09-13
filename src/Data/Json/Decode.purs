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
-- | `fromFn` is the one way out; reaching for it is a signal that a
-- | combinator is missing.
-- |
-- | Running a decoder is not an escape hatch, and the `run` prefix marks
-- | the difference: `runDecode` and `runDecodeFromString` *apply* a
-- | decoder, they are not decoders themselves. Only `fromFn` lets a hand
-- | written function back *in*.
module Data.Json.Decode
  ( module Data.Argonaut.Decode.Error
  , Json
  , DecodeJson
  , fromFn
  , runDecode
  , decodeFix
  , decodeRawJson
  , decodeFail
  , decodeRefine
  , decodeNamed
  , decodeAttempt
  , runDecodeFromString
  , decodeString
  , decodeNumber
  , decodeInt
  , decodeBoolean
  , decodeArray
  , decodeMaybe
  , decodeObject
  , decodeObjectWithKey
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
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
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
    runDecode (f a') json

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
fromFn :: ∀ a. (Json -> Either JsonDecodeError a) -> DecodeJson a
fromFn = DecodeJson

-- | Run a decoder against a `Json`. The counterpart of
-- | over by a library that speaks `Json` already - so there is no
-- | document left to parse.
-- |
-- | Unlike `fromFn` this is not an escape hatch. It consumes a decoder
-- | rather than manufacturing one, so it cannot be used to smuggle a
-- | hand-written `Json -> Either JsonDecodeError a` back into the
-- | vocabulary.
runDecode :: ∀ a. DecodeJson a -> Json -> Either JsonDecodeError a
runDecode (DecodeJson f) = f

-- | A decoder defined in terms of itself, for a recursive type.
-- |
-- | `decodeFoo = decodeArray decodeFoo` does not compile: PureScript
-- | refuses a top-level value that reaches itself with nothing in
-- | between (`CycleInDeclaration`), because evaluating the right-hand
-- | side would require the right-hand side already evaluated. Taking
-- | the decoder as a parameter breaks that - the reference is only
-- | forced once the decoder is applied to a `Json`.
-- |
-- | The argument shape is the one `Data.Codec.Argonaut.fix` uses, and
-- | for the same reason it is a parameter rather than a `Unit ->`
-- | thunk: the recursive reference is *bound*, so it cannot silently
-- | be some other decoder that happens to be in scope.
-- |
-- | Not `fromFn` in application code - this wraps it once, here, so a
-- | recursive type never has to reach past the vocabulary.
-- |
-- | ```purescript
-- | decodeFoo :: DecodeJson Foo
-- | decodeFoo = decodeFix \self -> decodeArray self
-- | ```
decodeFix :: ∀ a. (DecodeJson a -> DecodeJson a) -> DecodeJson a
decodeFix f = fromFn \json -> runDecode (f (decodeFix f)) json

-- | The decoder that does nothing: hands back the `Json` as it stands.
-- | Distinct from `pure`, which ignores its input and yields a constant.
-- | Use it to ask a container combinator for its raw contents, e.g.
decodeRawJson :: DecodeJson Json
decodeRawJson = DecodeJson Right

-- | The decoder that always fails, with the error you name. `pure`'s
-- | opposite, and what makes a `bind` chain able to reject: decode a tag,
-- | branch on it, and answer with this on the branch that has no reading.
-- | Without it a dispatching decoder has to drop to `fromFn` just to
-- | produce a `Left`.
decodeFail :: ∀ a. JsonDecodeError -> DecodeJson a
decodeFail err = DecodeJson \_ -> Left err

-- | Decode, then narrow the result - a parse into a smarter type, a range
-- | check, a lookup that can miss. The common shape behind "decode a
-- |
-- | This is the combinator that keeps refinement out of `fromFn`. The
-- | refining function sees the decoded `a`, not the `Json`; when the error
-- | wants to quote the original document, pair it in first - `Tuple <$>
-- | decodeRawJson <*> decodeString` hands you both, because `DecodeJson`
-- | is `Applicative`.
decodeRefine :: ∀ a b. (a -> Either JsonDecodeError b) -> DecodeJson a -> DecodeJson b
decodeRefine f (DecodeJson g) = DecodeJson \json -> f =<< g json

-- | Wrap whatever a decoder reports in a name, so an error says which
-- | thing failed to read and not only which key.
-- |
-- | Cheap and worth it on anything a nested structure reaches for: the
-- | difference between `at "euro": expected String` and `Euros > at
-- | "euro": expected String` is knowing where to look.
decodeNamed :: ∀ a. String -> DecodeJson a -> DecodeJson a
decodeNamed name (DecodeJson f) = DecodeJson (lmap (Named name) <<< f)

-- | Run a decoder and hand back its *result*, success or failure, without
-- | failing. For a field whose own decode failure must not abort the
-- | document around it - an envelope whose error array explains why the
-- | payload is unreadable, and would be lost if the payload aborted first.
decodeAttempt :: ∀ a. DecodeJson a -> DecodeJson (Either JsonDecodeError a)
decodeAttempt (DecodeJson f) = DecodeJson (Right <<< f)

-- | Run a decoder against a JSON document: parse and decode in one step.
-- |
-- | Named `run` rather than `decode` because it is not a codec - it
-- | *applies* one. The mirror of `Data.Json.Encode.runEncodeToString`.
-- |
-- | At the edge where a `String` arrives - a file, a response body - this
-- | is the whole journey, with no intermediate `Json` for a caller to
-- | hold. A parse failure is reported as a `TypeMismatch`, since
-- | Uses `runDecode`.
runDecodeFromString :: ∀ a. DecodeJson a -> String -> Either JsonDecodeError a
runDecodeFromString d raw = case Parser.jsonParser raw of
  Left err -> Left (TypeMismatch ("well-formed JSON, got: " <> err))
  Right json -> runDecode d json

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
-- Maybe
----------------------------------------------------------------------------------------------------

-- | The mirror of `encodeMaybe`, and absent for a while only because
-- | every consumer wrote it again locally.
-- |
-- | A *missing key* is the other convention and is not this: it is a
-- | property of the record the field sits in, so it lives in
decodeMaybe :: ∀ a. DecodeJson a -> DecodeJson (Maybe a)
decodeMaybe (DecodeJson f) = DecodeJson \json ->
  if Argonaut.isNull json then Right Nothing
  else map Just (f json)

----------------------------------------------------------------------------------------------------
-- Array
----------------------------------------------------------------------------------------------------

-- | Decode a JSON array, applying one decoder to every element.
decodeArray :: ∀ a. DecodeJson a -> DecodeJson (Array a)
decodeArray (DecodeJson f) = DecodeJson (Decoders.decodeArray f)

----------------------------------------------------------------------------------------------------
-- Object
----------------------------------------------------------------------------------------------------

-- | Decode a JSON object, applying one decoder to every value (keys stay
-- | as-is) - unlike `Data.Json.Decode.Record`, this is for an object
-- | whose *set* of keys isn't known ahead of time. Pass `decodeRawJson`
-- | to get the raw, undecoded `Object Json` back.
decodeObject :: ∀ a. DecodeJson a -> DecodeJson (Object a)
decodeObject (DecodeJson f) = DecodeJson (Decoders.decodeForeignObject f)

-- | For a wire format whose keys are data and whose values are read in
-- | terms of them - a response keyed by pair code, where the code belongs
-- | in the decoded value.
-- | Uses `runDecode`, `decodeObject`.
decodeObjectWithKey :: ∀ a. (String -> DecodeJson a) -> DecodeJson (Object a)
decodeObjectWithKey f = DecodeJson \json -> do
  obj <- runDecode (decodeObject decodeRawJson) json
  Obj.fromFoldable <$> for (Obj.toUnfoldable obj :: Array (String /\ Json))
    (\(k /\ v) -> map (Tuple k) (runDecode (f k) v))

-- | Decode a JSON object as a `Map k v`, parsing each key from its string.
-- | Uses `decodeTupleArrayFromObject`.
decodeMapFromObject
  :: ∀ k v
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
-- | Uses `decodeObject`, `fromFn`, `runDecode`.
decodeTupleArrayFromObject
  :: ∀ k v
   . (String -> Either JsonDecodeError k)
  -> DecodeJson v
  -> DecodeJson (Array (k /\ v))
decodeTupleArrayFromObject decodeK decodeV = do
  obj <- decodeObject decodeRawJson
  fromFn \_ ->
    for (Obj.toUnfoldable obj) \(kStr /\ vJson) -> do
      k <- decodeK kStr
      v <- runDecode decodeV vJson
      pure (k /\ v)

----------------------------------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------------------------------

-- | Parse a JSON string into a `Json` tree - the one step before any of
-- | the decoders above can run.
jsonParser :: String -> Either String Json
jsonParser = Parser.jsonParser
--
-- `decodeMapFromObject`
-- --------------------------------------------------------------------------------------------------
-- Tuple
-- --------------------------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------------------------
-- Map
-- --------------------------------------------------------------------------------------------------
