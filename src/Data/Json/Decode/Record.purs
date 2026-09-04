-- | Decode a `Record` field-by-field from a matching record of per-field
-- | decoders - see `Data.Json.Encode.Record` for the other direction.
module Data.Json.Decode.Record
  ( module Data.Argonaut.Decode.Error
  , decodeRecord
  , class DecodeRecord
  , gDecodeRecord
  , DecodeWithDefault
  , decodeWithDefault
  , decodeOptional
  , decodeRecordWithDefaults
  , DecodeDefaults
  ) where

import Prelude

import Data.Argonaut.Core (toObject)
import Data.Argonaut.Decode (getField)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Json.Decode (DecodeJson, Json, decodeMaybe, fromFn, runDecode)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Either as Either
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Heterogeneous.Mapping (class MappingWithIndex, hmapWithIndex, class HMapWithIndex)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

----------------------------------------------------------------------------------------------------
-- Decode
----------------------------------------------------------------------------------------------------

-- | per-field decoders, matched against `json`'s own keys.
decodeRecord
  :: ∀ rl rs r
   . RowToList rs rl
  => DecodeRecord rl rs r
  => Record rs
  -> DecodeJson (Record r)
decodeRecord rs = fromFn \json -> do
  obj <- Either.note (TypeMismatch "Object") (toObject json)
  gDecodeRecord @rl rs obj

-- | Generic derivation for `decodeRecord`, one field at a time via `rl`.
-- | Not meant to be used directly - go through `decodeRecord`.
class DecodeRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class DecodeRecord rl rs r | rl -> rs r where
  gDecodeRecord :: Record rs -> Object Json -> Either JsonDecodeError (Record r)

instance DecodeRecord RL.Nil () () where
  gDecodeRecord _ _ = pure {}

instance
  ( Row.Cons sym (DecodeJson a) rs' rs
  , Row.Cons sym a r' r
  , IsSymbol sym
  , DecodeRecord rl rs' r'
  , Row.Lacks sym rs'
  , Row.Lacks sym r'
  ) =>
  DecodeRecord (RL.Cons sym (DecodeJson a) rl) rs r where
  gDecodeRecord rs obj = do
    field :: Json <- getField obj fieldName
    parsed <- runDecode decode field
    tail <- getTail
    pure $ Record.insert (Proxy @sym) parsed tail
    where
    fieldName = reflectSymbol (Proxy @sym)
    decode = Record.get (Proxy @sym) rs

    getTail :: Either JsonDecodeError (Record r')
    getTail = gDecodeRecord @rl (Record.delete (Proxy @sym) rs) obj

-- | Only a *missing* key falls back to `default` - `getField` here is
-- | called with no decoder beyond "extract the raw `Json` at this key",
-- | so it can only fail on absence, never on the value's shape. A key
-- | that *is* present, `null` included, always goes through `decode`
-- | (the `Right field ->` branch) like any other value.
-- |
-- | This is deliberate, not an oversight: `null` isn't a universal
-- | "use the default" sentinel here, because it's already a meaningful,
-- | *different* value for some field types - a `Maybe a` field decodes
-- | `null` as `Nothing`, which is not the same thing as "no value was
-- | given, substitute the configured default". Treating every `null` as
-- | "missing" would silently turn a real `Nothing` into `default`
-- | instead, for any field whose type happens to be `Maybe a`. Letting
-- | `null` fall through to each field's own decoder means it means
-- | whatever that decoder says it means - `Nothing` for `Maybe a`, a
-- | genuine decode error for anything that doesn't accept `null` - with
-- | no special-casing needed here to get that right.
instance
  ( Row.Cons sym (DecodeWithDefault a) rs' rs
  , Row.Cons sym a r' r
  , IsSymbol sym
  , DecodeRecord rl rs' r'
  , Row.Lacks sym rs'
  , Row.Lacks sym r'
  ) =>
  DecodeRecord (RL.Cons sym (DecodeWithDefault a) rl) rs r where
  gDecodeRecord rs obj = case getField obj fieldName of
    Left _ -> do
      tail <- getTail
      pure $ Record.insert (Proxy @sym) defaultValue tail
    Right field -> do
      parsed <- runDecode decode field
      tail <- getTail
      pure $ Record.insert (Proxy @sym) parsed tail
    where
    fieldName = reflectSymbol (Proxy @sym)
    DecodeWithDefault { default: defaultValue, decode } = Record.get (Proxy @sym) rs

    getTail :: Either JsonDecodeError (Record r')
    getTail = gDecodeRecord @rl (Record.delete (Proxy @sym) rs) obj

----------------------------------------------------------------------------------------------------
-- Decode with defaults
----------------------------------------------------------------------------------------------------

-- | A field decoder plus what to use when the field is *missing*
-- | entirely - a distinct type from a plain `Json -> Either
-- | JsonDecodeError a`, not just a function with a fallback baked in, so
-- | `DecodeRecord`'s generic derivation can dispatch on it per field (see
-- | the `DecodeRecord (RL.Cons sym (DecodeWithDefault a) rl) rs r`
-- | instance above) and catch a missing key *before* `decode` ever runs,
-- | rather than needing every field in a record to opt into the same
-- | fallback behavior.
data DecodeWithDefault a = DecodeWithDefault
  { default :: a
  , decode :: DecodeJson a
  }

-- | Build a `DecodeWithDefault` from a default value and a decoder.
decodeWithDefault :: ∀ a. a -> DecodeJson a -> DecodeWithDefault a
decodeWithDefault def dec = DecodeWithDefault { default: def, decode: dec }

-- | A field whose absence means `Nothing` - the other half of
-- |
-- | Written in terms of `decodeWithDefault` because that is all it is,
-- | but it is worth its own name: every *other* default is one-way
-- | tolerance, a decision about what to believe when a writer said
-- | nothing. This one is invertible, and so is the only default a codec
-- | can be built from.
-- |
-- | An explicit `null` also reads as `Nothing`, which is a shade more
-- | tolerant than `encodeOptional` is generous. That costs nothing that
-- | matters: it is the *value* a round trip has to preserve, and both
-- | spellings of absence decode to the same one.
-- | Uses `decodeWithDefault`.
decodeOptional :: ∀ a. DecodeJson a -> DecodeWithDefault (Maybe a)
decodeOptional dec = decodeWithDefault Nothing (decodeMaybe dec)

-- | The `hmapWithIndex` "props" that pairs each field's plain decoder
-- | (from `decs` in `decodeRecordWithDefaults`) with that same field's
-- | default (looked up from `defs` by symbol) to build a
-- | `DecodeWithDefault a` per field - not meant to be reached for
-- | directly outside `decodeRecordWithDefaults`.
newtype DecodeDefaults defs = DecodeDefaults { | defs }

instance decodeDefaultsMapping ::
  ( IsSymbol sym
  , Row.Cons sym a x defs
  ) =>
  MappingWithIndex (DecodeDefaults defs) (Proxy sym) (DecodeJson a) (DecodeWithDefault a) where
  mappingWithIndex (DecodeDefaults defs) prop dec = decodeWithDefault (Record.get prop defs) dec

-- | value in `defs` when that field's key is missing from the JSON
-- | object entirely - see `DecodeWithDefault`'s own doc comment for why
-- | that's "missing", specifically, and not `null`. `defs` and `decs`
-- | share field names but not field types (`defs`'s fields are the plain
-- | values, `decs`'s are their decoders) - `hmapWithIndex`/
-- | despite each field having its own, different type, the same way
-- | types rather than requiring every field to share one.
-- |
-- | decodeBook :: DecodeJson { title :: String, pages :: Int }
-- | decodeBook = decodeRecordWithDefaults defaultBook
-- |   { title: decodeString
-- |   , pages: decodeInt
-- |   }
-- | Uses `decodeRecord`.
decodeRecordWithDefaults
  :: ∀ rl rdecsd r rdecs defs
   . RowToList rdecsd rl
  => DecodeRecord rl rdecsd r
  => HMapWithIndex (DecodeDefaults defs) (Record rdecs) (Record rdecsd)
  => Record defs
  -> Record rdecs
  -> DecodeJson (Record r)
decodeRecordWithDefaults defs decs = decodeRecord (hmapWithIndex (DecodeDefaults defs) decs)
