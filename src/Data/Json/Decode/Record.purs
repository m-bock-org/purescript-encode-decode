-- | Decode a `Record` field-by-field from a matching record of per-field
-- | decoders - see `Data.Json.Encode.Record` for the other direction.
module Data.Json.Decode.Record
  ( module Data.Argonaut.Decode.Error
  , decodeRecord
  , class DecodeRecord
  , gDecodeRecord
  , DecodeWithDefault
  , decodeWithDefault
  , decodeRecordWithDefaults
  , DecodeDefaults
  ) where

import Prelude

import Data.Argonaut.Core (Json, toObject)
import Data.Argonaut.Decode (getField)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..))
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

-- | `decodeRecord { name: decodeString } json` - `rs` is a record of
-- | per-field decoders, matched against `json`'s own keys.
decodeRecord :: forall rl rs r. RowToList rs rl => DecodeRecord rl rs r => Record rs -> Json -> Either JsonDecodeError (Record r)
decodeRecord rs json = do
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
  ( Row.Cons sym (Json -> Either JsonDecodeError a) rs' rs
  , Row.Cons sym a r' r
  , IsSymbol sym
  , DecodeRecord rl rs' r'
  , Row.Lacks sym rs'
  , Row.Lacks sym r'
  ) =>
  DecodeRecord (RL.Cons sym (Json -> Either JsonDecodeError a) rl) rs r where
  gDecodeRecord rs obj = do
    field :: Json <- getField obj fieldName
    parsed <- decode field
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
      parsed <- decode field
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
  , decode :: Json -> Either JsonDecodeError a
  }

-- | Build a `DecodeWithDefault` from a default value and a decoder.
decodeWithDefault :: forall a. a -> (Json -> Either JsonDecodeError a) -> DecodeWithDefault a
decodeWithDefault def dec = DecodeWithDefault { default: def, decode: dec }

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
  MappingWithIndex (DecodeDefaults defs) (Proxy sym) (Json -> Either JsonDecodeError a) (DecodeWithDefault a) where
  mappingWithIndex (DecodeDefaults defs) prop dec = decodeWithDefault (Record.get prop defs) dec

-- | `decodeRecord`, but every field falls back to its corresponding
-- | value in `defs` when that field's key is missing from the JSON
-- | object entirely - see `DecodeWithDefault`'s own doc comment for why
-- | that's "missing", specifically, and not `null`. `defs` and `decs`
-- | share field names but not field types (`defs`'s fields are the plain
-- | values, `decs`'s are their decoders) - `hmapWithIndex`/
-- | `DecodeDefaults` is what lets them be zipped together field-by-field
-- | despite each field having its own, different type, the same way
-- | `decodeRecord`/`encodeRecord` are already generic over per-field
-- | types rather than requiring every field to share one.
-- |
-- | ```purescript
-- | decodeBook :: Json -> Either JsonDecodeError { title :: String, pages :: Int }
-- | decodeBook = decodeRecordWithDefaults defaultBook
-- |   { title: decodeString
-- |   , pages: decodeInt
-- |   }
-- | ```
decodeRecordWithDefaults
  :: forall rl rdecsd r rdecs defs
   . RowToList rdecsd rl
  => DecodeRecord rl rdecsd r
  => HMapWithIndex (DecodeDefaults defs) (Record rdecs) (Record rdecsd)
  => Record defs
  -> Record rdecs
  -> Json
  -> Either JsonDecodeError (Record r)
decodeRecordWithDefaults defs decs = decodeRecord (hmapWithIndex (DecodeDefaults defs) decs)
