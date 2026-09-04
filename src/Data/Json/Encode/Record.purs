-- | Encode a `Record` field-by-field from a matching record of per-field
-- | encoders - see `Data.Json.Decode.Record` for the other direction.
module Data.Json.Encode.Record
  ( encodeRecord
  , class EncodeRecord
  , gEncodeRecord
  , EncodeOptional
  , encodeOptional
  ) where

import Data.Argonaut.Core (fromObject)
import Data.Json.Encode (EncodeJson, Json, fromFn, runEncode)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object as Obj
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

-- | record of per-field encoders, `r` the record of values they apply to.
encodeRecord
  :: ∀ rl rs r
   . RowToList rs rl
  => EncodeRecord rl rs r
  => Record rs
  -> EncodeJson (Record r)
encodeRecord rs = fromFn \r -> fromObject (gEncodeRecord @rl rs r)

-- | Generic derivation for `encodeRecord`, one field at a time via `rl`.
-- | Not meant to be used directly - go through `encodeRecord`.
class EncodeRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class EncodeRecord rl rs r | rl -> rs r where
  gEncodeRecord :: Record rs -> Record r -> Object Json

instance EncodeRecord RL.Nil () () where
  gEncodeRecord _ _ = Obj.empty

instance
  ( Row.Cons sym (EncodeJson a) rs' rs
  , Row.Cons sym a r' r
  , IsSymbol sym
  , EncodeRecord rl rs' r'
  , Row.Lacks sym rs'
  , Row.Lacks sym r'
  ) =>
  EncodeRecord (RL.Cons sym (EncodeJson a) rl) rs r where
  gEncodeRecord rs r = Obj.insert fieldName (runEncode encode value) tail
    where
    fieldName = reflectSymbol (Proxy @sym)
    encode = Record.get (Proxy @sym) rs
    value = Record.get (Proxy @sym) r

    tail = gEncodeRecord @rl (Record.delete (Proxy @sym) rs) (Record.delete (Proxy @sym) r)

----------------------------------------------------------------------------------------------------
-- Optional fields
----------------------------------------------------------------------------------------------------

-- | A field encoder that writes no key at all for `Nothing`.
-- |
-- | A distinct type rather than an encoder with a flag, for the same
-- | reason `DecodeWithDefault` is one: the generic derivation dispatches
-- | on it per field, so a record can have one optional field among
-- | required ones without every field opting into the same behaviour.
data EncodeOptional a = EncodeOptional (EncodeJson a)

-- | convention - `null` for `Nothing` - is `encodeMaybe`, and is a
-- | property of the value rather than of the record it sits in.
-- |
-- | This one *is* invertible, which is what separates it from a
-- | default: absent and present are two states, `Nothing` and `Just`
-- | are two states, and the mapping is a bijection. `decodeOptional` is
-- | the other half, and a codec can be built from the pair.
encodeOptional :: ∀ a. EncodeJson a -> EncodeOptional a
encodeOptional = EncodeOptional

instance
  ( Row.Cons sym (EncodeOptional a) rs' rs
  , Row.Cons sym (Maybe a) r' r
  , IsSymbol sym
  , EncodeRecord rl rs' r'
  , Row.Lacks sym rs'
  , Row.Lacks sym r'
  ) =>
  EncodeRecord (RL.Cons sym (EncodeOptional a) rl) rs r where
  gEncodeRecord rs r = case value of
    Nothing -> tail
    Just present -> Obj.insert fieldName (runEncode encode present) tail
    where
    fieldName = reflectSymbol (Proxy @sym)
    EncodeOptional encode = Record.get (Proxy @sym) rs
    value = Record.get (Proxy @sym) r

    tail = gEncodeRecord @rl (Record.delete (Proxy @sym) rs) (Record.delete (Proxy @sym) r)
