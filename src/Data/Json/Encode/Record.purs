-- | Encode a `Record` field-by-field from a matching record of per-field
-- | encoders - see `Data.Json.Decode.Record` for the other direction.
module Data.Json.Encode.Record
  ( encodeRecord
  , class EncodeRecord
  , gEncodeRecord
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromObject)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object as Obj
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

-- | `encodeRecord { name: encodeString } { name: "ada" }` - `rs` is a
-- | record of per-field encoders, `r` the record of values they apply to.
encodeRecord :: forall rl rs r. RowToList rs rl => EncodeRecord rl rs r => Record rs -> Record r -> Json
encodeRecord rs r = fromObject $ gEncodeRecord @rl rs r

-- | Generic derivation for `encodeRecord`, one field at a time via `rl`.
-- | Not meant to be used directly - go through `encodeRecord`.
class EncodeRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class EncodeRecord rl rs r | rl -> rs r where
  gEncodeRecord :: Record rs -> Record r -> Object Json

instance EncodeRecord RL.Nil () () where
  gEncodeRecord _ _ = Obj.empty

instance
  ( Row.Cons sym (a -> Json) rs' rs
  , Row.Cons sym a r' r
  , IsSymbol sym
  , EncodeRecord rl rs' r'
  , Row.Lacks sym rs'
  , Row.Lacks sym r'
  ) =>
  EncodeRecord (RL.Cons sym _x rl) rs r where
  gEncodeRecord rs r = Obj.insert fieldName (encode value) tail
    where
    fieldName = reflectSymbol (Proxy @sym)
    encode = Record.get (Proxy @sym) rs
    value = Record.get (Proxy @sym) r

    tail = gEncodeRecord @rl (Record.delete (Proxy @sym) rs) (Record.delete (Proxy @sym) r)
