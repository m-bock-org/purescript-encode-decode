module Data.Json.Record
  ( class EncodeRecord
  , gEncodeRecord
  , class DecodeRecord
  , gDecodeRecord
  , encodeRecord
  , decodeRecord
  ) where

import Prelude

import Data.Argonaut.Core (Json, fromObject, toObject)
import Data.Argonaut.Decode (getField)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either)
import Data.Either as Either
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object as Obj
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

----------------------------------------------------------------------------------------------------
-- Encode
----------------------------------------------------------------------------------------------------

encodeRecord :: forall rl rs r. RowToList rs rl => EncodeRecord rl rs r => Record rs -> Record r -> Json
encodeRecord rs r = fromObject $ gEncodeRecord @rl rs r

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

----------------------------------------------------------------------------------------------------
-- Decode
----------------------------------------------------------------------------------------------------

decodeRecord :: forall rl rs r. RowToList rs rl => DecodeRecord rl rs r => Record rs -> Json -> Either JsonDecodeError (Record r)
decodeRecord rs json = gDecodeRecord @rl rs json

class DecodeRecord :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class DecodeRecord rl rs r | rl -> rs r where
  gDecodeRecord :: Record rs -> Json -> Either JsonDecodeError (Record r)

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
  DecodeRecord (RL.Cons sym _x rl) rs r where
  gDecodeRecord rs json = do
    obj <- Either.note (TypeMismatch "Object") (toObject json)
    field :: Json <- getField obj fieldName
    parsed <- decode field
    tail <- getTail
    pure $ Record.insert (Proxy @sym) parsed tail
    where
    fieldName = reflectSymbol (Proxy @sym)
    decode = Record.get (Proxy @sym) rs

    getTail :: Either JsonDecodeError (Record r')
    getTail = gDecodeRecord @rl (Record.delete (Proxy @sym) rs) json
