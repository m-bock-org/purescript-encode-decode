-- | Encode a `Variant` case-by-case from a matching record of
-- | per-case encoders - see `Data.Json.Decode.Variant` for the other
-- | direction and `Data.Json.Sum.Encoding` for the wire format the two
-- | share.
-- |
-- | A `Variant` is a sum, so this is the sum modules again with the
-- | hard part removed: the row is carried in the type, so nothing has
-- | to be derived from a `Generic` representation, and the labels are
-- | the constructor names.
module Data.Json.Encode.Variant
  ( encodeEnum
  , encodeEnumWith
  , encodeVariant
  , encodeVariantWith
  , class EncodeEnum
  , class EncodeVariant
  , gEncodeEnum
  , gEncodeVariant
  ) where

import Data.Json.Encode (EncodeJson, Json, encodeString, fromFn, runEncode)
import Data.Json.Encode.Sum (encodeSumCase)
import Data.Json.Sum.Encoding (Encoding, variantEncoding)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

-- | Uses `encodeVariantWith`.
encodeVariant
  :: ∀ rl ri ro
   . RowToList ri rl
  => EncodeVariant rl ri ro
  => Record ri
  -> EncodeJson (Variant ro)
encodeVariant = encodeVariantWith variantEncoding

encodeVariantWith
  :: ∀ rl ri ro
   . RowToList ri rl
  => EncodeVariant rl ri ro
  => Encoding
  -> Record ri
  -> EncodeJson (Variant ro)
encodeVariantWith encoding ri = fromFn (gEncodeVariant @rl encoding ri)

-- | Generic derivation for `encodeVariant`, one case at a time via
-- | `rl`. Not meant to be used directly - go through `encodeVariant`.
-- |
-- | The recursion is `Data.Variant`'s own: `on` peels one label off the
-- | row and hands the rest to the next step, and `case_` is the base -
-- | a `Variant ()` has no inhabitants, so the walk is total by
-- | construction rather than by a fallthrough nobody can reach.
class EncodeVariant :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class EncodeVariant rl ri ro | rl -> ri ro where
  gEncodeVariant :: Encoding -> Record ri -> Variant ro -> Json

instance EncodeVariant RL.Nil () () where
  gEncodeVariant _ _ = V.case_

instance
  ( Row.Cons sym (EncodeJson a) ri' ri
  , Row.Cons sym a ro' ro
  , Row.Lacks sym ri'
  , Row.Lacks sym ro'
  , IsSymbol sym
  , EncodeVariant rl ri' ro'
  ) =>
  EncodeVariant (RL.Cons sym (EncodeJson a) rl) ri ro where
  gEncodeVariant encoding ri =
    V.on (Proxy @sym)
      (\a -> encodeSumCase encoding (reflectSymbol (Proxy @sym)) [ runEncode encode a ])
      (gEncodeVariant @rl encoding (Record.delete (Proxy @sym) ri))
    where
    encode = Record.get (Proxy @sym) ri

-- | Uses `encodeEnumWith`.
encodeEnum
  :: ∀ rl ro
   . RowToList ro rl
  => EncodeEnum rl ro
  => EncodeJson (Variant ro)
encodeEnum = encodeEnumWith (\said -> said)

encodeEnumWith
  :: ∀ rl ro
   . RowToList ro rl
  => EncodeEnum rl ro
  => (String -> String)
  -> EncodeJson (Variant ro)
encodeEnumWith mapTag = fromFn (gEncodeEnum @rl mapTag)

-- | Generic derivation for `encodeEnum`, one case at a time via
-- | `rl`. Not meant to be used directly.
class EncodeEnum :: RL.RowList Type -> Row Type -> Constraint
class EncodeEnum rl ro | rl -> ro where
  gEncodeEnum :: (String -> String) -> Variant ro -> Json

instance EncodeEnum RL.Nil () where
  gEncodeEnum _ = V.case_

instance
  ( Row.Cons sym (Record pr) ro' ro
  , Row.Lacks sym ro'
  , RowToList pr RL.Nil
  , IsSymbol sym
  , EncodeEnum rl ro'
  ) =>
  EncodeEnum (RL.Cons sym (Record pr) rl) ro where
  gEncodeEnum mapTag =
    V.on (Proxy @sym)
      (\_ -> runEncode encodeString (mapTag (reflectSymbol (Proxy @sym))))
      (gEncodeEnum @rl mapTag)

-- ## Context
--
-- `encodeEnum`
-- A `Variant` whose every case carries `{}`, as a plain JSON string -
-- the `Variant` counterpart of `Data.Json.Encode.Sum.encodeEnum`.
--
-- No record of per-case encoders, because there is nothing per case to
-- encode: `{}` has one value, and the tag is the whole of the
-- information. That is also what keeps the row in one type variable
-- rather than two - there is no `ri` to disagree with `ro`.
--
-- **`Record pr` with `RowToList pr RL.Nil`, not `{}`.** The empty row
-- cannot appear in an instance head - "all types appearing in instance
-- declarations must be of the form `T a_1 .. a_n`" - so the payload is
-- a variable and a constraint says it is empty. The effect is the one
-- wanted: a row with a payload anywhere in it has no instance, so a
-- case that starts carrying something is a compile error at the codec
-- rather than a payload silently dropped on the way out.
