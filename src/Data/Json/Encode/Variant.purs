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
  ( encodeVariant
  , encodeVariantWith
  , class EncodeVariant
  , gEncodeVariant
  ) where

import Data.Json.Encode (EncodeJson, Json, fromFn, runEncode)
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

-- | `encodeVariant { newState: encodeState }` - one entry per case.
encodeVariant
  :: forall rl ri ro
   . RowToList ri rl
  => EncodeVariant rl ri ro
  => Record ri
  -> EncodeJson (Variant ro)
encodeVariant = encodeVariantWith variantEncoding

-- | `encodeVariant` with an explicit wire format - see `Encoding`.
encodeVariantWith
  :: forall rl ri ro
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
