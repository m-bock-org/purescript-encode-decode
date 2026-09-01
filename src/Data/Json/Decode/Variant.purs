-- | Decode a `Variant` case-by-case from a matching record of per-case
-- | decoders - see `Data.Json.Encode.Variant` for the other direction.
module Data.Json.Decode.Variant
  ( decodeVariant
  , decodeVariantWith
  , class DecodeVariant
  , gDecodeVariant
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Json.Decode (DecodeJson, Json, fromFn, runDecode)
import Data.Json.Decode.Sum (Err(..), finalizeErr, jErr, lookupCase, singleValue)
import Data.Json.Sum.Encoding (Encoding, variantEncoding)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

-- | `decodeVariant { newState: decodeState }` - one entry per case,
-- | tried in turn until one claims the tag.
decodeVariant
  :: forall rl ri ro
   . RowToList ri rl
  => DecodeVariant rl ri ro
  => Record ri
  -> DecodeJson (Variant ro)
decodeVariant = decodeVariantWith variantEncoding

-- | `decodeVariant` with an explicit wire format - see `Encoding`.
decodeVariantWith
  :: forall rl ri ro
   . RowToList ri rl
  => DecodeVariant rl ri ro
  => Encoding
  -> Record ri
  -> DecodeJson (Variant ro)
decodeVariantWith encoding ri = fromFn (finalizeErr <<< gDecodeVariant @rl encoding ri)

-- | Generic derivation for `decodeVariant`, one case at a time via
-- | `rl`. Not meant to be used directly - go through `decodeVariant`.
-- |
-- | `ri` and `ro` do not shrink as the walk proceeds, unlike the encode
-- | side. Decoding injects *into* the whole row at whichever label
-- | claims the tag, so every step needs the full row - where encoding
-- | peels a label off and never looks at it again. The asymmetry is the
-- | directions', not an accident of the implementation.
-- |
-- | `Err` is the sum modules' own, and carries the same distinction for
-- | the same reason: a tag that did not match means try the next case,
-- | a payload that did not decode means stop. Collapsing them would
-- | report a broken payload as "no case matched".
class DecodeVariant :: RL.RowList Type -> Row Type -> Row Type -> Constraint
class DecodeVariant rl ri ro where
  gDecodeVariant :: Encoding -> Record ri -> Json -> Either Err (Variant ro)

instance DecodeVariant RL.Nil ri ro where
  gDecodeVariant _ _ _ = Left UnmatchedCase

instance
  ( Row.Cons sym (DecodeJson a) ri' ri
  , Row.Cons sym a ro' ro
  , IsSymbol sym
  , DecodeVariant rl ri ro
  ) =>
  DecodeVariant (RL.Cons sym (DecodeJson a) rl) ri ro where
  gDecodeVariant encoding ri json = case thisCase of
    Left UnmatchedCase -> gDecodeVariant @rl encoding ri json
    settled -> settled
    where
    decode = Record.get (Proxy @sym) ri

    thisCase :: Either Err (Variant ro)
    thisCase = do
      payload <- lookupCase encoding json (reflectSymbol (Proxy @sym))
      value <- singleValue encoding payload
      V.inj (Proxy @sym) <$> jErr (runDecode decode value)
