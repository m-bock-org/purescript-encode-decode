-- | A codec for a `Variant`, from one record of per-case codecs - the
-- | bidirectional counterpart of `Data.Json.Encode.Variant` and
-- | `Data.Json.Decode.Variant`.
module Data.Json.Codec.Variant
  ( codecVariant
  , codecVariantWith
  , class CodecVariant
  , codecVariantHalves
  ) where

import Data.Json.Codec (JsonCodec, codec)
import Data.Json.Codec.Internal (class SplitCodecs, splitDecoders, splitEncoders)
import Data.Json.Decode.Variant (class DecodeVariant, decodeVariantWith)
import Data.Json.Encode.Variant (class EncodeVariant, encodeVariantWith)
import Data.Json.Sum.Encoding (Encoding, variantEncoding)
import Data.Variant (Variant)
import Prim.RowList (class RowToList)

-- | both directions come out of it.
-- | Uses `codecVariantWith`.
codecVariant :: ∀ rcs ro. CodecVariant rcs ro => Record rcs -> JsonCodec (Variant ro)
codecVariant = codecVariantWith variantEncoding

codecVariantWith
  :: ∀ rcs ro
   . CodecVariant rcs ro
  => Encoding
  -> Record rcs
  -> JsonCodec (Variant ro)
codecVariantWith = codecVariantHalves

-- | What a record of per-case codecs has to satisfy.
-- |
-- | `ro` appears in both halves, so the two directions cannot disagree
-- | about which cases exist - which for a `Variant` is the whole type,
-- | not just its shape.
class CodecVariant rcs ro where
  codecVariantHalves :: Encoding -> Record rcs -> JsonCodec (Variant ro)

instance
  ( RowToList rcs rl
  , SplitCodecs rl rcs res rds
  , RowToList res rle
  , EncodeVariant rle res ro
  , RowToList rds rld
  , DecodeVariant rld rds ro
  ) =>
  CodecVariant rcs ro where
  codecVariantHalves encoding rcs = codec
    (encodeVariantWith encoding (splitEncoders @rl rcs))
    (decodeVariantWith encoding (splitDecoders @rl rcs))
