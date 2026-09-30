-- | A codec for a `Variant`, from one record of per-case codecs - the
-- | bidirectional counterpart of `Data.Json.Encode.Variant` and
-- | `Data.Json.Decode.Variant`.
module Data.Json.Codec.Variant
  ( codecEnum
  , codecEnumWith
  , codecVariant
  , codecVariantWith
  , class CodecEnum
  , class CodecVariant
  , codecEnumHalves
  , codecVariantHalves
  ) where

import Data.Json.Codec (JsonCodec, codec)
import Data.Json.Codec.Internal (class SplitCodecs, splitDecoders, splitEncoders)
import Data.Json.Decode.Variant
  ( class DecodeEnum
  , class DecodeVariant
  , decodeEnumWith
  , decodeVariantWith
  )
import Data.Json.Encode.Variant
  ( class EncodeEnum
  , class EncodeVariant
  , encodeEnumWith
  , encodeVariantWith
  )
import Data.Json.Sum.Encoding (Encoding, variantEncoding)
import Data.Variant (Variant)
import Prim.RowList (class RowToList)

-- | both directions come out of it.
-- | Uses `codecVariantWith`.
codecVariant :: ∀ rcs ro. CodecVariant rcs ro => Record rcs -> JsonCodec (Variant ro)
codecVariant = codecVariantWith variantEncoding

codecVariantWith :: ∀ rcs ro. CodecVariant rcs ro => Encoding -> Record rcs -> JsonCodec (Variant ro)
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

-- | A `Variant` whose every case carries `{}`, as a plain JSON string.
-- | No record: there is nothing per case to describe.
-- | Uses `codecEnumWith`.
codecEnum :: ∀ ro. CodecEnum ro => JsonCodec (Variant ro)
codecEnum = codecEnumWith (\said -> said)

-- | The same, with a rewrite from a label to the word on the wire -
-- | `lowerFirst`, say.
-- |
-- | One function, not two, and for the reason this module exists:
-- | written apart, a rewrite and its inverse are two things nothing
-- | checks against each other. With one, a rewrite that loses
-- | information fails to round-trip rather than compiling into a pair
-- | that disagrees.
codecEnumWith :: ∀ ro. CodecEnum ro => (String -> String) -> JsonCodec (Variant ro)
codecEnumWith = codecEnumHalves

-- | What a payload-free `Variant` has to satisfy.
-- |
-- | `ro` is the only parameter, so there is nothing for the two
-- | directions to disagree about.
class CodecEnum ro where
  codecEnumHalves :: (String -> String) -> JsonCodec (Variant ro)

instance
  ( RowToList ro rl
  , EncodeEnum rl ro
  , DecodeEnum rl ro
  ) =>
  CodecEnum ro where
  codecEnumHalves mapTag = codec (encodeEnumWith mapTag) (decodeEnumWith mapTag)

-- ## Context
--
-- `codecEnum`
-- A tag and nothing else, which is the shape a great many small types
-- have: a phase, a reach, a status. Written out, each of them costs a
-- pair of functions and a `String` in the middle - a printer, a
-- parser that has to say what it could not make of a word, and a
-- `codecRefine` joining them. Three times in the fleet before it was
-- worth being a function.
--
-- The `Variant` case rather than the `data` one, which
-- `Data.Json.Codec.Sum.codecEnum` already covers. It is the easier of
-- the two: the labels are in the type, so there is no `Generic`
-- representation to walk and no `newtype` of a row to see through.
--
-- What it does *not* do is a type that wraps one. A `newtype` over a
-- payload-free `Variant` - `Phase` in the regulator, whose `Ord` is
-- load-bearing and so cannot be a bare `Variant` - is this codec plus
-- `codecInvmap`, and that is the right place for the wrapper to be
-- dealt with: one line at the call site, rather than a second class
-- here that guesses at which `newtype`s are enums.
