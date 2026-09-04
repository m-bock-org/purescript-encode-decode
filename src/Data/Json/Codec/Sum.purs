-- | A codec for a `data` type, from one record of per-constructor
-- | codecs - the bidirectional counterpart of `Data.Json.Encode.Sum` and
-- | `Data.Json.Decode.Sum`.
-- |
-- | This is where a codec earns most. The two sum modules share a wire
-- | format that neither can decide alone, and `Encoding` has to be
-- | passed to both, and passed the *same*. Here it is one argument.
module Data.Json.Codec.Sum
  ( codecSum
  , codecSumWith
  , codecEnum
  , codecEnumWith
  , class CodecSum
  , codecSumHalves
  , class CodecEnum
  , codecEnumHalves
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Json.Codec (JsonCodec, codec)
import Data.Json.Codec.Internal (class SplitCodecs, splitDecoders, splitEncoders)
import Data.Json.Decode.Sum (class DecodeCases, class DecodeEnum, decodeEnumWith, decodeSumWith)
import Data.Json.Encode.Sum (class EncodeCases, class EncodeEnum, encodeEnumWith, encodeSumWith)
import Data.Json.Sum.Encoding (Encoding, defaultEncoding)
import Prim.RowList (class RowToList)

-- | "Blob": unit }` - one entry per constructor, checked against the
-- | type's `Generic` representation.
-- | Uses `codecSumWith`.
codecSum :: ∀ rcs a. CodecSum rcs a => Record rcs -> JsonCodec a
codecSum = codecSumWith defaultEncoding

-- |
-- | Written as two directions, this argument is the one most likely to
-- | be changed on one side and forgotten on the other: a `tagKey` tweak
-- | that stops the decoder reading what the encoder now writes, with
-- | both sides still compiling and only a round trip to catch it.
codecSumWith :: ∀ rcs a. CodecSum rcs a => Encoding -> Record rcs -> JsonCodec a
codecSumWith = codecSumHalves

-- | What a record of per-constructor codecs has to satisfy.
-- |
-- | The `rep` that both halves are checked against is one type variable,
-- | so the two directions cannot disagree about which constructors
-- | exist. Everything else - the row list, the two split records - stays
-- | in the instance context where a caller never meets it.
class CodecSum rcs a where
  codecSumHalves :: Encoding -> Record rcs -> JsonCodec a

instance
  ( Generic a rep
  , RowToList rcs rl
  , SplitCodecs rl rcs res rds
  , EncodeCases res rep
  , DecodeCases rds rep
  ) =>
  CodecSum rcs a where
  codecSumHalves encoding rcs = codec
    (encodeSumWith encoding (splitEncoders @rl rcs))
    (decodeSumWith encoding (splitDecoders @rl rcs))

-- | A type whose constructors are all nullary, as a plain JSON string.
-- | No record: there is nothing per-constructor to describe.
-- | Uses `codecEnumWith`.
codecEnum :: ∀ a. CodecEnum a => JsonCodec a
codecEnum = codecEnumWith identity

-- | wire - `lowerFirst`, say.
-- |
-- | One function, not two, and that is this module's argument in
-- | miniature: written apart, these are a rewrite and its inverse, and
-- | nothing checks that they are inverses. With one, a rewrite that
-- | loses information fails to round-trip rather than compiling into a
-- | pair that disagrees.
codecEnumWith :: ∀ a. CodecEnum a => (String -> String) -> JsonCodec a
codecEnumWith = codecEnumHalves

-- | What an all-nullary type has to satisfy.
class CodecEnum a where
  codecEnumHalves :: (String -> String) -> JsonCodec a

instance
  ( Generic a rep
  , EncodeEnum rep
  , DecodeEnum rep
  ) =>
  CodecEnum a where
  codecEnumHalves mapTag = codec (encodeEnumWith mapTag) (decodeEnumWith mapTag)
