-- | A `Record` codec, from one record of per-field codecs - the
-- | bidirectional counterpart of `Data.Json.Encode.Record` and
-- | `Data.Json.Decode.Record`.
module Data.Json.Codec.Record
  ( codecRecord
  , class CodecRecord
  , codecRecordHalves
  , CodecOptional
  , codecOptional
  ) where

import Data.Json.Codec (JsonCodec, decoder, encoder, codec)
import Data.Json.Codec.Internal (class SplitCodec, class SplitCodecs, splitDecoders, splitEncoders)
import Data.Json.Decode.Record (DecodeWithDefault, class DecodeRecord, decodeOptional, decodeRecord)
import Data.Json.Encode.Record (EncodeOptional, class EncodeRecord, encodeOptional, encodeRecord)
import Data.Maybe (Maybe)
import Prim.RowList (class RowToList)

-- | codecs, and both directions come out of it.
codecRecord :: ∀ rcs r. CodecRecord rcs r => Record rcs -> JsonCodec (Record r)
codecRecord = codecRecordHalves

-- | What a record of codecs has to satisfy: split into halves, each
-- | half a valid description of the *same* record type.
-- |
-- | One class with one instance, and that is the whole point of it. The
-- | work is all in the instance's context, where the two intermediate
-- | row types live and stay - so `codecRecord` reads with two type
-- | variables and one constraint instead of six and six. A signature is
-- | read far more often than an instance head.
-- |
-- | `r` appears once, in both `EncodeRecord` and `DecodeRecord`. So the
-- | halves are not merely built from one source - they are proved to
-- | describe the same type, and a field one writes and the other does
-- | not read is a compile error.
class CodecRecord rcs r where
  codecRecordHalves :: Record rcs -> JsonCodec (Record r)

instance
  ( RowToList rcs rl
  , SplitCodecs rl rcs res rds
  , RowToList res rle
  , EncodeRecord rle res r
  , RowToList rds rld
  , DecodeRecord rld rds r
  ) =>
  CodecRecord rcs r where
  codecRecordHalves rcs = codec
    (encodeRecord (splitEncoders @rl rcs))
    (decodeRecord (splitDecoders @rl rcs))

-- | A field that is absent from the document when it is `Nothing`.
newtype CodecOptional a = CodecOptional (JsonCodec a)

-- | type is `Maybe String`, and `Nothing` means the key is not written
-- | rather than written as `null`.
-- |
-- | The one default a codec can carry, and worth saying why. Every other
-- | default is a decision about what to believe when a writer said
-- | nothing, which only the decode side can make. Absence-means-`Nothing`
-- | is not a belief: absent and present are two states, `Nothing` and
-- | So it round-trips, and so it belongs here - while
codecOptional :: ∀ a. JsonCodec a -> CodecOptional a
codecOptional = CodecOptional

instance SplitCodec (CodecOptional a) (EncodeOptional a) (DecodeWithDefault (Maybe a)) where
  splitEncoder (CodecOptional c) = encodeOptional (encoder c)
  splitDecoder (CodecOptional c) = decodeOptional (decoder c)
