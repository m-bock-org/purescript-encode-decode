-- | A codec for a native tuple, as a flat JSON array - the
-- | bidirectional counterpart of `Data.Json.Encode.Tuple` and
-- | `Data.Json.Decode.Tuple`.
module Data.Json.Codec.Tuple
  ( codecTuple
  , class CodecTuple
  , codecTupleHalves
  ) where

import Data.Json.Codec (JsonCodec, codec)
import Data.Json.Codec.Internal (class SplitCodec, splitDecoder, splitEncoder)
import Data.Json.Decode.Tuple (class DecodeTupleParts, decodeTuple)
import Data.Json.Encode.Tuple (class EncodeTupleParts, encodeTuple)

-- | determined by the codec tuple's, so it never has to be written out.
codecTuple :: ∀ cs t. CodecTuple cs t => cs -> JsonCodec t
codecTuple = codecTupleHalves

-- | What a tuple of codecs has to satisfy. No row list here: `SplitCodec`
-- | already recurses through nested `/\`, which is exactly a tuple's
-- | shape, so the split needs no help.
class CodecTuple cs t where
  codecTupleHalves :: cs -> JsonCodec t

instance
  ( SplitCodec cs es ds
  , EncodeTupleParts es t
  , DecodeTupleParts ds t
  ) =>
  CodecTuple cs t where
  codecTupleHalves cs = codec
    (encodeTuple (splitEncoder cs))
    (decodeTuple (splitDecoder cs))
