-- | Decode a flat JSON array of any length as a native tuple, from a
-- | matching tuple of per-position decoders - see
-- | `Data.Json.Encode.Tuple` for the other direction.
-- |
-- | ```purescript
-- | decodeRow :: DecodeJson (String /\ Int /\ Boolean)
-- | decodeRow = decodeTuple (decodeString /\ decodeInt /\ decodeBoolean)
-- | ```
-- |
-- | The array's length has to match the tuple's exactly: a short array
-- | fails on the first position it cannot fill, a long one fails at the
-- | end rather than quietly dropping the surplus. Errors carry the
-- | failing position as `AtIndex`.
module Data.Json.Decode.Tuple
  ( decodeTuple
  , class DecodeTupleParts
  , gDecodeTupleParts
  ) where

import Prelude

import Data.Array (length, uncons) as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Json.Decode
  ( DecodeJson
  , Json
  , JsonDecodeError(..)
  , decodeArray
  , decodeRawJson
  , fromFn
  , runDecode
  )
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))

-- | `decodeTuple (decodeString /\ decodeInt)` - the result tuple's type
-- | is determined by the decoder tuple's, so it never has to be written
-- | out.
decodeTuple :: forall d t. DecodeTupleParts d t => d -> DecodeJson t
decodeTuple decoders = do
  jsons <- decodeArray decodeRawJson
  fromFn \_ -> gDecodeTupleParts 0 decoders jsons

-- | Generic derivation for `decodeTuple`, one position at a time,
-- | carrying the index for error reporting. Not meant to be used
-- | directly - go through `decodeTuple`.
-- |
-- | See `Data.Json.Encode.Tuple`'s matching class for why these two
-- | instances do not overlap, and why that depends on `DecodeJson`
-- | being a newtype rather than a type alias.
class DecodeTupleParts :: Type -> Type -> Constraint
class DecodeTupleParts d t | d -> t where
  gDecodeTupleParts :: Int -> d -> Array Json -> Either JsonDecodeError t

instance
  DecodeTupleParts drest trest =>
  DecodeTupleParts (DecodeJson a /\ drest) (a /\ trest) where
  gDecodeTupleParts index (decodeHead /\ decodeRest) jsons =
    case Array.uncons jsons of
      Nothing -> Left (AtIndex index MissingValue)
      Just { head, tail } -> do
        value <- lmap (AtIndex index) (runDecode decodeHead head)
        rest <- gDecodeTupleParts (index + 1) decodeRest tail
        pure (value /\ rest)

else instance DecodeTupleParts (DecodeJson a) a where
  gDecodeTupleParts index decodeLast jsons = case Array.uncons jsons of
    Nothing -> Left (AtIndex index MissingValue)
    Just { head, tail }
      | Array.length tail == 0 -> lmap (AtIndex index) (runDecode decodeLast head)
      | otherwise -> Left (TypeMismatch (show (index + 1) <> "-element array"))
