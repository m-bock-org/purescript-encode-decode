-- | How a `data` type's constructors map onto JSON. Shared by
-- | `Data.Json.Encode.Sum` and `Data.Json.Decode.Sum` - the two
-- | directions stay separate everywhere else in this library, but they
-- | have to agree on the wire format itself or they won't round-trip.
module Data.Json.Sum.Encoding
  ( Encoding(..)
  , defaultEncoding
  , variantEncoding
  , lowerFirst
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton, uncons) as SCU
import Data.String.Common (toLower) as String

-- | `EncodeTagged` puts the constructor name and its payload under two
-- | sibling keys: `{ "tag": "Circle", "values": [...] }`. `EncodeNested`
-- | uses the constructor name itself as the key:
-- | `{ "Circle": [...] }`.
-- |
-- | Options, all of which apply to both:
-- |
-- | - `mapTag` rewrites the constructor name before it hits the wire -
-- |   `lowerFirst` for camelCase output, say. Applied on decode too, so
-- |   it has to be a function whose output identifies the constructor
-- |   uniquely.
-- | - `unwrapSingleArguments` encodes a one-argument constructor's
-- |   payload directly instead of as a single-element array.
-- | - `omitEmptyArguments` (tagged only) drops the values key entirely
-- |   for a nullary constructor, leaving just the tag.
data Encoding
  = EncodeNested
      { unwrapSingleArguments :: Boolean
      , mapTag :: String -> String
      }
  | EncodeTagged
      { tagKey :: String
      , valuesKey :: String
      , omitEmptyArguments :: Boolean
      , unwrapSingleArguments :: Boolean
      , mapTag :: String -> String
      }

-- | Tagged, with constructor names verbatim and payloads always
-- | wrapped in an array. Override with `encodeSumWith`/`decodeSumWith`.
defaultEncoding :: Encoding
defaultEncoding = EncodeTagged
  { tagKey: "tag"
  , valuesKey: "values"
  , unwrapSingleArguments: false
  , omitEmptyArguments: false
  , mapTag: identity
  }

-- | What a `Variant` gets by default: `{ "tag": "newState", "value":
-- | ... }`.
-- |
-- | Different from `defaultEncoding` in two ways, both because a
-- | `Variant` case carries exactly one value where a constructor
-- | carries any number - so the key is `value` rather than `values`,
-- | and it is not wrapped in an array it could never have more than one
-- | element in.
variantEncoding :: Encoding
variantEncoding = EncodeTagged
  { tagKey: "tag"
  , valuesKey: "value"
  , unwrapSingleArguments: true
  , omitEmptyArguments: false
  , mapTag: identity
  }

-- | Lowercases the first character only - `"TradeTick"` becomes
-- | `"tradeTick"`. The usual `mapTag` for JSON that spells constructors
-- | in camelCase.
lowerFirst :: String -> String
lowerFirst s = case SCU.uncons s of
  Nothing -> s
  Just { head, tail } -> String.toLower (SCU.singleton head) <> tail
