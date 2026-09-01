-- | Taking a codec apart: the one piece of machinery the three codec
-- | modules share, and the reason none of them has to restate what
-- | `Data.Json.Encode.*` and `Data.Json.Decode.*` already do.
-- |
-- | A codec module is a thin wrapper: split the description into its two
-- | halves, hand each to the module that already knows that direction.
-- | The split is the only new work, and it is deliberately dumb - the
-- | logic worth getting right (a record's field walk, a sum's fall
-- | through from one constructor to the next) stays in one place and is
-- | reused, not copied.
module Data.Json.Codec.Internal
  ( class SplitCodec
  , splitEncoder
  , splitDecoder
  , class SplitCodecs
  , splitEncoders
  , splitDecoders
  ) where

import Prelude

import Data.Json.Codec (JsonCodec, decoder, encoder)
import Data.Json.Decode (DecodeJson)
import Data.Json.Encode (EncodeJson)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

-- | One position of a description, split in two.
-- |
-- | Three shapes, because that is what the sum modules accept per
-- | constructor: a codec, several joined with `/\`, or `unit` for a
-- | constructor with no arguments. The tuple instance is recursive
-- | rather than fixed-arity - `/\` nests to the right, so handling one
-- | level handles every depth.
class SplitCodec c e d | c -> e d where
  splitEncoder :: c -> e
  splitDecoder :: c -> d

instance SplitCodec (JsonCodec a) (EncodeJson a) (DecodeJson a) where
  splitEncoder = encoder
  splitDecoder = decoder

instance
  ( SplitCodec a ae ad
  , SplitCodec b be bd
  ) =>
  SplitCodec (a /\ b) (ae /\ be) (ad /\ bd) where
  splitEncoder (a /\ b) = splitEncoder a /\ splitEncoder b
  splitDecoder (a /\ b) = splitDecoder a /\ splitDecoder b

instance SplitCodec Unit Unit Unit where
  splitEncoder = identity
  splitDecoder = identity

-- | A whole record of descriptions, split in two - same labels, each
-- | field's type moved by `SplitCodec`.
-- |
-- | Two records out rather than a record of pairs, because that is what
-- | `encodeRecord`/`decodeRecord` and `encodeSum`/`decodeSum` take. The
-- | walk is by row list, the way every other generic derivation in this
-- | library is written.
class SplitCodecs :: RL.RowList Type -> Row Type -> Row Type -> Row Type -> Constraint
class SplitCodecs rl rcs res rds | rl -> rcs res rds where
  splitEncoders :: Record rcs -> Record res
  splitDecoders :: Record rcs -> Record rds

instance SplitCodecs RL.Nil () () () where
  splitEncoders _ = {}
  splitDecoders _ = {}

instance
  ( SplitCodec c e d
  , Row.Cons sym c rcs' rcs
  , Row.Cons sym e res' res
  , Row.Cons sym d rds' rds
  , Row.Lacks sym rcs'
  , Row.Lacks sym res'
  , Row.Lacks sym rds'
  , IsSymbol sym
  , SplitCodecs rl rcs' res' rds'
  ) =>
  SplitCodecs (RL.Cons sym c rl) rcs res rds where
  splitEncoders rcs =
    Record.insert (Proxy @sym)
      (splitEncoder (Record.get (Proxy @sym) rcs))
      (splitEncoders @rl (Record.delete (Proxy @sym) rcs))

  splitDecoders rcs =
    Record.insert (Proxy @sym)
      (splitDecoder (Record.get (Proxy @sym) rcs))
      (splitDecoders @rl (Record.delete (Proxy @sym) rcs))
