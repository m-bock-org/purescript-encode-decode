-- | Decode a `Variant` case-by-case from a matching record of per-case
-- | decoders - see `Data.Json.Encode.Variant` for the other direction.
module Data.Json.Decode.Variant
  ( decodeEnum
  , decodeEnumWith
  , decodeVariant
  , decodeVariantWith
  , class DecodeEnum
  , class DecodeVariant
  , gDecodeEnum
  , gDecodeVariant
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Json.Decode (DecodeJson, Json, JsonDecodeError(..), decodeString, fromFn, runDecode)
import Data.Json.Decode.Sum (Err(..), finalizeErr, jErr, lookupCase, singleValue)
import Data.Maybe (Maybe(..))
import Data.Json.Sum.Encoding (Encoding, variantEncoding)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

-- | tried in turn until one claims the tag.
-- | Uses `decodeVariantWith`.
decodeVariant
  :: ∀ rl ri ro
   . RowToList ri rl
  => DecodeVariant rl ri ro
  => Record ri
  -> DecodeJson (Variant ro)
decodeVariant = decodeVariantWith variantEncoding

decodeVariantWith
  :: ∀ rl ri ro
   . RowToList ri rl
  => DecodeVariant rl ri ro
  => Encoding
  -> Record ri
  -> DecodeJson (Variant ro)
decodeVariantWith encoding ri = fromFn (finalizeErr <<< gDecodeVariant @rl encoding ri)

-- | A `Variant` whose every case carries `{}`, read from a plain JSON
-- | string - the `Variant` counterpart of
-- | `Data.Json.Decode.Sum.decodeEnum`.
-- | Uses `decodeEnumWith`.
decodeEnum :: ∀ rl ro. RowToList ro rl => DecodeEnum rl ro => DecodeJson (Variant ro)
decodeEnum = decodeEnumWith (\said -> said)

-- | The same, with a rewrite from a label to the word on the wire -
-- | `lowerFirst`, say. One function rather than a pair, so a rewrite
-- | that loses information fails to round-trip instead of compiling
-- | into two halves that disagree.
decodeEnumWith
  :: ∀ rl ro
   . RowToList ro rl
  => DecodeEnum rl ro
  => (String -> String)
  -> DecodeJson (Variant ro)
decodeEnumWith mapTag = fromFn \json -> do
  said <- runDecode decodeString json
  case gDecodeEnum @rl mapTag said of
    Just one -> Right one
    Nothing -> Left (UnexpectedValue json)

-- | Generic derivation for `decodeEnum`, one case at a time via `rl`.
-- | Not meant to be used directly - go through `decodeEnum`.
class DecodeEnum :: RL.RowList Type -> Row Type -> Constraint
class DecodeEnum rl ro where
  gDecodeEnum :: (String -> String) -> String -> Maybe (Variant ro)

instance DecodeEnum RL.Nil ro where
  gDecodeEnum _ _ = Nothing

instance
  ( Row.Cons sym {} ro' ro
  , RowToList pr RL.Nil
  , IsSymbol sym
  , DecodeEnum rl ro
  ) =>
  DecodeEnum (RL.Cons sym (Record pr) rl) ro where
  gDecodeEnum mapTag said
    | said == mapTag (reflectSymbol (Proxy @sym)) = Just (V.inj (Proxy @sym) {})
    | otherwise = gDecodeEnum @rl mapTag said

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

-- ## Context
--
-- `decodeEnum`
-- The other half of `Data.Json.Encode.Variant.encodeEnum`, and the
-- same two decisions.
--
-- **The row is one type variable, not two.** There is no record of
-- per-case decoders to disagree with the row being built, because
-- there is nothing per case to decode: the tag is the whole of the
-- information.
--
-- **`Record pr` with `RowToList pr RL.Nil`, not `{}`.** The empty row
-- cannot appear in an instance head - "all types appearing in
-- instance declarations must be of the form `T a_1 .. a_n`" - so the
-- payload is a variable and a constraint says it is empty. What that
-- buys is the same as on the encode side: a row with a payload
-- anywhere in it has no instance, so a case that grows one is a
-- compile error at the codec rather than a field invented on the way
-- in.
--
-- The payload *value* comes from the context, not from a coercion.
-- `{}` cannot be written in the head, but `Row.Cons sym {} ro' ro`
-- may be written as a constraint - and that is what lets the case be
-- built with `V.inj (Proxy @sym) {}` and nothing coerced. The first
-- version of this reached for `unsafeCoerce {} :: Record pr`, on the
-- argument that a row whose `RowList` is `Nil` is the empty row at
-- runtime. True, and unnecessary: `no-unsafe-escape` refused it and
-- was right to, because the constraint the coercion was standing in
-- for is one the compiler will state.
--
-- `RowToList pr RL.Nil` stays beside it, doing the other half. Without
-- it `pr` is free in the head, so the instance matches a case with a
-- payload and then fails on the context - "no instance for
-- `Row.Cons`" where the truth is that this row is not an enum.
--
-- **An unknown word is `UnexpectedValue`, not "no case matched".**
-- `Err` next door exists because decoding a tagged sum has to tell
-- "try the next case" apart from "this payload is broken". Here there
-- is no payload, so the walk ending is not a step in a search - it is
-- the answer: this string is not one of the cases.
