-- | Encode a `data` type constructor-by-constructor from a matching
-- | record of per-constructor encoders - the same shape
-- | `Data.Json.Encode.Record` uses for products. See
-- | `Data.Json.Decode.Sum` for the other direction and
-- | `Data.Json.Sum.Encoding` for the wire format the two share.
-- |
-- | ```purescript
-- | data Shape = Circle Number | Rect Number Number | Blob
-- | derive instance Generic Shape _
-- |
-- | encodeShape :: EncodeJson Shape
-- | encodeShape = encodeSum
-- |   { "Circle": encodeNumber
-- |   , "Rect": encodeNumber /\ encodeNumber
-- |   , "Blob": unit
-- |   }
-- | ```
-- |
-- | The record must name every constructor exactly - a missing or
-- | misspelled one is a compile error, not a runtime surprise. Nullary
-- | constructors take `unit` (there is nothing to encode), single-
-- | argument ones take a plain encoder, and multi-argument ones take
-- | encoders joined with `/\`, one per argument in order.
module Data.Json.Encode.Sum
  ( encodeSum
  , encodeSumWith
  , encodeEnum
  , encodeEnumWith
  , class EncodeCases
  , gEncodeCases
  , class EncodeFields
  , gEncodeFields
  , class EncodeEnum
  , gEncodeEnum
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, Product(..), Sum(..), from)
import Data.Json.Encode
  ( EncodeJson
  , Json
  , encodeArray
  , encodeObject
  , encodeRawJson
  , encodeString
  , fromFn
  , toFn
  )
import Data.Json.Sum.Encoding (Encoding(..), defaultEncoding)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign.Object as Obj
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))

----------------------------------------------------------------------------------------------------
-- Sum
----------------------------------------------------------------------------------------------------

-- | `encodeSumWith` at `defaultEncoding`.
encodeSum :: forall r rep a. Generic a rep => EncodeCases r rep => Record r -> EncodeJson a
encodeSum = encodeSumWith defaultEncoding

-- | `encodeSum` with an explicit wire format - see `Encoding`.
encodeSumWith
  :: forall r rep a
   . Generic a rep
  => EncodeCases r rep
  => Encoding
  -> Record r
  -> EncodeJson a
encodeSumWith encoding r = fromFn (from >>> gEncodeCases encoding r)

-- | Generic derivation for `encodeSum`, one constructor at a time.
-- | Not meant to be used directly - go through `encodeSum`.
class EncodeCases :: Row Type -> Type -> Constraint
class EncodeCases r rep where
  gEncodeCases :: Encoding -> Record r -> rep -> Json

instance
  ( Row.Cons name Unit () r
  , IsSymbol name
  ) =>
  EncodeCases r (Constructor name NoArguments) where
  gEncodeCases encoding _ _ =
    encodeSumCase encoding (reflectSymbol (Proxy @name)) []

else instance
  ( Row.Cons name (EncodeJson a) () r
  , IsSymbol name
  ) =>
  EncodeCases r (Constructor name (Argument a)) where
  gEncodeCases encoding r (Constructor (Argument x)) =
    let
      encode = Record.get (Proxy @name) r :: EncodeJson a
    in
      encodeSumCase encoding (reflectSymbol (Proxy @name)) [ toFn encode x ]

else instance
  ( Row.Cons name encoders () r
  , EncodeFields encoders args
  , IsSymbol name
  ) =>
  EncodeCases r (Constructor name args) where
  gEncodeCases encoding r (Constructor rep) =
    let
      encoders = Record.get (Proxy @name) r :: encoders
    in
      encodeSumCase encoding (reflectSymbol (Proxy @name)) (gEncodeFields encoders rep)

instance
  ( EncodeCases r1 (Constructor name lhs)
  , EncodeCases r2 rhs
  , Row.Cons name encoder () r1
  , Row.Cons name encoder r2 r
  , Row.Union r1 r2 r
  , Row.Lacks name r2
  , IsSymbol name
  ) =>
  EncodeCases r (Sum (Constructor name lhs) rhs) where
  gEncodeCases encoding r =
    let
      encoder = Record.get (Proxy @name) r :: encoder
      r1 = Record.insert (Proxy @name) encoder {} :: Record r1
      r2 = Record.delete (Proxy @name) r :: Record r2
    in
      case _ of
        Inl lhs -> gEncodeCases encoding r1 lhs
        Inr rhs -> gEncodeCases encoding r2 rhs

----------------------------------------------------------------------------------------------------
-- Fields
----------------------------------------------------------------------------------------------------

-- | Per-argument encoders for one multi-argument constructor, joined
-- | with `/\` in argument order. Not meant to be used directly.
class EncodeFields :: Type -> Type -> Constraint
class EncodeFields encoders rep where
  gEncodeFields :: encoders -> rep -> Array Json

instance EncodeFields (EncodeJson a) (Argument a) where
  gEncodeFields encode (Argument x) = [ toFn encode x ]

instance
  ( EncodeFields encoder rep
  , EncodeFields encoders reps
  ) =>
  EncodeFields (encoder /\ encoders) (Product rep reps) where
  gEncodeFields (encoder /\ encoders) (Product rep reps) =
    gEncodeFields encoder rep <> gEncodeFields encoders reps

----------------------------------------------------------------------------------------------------
-- Enum
----------------------------------------------------------------------------------------------------

-- | For a `data` type whose constructors are *all* nullary: encodes to
-- | a plain JSON string, not a tagged object. `data Mode = Simulation |
-- | Realisation` becomes `"Simulation"` / `"Realisation"`.
encodeEnum :: forall rep a. Generic a rep => EncodeEnum rep => EncodeJson a
encodeEnum = encodeEnumWith identity

-- | `encodeEnum`, with the constructor name rewritten before it hits
-- | the wire - `lowerFirst`, say.
encodeEnumWith
  :: forall rep a
   . Generic a rep
  => EncodeEnum rep
  => (String -> String)
  -> EncodeJson a
encodeEnumWith mapTag = fromFn (from >>> gEncodeEnum mapTag >>> toFn encodeString)

-- | Generic derivation for `encodeEnum`. Not meant to be used directly.
class EncodeEnum :: Type -> Constraint
class EncodeEnum rep where
  gEncodeEnum :: (String -> String) -> rep -> String

instance IsSymbol name => EncodeEnum (Constructor name NoArguments) where
  gEncodeEnum mapTag _ = mapTag (reflectSymbol (Proxy @name))

instance
  ( EncodeEnum lhs
  , EncodeEnum rhs
  ) =>
  EncodeEnum (Sum lhs rhs) where
  gEncodeEnum mapTag = case _ of
    Inl lhs -> gEncodeEnum mapTag lhs
    Inr rhs -> gEncodeEnum mapTag rhs

----------------------------------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------------------------------

encodeSumCase :: Encoding -> String -> Array Json -> Json
encodeSumCase encoding rawTag jsons = case encoding of
  EncodeNested { unwrapSingleArguments, mapTag } ->
    let
      value = case jsons of
        [ json ] | unwrapSingleArguments -> json
        many -> toFn (encodeArray encodeRawJson) many
    in
      toFn (encodeObject encodeRawJson) (Obj.fromFoldable [ mapTag rawTag /\ value ])

  EncodeTagged { tagKey, valuesKey, unwrapSingleArguments, omitEmptyArguments, mapTag } ->
    let
      tagEntry = Just (tagKey /\ toFn encodeString (mapTag rawTag))

      valuesEntry = case jsons of
        [] | omitEmptyArguments -> Nothing
        [ json ] | unwrapSingleArguments -> Just (valuesKey /\ json)
        many -> Just (valuesKey /\ toFn (encodeArray encodeRawJson) many)
    in
      toFn (encodeObject encodeRawJson)
        (Obj.fromFoldable (catMaybes [ tagEntry, valuesEntry ]))
