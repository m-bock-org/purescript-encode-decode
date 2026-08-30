-- | Decode a `data` type constructor-by-constructor from a matching
-- | record of per-constructor decoders - see `Data.Json.Encode.Sum` for
-- | the other direction and the shape of the record, and
-- | `Data.Json.Sum.Encoding` for the wire format the two share.
-- |
-- | ```purescript
-- | decodeShape :: DecodeJson Shape
-- | decodeShape = decodeSum
-- |   { "Circle": decodeNumber
-- |   , "Rect": decodeNumber /\ decodeNumber
-- |   , "Blob": unit
-- |   }
-- | ```
module Data.Json.Decode.Sum
  ( decodeSum
  , decodeSumWith
  , decodeEnum
  , decodeEnumWith
  , Err(..)
  , class DecodeCases
  , gDecodeCases
  , class DecodeFields
  , gDecodeFields
  , class DecodeEnum
  , gDecodeEnum
  ) where

import Prelude

import Data.Array (uncons) as Array
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..), to)
import Data.Json.Decode
  ( DecodeJson
  , Json
  , JsonDecodeError(..)
  , decodeArray
  , decodeObject
  , decodeRawJson
  , decodeString
  , fromFn
  , runDecode
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
-- Errors
----------------------------------------------------------------------------------------------------

-- | Decoding a sum means trying each constructor in turn, so "this
-- | case's tag didn't match, try the next one" has to be a *different*
-- | outcome from "this case's tag matched but its payload was
-- | malformed". Collapsing the two would report a broken payload as
-- | "no case matched", hiding the real error and silently falling
-- | through to constructors that were never going to match either.
-- |
-- | Only surfaced because the generic classes below mention it - a
-- | caller of `decodeSum` gets a plain `JsonDecodeError`.
data Err
  = UnmatchedCase
  | JErr JsonDecodeError

derive instance Eq Err

instance Show Err where
  show = case _ of
    UnmatchedCase -> "UnmatchedCase"
    JErr err -> "(JErr " <> show err <> ")"

jErr :: forall a. Either JsonDecodeError a -> Either Err a
jErr = case _ of
  Left err -> Left (JErr err)
  Right a -> Right a

-- | `UnmatchedCase` means every constructor was tried and none claimed
-- | the tag - a type mismatch from the caller's point of view.
finalizeErr :: forall a. Either Err a -> Either JsonDecodeError a
finalizeErr = case _ of
  Left UnmatchedCase -> Left (TypeMismatch "no matching constructor")
  Left (JErr err) -> Left err
  Right a -> Right a

----------------------------------------------------------------------------------------------------
-- Sum
----------------------------------------------------------------------------------------------------

-- | `decodeSumWith` at `defaultEncoding`.
decodeSum
  :: forall r rep a
   . Generic a rep
  => DecodeCases r rep
  => Record r
  -> DecodeJson a
decodeSum = decodeSumWith defaultEncoding

-- | `decodeSum` with an explicit wire format - see `Encoding`.
decodeSumWith
  :: forall r rep a
   . Generic a rep
  => DecodeCases r rep
  => Encoding
  -> Record r
  -> DecodeJson a
decodeSumWith encoding r = fromFn \json -> to <$> finalizeErr (gDecodeCases encoding r json)

-- | Generic derivation for `decodeSum`, one constructor at a time.
-- | Not meant to be used directly - go through `decodeSum`.
class DecodeCases :: Row Type -> Type -> Constraint
class DecodeCases r rep where
  gDecodeCases :: Encoding -> Record r -> Json -> Either Err rep

instance
  ( Row.Cons name Unit () r
  , IsSymbol name
  ) =>
  DecodeCases r (Constructor name NoArguments) where
  gDecodeCases encoding _ json = do
    payload <- lookupCase encoding json (reflectSymbol (Proxy @name))
    case payload of
      Nothing -> pure unit
      Just raw -> do
        values <- jErr (runDecode (decodeArray decodeRawJson) raw)
        when (values /= []) (Left (JErr (TypeMismatch "no constructor arguments")))
    pure (Constructor NoArguments)

else instance
  ( Row.Cons name (DecodeJson a) () r
  , IsSymbol name
  ) =>
  DecodeCases r (Constructor name (Argument a)) where
  gDecodeCases encoding r json = do
    payload <- lookupCase encoding json (reflectSymbol (Proxy @name))
    value <- singleValue encoding payload
    let decode = Record.get (Proxy @name) r :: DecodeJson a
    Constructor <<< Argument <$> jErr (runDecode decode value)

else instance
  ( Row.Cons name decoders () r
  , DecodeFields decoders args
  , IsSymbol name
  ) =>
  DecodeCases r (Constructor name args) where
  gDecodeCases encoding r json = do
    payload <- lookupCase encoding json (reflectSymbol (Proxy @name))
    values <- manyValues payload
    let decoders = Record.get (Proxy @name) r :: decoders
    Constructor <$> jErr (gDecodeFields decoders values)

instance
  ( DecodeCases r1 (Constructor name lhs)
  , DecodeCases r2 rhs
  , Row.Cons name decoder () r1
  , Row.Cons name decoder r2 r
  , Row.Union r1 r2 r
  , Row.Lacks name r2
  , IsSymbol name
  ) =>
  DecodeCases r (Sum (Constructor name lhs) rhs) where
  gDecodeCases encoding r json =
    let
      decoder = Record.get (Proxy @name) r :: decoder
      r1 = Record.insert (Proxy @name) decoder {} :: Record r1
      r2 = Record.delete (Proxy @name) r :: Record r2

      lhs = gDecodeCases encoding r1 json :: Either Err (Constructor name lhs)
    in
      case lhs of
        -- Only a tag mismatch falls through to the remaining
        -- constructors - a real payload error stops here, see `Err`.
        Left UnmatchedCase -> Inr <$> gDecodeCases encoding r2 json
        Left (JErr err) -> Left (JErr err)
        Right val -> Right (Inl val)

----------------------------------------------------------------------------------------------------
-- Fields
----------------------------------------------------------------------------------------------------

-- | Per-argument decoders for one multi-argument constructor, joined
-- | with `/\` in argument order. Not meant to be used directly.
class DecodeFields :: Type -> Type -> Constraint
class DecodeFields decoders rep where
  gDecodeFields :: decoders -> Array Json -> Either JsonDecodeError rep

instance DecodeFields (DecodeJson a) (Argument a) where
  gDecodeFields decode = case _ of
    [ json ] -> Argument <$> runDecode decode json
    _ -> Left (TypeMismatch "exactly one constructor argument")

instance
  ( DecodeFields decoder rep
  , DecodeFields decoders reps
  ) =>
  DecodeFields (decoder /\ decoders) (Product rep reps) where
  gDecodeFields (decoder /\ decoders) values = do
    { head, tail } <- note (TypeMismatch "more constructor arguments") (Array.uncons values)
    rep <- gDecodeFields decoder [ head ]
    reps <- gDecodeFields decoders tail
    pure (Product rep reps)

----------------------------------------------------------------------------------------------------
-- Enum
----------------------------------------------------------------------------------------------------

-- | For a `data` type whose constructors are *all* nullary: decodes
-- | from a plain JSON string, not a tagged object.
decodeEnum :: forall rep a. Generic a rep => DecodeEnum rep => DecodeJson a
decodeEnum = decodeEnumWith identity

-- | `decodeEnum`, matching against constructor names rewritten by
-- | `mapTag` - which must be the same one `encodeEnumWith` used.
decodeEnumWith
  :: forall rep a
   . Generic a rep
  => DecodeEnum rep
  => (String -> String)
  -> DecodeJson a
decodeEnumWith mapTag = fromFn \json -> do
  tag <- runDecode decodeString json
  case gDecodeEnum mapTag tag of
    Just rep -> Right (to rep)
    Nothing -> Left (UnexpectedValue json)

-- | Generic derivation for `decodeEnum`. Not meant to be used directly.
class DecodeEnum :: Type -> Constraint
class DecodeEnum rep where
  gDecodeEnum :: (String -> String) -> String -> Maybe rep

instance IsSymbol name => DecodeEnum (Constructor name NoArguments) where
  gDecodeEnum mapTag tag
    | tag == mapTag (reflectSymbol (Proxy @name)) = Just (Constructor NoArguments)
    | otherwise = Nothing

instance
  ( DecodeEnum lhs
  , DecodeEnum rhs
  ) =>
  DecodeEnum (Sum lhs rhs) where
  gDecodeEnum mapTag tag = case gDecodeEnum mapTag tag of
    Just lhs -> Just (Inl lhs)
    Nothing -> Inr <$> gDecodeEnum mapTag tag

----------------------------------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------------------------------

-- | The raw payload for `expectedTagRaw`'s case, or `Nothing` when the
-- | encoding left it out entirely (`omitEmptyArguments`). `Left
-- | UnmatchedCase` when this isn't that constructor at all.
lookupCase :: Encoding -> Json -> String -> Either Err (Maybe Json)
lookupCase encoding json expectedTagRaw = do
  obj <- jErr (runDecode (decodeObject decodeRawJson) json)
  case encoding of
    EncodeNested { mapTag } ->
      Just <$> note UnmatchedCase (Obj.lookup (mapTag expectedTagRaw) obj)

    EncodeTagged { tagKey, valuesKey, mapTag } -> do
      rawTag <- note (JErr (AtKey tagKey MissingValue)) (Obj.lookup tagKey obj)
      tag <- jErr (runDecode decodeString rawTag)
      when (tag /= mapTag expectedTagRaw) (Left UnmatchedCase)
      pure (Obj.lookup valuesKey obj)

-- | The single argument of a one-argument constructor - taken directly
-- | when `unwrapSingleArguments` is on, unwrapped from a one-element
-- | array otherwise.
singleValue :: Encoding -> Maybe Json -> Either Err Json
singleValue encoding payload = do
  raw <- note (JErr (TypeMismatch "one constructor argument")) payload
  if unwraps then pure raw
  else do
    values <- jErr (runDecode (decodeArray decodeRawJson) raw)
    case values of
      [ value ] -> pure value
      _ -> Left (JErr (TypeMismatch "exactly one constructor argument"))
  where
  unwraps = case encoding of
    EncodeNested e -> e.unwrapSingleArguments
    EncodeTagged e -> e.unwrapSingleArguments

manyValues :: Maybe Json -> Either Err (Array Json)
manyValues payload = do
  raw <- note (JErr (TypeMismatch "constructor arguments")) payload
  jErr (runDecode (decodeArray decodeRawJson) raw)
