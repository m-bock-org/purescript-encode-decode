-- | JSON as a value you can pattern match on.
module Data.Json.Value
  ( JsonData(..)
  , fromJson
  , toJson
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Show.Generic (genericShow)
import Foreign.Object (Object)

-- | Every shape JSON has, as constructors rather than as a foreign type
-- | behind a six-way continuation.
data JsonData
  = JsonNull
  | JsonBoolean Boolean
  | JsonNumber Number
  -- | A whole number, because the source said so. Never produced by
  -- | `fromJson`: a parsed document cannot tell 1 from 1.0.
  | JsonInt Int
  | JsonString String
  | JsonArray (Array JsonData)
  | JsonObject (Object JsonData)

derive instance Generic JsonData _
derive instance Eq JsonData

instance Show JsonData where
  show j = genericShow j

-- | Read a payload into something you can walk.
-- Written with the argument named rather than point-free: a recursive
-- value defined by its own body is a cycle, not a definition.
fromJson :: Json -> JsonData
fromJson json = Argonaut.caseJson
  (const JsonNull)
  JsonBoolean
  JsonNumber
  JsonString
  (JsonArray <<< map fromJson)
  (JsonObject <<< map fromJson)
  json

-- | Back to what every other library speaks.
toJson :: JsonData -> Json
toJson = case _ of
  JsonNull -> Argonaut.jsonNull
  JsonBoolean b -> Argonaut.fromBoolean b
  JsonNumber n -> Argonaut.fromNumber n
  JsonInt i -> Argonaut.fromNumber (Int.toNumber i)
  JsonString s -> Argonaut.fromString s
  JsonArray xs -> Argonaut.fromArray (map toJson xs)
  JsonObject o -> Argonaut.fromObject (map toJson o)

-- Context: this is not "JSON as a value". It is the language something
-- producing data can speak, and it is deliberately richer than JSON,
-- because a producer knows things the wire format does not preserve.
-- `JsonInt` is the example: JSON has one number type, and by the time a
-- document is parsed, 1 and 1.0 are the same double - but a producer
-- reading an API whose ids are integers can simply say so. Nothing is
-- inferred; it is asserted.
--
-- Hence the asymmetry, which is the invariant to hold on to:
--
--   * `toJson` is total. Anything expressible here can be written out.
--   * `fromJson` is not onto. It yields only the JSON-grammar subset,
--     because that is all a document contains.
--
-- The lossy direction is the one nobody depends on. What consumers need
-- is to walk a value and match on it, and argonaut's `Json` is a foreign
-- type behind a six-way continuation - fine for reading a known field,
-- awkward for anything that walks a payload whose shape is not known in
-- advance.
--
-- Who benefits: code, not models. Serialised into a prompt, `JsonInt 42`
-- and `JsonNumber 42.0` are the same characters. The constructors pay
-- for the deterministic pass - dropping anything mentioning a known-bad
-- host, or any payload carrying a particular id - where a typed fold
-- beats a substring search on a serialised blob.
--
-- The name is `JsonData` rather than `Json` deliberately. A second type
-- called `Json` in a codebase that also imports argonaut is a rename
-- waiting to happen, and module namespaces are shared - nobody owns
-- `Data.*`, so a name that collides is worse than one slightly longer.
