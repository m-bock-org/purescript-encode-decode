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
import Data.Show.Generic (genericShow)
import Foreign.Object (Object)

-- | Every shape JSON has, as constructors rather than as a foreign type
-- | behind a six-way continuation.
data JsonData
  = JsonNull
  | JsonBoolean Boolean
  | JsonNumber Number
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
  JsonString s -> Argonaut.fromString s
  JsonArray xs -> Argonaut.fromArray (map toJson xs)
  JsonObject o -> Argonaut.fromObject (map toJson o)

-- Context: argonaut's `Json` is a foreign type, so looking inside one
-- means `caseJson` and six continuations - fine for reading a field,
-- awkward for anything that walks a payload whose shape is not known in
-- advance. That is exactly what a source of unstructured data hands you.
--
-- The name is `JsonData` rather than `Json` deliberately. A second type
-- called `Json` in a codebase that also imports argonaut is a rename
-- waiting to happen, and module namespaces are shared - nobody owns
-- `Data.*`, so a name that collides is worse than a name that is a
-- little longer.
--
-- Round-tripping is not free: `fromJson` copies the structure. That is
-- the price of a value you can match on, and it is paid once per
-- payload rather than once per lookup.
