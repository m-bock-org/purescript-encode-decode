# purescript-encode-decode

Plain, explicit JSON encode/decode functions for PureScript - no
typeclass-based `decodeJson`. Depend on this alone - never import
Argonaut directly.

## What's here

Every module splits by direction - encode and decode never share one,
matching the point of the library: you usually only need one.

- **`Data.Json.Decode`** / **`Data.Json.Encode`** - primitives (`decodeString`,
  `decodeInt`, `decodeNumber`, `decodeBoolean`), plus `decodeArray` and
  `decodeNativeTuple2` (a fixed-length, per-position-typed JSON array).
- **`Data.Json.Decode.Record`** / **`Data.Json.Encode.Record`** - decode/encode
  a `Record` field-by-field from a matching record of decoders/encoders:

  ```purescript
  decodeUser :: Json -> Either JsonDecodeError { name :: String, age :: Int }
  decodeUser = decodeRecord
    { name: decodeString
    , age: decodeInt
    }
  ```

## Usage

```yaml
extraPackages:
  encode-decode:
    git: https://github.com/m-bock/purescript-encode-decode.git
    ref: <commit or tag>
```

Then add `encode-decode` to your package's `dependencies`.

## Roadmap

Sum types are the notable gap - the record support above has no
equivalent for `data`, so every consumer hand-writes a tagged encoder and
a `case`-on-tag decoder (see `Trading.Journal.Json.encodeGuardrailOutcome`
in tick-duck for a representative example of what that boilerplate looks
like).

- **Sum types in general.** Encode/decode a `data` type field-by-field
  from a matching record of per-constructor encoders/decoders, the same
  shape `decodeRecord`/`encodeRecord` already use for products. Needs a
  decision on the wire encoding (an internally-tagged `{"kind": ...}`
  object is what consumers are hand-rolling today).
- **No-payload ADTs / enums.** The common special case - `data Mode =
  Simulation | Realisation` should map to a plain JSON string, not a
  tagged object, without the caller writing two `case` expressions per
  type. Worth its own function rather than falling out of the general
  sum-type support, since the encoding is genuinely different.
