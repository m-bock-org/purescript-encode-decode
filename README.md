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

- **`Data.Json.Decode.Sum`** / **`Data.Json.Encode.Sum`** - the same idea for
  `data` types: one entry per constructor, checked against the type's
  `Generic` representation, so a missing or misspelled constructor is a
  compile error rather than a runtime surprise.

  ```purescript
  data Shape = Circle Int | Rect Int String | Blob
  derive instance Generic Shape _

  encodeShape :: Shape -> Json
  encodeShape = encodeSum
    { "Circle": encodeInt                -- one argument
    , "Rect": encodeInt /\ encodeString  -- several, in order
    , "Blob": unit                       -- none
    }
  ```

  A type whose constructors are *all* nullary maps to a plain JSON string
  instead, via `encodeEnum`/`decodeEnum` - `data Mode = Simulation |
  Realisation` becomes `"Simulation"`, not a tagged object.

- **`Data.Json.Sum.Encoding`** - the wire format the two sum modules share
  (the one thing the two directions can't decide independently).
  `defaultEncoding` is `{"tag": ..., "values": [...]}`; `EncodeNested` keys
  by constructor name instead, and `tagKey` / `valuesKey` / `mapTag` /
  `unwrapSingleArguments` / `omitEmptyArguments` cover the usual variations.

## Usage

```yaml
extraPackages:
  encode-decode:
    git: https://github.com/m-bock/purescript-encode-decode.git
    ref: <commit or tag>
```

Then add `encode-decode` to your package's `dependencies`.

## Roadmap

- **`Variant` support.** A `Variant` is structurally a sum, so the two sum
  modules have an obvious counterpart - and an easier one, since a
  `Variant` carries its row type directly and needs no `Generic`
  derivation to inspect. Deliberately not built yet: the only place a
  `Variant` currently meets JSON in the consuming codebase (tick-duck's
  dashboard `Message.purs`) is on `codec-argonaut` and would need a
  broader migration to move, so there'd be no adopter. The plan is to
  live with the `data`-sum support above first and add this if it earns
  its place.

## Prior art

The sum-type modules are a port of the `Data.Codec.Argonaut.Sum` work in
[garyb/purescript-codec-argonaut](https://github.com/garyb/purescript-codec-argonaut)
(MIT, © Gary Burgess) - specifically the `Encoding` options and the
`UnmatchedCase`-vs-real-error distinction that makes trying constructors
in turn actually correct. It lives on that project's `master` but was
never released (see its issue #80). Adapted here rather than depended on:
the original returns one bidirectional `JsonCodec`, whereas this library
splits the two directions - which, for sums, makes the encode side
markedly simpler, since only decoding ever needs to try cases in turn.
