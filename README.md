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
