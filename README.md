# purescript-encode-decode

[![CI](https://github.com/m-bock/purescript-encode-decode/actions/workflows/ci.yml/badge.svg)](https://github.com/m-bock/purescript-encode-decode/actions/workflows/ci.yml)

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

## Both directions at once

Everything above splits encode from decode, and that stays the default.
`Data.Json.Codec` is the opt-in layer above it - one description that
yields both, so the two cannot drift apart:

```purescript
codecUser :: JsonCodec { name :: String, age :: Int, tags :: Array String }
codecUser = codecRecord
  { name: codecString
  , age: codecInt
  , tags: codecArray codecString
  }
```

`encoder` and `decoder` take a half back out, so nothing is trapped.
`codecRecord`, `codecSum`/`codecEnum`, `codecVariant` and `codecTuple`
mirror the modules of the same name, and `codecInvmap`/`codecRefine` move a codec
to another type - a codec is invariant, so neither `map` nor `>$<` can
be written for it, and two functions is what honesty costs.

**When to reach for it.** A format with no history - something this
program writes and this program reads, where "correct" means a round
trip returns what went in. Writing the two halves apart is then
duplication a compiler cannot check: nothing stops an encoder writing
`dayRate` and a decoder reading `day_rate`, and no test catches it when
both tests are written from the same wrong idea. Sums make the point
sharpest, since `Encoding` has to be passed to both directions and
passed the *same* - here it is one argument.

One default *is* bidirectional and is here: `codecOptional` makes a
field's absence mean `Nothing` and `Nothing` mean the key is not
written. That is a bijection rather than a belief about what a silent
writer meant, which is what separates it from every other default -
`decodeRecordWithDefaults` decides what to believe, and only a decoder
can do that.

**When not to.** Anything with readers or writers you do not deploy at
the same moment: a persisted file, a public API, a queue. There the
decoder must be more tolerant than the encoder is generous - it reads
what last year's version wrote, while the encoder only writes today's
shape. A codec forces the two to be mirror images, which is why
`decodeRecordWithDefaults` has no codec counterpart. That is not a gap
to be filled later.

The layer is thin on purpose: each module splits the description in two
and hands each half to the module that already knows that direction, so
the logic worth getting right - a record's field walk, a sum's fall
through from one constructor to the next - lives in one place and is
reused rather than copied. `Data.Json.Codec.Internal` is that split, and
it is all of the new machinery there is.

## Usage

```yaml
extraPackages:
  encode-decode:
    git: https://github.com/m-bock/purescript-encode-decode.git
    ref: <commit or tag>
```

Then add `encode-decode` to your package's `dependencies`.

- **`Data.Json.Decode.Variant`** / **`Data.Json.Encode.Variant`** - the
  same idea again for a `Variant`, and an easier one: the row is in the
  type, so nothing has to be derived from a `Generic` representation.
  The default wire format is `{"tag": ..., "value": ...}` with the value
  unwrapped, since a `Variant` case carries exactly one thing where a
  constructor carries any number.

  ```purescript
  type Msg = Variant (newState :: State)

  codecMsg :: JsonCodec Msg
  codecMsg = codecVariant { newState: codecState }
  ```

## Roadmap

Nothing outstanding. `Variant` support was the last item, and was built
when tick-duck's dashboard gave it an adopter - which was the condition
it was waiting on, rather than a guess about whether it would be
wanted.

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
