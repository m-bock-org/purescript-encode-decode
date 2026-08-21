# purescript-encode-decode

Independent encode and decode functions, instead of a bidirectional `Codec`.

## Why

A `Codec` (as in `purescript-codec`/`purescript-codec-argonaut`) bundles the
encode and decode directions of a type into one profunctor value. That's the
right tool when you genuinely need both directions to stay in lockstep. But
often you only need one - most JSON you decode was never meant to be
re-encoded by the same code, and most JSON you encode was never meant to be
decoded back. Forcing both directions into one `Codec` means:

- writing (or stubbing) a direction you'll never use, and
- composing through profunctor combinators (`dimap`, etc.) even when a plain
  `Functor`/`Applicative` would do.

Split into two plain functions - `decode :: Json -> Either JsonDecodeError a`
and `encode :: a -> Json` - and each direction composes with whatever your
target type's own ordinary instances already give you: decoders through
`Either`'s `Functor`/`Applicative`/`Monad` (via `Data.Argonaut.Decode` and
this library's `decodeRecord`), encoders as plain functions. No custom
profunctor machinery required.

## What's here

Currently: `Data.Json.Record` - `decodeRecord`/`encodeRecord`, row-polymorphic
helpers that decode/encode a `Record` field-by-field from a matching record of
per-field encode/decode functions, e.g.:

```purescript
decodeUser :: Json -> Either JsonDecodeError User
decodeUser = decodeRecord
  { name: decode_String
  , age: decode_Int
  }
```

Ported from `pursai-orange`'s `pursai-fast-api` package, where it originated -
extracted here so it can be shared across projects instead of living inside
one app.

## Usage

Add as a dependency (currently unpublished - use a `path:` or pinned `git:`
extraPackage in your `spago.yaml` workspace config, same as any other
git-sourced package).
