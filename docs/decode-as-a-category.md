# Decode as a category

Status: proposed, not implemented. Agreed in principle 2026-08-30.

## The problem

`DecodeJson a` is `Json -> Either JsonDecodeError a`, and decoding is
almost never one step. Every real decoder reads a representation and then
narrows it: a string into a decimal, a record into a `Euros`, an ISO date
into a `Date`. That second step has no type of its own, so it is written
as a bare function and applied by a combinator invented for the purpose:

```purescript
decodeRefine :: (a -> Either JsonDecodeError b) -> DecodeJson a -> DecodeJson b
```

Two symptoms say this is the wrong shape.

**The refinements are second-class.** `decimalFromString`,
`dateFromString`, `eurosFromRep` are all `x -> Either JsonDecodeError y`
by coincidence rather than by type. They cannot be composed, reused, or
passed around as decoders, because they are not decoders.

**Names have to carry what types do not.** With only `DecodeJson a`, a
decoder that reads a decimal out of a JSON string has to be *called*
something that says so - hence a proposed `decodeDecimalFromString` /
`encodeDecimalAsString` convention, and hence the existing lint exemption
for `decodeDecimalString`, which is "named for what it decodes from, not
a type". A naming convention that exists to compensate for a type is a
type problem wearing a style hat.

## The proposal

```purescript
newtype Decode a b = Decode (a -> Either JsonDecodeError b)

type DecodeJson a = Decode Json a
```

The alias is what makes this cheap: every existing `DecodeJson a`
signature keeps compiling untouched.

### Instances

```purescript
instance Semigroupoid Decode      -- (>>>), the composition; this is `refine`
instance Category Decode          -- identity = Decode Right
instance Profunctor Decode        -- dimap: adapt either end with a pure function
derive instance Functor (Decode a)
instance Apply (Decode a)
instance Applicative (Decode a)   -- pure = Decode <<< const <<< Right
instance Bind (Decode a)
instance Monad (Decode a)
instance Alt (Decode a)
```

Note which instance does what, because it is easy to conflate: **`>>>`
comes from `Semigroupoid`, not from `Profunctor`.** `dimap` adapts the
two ends of one decoder with ordinary functions; it does not chain two
decoders. Both are wanted, for different jobs.

The right-hand instances (`Functor` through `Alt`) live on `Decode a`
with the input fixed, so `do`-notation and everything built on it is
unaffected.

### What falls out

**`decodeRefine` stops existing.** It was Kleisli composition with a
name:

```purescript
decodeEuros = decodeRecord { euro: decodeString } >>> eurosFromRep
```

**Refinements become values, and stop being JSON-specific.**
`decimalFromString :: Decode String NonNegativeDecimal` is useful
wherever a string arrives, not only under a JSON decoder.

**The naming convention becomes unnecessary.**

```purescript
decodeDecimal = decodeString >>> decimalFromString
```

"From string" is in the expression. Nothing has to be encoded in a name,
and the `decodeDecimalString` exemption can go.

**`decodeRawJson` becomes `identity`.** `DecodeJson Json` is
`Decode Json Json`. One fewer primitive, and the codec-argonaut bridge in
tick-duck's `JournalLine` stops being a special case - it is a plain
`Decode Json a`, with no identity step to compose through.

That leaves `decodeFail` (`Decode (const (Left e))`) as very nearly the
only irreducible addition on the decode side.

## The error should be a parameter too

```purescript
newtype Decode e a b = Decode (a -> Either e b)

type DecodeJson a = Decode JsonDecodeError Json a
```

The argument for it is the refinements this whole design exists to make
first-class. `decimalFromString` fails because a string is not a decimal,
which has nothing to do with JSON - and yet with the error fixed it must
fabricate a `TypeMismatch` to fit the hole. Fixing the error type makes
every refinement lie about why it failed.

The argument against is that `Semigroupoid (Decode e)` composes only
within one `e`, so two stages with different errors need an explicit
map - friction landing exactly where the elegance was meant to be. And a
third parameter cuts against the reason for preferring a domain newtype
to `Star`: readable type errors.

**Resolution: parameterise it, alias it away, and keep one error type in
practice.** The parameter is not there to be used - in this codebase it
will be `JsonDecodeError` everywhere. It is there so the general type
does not lie: a decode from `a` to `b` has no business knowing that
failures are about JSON, and that they always are here is a fact about
our usage rather than about the abstraction. The parameter costs nothing while unused, because the alias
hides it at every call site and the instances are ordinary partial
applications - `Semigroupoid (Decode e)`, `Functor (Decode e a)`. But
the door stays open, and closing it later means changing the type
everything depends on.

The friction argument also partly inverts on inspection. That explicit
map at a boundary is a feature: if a decimal-parse failure becomes a JSON
decode error, something has to decide how, and today that translation
happens silently inside whichever function needed it. Making it a named
step at the point of use is the same reasoning as naming the three states
of an envelope rather than nesting `Maybe` and `Either`.

## Why this level and not another

Bare functions are too little structure. `Json -> Either err a` has no
laws to lean on and nothing to compose through, which is why every
project that starts there grows its own ad-hoc combinator for
"decode-then-narrow" - and why the escape hatches had to be banned by a
lint rule rather than by the type.

A bidirectional codec framework is too much. One value carrying both
directions, profunctor optics, free applicatives, generic derivation
everywhere: powerful, and paid for in type errors nobody can read and a
vocabulary someone has to learn before they can follow a decoder.

`Decode a b` sits between. It is a newtype with instances that already
exist in everyone's head - `>>>`, `map`, `do` - applied to a domain type.
Nothing new to learn, and the structure is enough to compose through.

The test it passes: **one type parameter buys four things.**

- `decodeRefine` stops existing; it was `>>>` with a name.
- `decodeRawJson` stops existing; it was `identity`.
- The `decodeDecimalFromString` naming convention stops being needed;
  the composition says it.
- Refinements become reusable values rather than functions that happen
  to fit a combinator's hole.

That ratio is the whole argument. An abstraction earning its keep four
times over is a different proposition from one earning it once - compare
the `OhlcPart` sum that was tried and reverted the same day, which
invented two concepts to hide one endpoint's quirk and paid for neither.

## Costs

**Type synonyms cannot be partially applied in PureScript.**
`DecodeJson` alone is not usable where a `Type -> Type` is expected. In
practice this never bites, because the instances are written against
`Decode a` rather than the alias, and nothing abstracts over the alias
itself. Worth knowing before meeting the error.

**The encode side does not get the same gift, and that is fine.**
`Encode a b = a -> b` has no failure, so its composition is ordinary
function composition - which is exactly what `cmap` already provides.
The asymmetry is not introduced here; it is revealed. Decode lacked
general composition *because* failure was involved.

**Migration.** Signatures are untouched thanks to the alias. What changes
is `decodeRefine`'s call sites (about fifteen in tick-duck), each
becoming `>>>`. Mechanical, and only after the type exists.

## Decided against: `Star (Either JsonDecodeError)`

`purescript-profunctor`'s `Star` is precisely this type and brings
`Category`, `Profunctor`, `Strong` and `Choice` for free. Rejected in
favour of a domain newtype: `Star` in a type error reads far worse than
`Decode`, and the point of this library is that a codec's type should
say what it is. The instances are a few lines each; the readability is
permanent.

`Strong` and `Choice` are available and deliberately not planned - add
them when something wants them, not before.

## Sequencing

Not part of `ban-codec-escape-hatches`, which is already large and
reviewed. This is its own change in the library, and tick-duck's call
sites follow afterwards.
