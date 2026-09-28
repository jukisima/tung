# tung language reference

this reference defineþ source syntax, module lookup, library manifests, and
runtime-visible behaviour. [`readme.md`](../readme.md) covereþ installation and
a short tour. [`development.md`](development.md) covereþ the compiler and
repository.

## files and modules

a `.tung` file containeþ declarations. a runnable file declareþ local `main`. it
takeþ `𝟙`, returneþ `𝟙`, and useþ only runner-supported effects. the runner
calleþ `only main` after pure module initialisation.

declarations are private by default. `show` publisheþ a declaration or visible
term. `show-ilk` publisheþ a visible type, effect, or frame. term and type
namespaces are separate. ordinary `use` doth not re-export anything.

## libraries and imports

`use path` importeþ a module. its final path segment becomeþ the default
namespace. an optional final name replaceþ it.

```tung
use ilk/list.tung
use ilk/table.tung hashmap

let xs = list~empty
let entries = hashmap~empty
```

visible imported names may remain bare when unambiguous. two different direct
imports cannot claim the same namespace. the import graph must be acyclic. a
local term shadoweþ an imported bare term in value and application positions. a
qualified name still reacheþ the import.

the host catalogue explicitly overloadeþ primitive names, such as `+` for
integer and float, within primitive scope. direct application trieþ their
signatures in catalogue order. as a value, the name selecteþ þe first signature.
constructors and frame members with the same spelling remain distinct
application candidates. later bindings shadow earlier ones of the same kind.

module lookup searcheþ paths relative to the importing file, git-pinned roots,
þen `TUNG_PATH` entries. a `.tung` path resolving to different files is
ambiguous. `ground.tung` is an optional import, not a prelude.

### manifests

libraries are git repositories. the standard library is an ordinary library
in [tung-bookhoard](https://github.com/jukisima/tung-bookhoard). a `tung.yaml`
manifest declareþ pinned libraries under `dependencies`:

```yaml
dependencies:
  bookhoard:
    repo: https://github.com/jukisima/tung-bookhoard.git
    hash: "aabb7ed4ad37670c1c997afbb8eb8307fce45a39"
```

each dependency key nameþ one library throughout the graph, not a local alias.
`repo` giveþ a git URL or local repository path, such as `../my-library`.
`hash` giveþ a hexadecimal commit hash of 40 or 64 digits. quote it so yaml
readeþ an all-digit hash as text. `use` paths name `.tung` files relative to
each library root.

tung findeþ the nearest `tung.yaml` above the source file. it checkeþ out each
pin under `.tung/libraries/` beside that manifest. after the first fetch, it
reuseþ the checkout offline. `tung --library-paths [project-path]` listeþ the
resolved roots. `TUNG_PATH` addeth unpinned roots for local development.

each library may declare `tung.yaml`. tung traverseþ the whole dependency graph,
including libraries the program doth not import. matching names, repository
locators, and commits share one checkout. before module checking, tung rejecteþ:

- one name with different repositories or commits;
- one repository locator with different names or commits.

if two libraries pin `shared` to different commits, the project faileþ. the
root manifest cannot override either pin. two names for one repository also
conflict, even at the same commit.

relative local repository paths resolve from the manifest that declareþ them.
local paths are lexically normalised. remote locators are compared as written.
use the same name and repository spelling in every manifest. `TUNG_PATH` roots
lie outside pinned resolution and conflict checking. the same `.tung` path in
different roots remaineþ ambiguous when imported.

to migrate from `tung.libraries`, move each repository and commit under
`dependencies` in `tung.yaml`. then remove the old file.

## names, comments, and literals

most non-space, non-reserved characters may occur in names. `~` qualifieþ
imports; `.` selecteþ record fields; `@` separateþ match and handler cases. type
alias, data, effect, and frame names are unqualified. so are þeir constructors,
operations, and members. outside `.tung` import paths, a name may contain at
most one `~`. its namespace and member must be nonempty and slash-free. `_*` is
an ordinary name; there is no general infix escape. `_` discardeþ a binding or
match value.

`#` starteþ a line comment. `/* ... */` formeþ a non-nested block comment. `##`
and `/** ... */` attach doc comments to the next declaration. editor help and
generated bookhoard pages use them.

integer literals are arbitrary-precision decimal values. float literals contain
a decimal point. a backtick introduceþ one unicode scalar, and single quotes
delimit text. both literal forms accept `\n`, `\r`, `\t`, `\'`, `\\`, and
decimal escapes such as `\23383;`.

## application

every function is curried. in an unmarked sequence of two or more terms, the
second is the function. the others are arguments in order.

```tung
a f           # f a
a f b c       # f a b c
a f $ g b     # g (f a) b
```

`$` groupeþ its left expression and useþ the next atom as the function. `$(`
starteþ a function-first segment.

`<(f, a, b, c)` lowereþ to `(a f b) f c`. `>(f, a, b, c)` lowereþ to
`a f (b f c)`. each requireþ a combining function and at least two values. `r(`,
`<(`, and `>(` are single opening delimiters. the parenthesis must touch the
marker.

partial application is pure. latent effects run only after saturation.
evaluation is strict: first the function, then arguments from left to right.

## declarations and types

`let` bindeþ a value. function headers use the second-is-function rule. typed
arguments may instead precede the function name as a group.

```tung
let x add-two y = x + y
let (x: ℤ, y: ℤ) add: ℤ = x + y
```

local blocks are parenthesised and end with `yield`.

```tung
(
  let six = 2 × 3
  yield six + 1
)
```

type variables are implicit. type application useþ second-is-function syntax.
`ℤ list` applieþ `list` to `ℤ`; `a ∏ b` applieþ the binary product type.
`let-ilk` defineþ a transparent type alias.

functions use `→`. immediate effect rows follow `!` on a function arrow. a
stored non-function value cannot carry an effect row. `func` is the binary
pure-function type constructor: `a func b` and `a → b` agree.
`(term: type)` constraineþ any term.

```tung
let-ilk a powerset = a func 𝟚
let (f: a → b ! e, x: a) call: b ! e = x f
let answer = (1 + 2: ℤ)
```

pure inferred lets are generalised. immediately effectful right-hand sides
remain monomorphic. creating a function with latent effects is pure.
generalisation quantifyþ variables free in the binding's type and requirements,
but not in the enclosing environment. thus, a local alias of a function
parameter retaineþ its type constraints.

## algebraic data and patterns

`ilk` declareþ an algebraic data type. type parameters and constructor arguments
follow the ordinary sequence rule.

```tung
ilk a option {
  none,
  a some
}
```

constructor application is curried. patterns may bind variables, use `_`, match
exact integer or text literals, or destructure constructors. each binder may
occur only once in a pattern row.

`match` consumeþ one or more comma-separated scrutinees. bare braces form an
anonymous function with one curried input per pattern. `@` separateþ a case from
its result. `|` joineþ complete alternative rows that share a result.

```tung
let chosen = match yea, nay {
  yea, b | b, yea @ b,
  nay, _ @ nay
}

let not: 𝟚 → 𝟚 = {
  yea @ nay,
  nay @ yea
}

let classify: text → ℤ = {
  'yes' @ 1,
  _ @ 0
}
```

the checker rejecteþ non-exhaustive and redundant rows over algebraic data.
finite rows of integer or text literals need a variable or `_`. both types have
infinitely many values. an empty function `{}` eliminateþ an annotated
uninhabited input such as `𝟘`.

## records

records are closed. `r(field: type)` formeþ a type; `r(field = value)` formeþ a
value. an update placeþ its base after `=`. later entries set or remove fields.

```tung
let person: r(name: text, age: ℤ) =
  r(name = 'naoki', age = 35)
let older = r(= person, age = 36)
let public = r(= person, - age)
let age = older.age
```

in `record.field`, `record` is a term and `field` an unqualified field name.
access chaineþ leftward: `record.outer.inner` selecteþ `outer`, then `inner`.
field sets must match an annotated record type. duplicate labels, unknown field
removal, and non-record updates are errors.

## frames, fills, and laws

`frame` declareþ a statically selected interface. `fill` provideþ evidence and
member implementations. leading `graiþ` clauses state required evidence for a
declaration, frame, fill, or individual member.

```tung
frame a equal {
  let a ≡ a: 𝟚
  law (x: a): x ≡ x ~ yea
}

graiþ a equal
let (a: a, b: a) ≢: 𝟚 = (a ≡ b) if nay yea

fill 𝟚 equal {
  let ≡ = {
    yea, yea @ yea,
    nay, nay @ yea,
    _, _ @ nay
  }
}
```

a fill must supply each required member not supplied by a parent. a child fill
also witnesseþ required parent frames and may define inherited members.
duplicate fills are incoherent.

selection preferreþ the shortest inheritance path. a direct fill outrankeþ an
inherited witness. primitive fills are fallbacks after every matching
user-defined fill. at the best rank, repeated paths to the same fill are
deduplicated. a structured type pattern outrankeþ a blanket pattern that it
strictly refineþ. incomparable matching fills are ambiguous. selection dependeþ
only on static types, never values or import order.

`law` recordeþ a type-checked equation. both sides must share a value type and
immediate effect row. they may use only the enclosing requirements. laws are
documentation; the compiler neither proveþ, executeþ, nor rewriteþ with them.

## effects and handlers

`deed` declareþ an algebraic effect. every operation is a function. one without
informative input takeþ `𝟙`. declaration application addeþ the effect to the
written latent row.

an effect row denoteþ an unordered, idempotent union. `e`, `e0`, and `e₁` stand
for whole rows. `e0, e1` denoteþ their union; they need not be disjoint.
repeated occurrences of one nominal effect must agree on type arguments.

function-type unification equateþ rows. a body's effects need only be a subset
of its annotation. unresolved union equations remain constraints on an inferred
polymorphic binding. each use instantiateþ them afresh. annotation variables are
rigid while checking the body.

an operation-specific handler for a parameterised effect requireþ a closed body
effect row. an open row could later contain the same effect wiþ other type
arguments. runtime handling matcheþ the effect identity.

```tung
deed e fail {
  e fail: a
}

deed a state {
  𝟙 get: a,
  a set: 𝟙
}
```

operations are called like functions and handled by `try`. a `yield` case
transformeþ the normal result. without one, that result is unchanged. operation
cases may call the implicit `eftgin` continuation any number of times.

if an effect and one of its operations share a name, a case without an argument
pattern nameþ the whole effect. write `_ name @ ...` to target the operation.
use `eftgin` when its input is unneeded.

```tung
deed ask {
  ℤ ask: ℤ
}

let answer: text = try 10 ask {
  yield n @ n to-text,
  n ask @ (n + 1) eftgin
}
```

handlers are deep: the same handler surroundeþ continued work. a case for an
entire effect removeþ it. operation-specific cases remove it only when every
sibling operation is covered. effects from case bodies remain in the surrounding
row.

## runtime boundary

the standard runner supporteþ `console`, `random`, `async`, `file`, `system`,
`clock`, `process`, and `web`. user-defined effects and `fail` must be handled
inside `main`. source handlers cannot intercept runner effects; the checker
rejecteþ such clauses. `state` and `fail` remain source-handled effects.

an annotated file-level `let` may bind a host operation. a text key precedeþ
`fremmed`. `_foreign.tung` chiefly useþ this boundary. the host key and full
declared type must match the compiler catalogue.

the
[bookhoard reference guide](https://github.com/jukisima/tung-bookhoard/blob/main/reference.adoc)
explaineþ the standard library's design boundaries. runnable examples live under
[`byspel/`](../byspel/).
