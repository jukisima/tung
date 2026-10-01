# tung language reference

this reference defineþ source syntax, module lookup, library manifests, and
runtime-visible behaviour. [`readme.md`](../readme.md) covereþ installation.
the checked [tutorial](../byspel/tutorial.tung) showeþ basic examples.
[`development.md`](development.md) covereþ the compiler and repository.

## files and modules

a `.tung` file containeþ declarations. a runnable file declareþ local `main`. it
takeþ `𝟙`, returneþ `𝟙`, and useþ only runner-supported effects. the runner
calleþ `only main` after pure module initialisation.

declarations are private by default. `show` publisheþ a declaration or visible
term. `show-ilk` publisheþ a visible type, effect, or flock. term and type
namespaces are separate. ordinary `use` doth not re-export anything.

## libraries and imports

`use path` importeþ a module. its final path segment becomeþ the default
namespace. an optional final name replaceþ it.

```tung
use ilk/list.tung
use ilk/table.tung hashmap

let xs ≔ list~empty
let entries ≔ hashmap~empty
```

visible imported names may remain bare when unambiguous. two different direct
imports cannot claim the same namespace. the import graph must be acyclic. a
local term shadoweþ an imported bare term in value and application positions. a
qualified name still reacheþ the import.

the host catalogue explicitly overloadeþ primitive names, such as `+` for
integer and float, within primitive scope. direct application trieþ their
signatures in catalogue order. as a value, the name selecteþ þe first signature.
constructors and flock members with the same spelling remain distinct
application candidates. later bindings shadow earlier ones of the same kind.

module lookup searcheþ paths relative to the importing file, declared library roots,
þen `TUNG_PATH` entries. a `.tung` path resolving to different files is
ambiguous. `ground.tung` is an optional import, not a prelude.

### manifests

the standard library is an ordinary library in [`bookhoard/`](../bookhoard/).
a `tung.yaml` manifest declareþ local library paths or pinned git repositories
under `dependencies`:

```yaml
dependencies:
  bookhoard:
    path: bookhoard
  example:
    repo: https://example.org/example.git
    hash: "0123456789012345678901234567890123456789"
    subdir: library
```

each dependency key nameþ one library throughout the graph, not a local alias.
`path` giveþ a live directory relative to the declaring manifest, or an
absolute directory. it need not be a git repository. changes there take effect
without a commit. `repo` giveþ a git URL or local git repository path, such as
`../my-library`. `hash` giveþ its full 40- or 64-digit hexadecimal commit hash.
quote a hash so yaml readeþ an all-digit value as text. specify either `path`
or both `repo` and `hash`. an optional `subdir` selecteþ a library inside the
pinned checkout. it must stay inside that checkout, even through symlinks.
omit `subdir` when the repository root is the library. external projects may
pin Bookhoard with `repo: https://github.com/jukisima/tung.git`, a published
commit hash, and `subdir: bookhoard`. `use` paths name `.tung` files relative to
the selected library root.

tung findeþ the nearest `tung.yaml` above the source file. local paths are
resolved from the manifest that declareþ them. git pins are checked out under
`.tung/libraries/` beside the root manifest. after the first fetch, tung
reuseþ the checkout offline. `tung --library-paths [project-path]` listeþ the
resolved roots. `TUNG_PATH` addeth roots outside manifest resolution.

each library may declare `tung.yaml`. tung traverseþ the whole dependency graph,
including libraries the program doth not import. matching git pins share one
checkout; matching local paths share one root. before module checking, tung
rejecteþ:

- one name with different sources, including a path and a git pin;
- one repository locator and subdirectory with different names or commits;
- one local path with different names.

if two libraries pin `shared` to different commits, the project faileþ. the
root manifest cannot override either pin. two names for one repository also
conflict when they select the same subdirectory, even at the same commit.
different subdirectories of one repository may supply distinct libraries.

relative git repository paths resolve from the manifest that declareþ them and
are lexically normalised. local library paths are canonicalised, so aliases to
one directory count as one path. remote locators are compared as written.
`subdir` is normalised and cannot contain `..`. use the same name and source in
every manifest. `TUNG_PATH` roots lie outside manifest conflict checking. the
same `.tung` path in different roots remaineþ ambiguous when imported.

to migrate from `tung.libraries`, move each repository and commit under
`dependencies` in `tung.yaml`. then remove the old file.

## names, comments, and literals

most non-space, non-reserved characters may occur in names. `~` qualifieþ
imports; `.` selecteþ record fields; `^` separateþ match and handler cases. type
alias, data, effect, and flock names are unqualified. so are þeir constructors,
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

`ilk` may name its parameters after the type, with a kind for each parameter.
`ilk` is the kind of ordinary types. `[ilk, ilk]` is a kind from types to types.
constructors may declare their argument types and result type in brackets.
the result type must name the declared `ilk` with its full arity. adjacent
constructor signatures need no commas.

```tung
ilk list [a:ilk] {
  empty [a list]
  _* [a, a list, a list]
}
```

constructors may choose different result indices. a match refineþ each index
within its own case. a type variable used only by a constructor is existential;
it cannot escape that case.

```tung
ilk tag [a:ilk] {
  integer [ℤ, ℤ tag]
  textual [text, text tag]
}
let unpack [x:a tag, a] match x {
  integer n ^ n,
  textual t ^ t
}
```

`let` bindeþ a value. `[type]` annotateþ a value. brackets with argument
slots describe a curried function.

```tung
let answer [ℤ] 42
let add [x:ℤ, y:ℤ, ℤ] x + y
let identity [@a:ilk, a, a] { x ^ x }
let x sameness ≔ x
let x add-after [y:ℤ, ℤ] x + y
```

in a bracketed `let`, `pattern:type` bindeþ an argument. patterns may
destructure constructors without grouping the whole pattern:
`let unbox [box x:a box, a] x`. symbolic infix patterns such as `a ∏ b`
also work without grouping. named patterns precede bare argument types.
a bare slot giveþ a type only; it bindeth no name. use `_` before the function
name or `_:type` in brackets to discard an argument. the final bare slot is
the result type. one bare slot after a name annotateþ a value; after
positional arguments, it giveþ the function result. the right-hand side
supplieþ any bare arguments. omit the final type to infer a result after a
named pattern. spaces around `:` are allowed; the formatter removeþ them.

leading `@name:ilk` declareþ a polymorphic type parameter. it contributeþ no
value argument; calls still supply only þe written value arguments. `@` is a
convention for type parameters now. optional argument syntax is a future task.
type variables without `@name:ilk` remain implicit.

untyped positional arguments follow the second-is-function rule:
`let a f b c ≔ ...` bindeþ `a`, `b`, and `c` to `f`. append brackets to
specify later typed arguments, a result, or effects. `let a f ≔ ...` is a
unary function even if `f` is also a type name. write `let a [f] ...` to
annotate a value.

local blocks are parenthesised and end with `yield`.

```tung
(
  let six ≔ 2 × 3
  yield six + 1
)
```

type variables are implicit. type application useþ second-is-function syntax.
`ℤ list` applieþ `list` to `ℤ`; `a ∏ b` applieþ the binary product type.
`let-ilk` defineþ a transparent type alias.

function types use `[argument, result]`. latent effects follow a semicolon:
`[argument, result; effect]`. several effects are comma-separated. nested
brackets describe function arguments. a stored non-function value cannot carry
an effect row. `→` is the binary pure-function type constructor for passing
to a flock: `a → b` and `[a, b]` agree. it associateþ to þe right, and
type application bindeþ more tightly.
`(term:type)` constraineþ any term.

```tung
let-ilk a powerset = a → 𝟚
let call [f:[a, b; e], x:a, b; e] x f
let answer ≔ (1 + 2:ℤ)
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
anonymous function with one curried input per pattern. `^` separateþ a case from
its result. `|` joineþ complete alternative rows that share a result.

```tung
let chosen ≔ match yea, nay {
  yea, b | b, yea ^ b,
  nay, _ ^ nay
}

let not [𝟚, 𝟚] {
  yea ^ nay,
  nay ^ yea
}

let classify [text, ℤ] {
  'yes' ^ 1,
  _ ^ 0
}
```

the checker rejecteþ non-exhaustive and redundant rows over algebraic data.
finite rows of integer or text literals need a variable or `_`. both types have
infinitely many values. an empty function `{}` eliminateþ an annotated
uninhabited input such as `𝟘`.

## records

records are closed. `r(field:type)` formeþ a type; `r(field = value)` formeþ a
value. an update placeþ its base after `=`. later entries set or remove fields.

```tung
let person [r(name:text, age:ℤ)]
  r(name = 'naoki', age = 35)
let older ≔ r(= person, age = 36)
let public ≔ r(= person, - age)
let age ≔ older.age
```

in `record.field`, `record` is a term and `field` an unqualified field name.
access chaineþ leftward: `record.outer.inner` selecteþ `outer`, then `inner`.
field sets must match an annotated record type. duplicate labels, unknown field
removal, and non-record updates are errors.

## flocks, bizens, and laws

| general term  | tung    |
| ------------- | ------- |
| type          | `ilk`   |
| type class    | `flock` |
| type instance | `bizen` |
| effect        | `deed`  |

`flock` putteþ its name first, then optional kinded parameters in `[]`, then
members in `()`. `a:ilk` declareþ a type parameter. `f:[ilk, ilk]` declareþ a
unary type constructor parameter. `bizen` provideþ evidence and
member implementations. leading `graiþ` clauses state required evidence for a
declaration, flock, bizen, or individual member. required members use a name
followed by bracketed types, wiþ the result last. laws bind typed patterns in
brackets. þeir equation beginneþ after `]`.

```tung
flock equal [a:ilk] (
  let ≡ [a, a, 𝟚]
  law [x:a] x ≡ x = yea
)

graiþ a equal
let ≢ [a:a, b:a, 𝟚] (a ≡ b) if nay yea

bizen 𝟚 equal {
  let ≡ ≔ {
    yea, yea ^ yea,
    nay, nay ^ yea,
    _, _ ^ nay
  }
}
```

in a `law`, `=` separateþ two expressions. a `let` without brackets useþ `≔`
before its definition. a bracketed `let` beginneþ its definition after `]`.
þis also applieþ to `let` implementations inside `bizen`.
law brackets require a type for each pattern. they also accept
constructor-first patterns. parentheses keep any nested `let` inside a law side.

a bizen must supply each required member not supplied by a parent. a child bizen
also witnesseþ required parent flocks and may define inherited members.
duplicate bizens are incoherent.

selection preferreþ the shortest inheritance path. a direct bizen outrankeþ an
inherited witness. primitive bizens are fallbacks after every matching
user-defined bizen. at the best rank, repeated paths to the same bizen are
deduplicated. a structured type pattern outrankeþ a blanket pattern that it
strictly refineþ. incomparable matching bizens are ambiguous. selection dependeþ
only on static types, never values or import order.

`law` recordeþ a type-checked equation. both sides must share a value type and
immediate effect row. they may use only the enclosing requirements. laws are
documentation; the compiler neither proveþ, executeþ, nor rewriteþ with them.

## effects and handlers

`deed` declareþ an algebraic effect. every operation is a function. one without
informative input takeþ `𝟙`. declaration application addeþ the effect to the
written latent row. an operation name is followed by bracketed argument types
and a final result type. effects follow `;`. one bracketed type may instead
name a complete function type.

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
  fail [e, a]
}

deed a state {
  get [𝟙, a],
  set [a, 𝟙]
}
```

operations are called like functions and handled by `try`. a `yield` case
transformeþ the normal result. without one, that result is unchanged. operation
cases may call the implicit `eftgin` continuation any number of times.

if an effect and one of its operations share a name, a case without an argument
pattern nameþ the whole effect. write `_ name ^ ...` to target the operation.
use `eftgin` when its input is unneeded.

```tung
deed ask {
  ask [ℤ, ℤ]
}

let answer [text] try 10 ask {
  yield n ^ n to-text,
  n ask ^ (n + 1) eftgin
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
[bookhoard reference guide](../bookhoard/reference.md)
explaineþ the standard library's design boundaries. runnable examples live under
[`byspel/`](../byspel/).
