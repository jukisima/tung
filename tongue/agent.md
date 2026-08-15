# agent notes

this file is meant to be self-contained. read this before changing the repo. it repeats the language specification on purpose. the repetition is useful: tung has unusual syntax, and future changes should preserve those unusual rules.

## work style

- never capitalise sentence starts in prose or source comments. preserve case only when syntax or an identifier requires it.
- use british spelling in prose and source comments. preserve fixed spellings in syntax, protocols, apis, and identifiers.
- prefer plain words and simple explanations.
- tung keywords and user-facing bookhoard names belong to the object language and may be anglishised.
- haskell compiler names, tests, and tooling have no anglish naming rule. use standard technical terms there, or mirror a tung term when that makes the source-to-implementation link clearer.
- keep source changes tied to the asked-for behaviour.
- prefer simpler code over broad abstraction.
- do not reset, remove, or revert unrelated user edits.
- use `rg` for search.
- use `apply_patch` for hand edits.
- run tests after parser, type checker, evaluator, bookhoard, or byspel changes.
- when syntax changes, update tests, byspels, bookhoard files, readme, and this file.

## current naming

- the language is named `tung`.
- source files use `.tung`.
- the cabal package and executable are named `tung`.
- vscode uses language id `tung`, file extension `.tung`, and setting key `tung.tonguePath`.
- the public import paths in tung code use `.tung`.
- the haskell module tree is still `Tung.*`. this is an implementation namespace only. do not rename it during ordinary language work unless the task specifically asks for an internal haskell namespace migration.
- do not reintroduce earlier language names, extensions, language ids, or command names.

## source of truth

- the implementation is authoritative when it conflicts with old discussion.
- this file is the broad language and maintainer spec.
- `readme.md` is the shorter user-facing language guide.
- `vscode/readme.md` is the editor-tooling guide.
- `vscode/client/`, `vscode/server/`, and `vscode/test/` contain typescript source; `vscode/out/` is generated commonjs and must not be edited.
- typescript uses `const` arrow functions by default, including exported helpers; use a function declaration only when its hoisting is required.
- `bookhoard/` files are not byspels. they are real standard-bookhoard source and must type check.
- `byspel/` files are runnable byspels and must pass runnable-file checking.
- tests are part of the specification. when behaviour changes, update tests to describe the intended rule.
- if a source file and this file disagree, inspect parser, type checker, evaluator, tests, and bookhoard before choosing which side is stale.

## agent workflow

when starting a task:

- read this file.
- inspect the touched area with `rg` before editing.
- check the current git status and assume unrelated dirty files belong to the user.
- locate existing tests for the behaviour before adding new ones.
- prefer changing one stage at a time: token, parse, validate, type, name, evaluate, bookhoard, editor.
- keep syntax and semantics aligned. a parser-only change is rarely enough.
- use the smallest change that preserves the language invariants below.

when ending a task:

- run the narrowest useful tests first.
- run `cabal test all` after compiler, bookhoard, or byspel changes.
- run `npm run check` and `npm test` after editor, lsp, syntax-highlight, or formatter changes.
- run `git diff --check`.
- run at least one byspel when changing runner behaviour.
- report any command that could not be run.

## core invariants

- syntax must remain unambiguous.
- every function is curried.
- second-is-function application is used consistently for expressions, type application, constructor declarations, function headers, and fill headers.
- `$` is low-precedence application grouping, not function-position swapping.
- partial application is pure.
- only saturated application may perform latent effects.
- effects are computation labels on function arrows, not stored value types.
- file evaluation is strict call-by-value.
- the cli requires a local `main` with one `𝟙` input, a `𝟙` result, and only runner-supported latent effects.
- imports use qualify-if-needed lookup. bare imported names are allowed only when unambiguous.
- `ground.tung` is an ordinary import, not a magic prelude.
- exports are explicit. declarations are private unless shown.
- type and term namespaces are separate.
- data types and effects share the type namespace.
- match exhaustiveness is checked at compile time where the type is algebraic.
- records are closed.
- handlers are deep and `resume` is multi-shot.
- bookhoard modules must stay acyclic and bottom-up.
- object-language syntax should stay small even if the implementation uses normal compiler terminology.

## common traps

- do not treat `ground.tung` as a prelude. users must bring it.
- do not force qualification for unique imported names or unique operators.
- do not colour or resolve a `bring` file path as ordinary names in editor tooling.
- do not make a non-function binding have a type like `integer ! console`. only function arrows carry effects.
- do not restore the final expression as an implicit program entry point. runnable files enter through `null main`.
- do not let partial application run native effects. `1 ÷` is pure; `1 ÷ 0` may fail.
- do not encode `fail` as one fixed division-by-zero error. `fail` is a parameterised effect with an error value and polymorphic answer.
- do not remove the type parameter from `state`.
- do not make handler `resume` one-shot unless the language spec changes. current resume is multi-shot.
- do not make handlers shallow. current handlers are deep.
- do not add `perform`; effects are called as ordinary operations and handled by `try`.
- do not restore scrutinee-less `match`. anonymous functions are bare braces.
- do not restore a general infix escape. `.*` is an ordinary name.
- do not restore old `$` function-swapping behaviour. current `$` groups low-precedence application.
- do not add explicit type-parameter syntax unless the spec changes.
- do not change object-language keywords to common compiler terms without checking the desired anglish direction.
- do not copy old byspels blindly after syntax changes. update bookhoard, byspels, tests, readme, and this file together.
- do not trust editor semantic analysis as the compiler. it is intentionally tolerant.
- do not hide a type-system weakness by changing the bookhoard to fit it. decide against the reference model first.

## commands

build everything:

```sh
cabal build all
```

run the whole test suite:

```sh
cabal test all
```

run a tung file:

```sh
cabal run tung -- ../byspel/fizzbuzz.tung
```

run any tung file:

```sh
cabal run tung -- path/to/file.tung [arguments...]
```

check the vscode extension and lsp server from `../vscode`:

```sh
npm run check
npm test
```

install the repository tools and pre-commit hook from the repository root:

```sh
brew bundle
lefthook install
```

the pre-commit hook runs fourmolu on staged haskell files and re-stages formatting fixes.

if cabal fails only because it cannot write under `~/.cabal/logs`, rerun the same cabal command with the needed permission. do not change source to work around cabal log permissions.

## compiler architecture

the compiler is a small staged implementation.

the main data flow is:

```text
source text
  -> lexTokens
  -> parseTokens
  -> validateProgram
  -> import expansion and name lookup
  -> type inference and checking
  -> runnable boundary checking when requested
  -> type-directed elaboration and evidence resolution
  -> CoreProgram validation
  -> evaluation
```

keep this layering:

- tokenisation must not need parser state.
- parsing must not need type information.
- validation must not need inference.
- only the type checker may construct `CoreProgram`.
- evaluation should not repair static errors.
- the lsp server may approximate, but the haskell compiler is authoritative.

## editor architecture

the editor has two deliberately different sources of knowledge.

the tolerant path tokenises incomplete buffers, assigns lexical and structural roles, builds declaration and import models, and resolves visible workspace names. semantic highlighting, navigation, completion, symbols, folding, rename, and documentation remain available while a file is unfinished.

the authoritative path sends complete source bundles to the haskell compiler for diagnostics and inferred types. editor analysis must not silently become a second type checker, and compiler failure must not erase useful tolerant highlighting.

document changes invalidate the affected model, imported-name views, semantic tokens, diagnostics, and inferred-type cache. changes to a brought file must also refresh open dependants.

formatting is a fast structural operation over source text. it must preserve syntax and comments, remain idempotent, and must not invoke the compiler.

bookhoard documentation is derived from the same public-definition and doc-comment model used by editor help. `npm run docs` regenerates the ignored html wiki from current source; generated output is never a source of truth.

## change discipline

- follow the compiler stages in order and keep every affected stage consistent.
- keep static rejection separate from deliberate runtime failure.
- test both successful and adversarial forms of semantic changes.
- keep bookhoards type-correct, byspels runnable, and editor analysis compatible with incomplete source.
- keep module-local invariants in source comments beside their implementation.

## language overview

tung is a small expression-oriented programming language.

tung has sequence application, curried functions, algebraic data types, closed records, type classes for ad-hoc polymorphism, imports, and explicit effect annotations.

tung source files use the `.tung` extension.

the implementation is in haskell.

## source files

a file is a sequence of self-delimiting declarations followed by an optional final expression.

a trailing semicolon at the end of a file may be omitted.

`bring` declarations require semicolons.

local block `let` declarations require semicolons.

file-level declarations do not require semicolons between them.

file-level semicolons are accepted as optional separators.

the outer file acts like an implicit block.

file-level `let` declarations run before the final expression.

later expressions see earlier bound values.

effect annotations appear on function types.

effectful work must be wrapped in a function. module initialisation is pure.

do not write an effect annotation on a non-function value type. effects belong to computations and function arrows, not stored values.

```tung
bring ground.tung;
bring data/list.tung;

let answer: integer = 42
```

`bring path;` imports another file.

the import graph must be acyclic. a direct or indirect bring cycle is a type error.

a shown imported definition is available as `namespace@name`.

the namespace is the full import path before `.tung`.

for `bring ground.tung;`, the namespace is `ground`.

for `bring data/list.tung;`, the namespace is `data/list`.

an imported name may also be used bare when it is unambiguous.

an imported bare name is accepted only when exactly one visible definition has that name.

if two visible definitions share a name, the checker must report ambiguity and require qualification.

do not special-case `ground` as magically bare. imports use the same qualify-if-needed model.

tung has two namespaces.

terms include values, functions, data constructors, effect operations, foreign terms, and shape methods.

types include primitive types, aliases, data types, and effect constructors.

both namespaces are private by default.

prefix a declaration with `show` to publish it.

when a declaration starts with `graith`, put `show` after the requirements and directly before the main declaration keyword.

```tung
show let answer = 42;
graith a equal show let (x: a, y: a) same: 𝟚 = x ≡ y;
show let-ilk count = integer;
show kin a option { none, a some }
show deed ask { integer ask: integer }
show class foreign { integer add-integer integer: integer }
show shape a identity { a identity: a }
graith a equal show shape a order { a ≤ a: 𝟚 }
```

showing a data declaration publishes its type and constructors.

showing a deed declaration publishes its effect type and operations.

showing a foreign declaration publishes its terms.

showing a shape also publishes its methods, including defaults.

fill declarations are instance evidence rather than named api entries. fill evidence crosses imports automatically and is not prefixed with `show`.

`show name;` re-exports an already visible term without redefining it.

`show-ilk name;` re-exports an already visible type, including an effect constructor. it does not re-export data constructors or effect operations.

term and type names are looked up in separate namespaces. data types and effect constructors share the type namespace. if a bare name is ambiguous within its namespace, the re-export is an error and the source must use a qualified name.

```tung
bring data/option.tung;
show-ilk data/option@option;
show data/option@default;
```

there is no wildcard re-export. re-export each term or type with `show name;` or `show-ilk name;`.

an ordinary `bring` never re-exports its imported surface.

private names are unavailable to importers both bare and qualified.

```tung
bring ground.tung;

let x = ground@yea;
let y = yea
```

when a file is run as a program, it must declare `main` locally.

`main` must take exactly one `𝟙` argument and return `𝟙`.

the runner evaluates the module and then calls `null main`.

only runner-provided effects may appear on the `main` arrow.

the runner-provided effects are `console`, `random`, `async`, `file`, `system`, and `clock`.

`arguments` receives only the command-line arguments after the source path.

user-defined effects must be handled inside `main`.

`fail` must be handled inside `main`.

an unhandled `fail` is not allowed on `main`.

## comments

`#` starts a line comment.

`/*` starts a block comment and `*/` ends it.

block comments are not nested.

`##` starts a line doc comment.

`/** ... */` is a block doc comment.

doc comments attach to the next declaration when no other declaration appears between the comment and that declaration.

the lsp exposes attached docs in hover and completion.

the bookhoard html wiki includes only shown declarations and attached docs. change the declaration or its doc comment, then regenerate the wiki; do not hand-edit generated html.

## names

most non-space characters can be part of a name.

special syntax characters are not ordinary name characters.

the parser treats delimiters, arrows, and separators as syntax, not as names.

`@` qualifies names.

qualified names are ordinary names after parsing.

```tung
data/list@empty
data/list@.*
console@write
```

`.*` is a name.

`.*` is the list prepend operator.

`.*` is not a `.` infix escape followed by `*`.

a standalone `.` is not an infix escape.

the language no longer has an infix escape feature.

`_` discards a binding or pattern.

`_` is a catch-all pattern.

## sequence application

application is written as a sequence.

every function is curried.

a single term is itself.

in an unmarked sequence of two or more terms, the second term is the function.

the other terms are arguments in order.

this rule is called second-is-function syntax in the code and tests.

`a f` means `f(a)`.

`a f b c` means `f(a, b, c)`.

`a b f` is not `a(b(f))`; it means `b(a, f)` because the second term is the function.

`b (a f)` and `a b f` are the same when expanded by the second-is-function rule.

`$` is a low-precedence pipeline separator.

the first chunk uses second-is-function syntax.

after `$`, the next atom is the function.

the previous chunk is inserted as the first argument of that function.

the remaining unparenthesised tail before the next `$` is parsed as function-first arguments to the function after `$`.

`$(` starts a function-first segment.

inside `$(`, the first term is the function and later terms are ordinary arguments.

`x $f a b` means `x f a b`.

`x $(f a b)` means `x f a b`.

`$` cannot start an expression.

`$h` and `$ h` are both accepted.

```tung
1 + 2        # (+ 1 2)
2 (1 +)      # curried partial application
1 $ + 2      # same as 1 + 2
a f $h b g
# means (a f) h b g

a f $(g b c) $h d
# means ((a f) g b c) h d
```

partial application builds a function value.

partial application must be pure.

partial application must not run latent effects.

only full application can run latent effects.

checking effects uses subsumption: a computation with fewer effects may stand where a larger declared row is allowed.

effect subsumption never hides an effect outside the declared row.

for byspel, `1 ÷` is pure and returns a function.

for byspel, `1 ÷ 0` is effectful and may perform `fail`.

## evaluation strategy

tung is strict call-by-value.

file declarations are evaluated in order.

after module initialisation, the cli applies `main` to `null`.

`let` evaluates its right-hand side before binding the name.

a bare brace function is already a value. its body is not evaluated until the function receives enough arguments.

application evaluates the function expression first.

application evaluates arguments left-to-right.

partial application returns a function value.

partial application does not run latent effects.

only saturated application runs native work or performs an effect operation.

records evaluate fields eagerly.

record updates evaluate the base record first, then replacement fields in order.

`match` evaluates all scrutinees before choosing a case.

match cases are tried top-to-bottom.

only the chosen case body is evaluated.

`try` evaluates the body first.

effect operation arguments are evaluated before the operation is handled.

`resume` continues the captured strict computation.

deep handlers are reinstalled around resumed computation.

## declarations

`let` always uses `=`.

```tung
let id: a → a = { x | x };
```

function definition headers use the same sequence rule as expressions.

the second term is the function name.

a type after a second-position header is the full curried function type.

```tung
let x add-two y = x + y;
let x add-two-typed y: integer → integer → integer = x + y;
```

typed non-infix arguments may also be grouped before the function name.

in grouped typed-argument syntax, each argument carries its own type.

in grouped typed-argument syntax, the type after the function name is the result type.

grouped typed-argument syntax is allowed for ordinary definitions and type-class defaults.

class constraints for a definition are written with leading `graith`.

```tung
graith a equal let (x: a, y: a) same: 𝟚 = x ≡ y;
```

the grouped typed-argument form makes this:

```tung
let (x: integer, y: integer) add: integer = x + y;
```

mean a curried function:

```tung
integer → integer → integer
```

polymorphic typed arguments may include effects.

effect annotations after the result in grouped typed-argument syntax belong to the function being defined, not to a stored non-function value.

```tung
let (f0: a → b ! e0, f1: b → c ! e1) compose: a → c ! e0, e1 =
  { x | (x f0) f1 };
```

second-is-function headers also work with named arguments:

```tung
let value default-option default: a option → a → a = match value {
  none | default,
  x some | x
}
```

## type aliases

type aliases use `let-ilk`.

```tung
let-ilk string = character list;
```

`string` is the standard alias for `character list` and is available before that alias is brought.

aliases are transparent during unification.

an alias is not a fresh nominal type.

## algebraic data

data declarations use `kin`.

data type parameters are implicit names before the data type name.

constructors use the same second-is-function sequence rule.

multi-argument type and constructor operators use the same sequence rule as expressions.

```tung
kin a option {
  none,
  a some
}
```

```tung
kin a ∏ b {
  a ∏ b
}

kin a ∐ b {
  a first,
  b other
}
```

these byspels mean:

- `option` has one type parameter.
- `none` is a nullary constructor.
- `some` takes one value.
- `∏` is both the type name and a constructor name.
- `∐` is a type name with constructors `first` and `other`.

type application follows second-is-function syntax too.

```tung
let pair-value: integer ∏ string = 1 ∏ 'one';
```

constructor application is curried.

nullary constructors behave like values.

## effects

effect declarations use `deed`.

effect operations are nonempty functions.

an operation with no information to receive takes `𝟙` and is called through ordinary second-is-function application, such as `null read`.

an operation with inputs has a function type. partial application stays pure and only full application performs the operation.

the latent effect is added by the declaration itself.

written latent effects are kept, so operations may mention effects besides their own effect.

```tung
deed console {
  string write: 𝟙,
  𝟙 read: string
}
```

parameterised effects are allowed.

`state` is parameterised.

```tung
deed a state {
  𝟙 get: a,
  a set: 𝟙
}
```

`fail` is extensible and parameterised by the error type.

`fail` operations carry an error value and have a polymorphic answer type.

```tung
deed e fail {
  e fail: a
}
```

`async` is a standard effect.

`async` uses `task`.

```tung
kin a task {
  a done
}

deed async {
  (𝟙 → a) fork: a task,
  (a task) wait: a,
  integer sleep: 𝟙
}
```

the default runner interprets `async` synchronously.

`sleep` blocks the current run.

`fork` runs the work immediately and wraps the answer in `done`.

`wait` unwraps `done`.

`file` is a standard effect.

the default runner handles whole-text file operations directly.

file-system errors perform `string fail`.

```tung
deed file {
  string read-file: string ! string fail,
  string write-file string: 𝟙 ! string fail,
  string append-file string: 𝟙 ! string fail
}
```

## type classes

the language keyword for type classes is `shape`.

the language keyword for instances is `fill`.

use current language terms consistently.

`shape` declarations may have requirements introduced by leading `graith`.

`graith` requirements use comma separation.

members inside `shape` use semicolon separators.

the last member may omit its semicolon before `}`.

required members are bare signatures.

do not put `let` before a required member signature.

default members use the same `let` syntax as ordinary functions.

default implementations are allowed.

a default implementation is checked like an ordinary member.

a default implementation is visible by the same bare or qualified name.

laws use `law (name: ilk, ...): left ~ right` inside a `shape`.

law parameters are comma-separated local term names and each parameter requires
a type annotation.

law entries use the same semicolon separators as other shape members. the final
law may omit its semicolon before `}`.

both law sides are inferred in one shared local context. they must have the same
value type and the same immediate effect row.

a law may require its enclosing shape and any shape named by the enclosing
`graith` clause. any other inferred shape requirement is rejected.

law type variables are universally checked. effect-row variables remain
inferred row variables so written polymorphic latent effects can agree across
both sides.

laws are static shape metadata. they do not add methods, do not create fill
obligations, and are erased before evaluation.

the compiler does not perform equational reasoning or prove that law sides are
equal yet.

when a default uses grouped typed arguments, the annotation after the member name is the result type.

```tung
shape a equal {
  a ≡ a: 𝟚;
  let a ≢ b = (a ≡ b) ¬;
  law (x: a): x ≡ x ~ yea
}
```

```tung
graith a equal shape a order {
  a ≤ a: 𝟚
}
```

```tung
graith a add shape a subtract {
  a - a: a
}
```

`fill` implements a `shape`.

`fill` headers use second-is-function syntax.

`fill` declarations may have requirements introduced by leading `graith`.

these requirements are the instance context.

when a matching instance solves a constraint, the solver also checks or emits that instance context.

the named `shape` must exist.

every required member must be supplied by the `fill`, by a default, or by an already available required parent `fill`.

duplicate members in a `shape` or `fill` are rejected.

duplicate operations in a `deed` are rejected.

a `fill` for a child shape also witnesses its required parent shapes. this is the language's deliberate shorthand for the usual explicit parent instances.

members inside `fill` use the same `let` syntax as ordinary functions.

members inside `fill` use semicolon separators.

the final semicolon before `}` may be omitted.

```tung
fill 𝟚 equal {
  let a ≡ b = match a {
    yea | b,
    nay | b ¬
  }
}
```

```tung
graith a equal fill (a box) equal {
  let (x box) ≡ (y box) = x ≡ y
}
```

a child `fill` may define inherited parent members. those definitions satisfy the corresponding parent requirement.

type-class member names may be bare if unambiguous.

type-class member names may be qualified if needed.

do not force qualification for unique operators.

if an operator name is unique, bare use should compile.

only require qualification when the name is ambiguous.

fill selection depends on static inferred types, never runtime values or bring order. result-only members such as `zero` use their expected result type.

one fill brought through a diamond remains one fill, while equally named shapes from distinct qualified modules remain distinct. duplicate, overlapping, or recursively required fills are type errors.

## expressions

integer literals are decimal.

float literals contain a decimal point.

character literals start with backtick.

character escapes are `\n`, `\r`, `\t`, `\'`, `\\`, and decimal unicode `\{123}`.

unicode codepoint character literals may also use backtick plus braces.

strings use single quotes.

string escapes are `\n`, `\r`, `\t`, `\'`, `\\`, and decimal unicode `\{123}`.

```tung
42
3.14
`c
`\n
`\{23383}
`{23383}
'text \{23383}'
```

anonymous functions use bare braces.

type-level `func` is allowed as the binary pure function type constructor.

`a func b` means `a → b`.

do not use `match` without a scrutinee.

`{ ... }` takes one or more cases.

each case takes one or more patterns, separated by commas.

multiple patterns in a bare brace function mean multiple curried arguments.

`{ x, y, z | body }` has a type shaped like `a → b → c → d`.

the argument order is the pattern order: first `x`, then `y`, then `z`.

each case uses `|`.

```tung
{ x | x }
{ x, y | x + y }
{ x, y, z | (x + y) + z }
```

blocks are parenthesised.

blocks contain local `let` declarations plus a final expression.

local block `let` declarations require semicolons.

the final expression in a block may be followed by an optional semicolon before `)`.

```tung
(
  let x = 2 × 3;
  x + 1
)
```

## match

`match` can consume scrutinees.

`match` can consume one scrutinee.

`match` can consume multiple scrutinees.

scrutinees are separated by commas.

```tung
let chosen = match yea, nay {
  yea, b | b,
  nay, _ | nay
};
```

anonymous functions do not use `match`.

bare `{ ... }` is a function.

when a bare brace function case has multiple patterns, the result is a curried function with one argument per pattern.

```tung
let not: 𝟚 → 𝟚 = {
  yea | nay,
  nay | yea
};
```

this bare brace form is recognised as a function over its pattern inputs.

match cases use `|`.

the consuming form is `match term { ... }`.

the function form is `{ ... }`.

the type checker rejects non-exhaustive matches over algebraic data at compile time.

coverage checking must include multi-scrutinee matches.

coverage checking must include nested constructor patterns.

an empty match is allowed when the scrutinee type is uninhabited.

the empty anonymous function `{}` is allowed when its annotated input type is uninhabited.

this is the eliminator for `𝟘`.

```tung
let initial: 𝟘 → a = {};
```

an unannotated empty anonymous match `{}` is not allowed, because it has no pattern arity.

variable patterns are catch-all patterns.

`_` patterns are catch-all patterns.

patterns are linear. one match row may not bind the same variable twice.

## records

records are closed.

record expressions use brackets.

record fields use `=`.

record type fields use `:`.

record update starts with `[= base, ...]`.

record update can set fields.

record update can remove fields.

record field access uses `@`.

normal qualified-name lookup wins first, so `file@name` still means an imported qualified name when such a name exists.

if no exact name is found, `record@field` reads `field` from `record`.

field removal uses `- field`.

```tung
let person = [name = 'naoki', age = 35];
let older = [= person, age = 36];
let public = [= person, - age];
```

closed records mean field sets must match when an annotated record type is checked.

record value and record type literals reject duplicate labels.

removing an unknown field is an error.

updating a non-record is an error.

## handlers

effects are invoked as ordinary calls.

effects are handled with `try`.

the keyword is `try`, not `perform`.

handlers use `try body { cases }`.

`try` always requires the handler braces and cases. it does not fall back to its body when braces are missing.

handlers may include a `return` case.

the `return` case handles the normal value produced by the body.

if no `return` case is written, the default is identity.

there may be at most one `return` case.

handler cases may name an operation.

handler cases may name an effect.

handler case arguments use the same header rule used by functions.

inside an operation handler, `resume` is bound implicitly.

the type of `resume` is the operation result type to the handler answer type.

for an operation `op: a → b`, inside a handler whose answer type is `c`, `resume` has type `b → c` plus the still-unhandled effects.

when an effect and one of its operations share a name, patterns prefer the operation form.

`fail | fallback` handles the `fail` effect without binding the error value.

`message fail | message` handles the `fail` operation and binds the error value.

`_ fail | fallback` handles the `fail` operation and ignores the error value.

do not require explicit `resume` binding in syntax.

`resume` is the captured continuation.

the continuation is multi-shot.

the handler may call `resume` zero times.

the handler may call `resume` one time.

the handler may call `resume` many times.

resuming is deep.

deep resuming means the same handler remains installed around the rest of the resumed computation.

```tung
deed choice {
  𝟙 pick: integer
}

let branched: integer =
  try (null pick) + (null pick) {
    pick | (1 resume) + (2 resume)
  };
```

```tung
deed ask {
  integer ask: integer
}

let answered: integer =
  try 10 ask {
    x ask | x resume
  };
```

```tung
let answered-text: string =
  try 10 ask {
    return n | n to-string,
    x ask | x resume
  };
```

effect-level handlers may not bind operation arguments.

a handler for an absent effect is a type error.

handler cases remove the handled effect.

an effect-wide case removes the whole effect.

operation cases remove the whole effect only when they cover every operation declared by that effect.

handling one operation while leaving a sibling operation uncovered does not erase the effect.

partial operation coverage does not combine across nested handlers. effect rows track effect labels, not per-operation subsets.

effects used by the return case and handler bodies are added to the surrounding computation.

`resume` must be type checked.

the argument passed to `resume` must match the operation result type.

## async byspel semantics

`async` is a standard effect.

the default runner interprets `async` synchronously.

this is still modelled as an effect, because it is a runner capability.

```tung
let answer: integer =
  try (
    let left = ({ _ | 20 }) fork;
    let right = ({ _ | 22 }) fork;
    (left wait) + (right wait)
  ) {
    work fork | ((null work) done) resume,
    (value done) wait | value resume
  };
```

## types

primitive types are:

- `integer`
- `float`
- `character`
- `string`

do not reintroduce `int`.

always use `integer`.

type application uses the same sequence rule as expression application.

`integer list` means `list<integer>`.

`a ∏ b` means `∏<a, b>`.

function types use `→`.

`func` is also available as a type-level binary function constructor.

`a func b` means the same pure function type as `a → b`.

effects appear after `!`.

only function types may carry effects.

`a → b ! console` is valid.

`b ! console` is not a type.

in typed-argument definitions, `name: b ! e` is shorthand for putting `! e` on the function being defined.

in typed-argument definitions, `name: b ! e` is not a value type.

```tung
a → b
a → b ! console
a → b ! console, string fail
a → b ! integer state
a → b ! file, string fail
(a → b ! e) → a list → b list ! e
```

effect entries use the same type application rule as types.

`integer state` is the `state` effect at `integer`.

`string fail` is the fail effect carrying string errors.

`e` is an open effect variable.

type variables are implicit.

the checker supports hindley-milner style polymorphism.

an inferred `let` binding is generalised only when its right-hand side is pure. an immediately effectful right-hand side stays monomorphic. creating a function is pure, so latent function effects do not prevent generalisation.

user-written polymorphic annotations are checked with rigid skolems. an implementation cannot claim `a → a` by returning one fixed concrete type.

abstract skolems stay open for `graith` solving; an enclosing class requirement may discharge a local binding's matching requirement.

the checker supports closed records.

the checker supports curried application.

the checker supports multi-scrutinee matches.

the checker supports imports.

the checker supports type classes.

the checker supports instances.

the checker supports effect annotations.

the checker supports handlers that remove handled effects and add handler-body effects.

## native surface

the checker and runner provide these names natively.

standard-bookhoard type classes may also expose the same symbols generically.

arithmetic operations are defined as type-class members in the bookhoard, but the runner also supports them natively for `integer` and `float` where appropriate.

native numeric surface:

```tung
to-string: float → string
from-string: string → float ! string fail
⌊: float → integer
≤: float → float → 𝟚
+: float → float → float
-: integer → integer → integer
-: float → float → float
×: float → float → float
÷: integer → integer → integer ! string fail
÷: float → float → float ! string fail
exponent: float → float
logarithm: float → float
sine: float → float
arctan-float: float → float → float
```

`arctan-float` takes real part then imaginary part, and returns an angle in the range `0` to `τ`.

native runner function surface:

```tung
write: string → 𝟙 ! console
sleep: integer → 𝟙 ! async
read-file: string → string ! file, string fail
write-file: string → string → 𝟙 ! file, string fail
append-file: string → string → 𝟙 ! file, string fail
environment: string → string option ! system
exit: integer → a ! system
```

native operations without informative inputs still use ordinary unary function types:

```tung
read: 𝟙 → string ! console
random: 𝟙 → float ! random
arguments: 𝟙 → string list ! system
unix-time: 𝟙 → integer ! clock
```

standard ground helper:

```tung
write-line: string → 𝟙 ! console
```

foreign bindings may be declared in source with `class foreign`.

```tung
class foreign {
  integer add-integer integer: integer;
  integer divide-integer integer: integer ! string fail
}
```

this is not a type class.

it declares host-provided functions with ordinary tung types.

declared names enter the same name environment as `let` names.

source bookhoards can use those names inside `fill` to provide class methods.

`from-string` fails with `string fail`.

division by zero fails with `string fail`.

`sleep` is an `async` effect in the current design.

`unix-time` returns whole seconds since `1970-01-01 00:00:00 utc`.

## current standard bookhoard shape

`ground.tung` is an ordinary optional umbrella import that re-exports core data, effects, classes, and helpers.

core data is split across:

- `𝟘`
- `𝟙`
- `𝟚`
- product `∏`
- sum `∐`
- `option`
- `list`
- `task`
- `disjunctive`
- `conjunctive`

`𝟘` exports `initial: 𝟘 → a`.

core effects re-exported by `ground.tung` include:

- `fail`
- `console`
- `random`
- `state`
- `async`
- `file`
- `system`
- `clock`

core type classes are split across bookhoard files:

- `equal`
- `order`
- `magma`
- `semigroup`
- `monoid`
- `group`
- `semigroupoid`
- `category`
- `groupoid`
- `add`
- `zero`
- `subtract`
- `multiply`
- `one`
- `semiring`
- `ring`
- `divide`
- `mod`
- `field`

## theory and reference model

use these languages as checks on the design, not as syntax templates:

- haskell 2010 report: <https://www.haskell.org/onlinereport/haskell2010/>
- purescript book: <https://book.purescript.org/>
- purescript prelude: <https://pursuit.purescript.org/packages/purescript-prelude/docs/Prelude>
- purescript monoid api: <https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Monoid>
- purescript semiring api: <https://pursuit.purescript.org/packages/purescript-prelude/docs/Data.Semiring>
- koka book: <https://koka-lang.github.io/koka/doc/book.html>
- koka row-polymorphic effects paper: <https://www.microsoft.com/en-us/research/publication/koka-programming-with-row-polymorphic-effect-types-2/>

use haskell as the reference for hindley-milner generalisation, rigid signature checking, algebraic data, exhaustive pattern reasoning, qualified types, and class requirements.

tung differs from haskell by being strict and by tracking algebraic effects directly rather than putting ordinary effects behind `io` values.

use purescript as the reference for strict pure functional evaluation, coherent class use, explicit imports, algebraic data, and disciplined record labels.

follow purescript's module split where it helps dependency direction: core classes stay in core modules, while alternate representations such as conjunctive and disjunctive wrappers stay in separate modules. this is a module-boundary guide, not a reason to copy purescript syntax.

tung records are closed. do not infer purescript-style open record rows unless the language specification first adds them.

use koka as the reference for effect rows, effect polymorphism, deep handlers, answer-type-changing return clauses, and multi-shot resume.

partial application is pure because latent effects belong to saturation of the function arrow. this is a core invariant even where tung's curried surface differs from koka's call syntax.

an operation handler follows algebraic-handler semantics: its body receives the operation arguments and an implicit continuation. the continuation result is the handler answer type.

## test expectations

tests should cover success cases and failure cases.

tests are grouped by the compiler stage which owns the rule. do not repeat a parser-only rule in every later group.

evaluation tests should check exact results when deterministic.

evaluation tests for errors should check error prefixes when exact wording is not important.

add tests before or with bug fixes when a bug is discovered.

adversarial tests should try false polymorphic claims, duplicated labels, duplicated members, repeated pattern binders, missing match products, unknown classes, incomplete instances, sibling effect operations, bad resume arguments, and disallowed runnable effects.

when a stricter test breaks a bookhoard, decide whether the checker or the bookhoard violates the reference model. do not weaken sound checking merely to keep old bookhoard code passing.
