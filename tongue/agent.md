# agent notes

this file is meant to be self-contained. read this before changing the repo. it repeats the language specification on purpose. the repetition is useful: jbini has unusual syntax, and future changes should preserve those unusual rules.

## work style

- write prose in lowercase unless a name, tool, or code sample requires otherwise.
- prefer plain words and simple explanations.
- keep source changes tied to the asked-for behavior.
- prefer simpler code over broad abstraction.
- do not reset, remove, or revert unrelated user edits.
- use `rg` for search.
- use `apply_patch` for hand edits.
- run tests after parser, type checker, evaluator, library, or sample changes.
- when syntax changes, update parser tests, type tests, eval tests, samples, library files, readme, and this file.

## commands

build everything:

```sh
cabal build all
```

run the whole test suite:

```sh
cabal test all
```

run a jbini file:

```sh
cabal run jbini -- ../sample/fizzbuzz.jbini
```

run any jbini file:

```sh
cabal run jbini -- path/to/file.jbini
```

if cabal fails only because it cannot write under `~/.cabal/logs`, rerun the same cabal command with the needed permission. do not change source to work around cabal log permissions.

## project map

- `jbini.cabal`: haskell package setup for library, command-line runner, and tests.
- `app/Main.hs`: command-line runner. it loads bundled libraries, checks that a file is runnable, then evaluates it.
- `src/Jbini.hs`: public haskell api.
- `src/Jbini/Syntax.hs`: syntax trees.
- `src/Jbini/Token.hs`: tokens, keywords, name characters, and source lexing.
- `src/Jbini/Parse.hs`: parser from token streams to syntax trees.
- `src/Jbini/Library.hs`: bundled library file list and loading helper.
- `src/Jbini/Type.hs`: imports, name lookup, type inference, type classes, instances, match coverage, effects, and runnable-file checks.
- `src/Jbini/Evaluate.hs`: interpreter, runtime values, native operations, imports, and effect handlers.
- `test/Main.hs`: tests for parsing, typing, name lookup, evaluation, libraries, and samples.
- `library/`: standard library written in jbini.
- `sample/`: runnable example programs.
- `readme.md`: user-facing overview and commands.
- `agent.md`: agent notes and the full tongue specification.

## implementation reminders

- parser changes often need type checker and evaluator changes too.
- type checker changes often need both success and failure tests.
- evaluator changes should usually include direct eval tests, not only type tests.
- import changes must be checked in both type checking and evaluation.
- effect changes must be checked at three levels: inference, handlers, and runnable-file boundary.
- evaluation is strict call-by-value. do not add lazy or call-by-name behavior unless the specification is changed first.
- partial application must stay pure. only a saturated call may produce latent effects.
- match exhaustiveness is compile-time behavior. do not rely on runtime non-exhaustive errors for algebraic data coverage.
- standard library files in `library/` are real jbini code. they must parse and type check.
- sample files in `sample/` must be runnable files. they must pass runnable-file type checking.

## language overview

jbini is a small expression-oriented programming language.

jbini has sequence application, curried functions, algebraic data types, closed records, type classes for ad-hoc polymorphism, imports, and explicit effect annotations.

jbini source files use the `.jbini` extension.

the implementation is in haskell.

## source files

a file is a sequence of declarations followed by an optional final expression.

a trailing semicolon at the end of a file may be omitted.

`bring` declarations require semicolons.

local block `let` declarations require semicolons.

file-level declarations do not require a final semicolon when they are the last thing in the file.

the outer file acts like an implicit block.

file-level `let` declarations run before the final expression.

later expressions see earlier bound values.

effect annotations appear on function types.

effectful value bindings either omit the annotation or wrap the work in a function.

do not write an effect annotation on a non-function value type. effects belong to computations and function arrows, not stored values.

```jbini
bring ground.jbini;
bring list.jbini;

let answer: integer = 42
```

`bring path;` imports another file.

an imported definition is always available as `namespace@name`.

the namespace is the first component of the import path.

for `bring ground.jbini;`, the namespace is `ground`.

for `bring list.jbini;`, the namespace is `list`.

an imported name may also be used bare when it is unambiguous.

an imported bare name is accepted only when exactly one visible definition has that name.

if two visible definitions share a name, the checker must report ambiguity and require qualification.

do not special-case `ground` as magically bare. imports use the same qualify-if-needed model.

`show name;` re-exports a value from the current file.

`show-type name;` re-exports a type name from the current file.

```jbini
bring ground.jbini;

let x = ground@yea;
let y = yea
```

when a file is run as a program, its final expression must return `𝟙`.

declarations without a final expression count as a pure `𝟙` file.

only runner-provided effects may remain at the runnable file boundary.

the runner-provided effects are `console`, `random`, `async`, and `file`.

user-defined effects must be handled before the end of the file.

`fail` must be handled before the end of the file.

an unhandled `fail` is not allowed at the runnable file boundary.

## names

most non-space characters can be part of a name.

special syntax characters are not ordinary name characters.

the parser treats delimiters, arrows, and separators as syntax, not as names.

`@` qualifies names.

qualified names are ordinary names after parsing.

```jbini
list@empty
list@.*
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

the remaining unparenthesized tail before the next `$` is parsed as one ordinary second-is-function expression.

`$(` starts a function-first segment.

inside `$(`, the first term is the function and later terms are ordinary arguments.

`x $f a b` means `x f (a b)`.

`x $(f a b)` means `x f a b`.

`$` cannot start an expression.

`$h` and `$ h` are both accepted.

```jbini
1 + 2        # (+ 1 2)
2 (1 +)      # curried partial application
1 $ + 2      # same as 1 + 2
a f $h b g
# means (a f) h (b g)

a f $(g b c) $h d
# means ((a f) g b c) h d
```

partial application builds a function value.

partial application must be pure.

partial application must not run latent effects.

only full application can run latent effects.

for example, `1 ÷` is pure and returns a function.

for example, `1 ÷ 0` is effectful and may perform `fail`.

## evaluation strategy

jbini is strict call-by-value.

file declarations are evaluated in order.

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

```jbini
let id: a → a = { x | x };
```

function definition headers use the same sequence rule as expressions.

the second term is the function name.

a type after a second-position header is the full curried function type.

```jbini
let x add-two y = x + y;
let x add-two-typed y: integer → integer → integer = x + y;
```

typed non-infix arguments may also be grouped before the function name.

in grouped typed-argument syntax, each argument carries its own type.

in grouped typed-argument syntax, the type after the function name is the result type.

grouped typed-argument syntax is allowed for ordinary definitions and type-class defaults.

class constraints for a definition are written with leading `given`.

```jbini
given a equal let (x: a, y: a) same: 𝟚 = x ≡ y;
```

the grouped typed-argument form makes this:

```jbini
let (x: integer, y: integer) add: integer = x + y;
```

mean a curried function:

```jbini
integer → integer → integer
```

polymorphic typed arguments may include effects.

effect annotations after the result in grouped typed-argument syntax belong to the function being defined, not to a stored non-function value.

```jbini
let (f0: a → b ! e0, f1: b → c ! e1) compose: a → c ! e0, e1 =
  { x | (x f0) f1 };
```

second-is-function headers also work with named arguments:

```jbini
let value default-option default: a option → a → a = match value {
  none | default,
  x some | x
}
```

## type aliases

type aliases use `type`.

```jbini
type string = character list;
```

`string` is also provided as a primitive name in the implementation surface, and the standard library defines it as `character list`.

keep parser and type checker behavior simple around aliases. do not add type alias complexity unless needed.

## algebraic data

data declarations use `data`.

data type parameters are implicit names before the data type name.

constructors use the same second-is-function sequence rule.

multi-argument type and constructor operators use the same sequence rule as expressions.

```jbini
data a option {
  none,
  a some
}
```

```jbini
data a ∏ b {
  a ∏ b
}

data a ∐ b {
  a first,
  b other
}
```

these examples mean:

- `option` has one type parameter.
- `none` is a nullary constructor.
- `some` takes one value.
- `∏` is both the type name and a constructor name.
- `∐` is a type name with constructors `first` and `other`.

type application follows second-is-function syntax too.

```jbini
let pair-value: integer ∏ string = 1 ∏ 'one';
```

constructor application is curried.

nullary constructors behave like values.

## effects

effect declarations use `effect`.

effect operation types must be function types.

the latent effect is added by the declaration itself.

written latent effects are kept, so operations may mention effects besides their own effect.

```jbini
effect console {
  string write: 𝟙,
  𝟙 read: string
}
```

parameterized effects are allowed.

`state` is parameterized.

```jbini
effect a state {
  𝟙 get: a,
  a set: 𝟙
}
```

`fail` is extensible and parameterized by the error type.

`fail` operations carry an error value and have a polymorphic answer type.

```jbini
effect e fail {
  e fail: a
}
```

`async` is a standard effect.

`async` uses `task`.

```jbini
data a task {
  a done
}

effect async {
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

```jbini
effect file {
  string read-file: string ! string fail,
  string write-file string: 𝟙 ! string fail,
  string append-file string: 𝟙 ! string fail
}
```

## type classes

the language keyword for type classes is `bone`.

the language keyword for instances is `flesh`.

use current language terms consistently.

`bone` declarations may have requirements introduced by leading `given`.

`given` requirements use comma separation.

members inside `bone` use semicolon separators.

the last member may omit its semicolon before `}`.

required members are bare signatures.

do not put `let` before a required member signature.

default members use the same `let` syntax as ordinary functions.

default implementations are allowed.

a default implementation is checked like an ordinary member.

a default implementation is visible by the same bare or qualified name.

when a default uses grouped typed arguments, the annotation after the member name is the result type.

```jbini
bone a equal {
  a ≡ a: 𝟚;
  let a ≢ b = (a ≡ b) ¬
}
```

```jbini
given a equal bone a order {
  a ≤ a: 𝟚
}
```

```jbini
given a add bone a subtract {
  a - a: a
}
```

`flesh` implements a `bone`.

`flesh` headers use second-is-function syntax.

`flesh` declarations may have requirements introduced by leading `given`.

these requirements are the instance context.

when a matching instance solves a constraint, the solver also checks or emits that instance context.

members inside `flesh` use the same `let` syntax as ordinary functions.

members inside `flesh` use semicolon separators.

the final semicolon before `}` may be omitted.

```jbini
flesh 𝟚 equal {
  let a ≡ b = match a {
    yea | b,
    nay | b ¬
  }
}
```

```jbini
given a equal flesh (a box) equal {
  let (x box) ≡ (y box) = x ≡ y
}
```

the type checker should allow a `flesh` of a more specific `bone` to define members required by parent bones when that simplifies implementation and matches the current model.

type-class member names may be bare if unambiguous.

type-class member names may be qualified if needed.

do not force qualification for unique operators.

if an operator name is unique, bare use should compile.

only require qualification when the name is ambiguous.

## expressions

integer literals are decimal.

float literals contain a decimal point.

character literals start with backtick.

character escapes are `\n`, `\r`, `\t`, `\'`, `\\`, and decimal unicode `\{123}`.

unicode codepoint character literals may also use backtick plus braces.

strings use single quotes.

string escapes are `\n`, `\r`, `\t`, `\'`, `\\`, and decimal unicode `\{123}`.

```jbini
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

```jbini
{ x | x }
{ x, y | x + y }
{ x, y, z | (x + y) + z }
```

blocks are parenthesized.

blocks contain local `let` declarations plus a final expression.

local block `let` declarations require semicolons.

the final expression in a block may be followed by an optional semicolon before `)`.

```jbini
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

```jbini
let chosen = match yea, nay {
  yea, b | b,
  nay, _ | nay
};
```

anonymous functions do not use `match`.

bare `{ ... }` is a function.

when a bare brace function case has multiple patterns, the result is a curried function with one argument per pattern.

```jbini
let not: 𝟚 → 𝟚 = {
  yea | nay,
  nay | yea
};
```

this bare brace form is recognized as a function over its pattern inputs.

match cases use `|`.

the consuming form is `match term { ... }`.

the function form is `{ ... }`.

the type checker rejects non-exhaustive matches over algebraic data at compile time.

coverage checking must include multi-scrutinee matches.

coverage checking must include nested constructor patterns.

an empty match is allowed when the scrutinee type is uninhabited.

the empty anonymous function `{}` is allowed when its annotated input type is uninhabited.

this is the eliminator for `𝟘`.

```jbini
let initial: 𝟘 → a = {};
```

an unannotated empty anonymous match `{}` is not allowed, because it has no pattern arity.

variable patterns are catch-all patterns.

`_` patterns are catch-all patterns.

the evaluator may still have a runtime non-exhaustive error for impossible or unchecked situations, but algebraic data coverage should be caught during type checking.

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

```jbini
let person = [name = 'naoki', age = 35];
let older = [= person, age = 36];
let public = [= person, - age];
```

closed records mean field sets must match when an annotated record type is checked.

removing an unknown field is an error.

updating a non-record is an error.

## handlers

effects are invoked as ordinary calls.

effects are handled with `try`.

the keyword is `try`, not `perform`.

handlers use `try body { cases }`.

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

when an effect and one of its operations share a name, patterns choose the operation form.

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

```jbini
effect choice {
  𝟙 choose: integer
}

let branched: integer =
  try (null choose) + (null choose) {
    choose | (1 resume) + (2 resume)
  };
```

```jbini
effect ask {
  integer ask: integer
}

let answered: integer =
  try 10 ask {
    x ask | x resume
  };
```

```jbini
let answered-text: string =
  try 10 ask {
    return n | n to-string,
    x ask | x resume
  };
```

effect-level handlers may not bind operation arguments.

a handler for an absent effect is a type error.

handler cases remove the handled effect.

effects used by the return case and handler bodies are added to the surrounding computation.

`resume` must be type checked.

the argument passed to `resume` must match the operation result type.

## async sample semantics

`async` is a standard effect.

the default runner interprets `async` synchronously.

this is still modeled as an effect, because it is a runner capability.

```jbini
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

```jbini
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

standard-library type classes may also expose the same symbols generically.

arithmetic operations are defined as type-class members in the library, but the runner also supports them natively for `integer` and `float` where appropriate.

native numeric surface:

```jbini
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

native runner surface:

```jbini
write: string → 𝟙 ! console
read: 𝟙 → string ! console
sleep: integer → 𝟙 ! async
random: 𝟙 → float ! random
read-file: string → string ! file, string fail
write-file: string → string → 𝟙 ! file, string fail
append-file: string → string → 𝟙 ! file, string fail
```

standard ground helper:

```jbini
write-line: string → 𝟙 ! console
```

foreign bindings may be declared in source with `class foreign`.

```jbini
class foreign {
  integer add-integer integer: integer;
  integer divide-integer integer: integer ! string fail
}
```

this is not a type class.

it declares host-provided functions with ordinary jbini types.

declared names enter the same name environment as `let` names.

source libraries can use those names inside `flesh` to provide class methods.

`from-string` fails with `string fail`.

division by zero fails with `string fail`.

`sleep` is an `async` effect in the current design.

## current standard library shape

the built-in public import path `ground.jbini` points at `library/ground.jbini`.

`foreign.jbini` defines primitive runtime-backed effects and task values.

`ground.jbini` re-exports those effects and task values.

core data is split across:

- `𝟘`
- `𝟙`
- `𝟚`
- product `∏`
- sum `∐`
- `option`
- `list`
- `task`

`𝟘` exports `initial: 𝟘 → a`.

core effects in `foreign.jbini`, re-exported by `ground.jbini`, include:

- `fail`
- `console`
- `random`
- `state`
- `async`
- `file`

core type classes are split across library files:

- `equal`
- `order`
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

library files:

- `ground.jbini`
- `_foreign.jbini`
- `data/zero.jbini`
- `data/one.jbini`
- `data/two.jbini`
- `data/product.jbini`
- `data/sum.jbini`
- `equal.jbini`
- `order.jbini`
- `algebra/*.jbini`
- `collection/*.jbini`
- `combinator.jbini`
- `numeric/constant.jbini`
- `data/natural.jbini`
- `list.jbini`
- `string.jbini`
- `string/*.jbini`
- `data/option.jbini`
- `numeric/absolute.jbini`
- `numeric/complex.jbini`
- `numeric/elementary.jbini`
- `numeric/integer.jbini`

when changing library syntax, check every file in `library/`.

when changing semantics, check every file in `sample/`.

## test expectations

tests should cover success cases and failure cases.

parser tests should cover current syntax and meaningful failure cases.

type tests should cover inference, annotations, polymorphism, effects, handlers, runnable-file boundaries, records, imports, and match coverage.

evaluation tests should check exact results when deterministic.

evaluation tests for errors should check error prefixes when exact wording is not important.

name resolution tests should check bare unique names, ambiguous names, qualified names, and missing qualified names.

library and sample tests should keep all bundled `.jbini` files valid.

add tests before or with bug fixes when a bug is discovered.
