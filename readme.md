# tung

tung is a functional programming tongue with

- static typing
- algebraic data types
- type classes
- effects and handlers
- infix notation

each push and pull request runneth the full test suite through
`.github/workflows/test.yml`.

## quick start

```sh
cd tongue

cabal build all

cabal test all

cabal run tung -- ../byspel/fizzbuzz.tung [arguments...]
```

vscode and lsp support liveth in `vscode/`. it provideth compiler diagnostics, inferred hover types, semantic highlighting, completion, navigation, symbols, rename, import links, folding, and formatting.

build and test everything, generate the documentation, and install the runner and vscode extension with:

```sh
make setup
```

setup installeth the standalone runner at `~/.local/bin/tung`. run a source file
from any directory with:

```sh
tung filename.tung
```

the runner also checketh files without running them, inspecteth inferred term types,
and starteth an interactive session:

```sh
tung --check filename.tung
tung --check-module module.tung
tung --type-of main filename.tung
tung --repl
```

the repl keepeth declarations after they pass checking. ordinary expressions are
evaluated without being kept. use `:type`, `:load`, `:check`, `:run`, `:clear`,
and `:quit`; wrap multiline source between `:{` and `:}`.

use `make install` to reinstall without running every test. set `PREFIX` or
`BIN_DIR` to choose another installation directory:

```sh
make install PREFIX=/usr/local
```

to regenerate only the searchable standard-bookhoard html wiki:

```sh
make docs
```

run the naive tree-recursive fibonacci benchmark, which prints fibonacci 40:

```sh
time make benchmark
```

install the development tools and git hooks with:

```sh
brew bundle
lefthook install
```

a file run by the cli must declare `main` locally. `main` taketh `𝟙`, returneth `𝟙`, and may use `console`, `random`, `async`, `file`, `system`, `clock`, or `process`. user-defined effects and `fail` must be handled inside `main`.

## small byspel

```tung
bring ground.tung;
bring data/list.tung;

kin fizzbuzz {
  fizz,
  buzz,
  fizzbuzz,
  integer raw
}

fill fizzbuzz to-text {
  let to-text = {
    fizz | 'fizz',
    buzz | 'buzz',
    fizzbuzz | 'fizzbuzz',
    i raw | i to-text
  }
}

let (i: integer) fizzbuzz-of: fizzbuzz =
  match 3 can-divide i, 5 can-divide i {
    yea, yea | fizzbuzz,
    yea, nay | fizz,
    nay, yea | buzz,
    nay, nay | i raw
  };

let main: 𝟙 → 𝟙 ! console = { _ |
  0 till 32 $ map fizzbuzz-of $ map to-text $ each write-line
}
```

## files and imports

a file is a module containing declarations and an optional final expression. the cli runneth it only when it declareth a local `main` function.

the entry type is:

```tung
𝟙 → 𝟙 ! console, random, async, file, system, clock, process
```

`main` may use any subset of those effects, including none. module initialisation must stay pure. after loading the declarations, the runner calleth `null main`.

`bring` imports another file.

```tung
bring ground.tung;
bring data/list.tung;
```

imports must form an acyclic graph. the checker rejecteth both direct and indirect bring cycles.

the runner resolveth local brings relative to the file containing them and loadeth
their nested brings before checking. an existing local file taketh precedence
over an embedded bookhoard path. if the same written bring path resolveth to two
different local files, loading faileth instead of silently choosing one.

`TUNG_PATH` addeth platform-separated module roots after owner-relative lookup and
before the embedded bookhoard fallback. for example:

```sh
TUNG_PATH=../shared:../vendor tung app/main.tung
```

if more than one module root containeth the same written bring path, loading faileth
and listeth the competing files.

shown names are imported as `namespace@name`, where the namespace is the full import path before `.tung`.

```tung
ground@yea
data/list@empty
```

an imported name may also be used bare when it is unambiguous. if two imported files expose the same bare name, the checker asketh for qualification. private names are unavailable both bare and qualified.

tung hath two namespaces. values, functions, data constructors, and effect operations are terms. primitive types, aliases, data types, and effect constructors are types. both namespaces are private by default. put `show` before a declaration to publish it.

```tung
show let answer = 42;
graith a equal show let (x: a, y: a) same: 𝟚 = x ≡ y;
show let-ilk count = integer;
show kin 𝟙 { null }
show deed ask { integer ask: integer }
show shape a identity { a identity: a }
graith a equal show shape a order-partial { a ≤ a: 𝟚 }
```

for a declaration with `graith`, put `show` after the requirements and directly before `let` or `shape`.

showing a `kin` declaration showeth its type and constructors. showing a `deed` declaration showeth its effect type and operations. showing a shape showeth its methods. fill evidence followeth imports without `show`.

`show name;` re-exports an already visible term. `show-ilk name;` re-exports an already visible type, including an effect constructor. use a qualified name when imports make the bare name ambiguous within that namespace. data constructors and effect operations are terms and must be shown separately from their type.

```tung
bring data/one.tung;
show-ilk one@𝟙;
show one@null;
show one@terminal;
```

there is no wildcard re-export. re-export each term or type with `show name;` or `show-ilk name;`.

## comments and names

```tung
# this is a comment

/* this is a block comment */

## this doc comment attacheth to the next shown declaration.
show let documented = 1;
```

most non-space characters can be part of a name. syntax characters such as `(`, `)`, `{`, `}`, `[`, `]`, `,`, `:`, `;`, `=`, `!`, `→`, `|`, `$`, `.`, `'`, and `\`` are not ordinary name characters.

`##` line comments and `/** ... */` block comments attach documentation to the next declaration. the language server showeth attached docs in hover, and `npm --prefix writ run docs` buildeth a searchable html wiki at `writ/bookhoard/index.html`.

`@` is used inside qualified names.

`_` discardeth a binding or matcheth anything in a pattern.

## primitive values and types

the core implementation knows these primitive value types:

- `integer`
- `float`
- `unicode`
- `text`

the standard bookhoard defineth these common data types:

- `𝟘`: empty type, with eliminator `initial`
- `𝟙`: unit type, with value `null`
- `𝟚`: boolean type, with values `yea` and `nay`
- `three`: comparison result, with values `fore`, `mid`, and `aft`
- `a option`: `none` or `a some`
- `a list`: `empty` or `a .* rest`
- `a nonempty`: list with a statically known first element
- `k table v`: list-backed finite association table
- `a powerset`: membership predicate, transparent with `a func 𝟚`
- `a set`: list-backed finite set
- `path`: file path kept distinct from arbitrary text
- `time-span`: duration measured in milliseconds
- `process-result`: child-process status, output, and error text
- `a task`: abstract, reusable handle to an asynchronous result

literals.

```tung
# integer
42

# float
6.283

# unicode code point
`a
`\n
`\r
`\t
`\'
`\\
`\{23383} # 字
`{23383} # 字

# text
'hello'
'\n\r\t\'\\\{23383}'
```

`text` is an opaque, strict unicode value backed by haskell `text`.

`integer` is arbitrary precision. `float` is an ieee 754 binary64 value.

the host bindings live in `bookhoard/_foreign.tung`; `text/structure.tung`
addeth fills over them. `join-text` is the binary joining primitive.
`text-to-list` changeth
`text` into `unicode list`; `list-to-text` buildeth `text` from `unicode list`;
`behead-text` exposeth the first code point without traversing the whole value;
and `fold-join-text` joineth a text list in one native pass. text and unicode have
equality and ordering fills, while text also hath semigroup and monoid fills for
`*` and `∅`. each `unicode` value holds exactly one unicode scalar code point.

```tung
unicode-to-integer: unicode → integer
integer-to-unicode: integer → unicode ! text fail
```

`integer-to-unicode` faileth when its input is negative, a surrogate, or greater
than `1114111`.

## application

a sequence of terms separated by space is an application with the second being the function.

```tung
a f       # f a in haskell
a f b c   # f a b c in haskell
```

the tongue curries all functions.

```tung
let add = { x, y | x + y };
let one-add = 1 add;
2 one-add # 3
```

`$` is a pipeline syntax.

```tung
a f $h b g
# (a f) h b g

a f $(g b c) $h d
# ((a f) g b c) h d
```

## evaluation

tung is strict call-by-value.

- declarations run in order
- `let` evaluateth the right-hand side before binding
- a bare brace function is a value; its body waiteth until the function is fully applied
- application evaluateth the function first, then arguments left-to-right
- partial application is pure and doth not run latent effects
- only full application can run native work or perform an effect
- `match` evaluateth scrutinees before choosing a case, then runneth only the chosen body
- `try` evaluateth its body first; handled operation arguments are already values
- `resume` continueth the captured strict computation
- after loading a runnable module, the cli evaluateth `null main`

all evaluation is checked first. module checking forbiddeth immediate top-level effects, runnable checking verifieth `main`, and the interactive evaluator permitteth a checked top-level computation. rejected source is never passed to the runtime.

## declarations

```tung
let answer: integer = 42;
let name = 'tung';
```

a host function useth `foreign` as the direct body of an annotated file-level
let. the compiler checketh its name and full type against the host registry.

```tung
show let join-text: text → text → text = foreign;
```

definitions can use the same sequence rule as applications.

```tung
let x add y = x + y;
let x add-typed y: integer → integer → integer = x + y;
```

or thou canst annotate each argument with a type.

```tung
let (x: integer, y: integer) add: integer = x + y;
```

definitions can require type-class.

```tung
graith a equal let (x: a, y: a) same: 𝟚 = x ≡ y;
```

## functions and match

function literal.

```tung
let id = { x | x };

let if = {
  yea, then, _ | then,
  nay, _, else | else
};
```

`match` consumeth one or more scrutinees.

```tung
match x {
  none | 0,
  n some | n
}

match a, b {
  nay, nay | 0,
  nay, yea | 1,
  yea, nay | 2,
  yea, yea | 3
}
```

cases must have the same number of patterns and be exhaustive.

a case wholly covered by earlier cases is rejected as redundant.

integer literals are exact patterns in consuming matches and bare-brace functions.

```tung
match n {
  0 | 'zero',
  _ | 'nonzero'
}

let sign = {
  -1 | 'negative one',
  0 | 'zero',
  _ | 'other'
};
```

an integer hath infinitely many values, so finitely many literal cases still need a catch-all case.

an empty match is allowed only when the scrutinee type hath no values. the empty anonymous function `{}` is allowed when its annotated input type is empty.

```tung
let initial: 𝟘 → a = {};
```

## algebraic data types

```tung
kin a option {
  none,
  a some
}

kin a ∏ b {
  a ∏ b
}

let pair: integer ∏ text = 0 ∏ 'zero';
```

## records

records are closed.

```tung
let person: [name: text, age: integer] =
  [name = 'john', age = 32];

let older = [= person, age = 33];

let age = person@age;

let public = [= person, - age];
```

## types

```tung
integer → integer
integer → integer → integer
(integer func integer) func integer
```

```tung
text → 𝟙 ! console
integer → integer ! text fail
```

```tung
𝟙 → text ! console, random
```

effect annotations belong to function arrows. partial application is pure; only full application can run latent effects.

```tung
text → text ! file, text fail
```

```tung
let-ilk count = integer;
let-ilk a powerset = a func 𝟚;
```

## type classes

```tung
shape a equal {
  a ≡ a: 𝟚;
  let a ≢ b = (a ≡ b) ¬;

  law (x: a): x ≡ x ~ yea
}

fill 𝟚 equal {
  let ≡ = {
    yea, yea | yea,
    nay, nay | yea,
    _, _ | nay
  }
}

graith a equal
shape a order-partial {
  a ≤ a: 𝟚
}

graith a order-partial
shape a order-total {
  let (a: a, b: a) compare: three = ...
}

shape f traverse {
  graith m applicative (a f) traverse (a → b m): (b f) m
}

graith a semigroup
fill (a option) semigroup {
  let * = {
    none, b | b,
    a, none | a,
    a some, b some | a * b $ some
  }
}
```

shape requirements use static dictionary passing. the checker chooseth one fill from the inferred types and addeth hidden evidence arguments; the evaluator never guesseth a fill from runtime values or bring order. a child shape receiveth its required parent dictionaries, and a fill may supply a parent directly only when it defineth that parent's member. duplicate fills, overlapping applicable fills, missing fills, and recursive evidence are compile-time errors. this also letteth result-only members such as `zero` select their fill from an expected result type.

every graith on a `fill` must be needed by one of its members or by construction of a required parent dictionary. redundant fill graiths are compile-time errors.

`order-partial` owneth `≤`, deriveth `<`, and stateth the reflexive,
antisymmetric, and transitive laws. `order-total` addeth comparability and the
three-way `compare`; `clamp` therefore requireth `order-total`. algebraic
semilattices remain independent because some useful representations can
compute infima and suprema without deciding their induced order.
`complement` requireth a bounded lattice and owneth `¬`; the function fill lifteth
complements pointwise, including powersets represented as predicates.

a `law` belongeth to its shape but is not a method that fills must implement. its
parameters are local typed names. the checker requireth both sides of `~` to
have the same value type, immediate effects, and only requirements supplied by
the shape or its parents. laws are erased before evaluation; tung doth not yet
prove that their two sides are equal.

## effects and handlers

```tung
deed e fail {
  e fail: a
}

deed console {
  text write: 𝟙,
  𝟙 read: text
}
```

every operation is a function with at least one input. an operation with no information to receive taketh `𝟙` and is called through ordinary second-is-function syntax, such as `null read`. partial application stayeth pure, and only full application performeth the deed. the deed declaration addeth its effect. written latent effects are kept.

```tung
text → float ! text fail
```

```tung
try 1 ÷ 0 {
  _ fail | 0
}
```

operation handlers may use the implicit `resume` function. `resume` is multi-shot, so it may be called more than once.

```tung
deed choice {
  𝟙 pick: integer
}

try (null pick) + (null pick) {
  pick | (1 resume) + (2 resume)
}
```

a handler may include one `return` case. it handleth the normal result and may change the answer type of the whole `try`.

```tung
try 2 + 3 {
  return n | n to-text
}
```

without a `return` case, the normal result is returned unchanged.

## standard effects

the standard bookhoard declareth these runtime-backed effects:

- `console`: `write`, `read`
- `random`: `random`
- `async`: `fork`, `wait`, `wait-for`, `fordo`, `sleep`
- `file`: `read-file`, `write-file`, `append-file`
- `system`: `arguments`, `environment`, `exit`
- `clock`: `unix-time`
- `process`: `run-process`
- `fail`: extensible failure with an error payload
- `state`: parameterised state effect

`ground.tung` re-exports those effects and defineth `write-line`, which writeth the text and then a newline.

the runtime currently provideth native operations named:

- `write`
- `read`
- `random`
- `fork`
- `wait`
- `wait-for`
- `fordo`
- `sleep`
- `read-file`
- `write-file`
- `append-file`
- `arguments`
- `environment`
- `exit`
- `unix-time`
- `run-process`

`fork` scheduleth suspended work in a bounded pool. when the pool is full, the caller runneth the new work itself, which keepeth nested forks from deadlocking while limiting background threads. `wait` blocketh until that work finisheth and may be called repeatedly on the same abstract task. `wait-for` returneth `none` after its millisecond timeout, while `fordo` stoppeth unfinished work. cancellation and host-thread failure become `text fail` when a task is observed.

all unfinished tasks are cancelled and joined when file evaluation endeth. `sleep` blocketh only the thread that performeth it, and independent sleeping tasks overlap.

file operations read and write whole text files. file-system errors perform `text fail`, so callers must handle failure before a runnable file endeth.

`arguments` returneth only the arguments after the source-file path. `environment` returneth `none` when the named host setting is absent. `exit` endeth the process with the given status. `unix-time` returneth whole seconds since `1970-01-01 00:00:00 utc`.

`run-process` taketh command text and a text list of arguments. it captureth the exit status, standard output, and standard error in `process-result`; host launch failures perform `text fail`.

the wider bookhoard also provideth list-backed tables, finite sets, and non-empty lists; powersets; unicode-aware text operations; typed path wrappers; time spans; and process-result accessors. both finite collection modules show a constructor named `from-list`; bring both and qualify the intended one as `data/table@from-list` or `data/set@from-list`. these are ordinary tung modules and remain private until brought and shown through their source files.

## project files

- `tongue/tung.cabal`: haskell compiler package setup
- `tongue/app/Main.hs`: command-line runner
- `tongue/src/Tung.hs`: public haskell api
- `tongue/src/Tung/Syntax.hs`: syntax trees
- `tongue/src/Tung/Token.hs`: tokens, keywords, and source lexing
- `tongue/src/Tung/Parse.hs`: parser from tokens to syntax trees
- `tongue/src/Tung/Diagnostic.hs`: source spans and structured compiler diagnostics
- `tongue/src/Tung/Primitive.hs`: shared host-function names, types, effects, and arities
- `tongue/src/Tung/Validate.hs`: structural checks over parsed trees
- `tongue/src/Tung/Import.hs`: shared import-stack and cycle handling
- `tongue/src/Tung/Name.hs`: qualification, namespace, and field-access name helpers
- `tongue/src/Tung/Coverage.hs`: pure constructor-matrix match coverage
- `tongue/src/Tung/Embed.hs`: build-time embedding of the sibling bookhoard
- `tongue/src/Tung/Bookhoard.hs`: bundled bookhoard file list and loading helper
- `tongue/src/Tung/Project.hs`: local, search-path, and embedded bring resolution
- `tongue/src/Tung/Type.hs`: imports, name lookup, type inference, classes, effects, match checking, and runnable-file checks
- `tongue/src/Tung/Core.hs`: checked evaluator input and core validation
- `tongue/src/Tung/Evaluate.hs`: interpreter, runtime values, native operations, imports, and effect handlers
- `tongue/test/Main.hs`: compiler tests
- `tongue/tool/language-names.tung`: tung program that emitteth editor lexical metadata
- `bookhoard/`: compiler-independent standard bookhoard written in tung and embedded for distribution
- `byspel/`: runnable byspels
- `benchmark/`: runnable performance workloads
- `writ/docs.ts`: standard-bookhoard html wiki generator
- `tongue/agent.md`: maintainer notes and detailed tongue specification

`make language-metadata` runneth the tung metadata tool and updateth `vscode/generated/language-names.json`. the semantic highlighter consumeth that generated file, so its keywords and primitive type names follow the object-language source instead of a second typescript table.
