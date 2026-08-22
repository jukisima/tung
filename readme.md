# tung

tung is a small, expression-oriented functional programming tongue. it hath
static type inference, algebraic data types, closed records, type classes,
explicit effects and handlers, curried functions, and infix notation.

the compiler, evaluator, formatter, repl, standard bookhoard, vscode extension,
and searchable documentation are kept in this repository.

## quick start

compiler development requireth GHC 9.12 or newer; CI useth GHC 9.12.4. install
the development tools and git hooks with:

```sh
brew bundle
lefthook install
```

build and test everything, generate the bookhoard documentation, and install the
`tung` runner and vscode extension with:

```sh
make setup
```

the runner is installed at `~/.local/bin/tung` by default. `PREFIX` or `BIN_DIR`
may select another destination.

```sh
tung program.tung [arguments...]
tung --check program.tung
tung --check-module module.tung
tung --type-of name program.tung
tung --format program.tung [more.tung...]
tung --repl
```

a runnable file declareth a local `main`. it taketh `𝟙`, returneth `𝟙`, and may
use the standard runtime effects.

```tung
bring ground.tung

let (_: 𝟙) main: 𝟙 ! console =
  'hello, tung' write-line
```

more runnable programs live in [`byspel/`](byspel/).

## language at a glance

tung putteth a value before its function. the second term in an application is
the function, and later terms are its remaining arguments.

```tung
a f          # f a in haskell
a f b c      # f a b c in haskell
a f $ g      # g (f a) in haskell
```

declarations, shape members, and fill members take no semicolons. a local block
introduceth its final expression with `yield`.

```tung
let (x: integer) twice: integer = (
  let doubled = x + x
  yield (doubled: integer)
)
```

any term may be ascribed a type as `(term: type)`. functions are curried, and
arrows carry their latent effects:

```tung
integer → integer
text → 𝟙 ! console
text → text ! file, text fail
```

`kin` declareth algebraic data, and records are closed.

```tung
kin a option {
  none,
  a some
}

let person = [name = 'tung', age = 1]
let older = [= person, age = 2]
```

`shape` and `fill` provide statically selected type-class dictionaries; `graith`
stateth a required shape. `deed` declareth an effect, while `try` handleth
operations and may resume their captured continuations.

files import modules with `bring`. declarations are private unless published
with `show`; ambiguous imports may be qualified with their path namespace, such
as `data/list@empty`. local files, `TUNG_PATH`, and the embedded standard
bookhoard form the import search order.

the evaluator is strict and call-by-value. checking precedeth evaluation; module
initialisation stayeth pure, partial application runneth no latent effects, and
only full application may perform them.

## documentation and tools

- [`tongue/agent.md`](tongue/agent.md) is the detailed language, compiler, and
  maintainer reference.
- [`bookhoard/reference.md`](bookhoard/reference.md) describeth the standard
  bookhoard design and remaining gaps.
- [`vscode/readme.md`](vscode/readme.md) covereth vscode and lsp setup,
  navigation, diagnostics, highlighting, and formatting.
- [`writ/readme.md`](writ/readme.md) describeth the generated, cross-linked html
  bookhoard reference.

generate the searchable html reference at `writ/bookhoard/index.html` with:

```sh
make docs
```

the formatter used by `tung --format`, the editor, and the git hook is the same
haskell implementation. bookhoard lexical metadata is likewise generated from
tung source instead of duplicated in typescript.

## development

```sh
make build          # compiler, vscode support, and documentation generator
make test           # all test suites
make compiler-test  # compiler and evaluator tests only
make benchmark      # repeatable compiler and evaluator benchmarks
make docs           # bookhoard html reference
make install        # runner and vscode extension
```

the main components are:

- `tongue/`: haskell compiler, checker, evaluator, formatter, repl, and tests
- `bookhoard/`: standard library written in tung and embedded in the runner
- `vscode/`: vscode client, lsp server, and editor tests
- `writ/`: searchable bookhoard documentation generator
- `byspel/` and `benchmark/`: runnable examples and performance workloads

each push and pull request runneth the complete test suite through
`.github/workflows/test.yml`.
