# jbini

*jbini* is a small infix programming language implemented in haskell.

## what it has

- sequence application with curried functions
- polymorphic types
- algebraic data types
- type classes
- closed records with update and field removal
- explicit effect annotations
- resumable, multi-shot handlers
- file imports with qualified names when needed

## commands

build the compiler and runner:

```sh
cabal build
```

run the test suite:

```sh
cabal test
```

run a jbini file:

```sh
cabal run jbini -- sample/fizzbuzz.jbini
```

or:

```sh
cabal run jbini -- path/to/file.jbini
```

the runner loads the bundled files in `library/` as import targets, checks that the file is runnable, then evaluates its final expression. a runnable file must return `𝟙`; only `console`, `random`, and `async` effects may remain at the file boundary.

## file map

- `jbini.cabal`: haskell package setup for the library, command-line runner, and tests.
- `app/Main.hs`: command-line entry point. it loads the bundled libraries, checks a `.jbini` file, and runs it.
- `src/Jbini.hs`: public module that re-exports the compiler, checker, and runner api.
- `src/Jbini/Syntax.hs`: abstract syntax trees and tokens.
- `src/Jbini/Parse.hs`: lexer and parser for `.jbini` source.
- `src/Jbini/TypeCheck.hs`: imports, name lookup, type inference, type-class checks, match coverage checks, effect checks, and runnable-file checks.
- `src/Jbini/Evaluate.hs`: interpreter, runtime values, native operations, imports, and effect handlers.
- `test/Main.hs`: haskell tests for parsing, typing, name lookup, evaluation, libraries, and samples.
- `library/`: standard library files written in jbini.
- `sample/`: runnable sample programs.
- `agent.md`: agent notes and the full language specification.
