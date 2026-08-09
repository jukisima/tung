# jbini

*jbini* is a small infix programming language written in flix.

## what it has

- infix functions
- polymorphic types
- algebraic data types
- type classes
- records
- effects and handlers

## how to run

```sh
# check the compiler
flix check src/*.flix

# test the compiler
flix test

# run a jbini file:
flix src/*.flix -- path/to/file.jbini
```

the runner loads the bundled files in `library/` as import targets, checks that the file is a runnable program, then evaluates its final expression. a runnable file must return `𝟙`; only `console` and `random` effects may remain at the file boundary.

## folder map

- `library/`: standard library files.
- `sample/`: sample jbini files.
- `specification.md`: language specification.
- `src/`: flix code for the language.
  - [src/parse.flix](src/Parse.flix): abstract syntax trees, lexer, and parser for `.jbini` code.
  - [src/types.flix](src/Types.flix): type-checker data structures.
  - [src/semantics.flix](src/Semantics.flix): public type-checking API and import handling.
  - [src/nameresolution.flix](src/NameResolution.flix): import namespaces, name lookup, ambiguity checks, built-in names, and declaration scope gathering.
  - [src/typesystem.flix](src/TypeSystem.flix): type conversion, schemes, fresh variables, substitution, unification, effect-set checks, and type display.
  - [src/declarationchecking.flix](src/DeclarationChecking.flix): top-level and local declaration checks, let generalization, recursion, and type class instance checks.
  - [src/expressioninference.flix](src/ExpressionInference.flix): expression inference for literals, records, calls, blocks, lambdas, matches, and handlers.
  - [src/patternchecking.flix](src/PatternChecking.flix): pattern type checks and pattern-bound environment construction.
  - [src/evaluate.flix](src/Evaluate.flix): interpreter, runtime values, runtime imports, built-in operations, records, matches, and small effect handling.
  - [src/main.flix](src/Main.flix): command-line entry point for running a `.jbini` file.

- `test/`: flix tests.
