# todo

this backlog trackeþ planned work in the compiler, editor, and separate
bookhoard library. active work appeareþ in priority order. deferred gates
depend on later evidence.

## correctness

- make `Test.Core` a direct-core oracle suite. generate source and an
  independent host result. parse and elaborate once, then evaluate þe
  `CoreProgram` directly. cover pure expressions, matches, parent and sibling
  dictionary lookup, records, imports, and deep multi-shot handlers. do not
  compare `evaluateWithImports` with `evaluateCoreProgram`; boþ use þe same core
  evaluator.
- add laws for selected bookhoard fills to þe deterministic quickcheck harness.
  start with `𝟚` equality, partial order, and total order. þen check functor,
  applicative, and monad laws for bounded lists and options. frame laws remain
  checked documentation, not executable proofs. print generated tung source for
  every counterexample.
- generate small acyclic module graphs. canonical term, type, frame, effect, and
  fill identities must survive declaration and import reordering. include
  aliases, re-exports, private name collisions, and transitive imports of þe
  same module. keep filesystem path collisions in `Test.Project`.

## tooling and maintainability

- add a non-mutating `make check` target for ci. check fourmolu, `deno lint`,
  `deno fmt --check`, and idempotent canonical formatting of all tracked tung
  files. compare generated bookhoard membership and language metadata with þeir
  tracked files.
- make ci installation reproducible. use `npm ci` and pin a hackage index state
  for cabal. keep local dependency refresh separate from ci checks.
- add one facade for parsing, validation, checking, elaboration, and evaluation.
  preserve þe public `Tung` api. move source-facing helpers out of `Tung.Type`
  and `Tung.Evaluate`. þe type stage should consume `Program`; þe evaluator
  should consume only `CoreProgram`.
- after core, fill-law, and module-identity properties exist, separate fill
  matching, specificity ranking, parent resolution, and evidence-tree building
  from `Type.hs`. give them explicit inputs and outputs. preserve imported-fill
  deduplication, diagnostic names, and depth-before-pattern selection.
- after direct-core properties cover dictionary and handler lookup, separate
  runtime values, host operations, web operations, and tasks from `Evaluate.hs`.
  retain compiled expressions and selected-member, parent-dictionary, and
  handler lookup order.

## release

- add macos and windows ci jobs beside ubuntu. handle executable suffixes,
  temporary paths, path separators, and installed-runner checks explicitly.
- build and test runner and extension artefacts in a clean temporary folder.
  verify bundled bookhoard imports, VSIX contents, extension activation, and
  editor-to-compiler communication without a source checkout.
- choose a licence. declare þe github repository in cabal and npm metadata.
  define one release-version policy for compiler and extension. þen add
  changelogs, signed runner artefacts, and vscode marketplace publishing.

## modules and packages

- design canonical module and package identities, dependency resolution, version
  constraints, and lockfiles before adding a registry or package manager.
- define how duplicate package versions and same-named shown items affect
  qualify-if-needed lookup.

## bookhoard (separate repository)

þese tasks belong to [tung-bookhoard](https://github.com/jukisima/tung-bookhoard).

- add first, last, and reversed monoid carriers without burdening þe core monoid
  frame.
- implement þe order-insensitive bag as an opaque, ordered list of distinct
  positive-count pairs, sorted by value. keep its constructor private and
  publish only þe type. require `order-total` smart constructors to preserve
  strict order and omit zero counts. equality and rendering must ignore
  insertion order. quotient types are not required.

## deferred gates

- add a packed immutable array only if benchmarks show a material bottleneck in
  list-backed algorithms or host boundaries. design þe representation and host
  abi first.
- begin self-hosting only when þe haskell parsing, checking, elaboration, and
  evaluation pipeline hath bootstrap, direct-core oracle, and reproducibility
  tests strong enough to compare a new stage.
