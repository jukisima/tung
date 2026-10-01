# todo

this backlog trackeþ planned work in the compiler, editor, and bookhoard
library. active work appeareþ in priority order. deferred gates
depend on later evidence.

## correctness

- extend `Test.Core` beyond its generated integer and boolean cases. generate
  varied matches, records, parent and sibling dictionary lookup, imports, and
  deep multi-shot handlers wiþ independent host results. parse and elaborate
  once, then evaluate þe `CoreProgram` directly. do not compare
  `evaluateWithImports` wiþ `evaluateCoreProgram`; boþ use þe same core evaluator.
- add laws for selected bookhoard bizens to þe deterministic quickcheck harness.
  start with `𝟚` equality, partial order, and total order. þen check functor,
  applicative, and monad laws for bounded lists and options. flock laws remain
  checked documentation, not executable proofs. print generated tung source for
  every counterexample.
- generate small acyclic module graphs. canonical term, type, flock, effect, and
  bizen identities must survive declaration and import reordering. include
  aliases, re-exports, private name collisions, and transitive imports of þe
  same module. keep filesystem path collisions in `Test.Project`.

## tooling and maintainability

- add a non-mutating `make check` target for ci. check fourmolu and idempotent
  canonical formatting of all tracked tung files.
- pin a hackage index state for cabal in ci. keep local dependency refresh
  separate from ci checks.
- add one facade for parsing, validation, checking, elaboration, and evaluation.
  preserve þe public `Tung` api. move source-facing helpers out of `Tung.Type`
  and `Tung.Evaluate`. þe type stage should consume `Program`; þe evaluator
  should consume only `CoreProgram`.
- after core, bizen-law, and module-identity properties exist, separate bizen
  matching, specificity ranking, parent resolution, and evidence-tree building
  from `Type.hs`. give them explicit inputs and outputs. preserve imported-bizen
  deduplication, diagnostic names, and depth-before-pattern selection.
- after direct-core properties cover dictionary and handler lookup, separate
  runtime values, host operations, web operations, and tasks from `Evaluate.hs`.
  retain compiled expressions and selected-member, parent-dictionary, and
  handler lookup order.

## release

- add macos compiler and editor checks and windows ci jobs beside ubuntu.
  handle executable suffixes, temporary paths, and path separators explicitly.
- build and test runner and extension artefacts from a clean checkout. check
  that þe installed runner resolveþ a git-pinned Bookhoard through `tung.yaml`
  and that þe packaged extension connecteþ to þe installed compiler.
- after publishing þe monorepo, update `Formula/tung.rb` to a commit wiþ þe
  manifest-based library resolver. test þe installed formula wiþ an external
  Bookhoard pin.
- choose a licence. declare þe github repository in cabal metadata. define a
  compatible release-version policy wiþ `vscode/`. þen add changelogs and
  signed runner artefacts.

## modules and packages

- define package identities, version constraints, and lockfiles before adding a
  registry or package manager. keep þe current exact-commit and local-path
  conflict rules until a version resolver hath explicit rules for transitive
  requirements and incompatible duplicates.
- define how same-named shown items from different packages affect
  qualify-if-needed lookup.

## bookhoard

þese tasks belong to [`bookhoard/`](bookhoard/).

- add first, last, and reversed monoid carriers without burdening þe core monoid
  flock.
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
