# todo

this is the project backlog. read it only when planning work or answering a
backlog question; ordinary changes should follow the request and nearby source.
sections and items are ordered by priority. finish and verify one item before
moving to the next.

## correctness

- add a host-side property harness for selected finite lawful fills, beginning
  with `𝟚` equality and order and the list and option algebraic fills. shape
  laws remain checked documentation rather than executable proofs.
- add differential tests which elaborate generated checked programs once and
  require source evaluation and direct `CoreProgram` evaluation to agree for
  pure expressions, matches, dictionaries, records, and handlers.

## tooling and maintainability

- add a non-mutating repository-wide style check and run it in ci: fourmolu for
  haskell, deno lint and formatting for typescript, and canonical idempotent
  tung formatting for every bookhoard module.
- after the evaluator differential tests exist, separate runtime values, host
  operations, and task machinery from `Evaluate.hs` without removing compiled
  expressions or changing selected-member, default-member, and parent-dictionary
  lookup order.

## release

- add macos and windows ci jobs alongside ubuntu.
- test installed runner and extension artefacts from a clean temporary folder,
  including bundled bookhoard imports and editor-to-compiler communication.
- choose a licence and repository home, then add changelogs, release versioning,
  signed runner artefacts, and vscode marketplace publishing.

## modules and packages

- design canonical module and package identities, dependency resolution, version
  constraints, and lockfiles before adding a registry or package manager.
- define how duplicate package versions and same-named shown items interact with
  qualify-if-needed lookup.

## bookhoard and writing

- give each public module at least one useful doc comment and one realistic
  byspel when its use is not clear from a signature alone.
- add first, last, and reversed monoid carriers without burdening the core
  monoid shape.
- implement the designed order-insensitive bag as an opaque ordered list of
  distinct positive-count pairs, with `order-total` smart constructors which
  preserve its representation invariant.
- add a packed immutable array when runtime representation work is justified.

## self-hosting

- keep parsing, checking, elaboration, and evaluation authoritative in haskell
  until a self-hosted stage hath bootstrap, differential, and reproducibility
  tests.
