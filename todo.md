# todo

this is the project backlog. read it only when planning work or answering a
backlog question; ordinary changes should follow the request and nearby source.

## correctness

- stabilise the concurrent-fork overlap test and determine why two sleeping
  tasks sometimes take their combined sequential duration despite two runtime
  task slots.
- add host-side property tests for selected lawful fills while shape laws remain
  checked documentation rather than executable proofs.

## modules and packages

- design canonical module and package identities, dependency resolution,
  version constraints, and lockfiles before adding a registry or package
  manager.
- define how duplicate package versions and same-named shown items interact with
  qualify-if-needed lookup.

## bookhoard and writing

- add first, last, and reversed monoid carriers without burdening the core
  monoid shape.
- add a packed immutable array when runtime representation work is justified.
- give each public module at least one useful doc comment and one realistic
  byspel when its use is not clear from a signature alone.
- add generated cross-links between types, shapes, functions, fills, and source
  modules to the html bookhoard.

## release

- add macos and windows ci jobs alongside ubuntu.
- test installed runner and extension artefacts from a clean temporary folder,
  including bundled bookhoard imports and editor-to-compiler communication.
- choose a licence and repository home, then add changelogs, release versioning,
  signed runner artefacts, and vscode marketplace publishing.

## self-hosting

- keep parsing, checking, elaboration, and evaluation authoritative in haskell
  until a self-hosted stage hath bootstrap, differential, and reproducibility
  tests.
