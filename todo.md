# todo

this is the project backlog. read it only when planning work or answering a
backlog question; ordinary changes should follow the request and nearby source.

## correctness

- attach an owning source path to every diagnostic from a brought file so the
  cli and editor can open the exact file and range.
- add generated and property-based checks for parser boundaries, inference,
  evidence coherence, coverage, handler resumption, and formatter idempotence.
- decide whether shape laws remain checked documentation, become executable
  property tests, or gain proof support; record the boundary in `agent.md`.

## speed and scale

- elaborate each module graph once and share checked core between checking and
  evaluation instead of parsing and checking brought modules again at runtime.
- keep a persistent haskell checker session behind the language server, with
  cancellation and versioned requests, instead of starting one process for each
  diagnostic or hover request.
- add repeatable compile-time, evaluation-time, allocation, and peak-memory
  benchmarks for large generated programs and representative byspels.
- track bookhoard directory membership during template-haskell embedding so
  adding or removing a module always invalidates an incremental build.

## modules and packages

- design canonical module and package identities, dependency resolution,
  version constraints, and lockfiles before adding a registry or package
  manager.
- define how duplicate package versions and same-named shown items interact with
  qualify-if-needed lookup.

## bookhoard and writing

- sharpen the numeric hierarchy around euclidean integer division, field
  division, and the limits of ieee floating-point laws.
- decide whether an order-insensitive bag waiteth for quotient types or useth a
  different representation with an enforceable invariant.
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

- move deterministic metadata or documentation tools into tung only when the
  tung version is smaller and still checked against the haskell source of truth.
- keep parsing, checking, elaboration, and evaluation authoritative in haskell
  until a self-hosted stage hath bootstrap, differential, and reproducibility
  tests.
