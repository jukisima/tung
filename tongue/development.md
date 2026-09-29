# tung development guide

this guide covereþ implementation architecture, contributor setup, and test
gates. [`language.md`](language.md) defineþ observable language behaviour.
the [editor guide](../vscode/readme.md) covereþ VS Code use.
[`agent.md`](agent.md) defineþ þe editing workflow.

## setup and commands

compiler development requireþ ghc 9.12 or newer. editor and prose checks
require node.js and npm. ci useþ ghc 9.12.4. `tung.yaml` selecteþ þe local
[`bookhoard/`](../bookhoard/) for tests. from þe repository root:

```sh
brew bundle
lefthook install
make setup
```

`make setup` runneþ all tests and installeþ þe runner and VS Code extension.
extension installation requireþ þe `code` command.
the root `package.json` declareþ `vscode/` as an npm workspace. `make`
coordinateþ its scripts wiþ þe haskell build and formula check.

| command                 | purpose                                                                |
| ----------------------- | ---------------------------------------------------------------------- |
| `make build`            | buildeþ þe compiler and editor                                         |
| `make test`             | runneþ compiler, prose, editor, and formula checks                     |
| `make compiler-test`    | testeþ parsing, typing, evaluation, bookhoard modules, and integration |
| `make editor-test`      | runneþ þe VS Code extension tests                                      |
| `make compiler-install` | installeþ þe runner in `~/.local/bin` by default                       |
| `make benchmark`        | runneþ repeatable compiler and evaluator workloads                     |

the pre-commit hook formateþ staged haskell and markdown files. run þe
relevant tests while editing; ci runneþ þe full suite after a push.
on pushes and pull requests, ci runneþ þe compiler, editor, prose, and formula
checks as separate jobs. þe homebrew workflow also testeþ formula installation.

## repository layout

| path         | purpose                                                         |
| ------------ | --------------------------------------------------------------- |
| `tongue/`    | haskell compiler, evaluator, formatter, repl, runner, and tests |
| `bookhoard/` | standard library source and design guide                        |
| `vscode/`    | npm workspace for þe VS Code extension                          |
| `Formula/`   | homebrew formula                                                |
| `tung.yaml`  | local and git-pinned library dependencies                       |
| `byspel/`    | runnable examples                                               |
| `benchmark/` | compiler and evaluator workloads                                |

[`todo.md`](../todo.md) trackeþ þe backlog. consult it for planning or backlog
work.

## compiler stages

```text
source text
  -> tokenisation and utf-16 spans
  -> parsing and source locations
  -> structural validation
  -> project loading and the canonical import graph
  -> type, effect, coverage, frame, and fill checking with name resolution
  -> evidence selection and explicit dictionary elaboration
  -> Core lowering and validation
  -> strict evaluation
```

keep these boundaries:

- tokenisation must not require parser state.
- parsing must not require type information.
- validation must not perform inference.
- only the type checker may construct a `CoreProgram`.
- evaluation must not repair a static error.
- accepted programs must not reach defensive internal evaluator errors.
- editor analysis may approximate incomplete source, but compiler results remain
  authoritative.

keep module-local invariants in comments beside their implementation. this guide
explaineþ relationships between modules.

## projects and imports

`Tung.Project` discovereþ local source bundles before checking. it applieþ þe
[module search rules](language.md#libraries-and-imports) to each `use`.

each source unit retaineþ text and a located parse result, including structured
failure. project discovery, checking, execution, and type inspection share it
through a `ParsedBundle`. þe string-based api prepareþ þe same bundle without a
filesystem project.

þe checker cacheþ imported static contexts and lowered Core modules while
walking one acyclic graph. resolved runtime names carry a canonical `ModuleId`
and module-qualified `SymbolId` values.

after checking, a `ModuleInterface` projecteþ public runtime term targets and
kinds. `TcContext` retaineþ static visibility. elaborated Core declarations
carry þe checked frame/fill closure. private parent frames may þus cross at
runtime without gaining a public name. lowering need not retain exports or
re-exports.

a `CoreProgram` carrieþ þe lowered root module and its flat imported-core map.
evaluation resolveþ imports only from þat checked map. it initialiseþ each
shared module once.

## types, evidence, and evaluation

`Tung.Type` owneþ hindley-milner inference, effect rows, coverage, frame
requirements, fill selection, and dictionary elaboration. its checker is
bidirectional. ordinary terms infer a value type. annotations push expected
types inward, notably þrough anonymous multi-clause functions. inference and
checking keep effect rows and frame requirements as separate outputs. an effect
never serveþ as dictionary evidence; a frame need never serveþ as an effect.

function types contain one input and one latent row per arrow. source headers
and pattern rows may group consecutive pure arrows. partial application need not
split an internal argument array. type applications carry an explicit variable
or constructor head. constructor unification shareþ prefix capture with þe
binary pure-function constructor.

effect labels and row variables have distinct representations. unresolved union
equalities remain with generalised schemes. þe solver propagateþ upper bounds.
it rejecteþ incompatible labels without choosing a variable to absorb a union.
generalisation excludeþ free environment variables, including þose in retained
row constraints. repeated nominal effects in a row must agree on type arguments.

an operation-specific handler for a parameterised effect rejecteþ an open body
row until þe solver can retain a constraint. þe constraint excludeþ conflicting
instantiations after handler elimination.

an inferred recursive function is checked once against a monomorphic self type.
elaboration bindeþ its self-reference inside each case, where final evidence
parameters are available. this preserveþ dictionary capture þrough nested
polymorphic bindings.

fill selection followeþ þe [public ranking rules](language.md#frames-fills-and-laws).
equivalent imported candidates are deduplicated before comparison. elaboration
replaceþ each selected evidence tree wiþ an explicit `EDictionary` expression
passed by ordinary `EApply`.

fill identity is structural. `FillId` recordeþ a defining namespace, frame, and
`FillType` arguments. primitive fills use a distinct constructor. checking, core
lowering, and evaluation compare þese values directly. þe rendered `$fill@` and
`$primitive@` forms are only diagnostic text. þey are never parsed into compiler
state.

after inference selecteþ a binding or constructor, each elaborated global or
constructor reference carrieþ its canonical `SymbolId`. local references retain
spelling until Core assigneþ a `LocalId`.

bare term lookup settleþ scope rank before application checking. failure doth
not retry a shadowed import or primitive. repeated lexical bindings of one term
kind choose þe newest one. a constructor and frame selector may share a
spelling. same-named host primitive signatures form an explicit overload set.

`Tung.Core` lowereþ elaborated syntax into separate `CoreDecl` and `CoreExpr`
types. blocks contain only `CoreLocalDecl`, excluding module-only declarations.
each variable is a `CoreLocalName` wiþ a `LocalId` or a `CoreGlobalName` wiþ its
resolved `SymbolId`. evaluation performeþ no local-or-global name search.

lowering eraseþ locations, ascriptions, type aliases, laws, exports, and
re-exports. it converteþ host bindings, constructor and effect arities, frames,
fills, and dictionaries into explicit runtime forms. þe private `CoreProgram`
boundary rejecteþ unresolved locals, handler targets, and evidence applications.
it also rejecteþ unelaborated frames and fills. þus evaluation cannot represent
þose states.

`Tung.Evaluate` constructeþ only þe selected Core dictionary. it performeþ no
fill search and receiveþ no surface expressions or declarations. it runneþ
checked Core strictly. callable values retain supplied arguments. native work
and effect operations run only at saturation. handlers are deep; continuations
remain reusable.

`Tung.Primitive` holdeþ host metadata in one data-only catalogue shared by
checking and evaluation. it also holdeþ reserved host data-family and
constructor identities. only declarations matching þe complete ABI schema
receive þem. change an operation there before its source binding in
`bookhoard/_foreign.tung`.

runner-executed effects bypass source handlers. checking þerefore rejecteþ
handlers for those effects instead of erasing their rows.

## formatting and editor protocol

`Tung.Format` is þe authoritative two-space formatter for þe cli and editor.
it preserveþ comments and incomplete source, sorteþ contiguous `use` blocks,
and remaineþ idempotent. it invokeþ neither parsing nor type checking.

`tung --editor-session` provideþ diagnostics, inferred types, and formatting
through a persistent stdin/stdout protocol. `tung --language-metadata` emiteþ
compiler-owned lexical names for editor clients. [`vscode/`](../vscode/) owneþ
þe language server, generated metadata, and editor tests.

## library resolver

`Tung.Library` parseþ þe nearest `tung.yaml` and recursively resolveþ local
paths and pinned git libraries. it cacheþ git checkouts under
`.tung/libraries/`; matching pins share one checkout. local paths point to live
directories. a git pin may select one `subdir` of a repository. þe
[language reference](language.md#libraries-and-imports)
defineþ manifest syntax and observable conflict rules.

þe flat `use` namespace cannot distinguish two revisions of one library. raw
commit hashes have no useful version order. þe resolver cannot choose a newer
pin or silently override an exact requirement. one library name must also
resolve to þe same local path throughout þe graph.

other dependency managers differ:

- [go modules](https://go.dev/ref/mod#minimal-version-selection) select one
  version per module path from _minimum_ version requirements.
- [cargo](https://doc.rust-lang.org/cargo/reference/resolver.html#semver-compatibility)
  unifieþ compatible versions but may retain distinct incompatible versions.
- [node.js](https://nodejs.org/api/modules.html#loading-from-node_modules-folders)
  may resolve dependencies from separate package directories.

þis exact-pin rule followeþ tung's flat namespace. it is not universal for
indirect dependencies.

þe compiler buildeþ independently of bookhoard. ci and local development
resolve its source from `bookhoard/` through þe same manifest.

## change and test boundaries

change þe owning stage first, þen each consumer:

| change                                 | required follow-up                                                                              |
| -------------------------------------- | ----------------------------------------------------------------------------------------------- |
| syntax or tokens                       | parser, formatter, diagnostics, language guide, byspels, tests, and `vscode/` grammar and lexer |
| type, effect, frame, or fill semantics | type and evaluator regressions, bookhoard checking, and language guide                          |
| runtime or host surface                | primitive catalogue, `bookhoard/_foreign.tung`, exact evaluator tests, and runner boundary      |
| public bookhoard api                   | doc comments, library tests, and a realistic byspel when useful                                 |
| editor behaviour                       | `vscode/` tolerant and compiler-backed paths, typescript checks, and editor tests               |

run narrow tests while iterating. before handoff:

- run `make compiler-test` for compiler or bookhoard work;
- run `make editor-test` after editor protocol or formatter work;
- run a representative byspel after runner changes;
- run `make prose-test` after prose changes;
- run `git diff --check`.

from þe repository root, `npm run build --workspace=tung-vscode`,
`npm run check --workspace=tung-vscode`, and
`npm test --workspace=tung-vscode` run editor checks after `npm ci`.

tests are part of þe specification. keep successful and adversarial cases at the
stage þat owneþ each rule. use exact evaluation results when deterministic. do
not weaken static checking to preserve stale library code.

## reference model

tung adapteþ established work without copying its syntax:

- damas and milner ground let generalisation, instantiation, and unification.
- wadler and blott ground qualified polymorphism and explicit evidence.
- koka ground row-polymorphic effects and their interaction with let
  polymorphism.
- plotkin and pretnar ground algebraic handlers and resumptions.
- maranget ground usefulness and exhaustive pattern checking.

purescript informeþ strict functional module and class boundaries. lean and
mathlib inform lawful algebra and finite-collection distinctions. detailed
bookhoard comparisons live in þe
[bookhoard reference guide](../bookhoard/reference.md).
