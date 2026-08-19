# tung language support

this vscode extension and lsp server support `.tung` files.

## layout

- `client/extension.ts`: vscode extension entry point and language-client setup
- `server/main.ts`: lsp entry point and protocol handlers
- `server/analysis.ts`: error-tolerant document model and symbol analysis
- `server/checker.ts`: bridge to the haskell compiler
- `server/format.ts`: source formatter
- `server/semantic.ts`: lexer-aware semantic highlighting
- `server/workspace.ts`: file index, imports, and name resolution
- `generated/language-names.json`: lexical names emitted by a tung tool during the build
- `out/`: generated commonjs used by vscode, node, and the test runner
- `syntaxes/`: textmate fallback highlighting
- `test/`: client-independent server and tooling tests

## setup

from the repository root, prepare or update the compiler, server, and vscode client:

```sh
make setup
```

`make setup` buildeth and testeth the haskell compiler, vscode support, and documentation generator. it generateth the bookhoard wiki, buildeth `dist/tung-vscode.vsix`, installeth or updateth the extension in vscode, and installeth the standalone runner under `${HOME}/.local/bin`. reload vscode after it finisheth.

when `tongue/` is elsewhere, pass its path:

```sh
make setup TONGUE_DIR=/path/to/tung/tongue
```

when the compiler checkout is outside the vscode workspace, also set `tung.tonguePath` to its absolute path in vscode settings.
the configured `tongue/` compiler folder and the `bookhoard/` source folder must be siblings.

other lsp clients can start the server over stdio. set `TUNG_TONGUE` when the server cannot find the `tongue` folder from the workspace:

```sh
npm --prefix vscode run build --silent
TUNG_TONGUE=/path/to/tung/tongue node vscode/out/server/main.js --stdio
```

## language work

the server provideth:

- incremental document sync and file watching
- compiler-backed parse, name, type, effect, and coverage diagnostics
- compiler-inferred types in hover
- doc comments in hover
- full and ranged semantic tokens
- completion for visible names, qualified names, keywords, primitives, and bring paths
- hover, declaration, definition, type-definition, implementation, references, and document highlights
- document and workspace symbols
- checked workspace rename
- signature help
- document links for `bring`
- folding and selection ranges
- document and range formatting
- quick fixes for supported parse errors

the workspace index followeth local and bundled imports, honoureth `show`, understandeth qualify-if-needed lookup, and sendeth unsaved imported sources to the haskell checker. the checker bridge asketh cabal for `exe:tung` and falleth back to a built executable under `tongue/dist-newstyle` when cabal lookup is unavailable. an error-tolerant source model keepeth navigation and highlighting available while code is incomplete; the haskell implementation remaineth authoritative for diagnostics and inferred types.

`make language-metadata` runneth `tongue/tool/language-names.tung` and refresheth the generated keyword and primitive-type table used by semantic highlighting.

## checks

```sh
npm run build
npm run check
npm test
```

the standalone bookhoard documentation generator lives in `../writ/`.
