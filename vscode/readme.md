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
- `out/`: generated commonjs used by vscode, node, and the test runner
- `syntaxes/`: textmate fallback highlighting
- `test/`: client-independent server and tooling tests

## setup

from the repository root, prepare or update the compiler, server, and vscode client:

```sh
./setup.sh
```

the setup builds the haskell compiler, installs node dependencies, runs the checks and tests, builds `dist/tung-vscode.vsix`, and installs or updates it in vscode. reload vscode after it finishes.

when `tongue/` is elsewhere, pass its path:

```sh
TUNG_TONGUE=/path/to/tung/tongue ./setup.sh
```

when the compiler checkout is outside the vscode workspace, also set `tung.tonguePath` to its absolute path in vscode settings.

other lsp clients can start the server over stdio. set `TUNG_TONGUE` when the server cannot find the `tongue` folder from the workspace:

```sh
npm --prefix vscode run build --silent
TUNG_TONGUE=/path/to/tung/tongue node vscode/out/server/main.js --stdio
```

## language work

the server provides:

- incremental document sync and file watching
- compiler-backed parse, name, type, effect, and coverage diagnostics
- compiler-inferred types in hover
- doc comments in hover and generated bookhoard docs
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

the workspace index follows local and bundled imports, honors `show`, understands qualify-if-needed lookup, and sends unsaved imported sources to the haskell checker. the checker bridge asks cabal for `exe:tung` and falls back to a built executable under `tongue/dist-newstyle` when cabal lookup is unavailable. an error-tolerant source model keeps navigation and highlighting available while code is incomplete; the haskell implementation remains authoritative for diagnostics and inferred types.

## checks

```sh
npm run build
npm run check
npm test
npm run docs
```
