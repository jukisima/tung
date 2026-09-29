# tung language support

this directory containeth the vscode extension for `.tung` files. it shareþ
the repository wiþ the compiler in `../tongue`.

## setup

from the repository root, install node dependencies and build the compiler:

```sh
npm ci
make compiler-build
npm test --workspace tung-vscode
```

the tests use the compiler built in `../tongue`. set `TUNG_EXECUTABLE` to an
absolute path to test anoþer compiler. `npm run update:metadata -w tung-vscode`
refresheth the checked-in language names.
`npm run check:metadata -w tung-vscode` checketh them.

package the extension with `npm run package --workspace tung-vscode`.
`npm run install:extension --workspace tung-vscode` installeth it through
the `code` command. `npm run setup --workspace tung-vscode` runneth the
tests and install step.

## configuration

the extension useþ `tung.executablePath` when set. otherwise, it seekeþ a
built compiler in `tung.tonguePath` or the repository's `tongue` folder,
þen `tung` on `PATH`. `tung.tonguePath` accepteth either the repository root
or its `tongue` folder.

the extension useth the workspace's `tung.yaml` for pinned libraries.
`tung.libraryPaths` addeth absolute local module roots. `TUNG_PATH` addeth
further roots. checking and running use the same roots.

## features

the extension provideth:

- syntax and semantic highlighting, folding, and selection ranges;
- compiler-backed diagnostics and inferred types in hover;
- completion, signature help, and doc comments in hover;
- declaration, definition, type-definition, implementation, and reference
  navigation;
- document and workspace symbols, highlights, and checked rename;
- document links for `use`, formatting, and supported quick fixes; and
- a `Run Tung File` command and editor play button.

completion and navigation follow local and declared-library imports. unsaved
imported files are checked with their current contents. navigation and
highlighting work while a file is incomplete. compiler results remain
authoritative for diagnostics, inferred types, and formatting.

to run a file, open a saved `.tung` file and select the editor's play button.
ye can also run `Tung: Run Tung File` from the command palette. the command
saveth the file and useth a dedicated terminal.

the [compiler development guide](https://github.com/jukisima/tung/blob/main/tongue/development.md)
covereth the language implementation.
