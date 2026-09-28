# tung language support

this guide covereþ setup and use of þe vscode extension for `.tung` files.

## setup

from the repository root, install or update the compiler and vscode extension:

```sh
make setup
```

`make setup` buildeþ and testeþ boþ components. it installeþ the extension
and the runner under `${HOME}/.local/bin`.

reload vscode when setup finisheþ.

when the compiler checkout is elsewhere, pass its path:

```sh
make setup TONGUE_DIR=/path/to/tung/tongue
```

## configuration

set `tung.tonguePath` to the absolute compiler path when vscode cannot find
`tongue/` from the workspace. `TONGUE_DIR` selecteþ the compiler for the build;
`tung.tonguePath` selecteþ it for the running extension.

the extension useþ the workspace's `tung.yaml` for pinned libraries.
`tung.libraryPaths` addeþ absolute local module roots. `TUNG_PATH` addeþ
further roots. checking and running use þe same roots.

## features

the extension provideþ:

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

## run a file

to run a file, open a saved `.tung` file and select the editor's play button. ye
can also run `Tung: Run Tung File` from the command palette. the command saveþ
the file and useþ a dedicated terminal. it trieþ the compiler found by the
language server, then `tung` on `PATH`.

for implementation details and checks, see the
[development guide](https://github.com/jukisima/tung/blob/main/tongue/development.md).
