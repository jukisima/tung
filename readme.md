# tung

this guide helpeþ new users install and run tung. the checked
[tutorial](byspel/tutorial.tung) showeþ its basic features wiþ short comments.

## install

```sh
brew tap jukisima/tung https://github.com/jukisima/tung.git
brew install jukisima/tung/tung
```

or, wiþ ghc 9.12 or newer and cabal installed:

```sh
brew bundle
make compiler-install
# þe command is installed at `~/.local/bin/tung` by default.
# set `PREFIX` or `BIN_DIR` to choose anoþer destination.
```

## add a library

this repository keepeth þe standard library in [`bookhoard/`](bookhoard/). its
`tung.yaml` declareþ a local path:

```yaml
dependencies:
  bookhoard:
    path: bookhoard
```

outside this checkout, projects may pin this repository at a commit with
`subdir: bookhoard`. they may also use any other git library. see
[libraries and imports](tongue/language.md#libraries-and-imports) for manifest
syntax, transitive dependencies, and conflict rules.

## run

| command                                  | purpose                                  |
| ---------------------------------------- | ---------------------------------------- |
| `tung program.tung [arguments...]`       | runneþ `main` wiþ þe remaining arguments |
| `tung --check program.tung`              | checkeþ a program wiþ `main`             |
| `tung --check-module module.tung`        | checkeþ a program wiþout `main`          |
| `tung --type-of name program.tung`       | printeþ þe inferred type of a name       |
| `tung --format file.tung [more.tung...]` | formateþ files in stead                  |

`main` takeþ `𝟙`, yieldeþ `𝟙`, and may use þe standard runtime effects. run
the tutorial wiþ `tung byspel/tutorial.tung`. more byspels lie in
[`byspel/`](byspel/).

## editor

the VS Code extension liveþ in [`vscode/`](vscode/). `make editor-build` buildeth
it. `make editor-test` testeth it. `make extension-install` packageþ and
installeþ it through þe `code` command. see þe [editor guide](vscode/readme.md).

## furþer reading

- [language reference](tongue/language.md)
- [bookhoard design guide](bookhoard/reference.md)
- [development guide](tongue/development.md)
- [repository change guide](tongue/agent.md)
