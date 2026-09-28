# tung

this guide helpeþ new users install and run tung. tung is a small functional
programming tongue wiþ

- type inference,
- algebraic data types,
- closed records,
- type classes,
- algebraic effects, and
- infix notation.

## install

```sh
brew tap jukisima/tung
brew install jukisima/tung/tung
```

or

```sh
brew bundle
make install
# þe command is installed at `~/.local/bin/tung` by default.
# set `PREFIX` or `BIN_DIR` to choose anoþer destination.
```

## add a library

[tung-bookhoard](https://github.com/jukisima/tung-bookhoard) is a separate git
library. pin it in the project's `tung.yaml`:

```yaml
dependencies:
  bookhoard:
    repo: https://github.com/jukisima/tung-bookhoard.git
    hash: "aabb7ed4ad37670c1c997afbb8eb8307fce45a39"
```

see [libraries and imports](tongue/language.md#libraries-and-imports) for local
paths, transitive dependencies, and conflict rules.

## run

| command                                  | purpose                                  |
| ---------------------------------------- | ---------------------------------------- |
| `tung program.tung [arguments...]`       | runneþ `main` wiþ þe remaining arguments |
| `tung --check program.tung`              | checkeþ a program wiþ `main`             |
| `tung --check-module module.tung`        | checkeþ a program wiþout `main`          |
| `tung --type-of name program.tung`       | printeþ þe inferred type of a name       |
| `tung --format file.tung [more.tung...]` | formateþ files in stead                  |

`main` takeþ `𝟙`, yieldeþ `𝟙`, and may use þe standard runtime effects.

```tung
use ground.tung

let (_: 𝟙) main: 𝟙 ! console =
  'hello, tung' write-line
```

byspels lie in [`byspel/`](byspel/).

## syntax

tung putteþ þe function after its first argument. functions are curried.

```tung
              # in haskell
a f           # f a
a f b c       # f a b c
a f $ g b     # g (f a) b
```

## furþer reading

- [language reference](tongue/language.md)
- [editor guide](vscode/readme.md)
- [bookhoard design guide](https://github.com/jukisima/tung-bookhoard/blob/main/reference.adoc)
- [development guide](tongue/development.md)
- [repository change guide](tongue/agent.md)
