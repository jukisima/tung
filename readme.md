# tung

tung is a functional programming tongue with

- static typing
- algebraic data types
- type classes
- effects and handlers
- infix notation

## quick start

```sh
cd tongue

cabal build all

cabal test all

cabal run tung -- ../byspel/fizzbuzz.tung [arguments...]
```

vscode and lsp support lives in `vscode/`. it provides compiler diagnostics, inferred hover types, semantic highlighting, completion, navigation, symbols, rename, import links, folding, and formatting.

prepare or update the language support with:

```sh
./setup.sh
```

generate the searchable standard-bookhoard html wiki with:

```sh
cd vscode
npm run docs
```

install the development tools and git hooks with:

```sh
brew bundle
lefthook install
```

a file run by the cli must declare `main` locally. `main` takes `𝟙`, returns `𝟙`, and may use `console`, `random`, `async`, `file`, `system`, or `clock`. user-defined effects and `fail` must be handled inside `main`.

## small byspel

```tung
bring ground.tung;
bring data/list.tung;

kin fizzbuzz {
  fizz,
  buzz,
  fizzbuzz,
  integer raw
}

fill fizzbuzz to-string {
  let to-string = {
    fizz | 'fizz',
    buzz | 'buzz',
    fizzbuzz | 'fizzbuzz',
    i raw | i to-string
  }
}

let (i: integer) fizzbuzz-of: fizzbuzz =
  match 3 can-divide i, 5 can-divide i {
    yea, yea | fizzbuzz,
    yea, nay | fizz,
    nay, yea | buzz,
    nay, nay | i raw
  };

let main: 𝟙 → 𝟙 ! console = { _ |
  0 till 32 $ map fizzbuzz-of $ map to-string $ each write-line
}
```

## files and imports

a file is a module containing declarations and an optional final expression. the cli runs it only when it declares a local `main` function.

the entry type is:

```tung
𝟙 → 𝟙 ! console, random, async, file, system, clock
```

`main` may use any subset of those effects, including none. module initialisation must stay pure. after loading the declarations, the runner calls `null main`.

`bring` imports another file.

```tung
bring ground.tung;
bring data/list.tung;
```

imports must form an acyclic graph. the checker rejects both direct and indirect bring cycles.

shown names are imported as `namespace@name`, where the namespace is the full import path before `.tung`.

```tung
ground@yea
data/list@empty
```

an imported name may also be used bare when it is unambiguous. if two imported files expose the same bare name, the checker asks for qualification. private names are unavailable both bare and qualified.

tung has two namespaces. values, functions, data constructors, and effect operations are terms. primitive types, aliases, data types, and effect constructors are types. both namespaces are private by default. put `show` before a declaration to publish it.

```tung
show let answer = 42;
graith a equal show let (x: a, y: a) same: 𝟚 = x ≡ y;
show let-ilk count = integer;
show kin 𝟙 { null }
show deed ask { integer ask: integer }
show shape a identity { a identity: a }
graith a equal show shape a order { a ≤ a: 𝟚 }
```

for a declaration with `graith`, put `show` after the requirements and directly before `let` or `shape`.

showing a `kin` declaration shows its type and constructors. showing a `deed` declaration shows its effect type and operations. showing a shape shows its methods. fill evidence follows imports without `show`.

`show name;` re-exports an already visible term. `show-ilk name;` re-exports an already visible type, including an effect constructor. use a qualified name when imports make the bare name ambiguous within that namespace. data constructors and effect operations are terms and must be shown separately from their type.

```tung
bring data/one.tung;
show-ilk one@𝟙;
show one@null;
show one@terminal;
```

there is no wildcard re-export. re-export each term or type with `show name;` or `show-ilk name;`.

## comments and names

```tung
# this is a comment

/* this is a block comment */

## this doc comment attaches to the next shown declaration.
show let documented = 1;
```

most non-space characters can be part of a name. syntax characters such as `(`, `)`, `{`, `}`, `[`, `]`, `,`, `:`, `;`, `=`, `!`, `→`, `|`, `$`, `.`, `'`, and `\`` are not ordinary name characters.

`##` line comments and `/** ... */` block comments attach documentation to the next declaration. the language server shows attached docs in hover, and `npm run docs` builds a searchable html wiki at `writ/bookhoard/index.html`.

`@` is used inside qualified names.

`_` discards a binding or matches anything in a pattern.

## primitive values and types

the core implementation knows these primitive value types:

- `integer`
- `float`
- `character`
- `string`

the standard bookhoard defines these common data types:

- `𝟘`: empty type, with eliminator `initial`
- `𝟙`: unit type, with value `null`
- `𝟚`: boolean type, with values `yea` and `nay`
- `a option`: `none` or `a some`
- `a list`: `empty` or `a .* rest`
- `a task`: async task value, currently represented by `a done`

literals.

```tung
# integer
42

# float
6.283

# character
`a
`\n
`\r
`\t
`\'
`\\
`\{23383} # 字
`{23383} # 字

# string
'hello'
'\n\r\t\'\\\{23383}'
```

## application

a sequence of terms separated by space is an application with the second being the function.

```tung
a f       # f a in haskell
a f b c   # f a b c in haskell
```

the tongue curries all functions.

```tung
let add = { x, y | x + y };
let one-add = 1 add;
2 one-add # 3
```

`$` is a pipeline syntax.

```tung
a f $h b g
# (a f) h b g

a f $(g b c) $h d
# ((a f) g b c) h d
```

## evaluation

tung is strict call-by-value.

- declarations run in order
- `let` evaluates the right-hand side before binding
- a bare brace function is a value; its body waits until the function is fully applied
- application evaluates the function first, then arguments left-to-right
- partial application is pure and does not run latent effects
- only full application can run native work or perform an effect
- `match` evaluates scrutinees before choosing a case, then runs only the chosen body
- `try` evaluates its body first; handled operation arguments are already values
- `resume` continues the captured strict computation
- after loading a runnable module, the cli evaluates `null main`

all evaluation is checked first. module checking forbids immediate top-level effects, runnable checking verifies `main`, and the interactive evaluator permits a checked top-level computation. rejected source is never passed to the runtime.

## declarations

```tung
let answer: integer = 42;
let name = 'tung';
```

definitions can use the same sequence rule as applications.

```tung
let x add y = x + y;
let x add-typed y: integer → integer → integer = x + y;
```

or thou canst annotate each argument with a type.

```tung
let (x: integer, y: integer) add: integer = x + y;
```

definitions can require type-class.

```tung
graith a equal let (x: a, y: a) same: 𝟚 = x ≡ y;
```

## functions and match

function literal.

```tung
let id = { x | x };

let if = {
  yea, then, _ | then,
  nay, _, else | else
};
```

`match` consumes one or more scrutinees.

```tung
match x {
  none | 0,
  n some | n
}

match a, b {
  nay, nay | 0,
  nay, yea | 1,
  yea, nay | 2,
  yea, yea | 3
}
```

cases must have the same number of patterns and be exhaustive.

an empty match is allowed only when the scrutinee type has no values. the empty anonymous function `{}` is allowed when its annotated input type is empty.

```tung
let initial: 𝟘 → a = {};
```

## algebraic data types

```tung
kin a option {
  none,
  a some
}

kin a ∏ b {
  a ∏ b
}

let pair: integer ∏ string = 0 ∏ 'zero';
```

## records

records are closed.

```tung
let person: [name: string, age: integer] =
  [name = 'john', age = 32];

let older = [= person, age = 33];

let age = person@age;

let public = [= person, - age];
```

## types

```tung
integer → integer
integer → integer → integer
(integer func integer) func integer
```

```tung
string → 𝟙 ! console
integer → integer ! string fail
```

```tung
𝟙 → string ! console, random
```

effect annotations belong to function arrows. partial application is pure; only full application can run latent effects.

```tung
string → string ! file, string fail
```

```tung
let-ilk string = character list;
```

## type classes

```tung
shape a equal {
  a ≡ a: 𝟚;
  let a ≢ b = (a ≡ b) ¬;

  law (x: a): x ≡ x ~ yea
}

fill 𝟚 equal {
  let ≡ = {
    yea, yea | yea,
    nay, nay | yea,
    _, _ | nay
  }
}

graith a equal
shape a order {
  a ≤ a: 𝟚
}

graith a semigroup
fill (a option) semigroup {
  let * = {
    none, b | b,
    a, none | a,
    a some, b some | a * b $ some
  }
}
```

shape requirements use static dictionary passing. the checker chooses one fill from the inferred types and adds hidden evidence arguments; the evaluator never guesses a fill from runtime values or bring order. a child shape receives its required parent dictionaries, and a fill may supply a parent directly only when it defines that parent's member. duplicate fills, overlapping applicable fills, missing fills, and recursive evidence are compile-time errors. this also lets result-only members such as `zero` select their fill from an expected result type.

a `law` belongs to its shape but is not a method that fills must implement. its
parameters are local typed names. the checker requires both sides of `~` to
have the same value type, immediate effects, and only requirements supplied by
the shape or its parents. laws are erased before evaluation; tung does not yet
prove that their two sides are equal.

## effects and handlers

```tung
deed e fail {
  e fail: a
}

deed console {
  string write: 𝟙,
  𝟙 read: string
}
```

every operation is a function with at least one input. an operation with no information to receive takes `𝟙` and is called through ordinary second-is-function syntax, such as `null read`. partial application stays pure, and only full application performs the deed. the deed declaration adds its effect. written latent effects are kept.

```tung
string → float ! string fail
```

```tung
try 1 ÷ 0 {
  _ fail | 0
}
```

operation handlers may use the implicit `resume` function. `resume` is multi-shot, so it may be called more than once.

```tung
deed choice {
  𝟙 pick: integer
}

try (null pick) + (null pick) {
  pick | (1 resume) + (2 resume)
}
```

a handler may include one `return` case. it handles the normal result and may change the answer type of the whole `try`.

```tung
try 2 + 3 {
  return n | n to-string
}
```

without a `return` case, the normal result is returned unchanged.

## standard effects

the standard bookhoard declares these runtime-backed effects:

- `console`: `write`, `read`
- `random`: `random`
- `async`: `fork`, `wait`, `sleep`
- `file`: `read-file`, `write-file`, `append-file`
- `system`: `arguments`, `environment`, `exit`
- `clock`: `unix-time`
- `fail`: extensible failure with an error payload
- `state`: parameterised state effect

`ground.tung` re-exports those effects and defines `write-line`, which writes the text and then a newline.

the runtime currently provides native operations named:

- `write`
- `read`
- `random`
- `sleep`
- `read-file`
- `write-file`
- `append-file`
- `arguments`
- `environment`
- `exit`
- `unix-time`

`async` is interpreted synchronously for now: `fork` runs immediately, `wait` unwraps a finished task, and `sleep` blocks the current run.

file operations read and write whole text files. file-system errors perform `string fail`, so callers must handle failure before a runnable file ends.

`arguments` returns only the arguments after the source-file path. `environment` returns `none` when the named host setting is absent. `exit` ends the process with the given status. `unix-time` returns whole seconds since `1970-01-01 00:00:00 utc`.

## project files

- `tung.cabal`: haskell package setup
- `app/Main.hs`: command-line runner
- `src/Tung.hs`: public haskell api
- `src/Tung/Syntax.hs`: syntax trees
- `src/Tung/Token.hs`: tokens, keywords, and source lexing
- `src/Tung/Parse.hs`: parser from tokens to syntax trees
- `src/Tung/Validate.hs`: structural checks over parsed trees
- `src/Tung/Import.hs`: shared import-stack and cycle handling
- `src/Tung/Bookhoard.hs`: bundled bookhoard file list and loading helper
- `src/Tung/Type.hs`: imports, name lookup, type inference, classes, match coverage, effects, and runnable-file checks
- `src/Tung/Evaluate.hs`: interpreter, runtime values, native operations, imports, and effect handlers
- `test/Main.hs`: tests
- `bookhoard/`: standard bookhoard written in tung
- `byspel/`: runnable byspels
- `vscode/server/docs.ts`: standard-bookhoard html wiki generator
- `agent.md`: maintainer notes and detailed tongue specification
