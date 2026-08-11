# jbini

jbini is a functional programming language with

- static typing
- algebraic data types
- type classes
- effects and handlers
- infix notation

## quick start

```sh
cabal build all

cabal test all

cabal run jbini -- sample/fizzbuzz.jbini
```

a runnable file must produce `𝟙`. only `console`, `random`, and `async` effects may remain unhandled at the file boundary. user-defined effects and `fail` must be handled before the end of the file.

## small example

```jbini
bring ground.jbini;
bring list.jbini;

let (i: integer) fizzbuzz: string =
  match 3 can-divide i, 5 can-divide i {
    yea, yea | 'fizzbuzz',
    yea, nay | 'fizz',
    nay, yea | 'buzz',
    nay, nay | i to-string
  };

0 till 32 $ map fizzbuzz $ each write-line
```

## files and imports

a file is a sequence of declarations and an optional final expression.

`bring` imports another file.

```jbini
bring ground.jbini;
bring list.jbini;
```

imported names are always available as `namespace@name`, where the namespace is the file name before `.jbini`.

```jbini
ground@yea
list@empty
```

an imported name may also be used bare when it is unambiguous. if two imported files expose the same bare name, the checker asks for qualification.

`show` re-exports a value from the current file. `show-type` re-exports a type name.

```jbini
show-type 𝟙;
show null;
```

## comments and names

```jbini
# this is a comment
```

most non-space characters can be part of a name. syntax characters such as `(`, `)`, `{`, `}`, `[`, `]`, `,`, `:`, `;`, `=`, `!`, `→`, `|`, `$`, `.`, `'`, and `\`` are not ordinary name characters.

`@` is used inside qualified names.

`_` discards a binding or matches anything in a pattern.

## primitive values and types

the core implementation knows these primitive value types:

- `integer`
- `float`
- `character`

the standard library defines these common data types:

- `𝟘`: empty type
- `𝟙`: unit type, with value `null`
- `𝟚`: boolean type, with values `yea` and `nay`
- `a option`: `none` or `a some`
- `a list`: `empty` or `a .* rest`
- `a task`: async task value, currently represented by `a done`

literals.

```jbini
# integer
42

# float
6.283

# character
`a
`\23383 # 字

# string
'hello'
```

## application

a sequence of terms separated by space is an application with the second being the function.

```jbini
a f       # f a in haskell
a f b c   # f a b c in haskell
```

the language curries all functions.

```jbini
let add = { x, y | x + y };
let one-add = 1 add;
2 one-add # 3
```

`$` is a pipeline syntax.

```jbini
a f b $ g c d $ h
# ((a f b) g c d) h
```

## evaluation

jbini is strict call-by-value.

- declarations run in order
- `let` evaluates the right-hand side before binding
- a bare brace function is a value; its body waits until the function is fully applied
- application evaluates the function first, then arguments left-to-right
- partial application is pure and does not run latent effects
- only full application can run native work or perform an effect
- `match` evaluates scrutinees before choosing a case, then runs only the chosen body
- `try` evaluates its body first; handled operation arguments are already values
- `resume` continues the captured strict computation

## declarations

```jbini
let answer: integer = 42;
let name = 'jbini';
```

definitions can use the same sequence rule as applications.

```jbini
let x add y = x + y;
let x add-typed y: integer → integer → integer = x + y;
```

or thou canst annotate each argument with a type.

```jbini
let (x: integer, y: integer) add: integer = x + y;
```

definitions can require type-class.

```jbini
given a equal let (x: a, y: a) same: 𝟚 = x ≡ y;
```

## functions and match

function literal.

```jbini
let id = { x | x };

let if = {
  yea, then, _ | then,
  nay, _, else | else
};
```

`match` consumes one or more scrutinees.

```jbini
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

## algebraic data types

```jbini
data a option {
  none,
  a some
}

data a ∏ b {
  a ∏ b
}

let pair: integer ∏ string = 0 ∏ 'zero';
```

## records

records are closed.

```jbini
let person: [name: string, age: integer] =
  [name = 'john', age = 32];

let older = [= person, age = 33];

let public = [= person, - age];
```

## types

```jbini
integer → integer
integer → integer → integer
(integer func integer) func integer
```

```jbini
string → 𝟙 ! console
integer → integer ! string fail
```

```jbini
𝟙 → string ! console, random
```

effect annotations belong to function arrows. partial application is pure; only full application can run latent effects.

```jbini
type string = character list;
```

## type classes

```jbini
bone a equal {
  a ≡ a: 𝟚;
  let a ≢ b = (a ≡ b) ¬
}

flesh 𝟚 equal {
  let ≡ = {
    yea, yea | yea,
    nay, nay | yea,
    _, _ | nay
  }
}

given a equal
bone a order {
  a ≤ a: 𝟚
}

given a semigroup
flesh (a option) semigroup {
  let * = {
    none, b | b,
    a, none | a,
    a some, b some | a * b $ some
  }
}
```

## effects and handlers

```jbini
effect e fail {
  e fail: a
}

effect console {
  string write: 𝟙,
  𝟙 read: string
}
```

effect operation types must be function types. the effect declaration itself adds the latent effect, so operation signatures do not write the same effect at the top level.

```jbini
string → float ! string fail
```

```jbini
try 1 ÷ 0 {
  _ fail | 0
}
```

operation handlers may use the implicit `resume` function. `resume` is multi-shot, so it may be called more than once.

```jbini
effect choice {
  𝟙 choose: integer
}

try (null choose) + (null choose) {
  choose | (1 resume) + (2 resume)
}
```

a handler may include one `return` case. it handles the normal result and may change the answer type of the whole `try`.

```jbini
try 2 + 3 {
  return n | n to-string
}
```

without a `return` case, the normal result is returned unchanged.

## standard effects

`foreign.jbini` declares the runtime-backed standard effects:

- `console`: `write`, `read`
- `random`: `random`
- `async`: `fork`, `wait`, `sleep`
- `fail`: extensible failure with an error payload
- `state`: parameterized state effect

`ground.jbini` re-exports those effects and defines `write-line`, which writes the text and then a newline.

the runtime currently provides native operations named:

- `write`
- `read`
- `random`
- `sleep`

`async` is interpreted synchronously for now: `fork` runs immediately, `wait` unwraps a finished task, and `sleep` blocks the current run.

## project files

- `jbini.cabal`: haskell package setup
- `app/Main.hs`: command-line runner
- `src/Jbini.hs`: public haskell api
- `src/Jbini/Syntax.hs`: syntax trees and tokens
- `src/Jbini/Parse.hs`: lexer and parser
- `src/Jbini/TypeCheck.hs`: imports, name lookup, type inference, classes, match coverage, effects, and runnable-file checks
- `src/Jbini/Evaluate.hs`: interpreter, runtime values, native operations, imports, and effect handlers
- `test/Main.hs`: tests
- `library/`: standard library written in jbini
- `sample/`: runnable examples
- `agent.md`: maintainer notes and detailed language specification
