# jbini specification

jbini is a small expression-oriented programming language with sequence application, curried functions, algebraic data, closed records, type classes for ad-hoc polymorphism, imports, and explicit effect annotations.

## source files

A file is a sequence of declarations followed by an optional final expression. A trailing semicolon at the end of a file may be omitted. `bring` declarations and local block `let` declarations require semicolons.

```jbini
bring prelude.jbini;
bring list.jbini;

let answer: integer = 42
```

`bring path;` imports another file. Imported definitions are available as `namespace@name`, where `namespace` is the first component of the import path. An unqualified imported name is accepted only when exactly one visible definition has that name; otherwise the checker requires qualification.

## names and application

Application is written as a sequence and every function is curried. A single term is itself. In an unmarked sequence of two or more terms, the second term is the function and the other terms are arguments in order. `$` explicitly marks the function term and may appear anywhere in the sequence.

```jbini
1 + 2        # (+ 1 2)
$+ 1 2       # same as 1 + 2
1 2 $+       # same as 1 + 2
2 (1 +)      # curried partial application
```

`@` qualifies names:

```jbini
list@empty
list@.*
console@write
```

Dot-prefixed operators such as `.*` are ordinary operator names. A standalone `.` is not an infix escape.

`_` discards a binding or pattern.

## declarations

```jbini
type string = character list;
```

```jbini
data a option {
  none,
  a some
}
```

Multi-argument type and constructor operators use the same sequence rule:

```jbini
data a ∏ b {
  a ∏ b
}

data a ∐ b {
  a first,
  b other
}

let pair-value: integer ∏ string = 1 ∏ 'one';
let pair-value-explicit: integer string $∏ = 1 ∏ 'one'
```

```jbini
effect console {
  write: string → 𝟙,
  read: 𝟙 → string
}
```

```jbini
bone a equal {
  a ≡ a: 𝟚,
  let a ≢ b = (a ≡ b) ¬
}
```

type-class members are either signatures or `let` defaults. a default is checked like an ordinary member and is visible by the same bare or qualified name. when the default uses grouped typed arguments, the annotation after the member name is the result type.

```jbini
bone a subtract need a add, a negate {
  let (a: a, b: a) -: a = a + (b ¯)
}
```

```jbini
flesh 𝟚 equal {
  a ≡ b = match a {
    yea ↦ b,
    nay ↦ b ¬
  }
}
```

`let` always uses `=`. Function definition headers use the same sequence rule: the second term is the function name unless `$` marks it explicitly. A type after a second-position header is the full curried function type.

typed non-infix arguments may also be grouped before the function name. in that form, each argument carries its own type, and the type after the function name is the result type.

```jbini
let id: a → a = func x ↦ x;

let x add-two y = x + y;
let x add-two-typed y: integer → integer → integer = x + y;

let (x: integer, y: integer) add-two-old: integer = x + y;

let (f0: a → b ! e0, f1: b → c ! e1) compose: a → c ! e0, e1 =
  func x ↦ (x f0) f1;

let value default-option default: a option → a → a = match value {
  none ↦ default,
  x some ↦ x
}
```

## expressions

Literals:

```jbini
42
3.14
`c
`{23383}
'text'
```

Functions:

```jbini
func x ↦ x
func x, y ↦ x + y
```

Blocks are parenthesized local `let` declarations plus a final expression.

```jbini
(
  let x = 2 × 3;
  x + 1
)
```

`match` can be a function when scrutinees are omitted, or can consume one or more scrutinees directly.

```jbini
let not: 𝟚 → 𝟚 = match {
  yea ↦ nay,
  nay ↦ yea
};

let chosen = match yea, nay {
  yea, b ↦ b,
  nay, _ ↦ nay
};
```

the type checker rejects non-exhaustive matches over algebraic data at compile time, including multi-scrutinee and nested constructor patterns. variable and `_` patterns are catch-all patterns.

Records are closed. Update starts with `[= base, ...]`; fields can be set or removed.

```jbini
let person = [name = 'naoki', age = 35];
let older = [= person, age = 36];
let public = [= person, -age];
```

Effects are invoked as ordinary calls and handled with `try`.

```jbini
let safe-divide: integer → integer → integer = func x, y ↦
  try x divide y {
    zero-divideth ↦ 0
  };
```

## types

Primitive types are `integer`, `float`, `character`, and `string`. Type application uses the same sequence rule as expression application: `integer list` means `list<integer>`, `a ∏ b` means `∏<a, b>`, and `a b $∏` is the explicit-marker form.

Function types use `→`; effects appear after `!`.

```jbini
a → b
a → b ! console
a → b ! console, zero-divideth
(a → b ! e) → a list → b list ! e
```

Type variables are implicit. `\\` is explicit forall.

```jbini
a, b \\ a → b → a
```

The checker supports Hindley-Milner style polymorphism, closed records, curried application, multi-scrutinee matches, imports, type classes, instances, and effect annotations. Handler cases remove the handled effect and add effects used by the handler body.

## native surface

The checker and runner provide these names natively. Standard-library type classes may also expose the same symbols generically.

```jbini
to-string: float → string
⌊: float → integer
≤: float → float → 𝟚
+: float → float → float
¯: float → float
×: float → float → float
/: float → float ! zero-divideth
exp: float → float
log: float → float
sin: float → float
cos: float → float

divide: integer → integer → integer ! zero-divideth
write-line: string → 𝟙 ! console
read-line: 𝟙 → string ! console
```
