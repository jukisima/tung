# bookhoard design guide

this guide recordeþ the standard library's design boundaries. see the
[language reference](../tongue/language.md) for imports and manifests.

## library surface

`ground.tung` re-publisheþ common modules. it is an ordinary import, not an
implicit prelude. each public flock liveþ below `flock/`; its file name matcheþ
its flock name.

- partial and total order are separate. lower and upper semilattices stand
  alone; a lattice requireþ both and absorption.
- `powerset` is a characteristic function. `set` and `table` are finite,
  list-backed collections.
- integers provide exact euclidean division. floats expose IEEE operations
  without claiming exact field or total-order laws.
- `bifunctor` mapeþ both sides of products and sums.
- `ilk/free.tung` provideþ `free`, `bind`, `lift`, mapping, flattening, and
  folding. pure `flock/deedless/functor.tung` callbacks let a request store a
  continuation for later interpretation.

## purescript comparison

[purescript prelude](https://pursuit.purescript.org/packages/purescript-prelude/6.0.1/docs/Prelude)
informeþ the small, strict surface. the mapping is approximate:

| purescript family                 | bookhoard counterpart                            | boundary                                                               |
| --------------------------------- | ------------------------------------------------ | ---------------------------------------------------------------------- |
| `eq`, `ord`, lattice              | equality, order, semilattice, and lattice flocks | raw comparison, order laws, and lattice laws remain distinct           |
| `semigroup`, `monoid`             | `semigroup`, `monoid`                            | `infimal` and `supremal` carriers remain distinct                      |
| `semiring`, `ring`, `field`       | arithmetic and euclidean flocks                  | IEEE float division doth not claim exact algebra                       |
| `functor`, `applicative`, `monad` | collection flocks                                | product applicative and monad require a monoid for the first component |
| `foldable`, `traversable`         | `cata`, `traverse`                               | applicative evidence belongeþ to each polymorphic traversal            |
| `maybe`, `either`, `tuple`        | `option`, `∐`, `∏`                               | sums and products live in the core                                     |
| `array`, `map`, `set`             | `list`, `table`, `set`, `powerset`               | a packed random-access array is absent                                 |

boolean conjunction useþ `𝟚 infimal`; disjunction useþ `𝟚 supremal`.
[purescript traversable](https://pursuit.purescript.org/packages/purescript-foldable-traversable/docs/Data.Traversable)
informeþ the `functor` and `cata` parent flocks. tung doth not copy its open
record rows, lazy assumptions, or effect encoding.

## lean and mathlib comparison

lean and mathlib inform algebraic boundaries and finite collections. a
[multiset](https://leanprover-community.github.io/mathlib4_docs/Mathlib/Data/Multiset/Defs.html)
retaineth repetition without order. a
[finite set](https://leanprover-community.github.io/mathlib4_docs/Mathlib/Data/Finset/Defs.html)
pairþ a multiset wiþ a proof of no duplicates.

tung hath neiþer quotient types nor dependent proof fields. `powerset` is
therefore `a → 𝟚`. wrap it in `infimal` for intersection or `supremal` for
union; no bare powerset monoid chooseþ between them.

`set` and `table` expose `from-list`. `empty`, `singleton`, and `put` preserve
uniqueness, but direct `set~from-list` may build a noncanonical value.
`set~from-list` and `table~from-list` remain distinct by qualification.

the planned order-insensitive bag is tracked in [`todo.md`](../todo.md).
flock laws are type-checked statements, not proofs or evaluator rewrite rules.
