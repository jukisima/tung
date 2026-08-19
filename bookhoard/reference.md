# bookhoard reference guide

this guide recordeth semantic comparisons and design boundaries. generated api
pages still come from shown tung declarations and their doc comments.

## purescript comparison

the [purescript prelude](https://pursuit.purescript.org/packages/purescript-prelude/6.0.1/docs/Prelude)
is the reference for the small, strict, general-purpose surface. tung now hath
direct counterparts for its central families:

| purescript family | tung bookhoard | standing |
| --- | --- | --- |
| `eq`, `ord`, `ordering`, lattice | `equal`, `order-partial`, `order-total`, `semilattice-infimum`, `semilattice-supremum`, `lattice`, bounded variants, `complement`, `three` | partial and total comparison are distinct; each semilattice standeth alone, lattice addeth absorption, complement owneth `¬`, and none entaileth total order |
| `semigroup`, `monoid` | `semigroup`, `monoid` | alternate additive, multiplicative, infimal, and supremal carriers stay separate |
| `semiring`, `ring`, `field` | arithmetic shapes | the broad hierarchy existeth, but exact euclidean division and mathematically lawful floating-point boundaries still need sharper shapes |
| `functor`, `applicative`, `monad` | collection shapes | list and option have the common fills; tung effects remain separate from data monads |
| `foldable` | `cata` | `foldr`, `foldl`, `fold`, `fold-map`, `…∧`, `…∨`, `…+`, and `…×` cover the basic reductions |
| `traversable` | `traverse` | applicative evidence belongeth to each polymorphic method; list and option provide fills |
| `maybe`, `either`, `tuple` | `option`, `∐`, `∏` | the core sums and products are present |
| `array`, `map`, `set` | `list`, `table`, `set`, `powerset` | powersets and persistent list-backed finite collections exist; a packed random-access array is still absent |

the [purescript traversable api](https://pursuit.purescript.org/packages/purescript-foldable-traversable/docs/Data.Traversable)
guides the `functor` plus `cata` parent shapes and the method-local
`applicative` requirement. tung doth not copy purescript's open record rows,
lazy assumptions, or effect encoding.

## lean and mathlib comparison

lean and mathlib are references for algebraic boundaries, lawful naming, and
the separation of finite collection meanings. they are not syntax templates.

mathlib defineth a
[multiset](https://leanprover-community.github.io/mathlib4_docs/Mathlib/Data/Multiset/Defs.html)
as a list quotiented by permutation, so repetitions remain but order doth not.
it defineth a
[finite set](https://leanprover-community.github.io/mathlib4_docs/Mathlib/Data/Finset/Defs.html)
as a multiset paired with a proof of no duplicates, and useth finite sets as a
base for finite sums and products.

tung currently hath neither quotient types nor dependent proof fields. `powerset` is
therefore the characteristic-function alias `a func 𝟚`, matching lean's
predicate view and avoiding an unenforced representation invariant.
its intersection and union are selected through the overloaded infimum and
supremum operations. no bare powerset monoid chooseth between them: wrap a
powerset in `infimal` for the intersection monoid or `supremal` for the union
monoid.
`set` and `table` expose list-backed `from-list` constructors; operations
built through `empty`, `singleton`, and `put` preserve finite-set uniqueness,
but direct `set@from-list` use may build a noncanonical value. the shared
constructor name is deliberate: `data/set@from-list` and
`data/table@from-list` remain distinct through ordinary qualification. a true
order-insensitive bag should wait for a representation whose invariant can be
enforced rather than being documented only.

mathlib's theorem hierarchy is much richer than tung's checked shape laws.
shape laws in tung are type-checked statements, not proofs and not evaluator
rewrite rules. additions from mathlib should therefore first improve data and
shape boundaries; proof-dependent structures belong after tung gaineth an
explicit proof story.

## next gaps

- split euclidean integer division from field division, with explicit quotient
  and remainder laws
- add first, last, and reversed monoid carriers without burdening the core
  monoid shape
- add a packed immutable array when runtime representation work is justified
- add property testing for laws before treating them as more than checked
  documentation
