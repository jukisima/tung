-- current declaration and expression syntax, exact lowering, and malformed separators.
module Test.Parse (group) where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Harness (Group, Test, expectEq, parseErr, parseOk)
import Test.Harness qualified as Harness
import Test.QuickCheck qualified as QuickCheck
import Tung

group :: IO Group
group =
  Harness.group "parse" $
    map (uncurry parseOk) accepted
      ++ map (uncurry parseErr) rejected
      ++ importCases
      ++ map expressionCase expressions
      ++ generatedDefinitionForms
      ++ parserProperties

accepted :: [(String, String)]
accepted =
  [ ("last let declaration", "let answer ≔ 42"),
    ("single bracket annotateþ a value", "let answer [ℤ] 42"),
    ("bracketed let ends at next declaration", "let answer [ℤ] 42 let other ≔ 1"),
    ("single bracket may contain a function type", "let identity [[a, a]] { x ^ x }"),
    ("untyped positional argument infers result", "let x identity ≔ x"),
    ("positional arguments precede bracket slots", "let a f b c [d: d, result] c"),
    ("positional argument with bracketed result", "let x identity [a] x"),
    ("positional argument with bracketed effect", "let x run [a; e] x"),
    ("bracketed function value type", "let identity [a, a] { x ^ x }"),
    ("bracketed named arguments and inferred result", "let identity [x: a] x"),
    ("leading polymorphic type parameters", "let constant [@a:ilk, @b:ilk, x:a, _:b, a] x"),
    ("named prefix, nested bare argument, and effect row", "let x f [y: b, [c, d; e], d; e] { z ^ z }"),
    ("bracketed constructor-first pattern", "ilk a box { a box } let unbox [box x: a box, a] x"),
    ("bracketed grouped infix pattern", "ilk a ∏ b { a ∏ b } let uncurry [(a ∏ b): a ∏ b, f: [a, b, c], c] a f b"),
    ("bracketed infix pattern needs no grouping", "ilk a ∏ b { a ∏ b } let uncurry [a ∏ b: a ∏ b, f: [a, b, c], c] a f b"),
    ("bracketed required member and law", "flock identity [a:ilk] ( let identity [a, a] law [x: a] x identity = x )"),
    ("bracketed higher-order flock members", "flock cata [f:[ilk, ilk]] ( let fold₀ [a f, b, [a, b, b; e], b; e] let fold₁ [a f, b, [b, a, b; e], b; e] )"),
    ("single bracket required member", "flock origin [a:ilk] ( let origin [a] )"),
    ("bracketed law constructor pattern", "ilk a box { a box } flock identity [a:ilk] ( law [box x: a box] x = x )"),
    ("use accepteþ an optional alias", "use ilk/list.tung list yield list~empty"),
    ("adjacent top-level declarations", "let x ≔ 1 let y ≔ 2"),
    ("type alias declaration boundary", "let-ilk count = ℤ let answer [count] 42"),
    ("yield introduceþ the final file expression", "let answer ≔ 42 yield answer"),
    ("type alias", "let-ilk count = ℤ"),
    ("parameterised type alias", "let-ilk a powerset = a → 𝟚"),
    ("algebraic data", "ilk a option { none, a some }"),
    ("kinded data with adjacent constructor signatures", "ilk list [a:ilk] { empty [a list] _* [a, a list, a list] }"),
    ("higher-kinded data parameter", "ilk free [f:[ilk, ilk], a:ilk] { done [a, f free a] more [(f free a) f, f free a] }"),
    ("nullary bracketed data", "ilk ℕ { zero [ℕ] suc [ℕ, ℕ] }"),
    ("bracketed function type may start a constructor header", "ilk a request { [ℤ, a] ask, stop }"),
    ("empty algebraic data", "ilk 𝟘 {}"),
    ("parameterised effect", "deed a state { get [𝟙, a], set [a, 𝟙] }"),
    ("effect operation with an effect row", "deed e fail { fail [e, a] } deed web { serve [ℤ, [request, response; e], 𝟙; e, text fail], stop [𝟙, 𝟙] }"),
    ("bracketed function type may start an operation header", "deed async { fork [[𝟙, a; e], a; e] }"),
    ("effect row with several entries before next operation", "deed signal { send [ℤ, ℤ; fail, state, console], stop [𝟙, 𝟙] }"),
    ("text-keyed fremmed let", "let plus [ℤ, ℤ, ℤ] 'add-integer' fremmed"),
    ("flock requirement", "graiþ a equal flock order-partial [a:ilk] ( let ≤ [a, a, 𝟚] )"),
    ("flock member requirement", "flock traverse [f:[ilk, ilk]] ( graiþ m applicative let traverse [a f, [a, b m], (b f) m] )"),
    ("flock law", "flock identity [a:ilk] ( let identity [a, a] law [x: a] x identity = x )"),
    ("law equality distinguisheþ parenthesised lets", "flock identity [a:ilk] ( law [x: a] (let y ≔ x yield y) = (let z ≔ x yield z) )"),
    ("flock members need no separators", "flock identity [a:ilk] ( let identity [a, a] let same [a, a] law [x: a] x identity = x )"),
    ("flock names precede kinded parameters while bizen uses second-is-function order", "flock convert [a:ilk, b:ilk] ( let convert [a, b] ) bizen ℤ convert text { let x convert ≔ 'x' }"),
    ("flock without parameters", "flock marker ( let mark [ℤ] )"),
    ("flock with a binary type constructor", "flock category [⇨:[ilk, ilk, ilk]] ( let same [a ⇨ a] )"),
    ("bizen methods use let", "graiþ a equal bizen (a box) equal { let (x box) ≡ (y box) ≔ x ≡ y }"),
    ("adjacent bizen members", "bizen ℤ linked { let x first ≔ x let x second ≔ x first }"),
    ("graiþ bizen member boundary", "bizen ℤ linked { graiþ a equal let x first ≔ x let x second ≔ x }"),
    ("exported value", "show let answer ≔ 42"),
    ("exported graiþ value", "graiþ a equal show let same [x: a, y: a, 𝟚] x ≡ y"),
    ("exported type alias", "show let-ilk count = ℤ"),
    ("exported parameterised type alias", "show let-ilk a powerset = a → 𝟚"),
    ("exported data", "show ilk a option { none, a some }"),
    ("exported effect", "show deed ask { ask [ℤ, ℤ] }"),
    ("exported fremmed let", "show let plus [ℤ, ℤ, ℤ] 'add-integer' fremmed"),
    ("exported class", "show flock equal [a:ilk] ( let ≡ [a, a, 𝟚] )"),
    ("exported class with constraints", "graiþ a equal show flock order-partial [a:ilk] ( let ≤ [a, a, 𝟚] )"),
    ("name re-export", "use file.tung show file~value"),
    ("grouped name re-export", "use file.tung show file~first, file~second"),
    ("type re-export", "use file.tung show-ilk file~value"),
    ("grouped type re-export", "use file.tung show-ilk file~first, file~second"),
    ("closed record and update", "let person ≔ r(name = 'n', age = 1) yield r(= person, age = 2, - name)"),
    ("record removal needeþ space", "yield r(= person, - age)"),
    ("multi-scrutinee match", "yield match 1, 2 { a, b ^ a }"),
    ("ℤ patterns in a match", "yield match 1 { 0 ^ 0, 1 ^ 1, _ ^ 2 }"),
    ("text patterns in a match", "yield match 'yes' { 'yes' ^ 1, _ ^ 0 }"),
    ("ℤ pattern in a function header", "let 0 zero-only ≔ 0"),
    ("bare brace unary function", "yield { x ^ x }"),
    ("bare brace curried function", "yield { x, y, z ^ z }"),
    ("empty consuming match", "yield match x {}"),
    ("handler yield and operation cases", "yield try action { yield x ^ x, message fail ^ message }"),
    ("parenthesised local block", "yield (let x ≔ 1 yield x + 2)"),
    ("adjacent local lets", "yield (let x ≔ 1 let y ≔ 2 yield x + y)"),
    ("term type ascription", "let answer ≔ (1 + 2: ℤ)"),
    ("arrow is a binary type constructor", "bizen → semigroupoid {}"),
    ("arrow type is right associative", "let-ilk curried = ℤ → text → ℤ"),
    ("type application bindeþ before arrow", "let-ilk a result = a list → a option")
  ]

rejected :: [(String, String)]
rejected =
  [ ("bare final expression requireþ yield", "42"),
    ("old let definition marker is rejected", "let answer = 42"),
    ("empty let brackets are rejected", "let bad [] { x ^ x }"),
    ("type parameters follow value arguments", "let bad [x:a, @a:ilk, a] x"),
    ("duplicate type parameters", "let bad [@a:ilk, @a:ilk, x:a, a] x"),
    ("type parameter requireþ a kind", "let bad [@a, x:a, a] x"),
    ("type parameter requireþ ilk", "let bad [@a:ℤ, x:a, a] x"),
    ("equals after bracketed value is rejected", "let bad [ℤ] = 1"),
    ("equals after bracketed function is rejected", "let x bad [ℤ] = x"),
    ("colon result type in bizen is rejected", "flock identity [a:ilk] ( let identity [a, a] ) bizen ℤ identity { let x identity: ℤ ≔ x }"),
    ("old grouped typed let header is rejected", "ilk a ∏ b { a ∏ b } let (a ∏ b: a ∏ b, f: [a, b, c]) uncurry: c ≔ a f b"),
    ("untyped names belong before brackets", "let bad [x:] x"),
    ("colon value annotation is rejected", "let bad: ℤ ≔ 1"),
    ("named arguments precede bare types", "let bad [a, x: b, c] x"),
    ("positional arguments precede bare types", "let x bad [a, y: b, c] x"),
    ("non-function flock member type requireþ brackets", "flock bad [a:ilk] ( let member a )"),
    ("flock member signature cannot bind type parameters", "flock bad [a:ilk] ( let member [@a:ilk, a] )"),
    ("colon flock value annotation is rejected", "flock bad [a:ilk] ( let member: a )"),
    ("old positional flock signature is rejected", "flock cata [f:[ilk, ilk]] ( let (a f) fold₀ b [a, b, b; e]: b ! e )"),
    ("law brackets require typed names", "flock bad [a:ilk] ( law [x:] x = x )"),
    ("old parenthesised law header is rejected", "flock bad [a:ilk] ( law (x: a): x = x )"),
    ("use requireþ a path", "use"),
    ("use alias must be unqualified", "use ilk/list.tung list~short"),
    ("use alias must be slash-free", "use ilk/list.tung sequence/list"),
    ("qualified namespace must be slash-free", "use ilk/list.tung yield ilk/list~empty"),
    ("show requireþ a name", "show"),
    ("show cannot prefix use", "show use ground.tung"),
    ("show-ilk requireþ a name", "show-ilk"),
    ("show cannot prefix an expression", "show 42"),
    ("local block requireþ yield", "yield (let x ≔ 1 x)"),
    ("empty typed argument list", "let () bad: ℤ ≔ 1"),
    ("ilk requireþ a body", "ilk option"),
    ("kinded data parameter requireþ a kind", "ilk box [a] { box [a, a box] }"),
    ("constructor signature requireþ a result", "ilk box [a:ilk] { box [] }"),
    ("higher-kinded parameter cannot be a value type", "ilk wrong [f:[ilk, ilk]] { make [f, f wrong] }"),
    ("ordinary ilk parameter cannot be applied", "ilk box [a:ilk] { make [a a, a box] }"),
    ("primitive type cannot bizen a higher kind parameter", "ilk free [f:[ilk, ilk], a:ilk] { bad [ℤ free a] }"),
    ("deed operation requireþ a type", "deed pulse { pulse }"),
    ("old flock header and braces are rejected", "flock a add { let + [a, a, a] }"),
    ("flock parameters require kinds", "flock add [a] ( let + [a, a, a] )"),
    ("flock member cannot use a constructor parameter as a value type", "flock bad [f:[ilk, ilk]] ( let wrong [f, f] )"),
    ("flock member cannot apply an ordinary type parameter", "flock bad [a:ilk] ( let wrong [ℤ a, ℤ] )"),
    ("bizen requireþ a target type", "bizen equal {}"),
    ("law parameters require types", "flock bad [a:ilk] ( law [x] x = x )"),
    ("old law separator is rejected", "flock bad [a:ilk] ( law [x: a] x ~ x )"),
    ("law requireþ equivalence separator", "flock bad [a:ilk] ( law [x: a] x )"),
    ("flock member requirement needeþ a member", "flock bad [f:[ilk, ilk]] ( graiþ m applicative )"),
    ("required flock member needeþ let", "flock bad [a:ilk] ( a bad: a )"),
    ("graiþ flock member needeþ let", "flock bad [f:[ilk, ilk]] ( graiþ m applicative (a f) bad: a )"),
    ("flock members reject definitions", "flock bad [a:ilk] ( let same [x: a, a] x )"),
    ("match arm requireþ ^", "yield match 1 { _ 1 }"),
    ("try requireþ handler cases", "yield try action"),
    ("dollar requireþ a left expression", "yield $ + 1 2"),
    ("dollar requireþ a function", "yield a f $"),
    ("record removal without space is a name", "yield r(= person, -age)"),
    ("record update requireþ a base", "yield r(= , value = 1)"),
    ("record marker must touch its body", "yield r (value = 1)"),
    ("record type marker must touch its body", "let value [r (field: ℤ)] r(field = 1)"),
    ("left association marker must touch its body", "yield < (+, 1, 2)"),
    ("right association marker must touch its body", "yield > (+, 1, 2)"),
    ("left association requireþ a combining function", "yield <()"),
    ("right association requireþ a combining function", "yield >()"),
    ("left association requireþ values", "yield <(f)"),
    ("right association requireþ values", "yield >(f)"),
    ("left association requireþ at least two values", "yield <(f, a)"),
    ("right association requireþ at least two values", "yield >(f, a)"),
    ("effects require a function type", "let bad [ℤ ! fail] 1"),
    ("deed members use commas", "deed e { one [𝟙, 𝟙] two [𝟙, 𝟙] }"),
    ("old positional deed operation is rejected", "deed web { ℤ serve [request, response; e]: 𝟙 ! e, text fail }"),
    ("bizen members reject commas", "bizen ℤ equal { let x ≡ y ≔ x, let x ≢ y ≔ y }"),
    ("handler hath at most one yield clause", "yield try 1 { yield x ^ x, yield y ^ y }")
  ]

importCases :: [Test]
importCases =
  [ expectEq ("complete use ast " ++ source) (Right (Program [Import path alias])) (parse source)
  | path <- ["ground.tung", "ilk/list.tung", "a.extra.tung", "pkg.one/query.tung"],
    alias <- [Nothing, Just "chosen"],
    let source = "use " ++ path ++ maybe "" (" " ++) alias
  ]

expressions :: [(String, String, Expr)]
expressions =
  [ ("term type ascription", "(1: ℤ)", EAscribe (EInteger 1) (TypeName "ℤ")),
    ("record expression", "r(left = 1, right = 2)", ERecord [("left", EInteger 1), ("right", EInteger 2)]),
    ("record access expression", "person.age", EField (EVar "person") "age"),
    ("chained record access expression", "person.address.city", EField (EField (EVar "person") "address") "city"),
    ("record update expression", "r(= value, right = 3, - left)", EUpdate (EVar "value") [RecordSet "right" (EInteger 3), RecordRemove "left"]),
    ("left-associated sequence", "<(+, a, b, c, d)", apply (EVar "+") [apply (EVar "+") [apply (EVar "+") [EVar "a", EVar "b"], EVar "c"], EVar "d"]),
    ("right-associated sequence", ">(_*, a, b, c, d)", apply (EVar "_*") [EVar "a", apply (EVar "_*") [EVar "b", apply (EVar "_*") [EVar "c", EVar "d"]]]),
    ("separated left marker remaineþ an ordinary name", "1 < (2)", apply (EVar "<") [EInteger 1, EInteger 2]),
    ("separated right marker remaineþ an ordinary name", "1 > (2)", apply (EVar ">") [EInteger 1, EInteger 2]),
    ("ℤ match pattern", "match 1 { 0 ^ 2, _ ^ 3 }", EMatch [EInteger 1] [MatchCase (PInteger 0 :| []) (EInteger 2), MatchCase (PVar "_" :| []) (EInteger 3)]),
    ("alternative match patterns", "match 1 { 0 | 1 ^ 2, _ ^ 3 }", EMatch [EInteger 1] [MatchCase (PInteger 0 :| []) (EInteger 2), MatchCase (PInteger 1 :| []) (EInteger 2), MatchCase (PVar "_" :| []) (EInteger 3)]),
    ("multi-pattern match preserves order", "match 1, 2, 3 { 0, 1, 2 ^ 4 }", EMatch [EInteger 1, EInteger 2, EInteger 3] [MatchCase (PInteger 0 :| [PInteger 1, PInteger 2]) (EInteger 4)]),
    ("text match pattern", "match 'yes' { 'yes' ^ 1, _ ^ 0 }", EMatch [EText "yes"] [MatchCase (PText "yes" :| []) (EInteger 1), MatchCase (PVar "_" :| []) (EInteger 0)]),
    ("second term is function", "1 + 2", apply (EVar "+") [EInteger 1, EInteger 2]),
    ("unmarked sequence sendeþ every argument", "a f b g", apply (EVar "f") [EVar "a", EVar "b", EVar "g"]),
    ("dollar bare segment", "a f $h b g", apply (EVar "h") [apply (EVar "f") [EVar "a"], EVar "b", EVar "g"]),
    ("dollar accepteþ space before function", "a f $ h b g", apply (EVar "h") [apply (EVar "f") [EVar "a"], EVar "b", EVar "g"]),
    ("dollar chain", "a f $g b $h c", apply (EVar "h") [apply (EVar "g") [apply (EVar "f") [EVar "a"], EVar "b"], EVar "c"]),
    ("dollar bare tail is function first", "a f $g b c $h d", apply (EVar "h") [apply (EVar "g") [apply (EVar "f") [EVar "a"], EVar "b", EVar "c"], EVar "d"]),
    ("dollar parens are function first", "a f $(g b c) $h d", apply (EVar "h") [apply (EVar "g") [apply (EVar "f") [EVar "a"], EVar "b", EVar "c"], EVar "d"]),
    ("dollar keepeþ multiple parenthesised arguments separate", "a f $ if (a some) none", apply (EVar "if") [apply (EVar "f") [EVar "a"], apply (EVar "some") [EVar "a"], EVar "none"]),
    ("anonymous function argument", "l fold₁ natural~zero { n, _ ^ n suc }", apply (EVar "fold₁") [EVar "l", EVar "natural~zero", EMatch [] [MatchCase (PVar "n" :| [PVar "_"]) (apply (EVar "suc") [EVar "n"])]])
  ]

expressionCase :: (String, String, Expr) -> Test
expressionCase (name, source, expected) = case parse ("yield " ++ source) of
  Right (Program [Let "_" Nothing actual]) -> expectEq name expected actual
  Right actual -> pure (Just (name ++ ": unexpected ast " ++ show actual))
  Left message -> pure (Just (name ++ ": parse failed: " ++ message))

apply :: Expr -> [Expr] -> Expr
apply = foldl (\f arg -> EApply f (arg :| []))

generatedDefinitionForms :: [Test]
generatedDefinitionForms =
  [ parseOk (owner ++ " form " ++ show n) (wrap source)
  | (owner, wrap) <-
      [ ("let", id),
        ("bizen method", \source -> "bizen thing klass { " ++ source ++ " }")
      ],
    (n, source) <- zip [(1 :: Int) ..] definitionForms
  ]

definitionForms :: [String]
definitionForms =
  [ "let a foo b c [d] c",
    "let a foo b c ≔ c",
    "let foo [a: a, b: b, c: c, d] c",
    "let foo [a: a, b: b, c: c] c",
    "let foo [a: a, b: t1, c: c, d] c",
    "let foo [a: a, b: t1, c: c] c",
    "graiþ a functor, b monoid let a foo b c [d] c",
    "graiþ a functor, b monoid let foo [a: a, b: b, c: c, d] c"
  ]

parserProperties :: [Test]
parserProperties =
  [ Harness.propertyTest ("property: top-level let boundary " ++ show separator) $
      QuickCheck.forAll (QuickCheck.chooseInteger (-999, 999)) \first ->
        QuickCheck.forAll (QuickCheck.chooseInteger (-999, 999)) \second ->
          let source = "let first ≔ " ++ show first ++ separator ++ "let second ≔ " ++ show second
              expected = Program [Let "first" Nothing (EInteger first), Let "second" Nothing (EInteger second)]
           in QuickCheck.counterexample source (parse source QuickCheck.=== Right expected)
  | separator <- [" ", "\n", "\t", " # boundary\n", " /* boundary */ "]
  ]
