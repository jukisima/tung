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
  [ ("last let declaration", "let answer = 42"),
    ("single bracket annotateþ a value", "let answer [ℤ] 42"),
    ("bracketed let ends at next declaration", "let answer [ℤ] 42 let other = 1"),
    ("single bracket may contain a function type", "let identity [[a, a]] { x ^ x }"),
    ("untyped positional argument infers result", "let x identity = x"),
    ("positional arguments precede bracket slots", "let a f b c [d: d, result] c"),
    ("positional argument with bracketed result", "let x identity [a] x"),
    ("positional argument with bracketed effect", "let x run [a; e] x"),
    ("bracketed function value type", "let identity [a, a] { x ^ x }"),
    ("bracketed named arguments and inferred result", "let identity [x: a] x"),
    ("named prefix, nested bare argument, and effect row", "let x f [y: b, [c, d; e], d; e] { z ^ z }"),
    ("bracketed constructor-first pattern", "ilk a box { a box } let unbox [box x: a box, a] x"),
    ("bracketed grouped infix pattern", "ilk a ∏ b { a ∏ b } let uncurry [(a ∏ b): a ∏ b, f: [a, b, c], c] a f b"),
    ("bracketed infix pattern needs no grouping", "ilk a ∏ b { a ∏ b } let uncurry [a ∏ b: a ∏ b, f: [a, b, c], c] a f b"),
    ("bracketed required member and law", "frame a identity { let identity [a, a] law [x: a] x identity = x }"),
    ("bracketed higher-order frame members", "frame f cata { let fold₀ [a f, b, [a, b, b; e], b; e] let fold₁ [a f, b, [b, a, b; e], b; e] }"),
    ("single bracket required member", "frame a origin { let origin [a] }"),
    ("bracketed law constructor pattern", "ilk a box { a box } frame a identity { law [box x: a box] x = x }"),
    ("use accepteþ an optional alias", "use ilk/list.tung list yield list~empty"),
    ("adjacent top-level declarations", "let x = 1 let y = 2"),
    ("type alias declaration boundary", "let-ilk count = ℤ let answer [count] 42"),
    ("yield introduceþ the final file expression", "let answer = 42 yield answer"),
    ("type alias", "let-ilk count = ℤ"),
    ("parameterised type alias", "let-ilk a powerset = a func 𝟚"),
    ("algebraic data", "ilk a option { none, a some }"),
    ("bracketed function type may start a constructor header", "ilk a request { [ℤ, a] ask, stop }"),
    ("empty algebraic data", "ilk 𝟘 {}"),
    ("parameterised effect", "deed a state { get [𝟙, a], set [a, 𝟙] }"),
    ("effect operation with an effect row", "deed e fail { fail [e, a] } deed web { serve [ℤ, [request, response; e], 𝟙; e, text fail], stop [𝟙, 𝟙] }"),
    ("bracketed function type may start an operation header", "deed async { fork [[𝟙, a; e], a; e] }"),
    ("effect row with several entries before next operation", "deed signal { send [ℤ, ℤ; fail, state, console], stop [𝟙, 𝟙] }"),
    ("text-keyed fremmed let", "let plus [ℤ, ℤ, ℤ] 'add-integer' fremmed"),
    ("frame requirement", "graiþ a equal frame a order-partial { let ≤ [a, a, 𝟚] }"),
    ("frame member requirement", "frame f traverse { graiþ m applicative let traverse [a f, [a, b m], (b f) m] }"),
    ("frame law", "frame a identity { let identity [a, a] law [x: a] x identity = x }"),
    ("law equality distinguisheþ parenthesised lets", "frame a identity { law [x: a] (let y = x yield y) = (let z = x yield z) }"),
    ("frame members need no separators", "frame a identity { let identity [a, a] let same [a, a] law [x: a] x identity = x }"),
    ("frame and fill headers use second-is-function order", "frame a convert b { let convert [a, b] } fill ℤ convert text { let x convert = 'x' }"),
    ("fill methods use let", "graiþ a equal fill (a box) equal { let (x box) ≡ (y box) = x ≡ y }"),
    ("adjacent fill members", "fill ℤ linked { let x first = x let x second = x first }"),
    ("graiþ fill member boundary", "fill ℤ linked { graiþ a equal let x first = x let x second = x }"),
    ("exported value", "show let answer = 42"),
    ("exported graiþ value", "graiþ a equal show let same [x: a, y: a, 𝟚] x ≡ y"),
    ("exported type alias", "show let-ilk count = ℤ"),
    ("exported parameterised type alias", "show let-ilk a powerset = a func 𝟚"),
    ("exported data", "show ilk a option { none, a some }"),
    ("exported effect", "show deed ask { ask [ℤ, ℤ] }"),
    ("exported fremmed let", "show let plus [ℤ, ℤ, ℤ] 'add-integer' fremmed"),
    ("exported frame", "show frame a equal { let ≡ [a, a, 𝟚] }"),
    ("exported graiþ frame", "graiþ a equal show frame a order-partial { let ≤ [a, a, 𝟚] }"),
    ("name re-export", "use file.tung show file~value"),
    ("grouped name re-export", "use file.tung show file~first, file~second"),
    ("type re-export", "use file.tung show-ilk file~value"),
    ("grouped type re-export", "use file.tung show-ilk file~first, file~second"),
    ("closed record and update", "let person = r(name = 'n', age = 1) yield r(= person, age = 2, - name)"),
    ("record removal needeþ space", "yield r(= person, - age)"),
    ("multi-scrutinee match", "yield match 1, 2 { a, b ^ a }"),
    ("ℤ patterns in a match", "yield match 1 { 0 ^ 0, 1 ^ 1, _ ^ 2 }"),
    ("text patterns in a match", "yield match 'yes' { 'yes' ^ 1, _ ^ 0 }"),
    ("ℤ pattern in a function header", "let 0 zero-only = 0"),
    ("bare brace unary function", "yield { x ^ x }"),
    ("bare brace curried function", "yield { x, y, z ^ z }"),
    ("empty consuming match", "yield match x {}"),
    ("handler yield and operation cases", "yield try action { yield x ^ x, message fail ^ message }"),
    ("parenthesised local block", "yield (let x = 1 yield x + 2)"),
    ("adjacent local lets", "yield (let x = 1 let y = 2 yield x + y)"),
    ("term type ascription", "let answer = (1 + 2: ℤ)"),
    ("func is a binary type constructor", "let id [x: ℤ, ℤ] x")
  ]

rejected :: [(String, String)]
rejected =
  [ ("bare final expression requireþ yield", "42"),
    ("empty let brackets are rejected", "let bad [] { x ^ x }"),
    ("equals after bracketed value is rejected", "let bad [ℤ] = 1"),
    ("equals after bracketed function is rejected", "let x bad [ℤ] = x"),
    ("colon result type in fill is rejected", "frame a identity { let identity [a, a] } fill ℤ identity { let x identity: ℤ = x }"),
    ("old grouped typed let header is rejected", "ilk a ∏ b { a ∏ b } let (a ∏ b: a ∏ b, f: [a, b, c]) uncurry: c = a f b"),
    ("arrow function type is rejected", "let-ilk unary = ℤ → ℤ"),
    ("untyped names belong before brackets", "let bad [x:] x"),
    ("colon value annotation is rejected", "let bad: ℤ = 1"),
    ("named arguments precede bare types", "let bad [a, x: b, c] x"),
    ("positional arguments precede bare types", "let x bad [a, y: b, c] x"),
    ("non-function frame member type requireþ brackets", "frame a bad { let member a }"),
    ("colon frame value annotation is rejected", "frame a bad { let member: a }"),
    ("old positional frame signature is rejected", "frame f cata { let (a f) fold₀ b [a, b, b; e]: b ! e }"),
    ("law brackets require typed names", "frame a bad { law [x:] x = x }"),
    ("old parenthesised law header is rejected", "frame a bad { law (x: a): x = x }"),
    ("use requireþ a path", "use"),
    ("use alias must be unqualified", "use ilk/list.tung list~short"),
    ("use alias must be slash-free", "use ilk/list.tung sequence/list"),
    ("qualified namespace must be slash-free", "use ilk/list.tung yield ilk/list~empty"),
    ("show requireþ a name", "show"),
    ("show cannot prefix use", "show use ground.tung"),
    ("show-ilk requireþ a name", "show-ilk"),
    ("show cannot prefix an expression", "show 42"),
    ("local block requireþ yield", "yield (let x = 1 x)"),
    ("empty typed argument list", "let () bad: ℤ = 1"),
    ("ilk requireþ a body", "ilk option"),
    ("deed operation requireþ a type", "deed pulse { pulse }"),
    ("fill requireþ a target type", "fill equal {}"),
    ("law parameters require types", "frame a bad { law [x] x = x }"),
    ("old law separator is rejected", "frame a bad { law [x: a] x ~ x }"),
    ("law requireþ equivalence separator", "frame a bad { law [x: a] x }"),
    ("frame member requirement needeþ a member", "frame f bad { graiþ m applicative }"),
    ("required frame member needeþ let", "frame a bad { a bad: a }"),
    ("graiþ frame member needeþ let", "frame f bad { graiþ m applicative (a f) bad: a }"),
    ("frame members reject definitions", "frame a bad { let same [x: a, a] x }"),
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
    ("fill members reject commas", "fill ℤ equal { let x ≡ y = x, let x ≢ y = y }"),
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
        ("fill method", \source -> "fill thing klass { " ++ source ++ " }")
      ],
    (n, source) <- zip [(1 :: Int) ..] definitionForms
  ]

definitionForms :: [String]
definitionForms =
  [ "let a foo b c [d] c",
    "let a foo b c = c",
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
          let source = "let first = " ++ show first ++ separator ++ "let second = " ++ show second
              expected = Program [Let "first" Nothing (EInteger first), Let "second" Nothing (EInteger second)]
           in QuickCheck.counterexample source (parse source QuickCheck.=== Right expected)
  | separator <- [" ", "\n", "\t", " # boundary\n", " /* boundary */ "]
  ]
