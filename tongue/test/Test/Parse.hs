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
      ++ map expressionCase expressions
      ++ generatedDefinitionForms
      ++ parserProperties

accepted :: [(String, String)]
accepted =
  [ ("last let declaration needeth no semicolon", "let answer = 42")
  , ("top-level declaration boundary needeth no semicolon", "let x = 1 let y = 2")
  , ("type alias boundary needeth no semicolon", "let-ilk count = integer let answer: count = 42")
  , ("yield introduceth the final file expression", "let answer = 42 yield answer")
  , ("type alias", "let-ilk count = integer")
  , ("parameterised type alias", "let-ilk a powerset = a func 𝟚")
  , ("algebraic data", "kin a option { none, a some }")
  , ("empty algebraic data", "kin 𝟘 {}")
  , ("parameterised effect", "deed a state { 𝟙 get: a, a set: 𝟙 }")
  , ("foreign let", "let add-integer: integer → integer → integer = foreign")
  , ("shape requirement and default", "graith a equal shape a order-partial { let a ≤ a: 𝟚 let a < b = a ≤ b }")
  , ("shape member requirement", "shape f traverse { graith m applicative let (a f) traverse (a → b m): (b f) m }")
  , ("shape law", "shape a identity { let a identity: a law (x: a): x identity ~ x }")
  , ("shape members need no separators", "shape a identity { let a identity: a let (x: a) same: a = x law (x: a): x identity ~ x }")
  , ("shape and fill headers use second-is-function order", "shape a convert b { let a convert: b } fill integer convert text { let x convert = 'x' }")
  , ("fill methods use let", "graith a equal fill (a box) equal { let (x box) ≡ (y box) = x ≡ y }")
  , ("fill members need no semicolons", "fill integer linked { let x first = x let x second = x first }")
  , ("graith fill members need no semicolons", "fill integer linked { graith a equal let x first = x let x second = x }")
  , ("exported value", "show let answer = 42")
  , ("exported graith value", "graith a equal show let (x: a, y: a) same: 𝟚 = x ≡ y")
  , ("exported type alias", "show let-ilk count = integer")
  , ("exported parameterised type alias", "show let-ilk a powerset = a func 𝟚")
  , ("exported data", "show kin a option { none, a some }")
  , ("exported effect", "show deed ask { integer ask: integer }")
  , ("exported foreign let", "show let add-integer: integer → integer → integer = foreign")
  , ("exported shape", "show shape a equal { let a ≡ a: 𝟚 }")
  , ("exported graith shape", "graith a equal show shape a order-partial { let a ≤ a: 𝟚 }")
  , ("name re-export", "bring file.tung show file@value")
  , ("grouped name re-export", "bring file.tung show file@first, file@second")
  , ("type re-export", "bring file.tung show-ilk file@value")
  , ("grouped type re-export", "bring file.tung show-ilk file@first, file@second")
  , ("closed record and update", "let person = [name = 'n', age = 1] yield [= person, age = 2, - name]")
  , ("record removal needeth space", "yield [= person, - age]")
  , ("multi-scrutinee match", "yield match 1, 2 { a, b | a }")
  , ("integer patterns in a match", "yield match 1 { 0 | 0, 1 | 1, _ | 2 }")
  , ("integer pattern in a function header", "let 0 zero-only = 0")
  , ("bare brace unary function", "yield { x | x }")
  , ("bare brace curried function", "yield { x, y, z | z }")
  , ("empty consuming match", "yield match x {}")
  , ("handler return and operation cases", "yield try action { return x | x, message fail | message }")
  , ("parenthesised local block", "yield (let x = 1 yield x + 2)")
  , ("multiple local lets need no semicolons", "yield (let x = 1 let y = 2 yield x + y)")
  , ("term type ascription", "let answer = (1 + 2: integer)")
  , ("func is a binary type constructor", "let (x: integer) id: integer = x")
  ]

rejected :: [(String, String)]
rejected =
  [ ("bare final expression requireth yield", "42")
  , ("go is an ordinary name rather than a result keyword", "go 42")
  , ("top-level semicolon is rejected", "let x = 1;")
  , ("bring requireth a path", "bring")
  , ("show requireth a name", "show;")
  , ("show cannot prefix bring", "show bring ground.tung;")
  , ("show-ilk requireth a name", "show-ilk;")
  , ("show cannot prefix an expression", "show 42")
  , ("local block requireth yield", "yield (let x = 1 x)")
  , ("local let rejecteth semicolon", "yield (let x = 1; yield x)")
  , ("empty typed argument list", "let () bad: integer = 1")
  , ("kin requireth a body", "kin option;")
  , ("deed operation requireth a type", "deed pulse { pulse }")
  , ("fill requireth a target type", "fill equal {}")
  , ("law parameters require types", "shape a bad { law (x): x ~ x }")
  , ("law requireth equivalence separator", "shape a bad { law (x: a): x }")
  , ("shape member requirement needeth a member", "shape f bad { graith m applicative }")
  , ("required shape member needeth let", "shape a bad { a bad: a }")
  , ("shape member rejecteth semicolon", "shape a bad { let a bad: a; }")
  , ("graith shape member needeth let", "shape f bad { graith m applicative (a f) bad: a }")
  , ("match arm requireth bar", "match 1 { _ 1 }")
  , ("try requireth handler cases", "try action")
  , ("dollar requireth a left expression", "$ + 1 2")
  , ("dollar requireth a function", "a f $")
  , ("record removal without space is a name", "[= person, -age]")
  , ("record update requireth a base", "[= , value = 1]")
  , ("effects require a function type", "let bad: integer ! fail = 1")
  , ("deed members use commas", "deed e { 𝟙 one: 𝟙; 𝟙 two: 𝟙 }")
  , ("fill members reject commas", "fill integer equal { let x ≡ y = x, let x ≢ y = y }")
  , ("fill members reject semicolons", "fill integer bad { let x first = x; let x second = x }")
  , ("handler hath at most one return clause", "try 1 { return x | x, return y | y }")
  ]

expressions :: [(String, String, Expr)]
expressions =
  [ ("term type ascription", "(1: integer)", EAscribe (EInteger 1) (TypeName "integer"))
  , ("integer match pattern", "match 1 { 0 | 2, _ | 3 }", EMatch [EInteger 1] [MatchCase (PInteger 0 :| []) (EInteger 2), MatchCase (PVar "_" :| []) (EInteger 3)])
  , ("second term is function", "1 + 2", apply (EVar "+") [EInteger 1, EInteger 2])
  , ("unmarked sequence sendeth every argument", "a f b g", apply (EVar "f") [EVar "a", EVar "b", EVar "g"])
  , ("dollar bare segment", "a f $h b g", apply (EVar "h") [apply (EVar "f") [EVar "a"], EVar "b", EVar "g"])
  , ("dollar accepteth space before function", "a f $ h b g", apply (EVar "h") [apply (EVar "f") [EVar "a"], EVar "b", EVar "g"])
  , ("dollar chain", "a f $g b $h c", apply (EVar "h") [apply (EVar "g") [apply (EVar "f") [EVar "a"], EVar "b"], EVar "c"])
  , ("dollar bare tail is function first", "a f $g b c $h d", apply (EVar "h") [apply (EVar "g") [apply (EVar "f") [EVar "a"], EVar "b", EVar "c"], EVar "d"])
  , ("dollar parens are function first", "a f $(g b c) $h d", apply (EVar "h") [apply (EVar "g") [apply (EVar "f") [EVar "a"], EVar "b", EVar "c"], EVar "d"])
  , ("dollar keepeth multiple parenthesised arguments separate", "a f $ if (a some) none", apply (EVar "if") [apply (EVar "f") [EVar "a"], apply (EVar "some") [EVar "a"], EVar "none"])
  , ("anonymous function argument", "l foldl natural@zero { n, _ | n suc }", apply (EVar "foldl") [EVar "l", EVar "natural@zero", EMatch [] [MatchCase (PVar "n" :| [PVar "_"]) (apply (EVar "suc") [EVar "n"])]])
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
      [ ("let", id)
      , ("shape default", \source -> "shape thing klass { " ++ source ++ " }")
      , ("fill method", \source -> "fill thing klass { " ++ source ++ " }")
      ]
  , (n, source) <- zip [(1 :: Int) ..] definitionForms
  ]

definitionForms :: [String]
definitionForms =
  [ "let a foo b c: d = c"
  , "let a foo b c = c"
  , "let (a: a, b: b, c: c) foo: d = c"
  , "let (a: a, b: b, c: c) foo = c"
  , "let (a: a, b, c: c) foo: d = c"
  , "let (a: a, b, c: c) foo = c"
  , "graith a functor, b monoid let a foo b c: d = c"
  , "graith a functor, b monoid let (a: a, b: b, c: c) foo: d = c"
  ]

parserProperties :: [Test]
parserProperties =
  [ Harness.propertyTest "property: top-level let boundaries need no semicolons" $
      QuickCheck.forAll boundarySeparator \separator ->
        QuickCheck.forAll (QuickCheck.chooseInt (0, 999)) \first ->
          QuickCheck.forAll (QuickCheck.chooseInt (0, 999)) \second ->
            let source = "let first = " ++ show first ++ separator ++ "let second = " ++ show second
             in QuickCheck.counterexample source (isRight (parse source))
  , Harness.propertyTest "property: bring needeth no semicolon" $
      QuickCheck.forAll (QuickCheck.elements ["ground.tung", "data/list.tung", "nested/deep.tung"]) \path ->
        let source = "bring " ++ path
         in QuickCheck.counterexample source (isRight (parse source))
  ]
 where
  boundarySeparator = QuickCheck.elements [" ", "\n", "\t", " # boundary\n", " /* boundary */ "]

isRight :: Either a b -> Bool
isRight = either (const False) (const True)
