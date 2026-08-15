-- current declaration and expression syntax, exact lowering, and malformed separators.
module Test.Parse (group) where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Harness (Group, Test, expectEq, parseErr, parseOk)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group =
  Harness.group "parse" $
    map (uncurry parseOk) accepted
      ++ map (uncurry parseErr) rejected
      ++ map expressionCase expressions
      ++ generatedDefinitionForms

accepted :: [(String, String)]
accepted =
  [ ("last declaration may omit semicolon", "let answer = 42")
  , ("top-level declaration boundary needs no semicolon", "let x = 1 let y = 2")
  , ("last expression may omit semicolon", "let answer = 42; answer")
  , ("type alias", "let-ilk count = integer;")
  , ("algebraic data", "kin a option { none, a some };")
  , ("empty algebraic data", "kin 𝟘 {};")
  , ("parameterised effect", "deed a state { 𝟙 get: a, a set: 𝟙 };")
  , ("foreign declaration", "class foreign { integer add-integer integer: integer; integer divide-integer integer: integer ! string fail };")
  , ("shape requirement and default", "graith a equal shape a order { a ≤ a: 𝟚; let a < b = a ≤ b };")
  , ("shape law", "shape a identity { a identity: a; law (x: a): x identity ~ x };")
  , ("fill methods use let", "graith a equal fill (a box) equal { let (x box) ≡ (y box) = x ≡ y };")
  , ("exported value", "show let answer = 42;")
  , ("exported graith value", "graith a equal show let (x: a, y: a) same: 𝟚 = x ≡ y;")
  , ("exported type alias", "show let-ilk count = integer;")
  , ("exported data", "show kin a option { none, a some };")
  , ("exported effect", "show deed ask { integer ask: integer };")
  , ("exported foreign terms", "show class foreign { integer add-integer integer: integer };")
  , ("exported shape", "show shape a equal { a ≡ a: 𝟚 };")
  , ("exported graith shape", "graith a equal show shape a order { a ≤ a: 𝟚 };")
  , ("name re-export", "bring file.tung; show file@value;")
  , ("grouped name re-export", "bring file.tung; show file@first, file@second;")
  , ("type re-export", "bring file.tung; show-ilk file@value;")
  , ("grouped type re-export", "bring file.tung; show-ilk file@first, file@second;")
  , ("closed record and update", "let person = [name = 'n', age = 1]; [= person, age = 2, - name]")
  , ("record removal needs space", "[= person, - age]")
  , ("multi-scrutinee match", "match 1, 2 { a, b | a }")
  , ("bare brace unary function", "{ x | x }")
  , ("bare brace curried function", "{ x, y, z | z }")
  , ("empty consuming match", "match x {}")
  , ("handler return and operation cases", "try action { return x | x, message fail | message }")
  , ("parenthesised local block", "(let x = 1; x + 2;)")
  , ("func is a binary type constructor", "let id: integer func integer = { x | x };")
  ]

rejected :: [(String, String)]
rejected =
  [ ("bring requires semicolon", "bring ground.tung")
  , ("bring requires a path", "bring ;")
  , ("show requires a name", "show;")
  , ("show cannot prefix bring", "show bring ground.tung;")
  , ("show-ilk requires a name", "show-ilk;")
  , ("show cannot prefix an expression", "show 42")
  , ("local let requires semicolon", "(let x = 1 x)")
  , ("empty typed argument list", "let () bad: integer = 1")
  , ("kin requires a body", "kin option;")
  , ("deed operation requires a type", "deed pulse { pulse }")
  , ("foreign member requires a type", "class foreign { integer add integer }")
  , ("fill requires a target type", "fill equal {}")
  , ("law parameters require types", "shape a bad { law (x): x ~ x }")
  , ("law requires equivalence separator", "shape a bad { law (x: a): x }")
  , ("match arm requires bar", "match 1 { _ 1 }")
  , ("try requires handler cases", "try action")
  , ("dollar requires a left expression", "$ + 1 2")
  , ("dollar requires a function", "a f $")
  , ("record removal without space is a name", "[= person, -age]")
  , ("record update requires a base", "[= , value = 1]")
  , ("effects require a function type", "let bad: integer ! fail = 1")
  , ("deed members use commas", "deed e { 𝟙 one: 𝟙; 𝟙 two: 𝟙 }")
  , ("fill members use semicolons", "fill integer equal { let x ≡ y = x, let x ≢ y = y }")
  , ("handler has at most one return clause", "try 1 { return x | x, return y | y }")
  ]

expressions :: [(String, String, Expr)]
expressions =
  [ ("second term is function", "1 + 2", apply (EVar "+") [EInteger 1, EInteger 2])
  , ("unmarked sequence sends every argument", "a f b g", apply (EVar "f") [EVar "a", EVar "b", EVar "g"])
  , ("dollar bare segment", "a f $h b g", apply (EVar "h") [apply (EVar "f") [EVar "a"], EVar "b", EVar "g"])
  , ("dollar accepts space before function", "a f $ h b g", apply (EVar "h") [apply (EVar "f") [EVar "a"], EVar "b", EVar "g"])
  , ("dollar chain", "a f $g b $h c", apply (EVar "h") [apply (EVar "g") [apply (EVar "f") [EVar "a"], EVar "b"], EVar "c"])
  , ("dollar bare tail is function first", "a f $g b c $h d", apply (EVar "h") [apply (EVar "g") [apply (EVar "f") [EVar "a"], EVar "b", EVar "c"], EVar "d"])
  , ("dollar parens are function first", "a f $(g b c) $h d", apply (EVar "h") [apply (EVar "g") [apply (EVar "f") [EVar "a"], EVar "b", EVar "c"], EVar "d"])
  , ("dollar keeps multiple parenthesised arguments separate", "a f $ if (a some) none", apply (EVar "if") [apply (EVar "f") [EVar "a"], apply (EVar "some") [EVar "a"], EVar "none"])
  , ("anonymous function argument", "l foldl natural@zero { n, _ | n suc }", apply (EVar "foldl") [EVar "l", EVar "natural@zero", EMatch [] [MatchCase (PVar "n" :| [PVar "_"]) (apply (EVar "suc") [EVar "n"])]])
  ]

expressionCase :: (String, String, Expr) -> Test
expressionCase (name, source, expected) = case parse source of
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
  , "let (a: a, b: b) foo: c → d = { c | c }"
  , "let (a: a, b) foo: c → d = { c | c }"
  , "graith a functor, b monoid let a foo b c: d = c"
  , "graith a functor, b monoid let (a: a, b: b, c: c) foo: d = c"
  ]
