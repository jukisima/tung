module Main where

import Data.List (isInfixOf, isPrefixOf)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import Jbini
import System.Exit (exitFailure)

type Test = IO (Maybe String)

main :: IO ()
main = do
  tests <- sequence
    [ parserTests
    , typeTests
    , nameResolutionTests
    , evaluationTests
    , libraryAndSampleTests
    ]
  let failures = concat tests
  if null failures
    then putStrLn "ok"
    else mapM_ putStrLn failures >> exitFailure

parserTests :: IO [String]
parserTests = runGroup "parser" $
  [ expectExpr "second term is function" "1 + 2" curriedPlus
  , expectExpr "dollar marks function first" "$+ 1 2" curriedPlus
  , expectExpr "dollar marks function last" "1 2 $+" curriedPlus
  , expectParseOk "multi-scrutinee match" "match 1, 2 { a, b ↦ a }"
  , expectParseOk "bare braces make anonymous function" "{ x ↦ x }"
  , expectParseOk "bare brace function may take three arguments" "{ x, y, z ↦ z }"
  , expectParseOk "typed argument definition" "let (x: integer, y: integer) add: integer = x + y; 1 add 2"
  , expectParseOk "typed argument definition with given" "given a equal let (x: a, y: a) same: 𝟚 = x ≡ y"
  , expectParseOk "flesh header with given" "given a equal flesh (a box) equal { let (x box) ≡ (y box) = x ≡ y }"
  , expectParseOk "bone header with given" "given a equal bone a order { a ≤ a: 𝟚 }"
  , expectParseOk "parameterized state effect" "effect a state { get: 𝟙 → a, set: a → 𝟙 }"
  , expectParseErr "old grouped need rejected" "let (need a equal, x: a, y: a) same: 𝟚 = x ≡ y"
  , expectParseErr "old bone need rejected" "bone a order need a equal { a ≤ a: 𝟚 }"
  , expectParseErr "old flesh need rejected" "flesh (a box) equal need a equal { let (x box) ≡ (y box) = x ≡ y }"
  , expectParseErr "func syntax removed" "func x ↦ x"
  , expectParseErr "scrutinee-less match keyword removed" "match { x ↦ x }"
  , expectParseErr "bare flesh method rejected" "flesh 𝟙 to-string { x to-string = match x { null ↦ 'null' } }"
  , expectParseErr "comma-separated flesh methods rejected" "flesh 𝟙 sample { let one = 1, let two = 2 }"
  , expectParseOk "bone default uses ordinary let syntax" "bone a equal { a ≡ a: 𝟚; let a ≢ b = b }"
  , expectParseErr "comma-separated bone members rejected" "bone a sample { one: a, two: a }"
  , expectParseErr "bone defaults require let" "bone a sample { one = 1 }"
  , expectParseErr "let-style bone signature rejected" "bone a sample { let one: a }"
  , expectParseErr "empty typed argument list" "let () bad: integer = 1"
  , expectParseErr "match arm must use arrow" "match 1 { _ 1 }"
  ] ++ requestedLetSyntaxTests ++ requestedFleshLetSyntaxTests ++ requestedBoneDefaultSyntaxTests

requestedLetSyntaxTests :: [Test]
requestedLetSyntaxTests =
  [ expectParseOk ("requested let form " ++ show n) source
  | (n, source) <- zip [(1 :: Int)..] requestedLetForms
  ]

requestedFleshLetSyntaxTests :: [Test]
requestedFleshLetSyntaxTests =
  [ expectParseOk ("requested flesh method form " ++ show n) ("flesh thing klass { " ++ source ++ " }")
  | (n, source) <- zip [(1 :: Int)..] requestedLetForms
  ]

requestedBoneDefaultSyntaxTests :: [Test]
requestedBoneDefaultSyntaxTests =
  [ expectParseOk ("requested bone default form " ++ show n) ("bone thing klass { " ++ source ++ " }")
  | (n, source) <- zip [(1 :: Int)..] requestedLetForms
  ]

requestedLetForms :: [String]
requestedLetForms =
  [ "let a foo b c: d = c"
  , "let a foo b c = c"
  , "let (a: a, b: b, c: c) foo: d = c"
  , "let (a: a, b: b, c: c) foo = c"
  , "let (a: a, b, c: c) foo: d = c"
  , "let (a: a, b, c: c) foo = c"
  , "let (a: a, b: b) foo: c → d = { c ↦ c }"
  , "let (a: a, b) foo: c → d = { c ↦ c }"
  , "given a functor, b monoid let a foo b c: d = c"
  , "given a functor, b monoid let a foo b c = c"
  , "given a functor, b monoid let (a: a, b: b, c: c) foo: d = c"
  , "given a functor, b monoid let (a: a, b: b, c: c) foo = c"
  , "given a functor, b monoid let (a: a, b, c: c) foo: d = c"
  , "given a functor, b monoid let (a: a, b, c: c) foo = c"
  , "given a functor, b monoid let (a: a, b: b) foo: c → d = { c ↦ c }"
  , "given a functor, b monoid let (a: a, b) foo: c → d = { c ↦ c }"
  ]

typeTests :: IO [String]
typeTests = runGroup "typing"
  [ expectEq "literal type mismatch" (check "let bad: integer = 'hello';") "type error:"
  , expectEq "non-exhaustive match rejected" (check "data two { yea, nay } let bad: integer = match yea { yea ↦ 1 }") "type error:"
  , expectTypeOk "bare brace function has curried triple type" "let pick: integer → string → float → integer = { x, _, _ ↦ x }"
  , expectTypeErr "bare brace function cases must share arity" "let bad = { x ↦ x, x, y ↦ x }"
  , expectTypeOk "multi-scrutinee exhaustive match" "data two { yea, nay } let ok: integer = match yea, nay { yea, yea ↦ 1, yea, nay ↦ 2, nay, yea ↦ 3, nay, nay ↦ 4 }"
  , expectTypeErr "match cases must share arity" "data two { yea, nay } let bad: integer = match yea, nay { yea, nay ↦ 1, nay ↦ 2 }"
  , expectTypeErr "multi-scrutinee missing case" "data two { yea, nay } let bad: integer = match yea, nay { yea, yea ↦ 1, yea, nay ↦ 2, nay, yea ↦ 3 }"
  , expectTypeOk "nested constructor exhaustive match" "data two { yea, nay } data a option { none, a some } let ok: integer = match yea some { yea some ↦ 1, nay some ↦ 2, none ↦ 0 }"
  , expectTypeErr "nested constructor missing case" "data two { yea, nay } data a option { none, a some } let bad: integer = match yea some { yea some ↦ 1, none ↦ 0 }"
  , expectTypeOk "function given permits open class use" "data 𝟚 { yea, nay } bone a equal { a ≡ a: 𝟚 } given a equal let (x: a, y: a) same: 𝟚 = x ≡ y"
  , expectTypeOk "given permits open class use" "data 𝟚 { yea, nay } bone a equal { a ≡ a: 𝟚 } given a equal let x same y: 𝟚 = x ≡ y"
  , expectTypeErr "missing function given rejected" "data 𝟚 { yea, nay } bone a equal { a ≡ a: 𝟚 } let (x: a, y: a) same: 𝟚 = x ≡ y"
  , expectTypeErr "insufficient function given rejected" "data 𝟚 { yea, nay } bone a equal { a ≡ a: 𝟚 } given a equal bone a order { a ≤ a: 𝟚 } given a equal let (x: a, y: a) leq: 𝟚 = x ≤ y"
  , expectTypeOk "bone default can use another class" "data 𝟚 { yea, nay } bone a semigroup { a * a: a } given a semigroup bone a monoid { ∅: a } bone f cata { (a f) foldr a (a → a → a): a; given a monoid let (x: a f) fold: a = x foldr ∅ * }"
  , expectTypeOk "flesh given reduces instance context" "data 𝟚 { yea, nay } bone a equal { a ≡ a: 𝟚 } data a box { a box } given a equal flesh (a box) equal { let (x box) ≡ (y box) = x ≡ y } given a equal let (x: a box, y: a box) same-box: 𝟚 = x ≡ y"
  , expectTypeErr "insufficient flesh given rejected" "data 𝟚 { yea, nay } bone a equal { a ≡ a: 𝟚 } data a box { a box } flesh (a box) equal { let (x box) ≡ (y box) = x ≡ y }"
  , expectTypeErr "closed record rejects extra field" "let bad: [name: string] = [name = 'naoki', age = 35];"
  , expectTypeErr "record update rejects unknown removal" "let person = [name = 'naoki']; let bad = [= person, -age];"
  , expectEq "effect operation must be function" (check "effect bad { tick: integer }") "type error:"
  , expectEq "effect operation has no extra top-level effects" (check "effect bad { op: integer → integer ! console }") "type error:"
  , expectEq "handler removes fail" (check "let ok: integer = try 1 divide 0 { fail ↦ 9 }") "type ok"
  , expectTypeOk "operation handler may resume" "effect ask { ask: integer → integer } let ok: integer = try 10 ask { x ask ↦ x resume }"
  , expectEq "resume argument checked" (check "effect ask { ask: integer → integer } let bad: integer = try 10 ask { x ask ↦ 'bad' resume }") "type error:"
  , expectTypeErr "handler for absent effect rejected" "let bad: integer = try 1 { fail ↦ 2 }"
  , expectTypeErr "effect-level handler cannot bind arguments" "let bad: integer = try 1 divide 0 { x fail ↦ 0 }"
  , expectEq "state effect is parameterized" (check "effect a state { get: 𝟙 → a, set: a → 𝟙 } let got: 𝟙 → integer ! integer state = get;") "type ok"
  , expectEq "state parameter mismatch" (check "effect a state { get: 𝟙 → a, set: a → 𝟙 } let got: 𝟙 → integer ! string state = get;") "type error:"
  , expectEq "partial effectful call is pure at file boundary" (checkRunnableWithImports "let half: integer → integer ! string fail = 1 divide" Map.empty) "type ok"
  , expectRunnableOk "runner permits console effect" "let _ = 'hello' write-line;"
  , expectRunnableErr "runner rejects custom effect" "data 𝟙 { null } effect ask { ask: 𝟙 → 𝟙 } let _ = null ask;"
  , expectEq "non-unit runnable result rejected" (checkRunnableWithImports "42" Map.empty) "type error:"
  , expectEq "unhandled fail rejected at runnable boundary" (checkRunnableWithImports "1 divide 0" Map.empty) "type error:"
  ]

nameResolutionTests :: IO [String]
nameResolutionTests = runGroup "names"
  [ expectEq "unique imported bare value" (checkWithImports "bring file.jbini; let ok: integer = foo;" oneImport) "type ok"
  , expectEq "ambiguous imported bare value" (checkWithImports "bring file.jbini; bring other.jbini; let bad: integer = foo;" duplicateImports) "type error:"
  , expectTypeOkWithImports "qualified duplicate import resolves" "bring file.jbini; bring other.jbini; let ok: integer = file@foo;" duplicateImports
  , expectTypeErrWithImports "unknown qualified import name" "bring file.jbini; let bad: integer = other@foo;" oneImport
  , lookupTypeTest
  , canonicalTypeTest
  ]
  where
    oneImport = Map.fromList [("file.jbini", "let foo: integer = 1;")]
    duplicateImports = Map.fromList
      [ ("file.jbini", "let foo: integer = 1;")
      , ("other.jbini", "let foo: integer = 2;")
      ]

evaluationTests :: IO [String]
evaluationTests = runGroup "evaluation"
  [ expectEval "integer addition" "1 + 2" "eval ok: 3"
  , expectEval "curried partial application" "let f = { a, b ↦ a + b }; 2 (1 f)" "eval ok: 3"
  , expectEval "three-argument brace function" "let add3 = { x, y, z ↦ (x + y) + z }; 1 add3 2 3" "eval ok: 6"
  , expectEval "partial three-argument brace function" "let add3 = { x, y, z ↦ (x + y) + z }; let add-one = 1 add3; 2 add-one 3" "eval ok: 6"
  , expectEval "second-term function definition" "let a add-two b = a + b; 1 add-two 2" "eval ok: 3"
  , expectEval "multi-scrutinee match" "data two { yea, nay } match yea, nay { yea, yea ↦ 1, yea, nay ↦ 2, nay, yea ↦ 3, nay, nay ↦ 4 }" "eval ok: 2"
  , expectEval "nested constructor pattern" "data two { yea, nay } data a option { none, a some } match nay some { yea some ↦ 1, nay some ↦ 2, none ↦ 0 }" "eval ok: 2"
  , expectEval "float from-string" "'0.25' from-string" "eval ok: 0.25"
  , expectEval "float arithmetic" "let half = '0.5' from-string; let quarter = '0.25' from-string; half + quarter" "eval ok: 0.75"
  , expectEval "handled from-string fail" "try 'oops' from-string { fail ↦ 0.0 }" "eval ok: 0.0"
  , expectEval "record update" "let person = [name = 'naoki', age = 35]; [= person, age = 36]" "eval ok: [age = 36, name = 'naoki']"
  , expectEval "record removal" "let person = [name = 'naoki', age = 35]; [= person, -age]" "eval ok: [name = 'naoki']"
  , expectEvalPrefix "record removal error" "let person = [name = 'naoki']; [= person, -age]" "eval error:"
  , expectEval "handled fail" "try 1 divide 0 { fail ↦ 9 }" "eval ok: 9"
  , expectEval "partial effect only runs when full" "try 0 (1 divide) { fail ↦ 7 }" "eval ok: 7"
  , expectEval "operation handler resumes" "effect ask { ask: integer → integer } try 10 ask { x ask ↦ (x + 1) resume }" "eval ok: 11"
  , expectEval "operation handler may skip resume" "effect ask { ask: integer → integer } try 10 ask { x ask ↦ x + 1 }" "eval ok: 11"
  , expectEval "multi-shot handler" "data 𝟙 { null } effect choice { choose: 𝟙 → integer } try (null choose) + (null choose) { choose ↦ (1 resume) + (2 resume) }" "eval ok: 12"
  , expectEval "sleep native" "data 𝟙 { null } 0 sleep" "eval ok: null"
  , expectEvalPrefix "constructor overapplication rejected" "data box { a box } 1 box 2" "eval error:"
  , expectEvalWithImports "imported call" "bring file.jbini; 2 inc" (Map.fromList [("file.jbini", "let inc = { x ↦ x + 1 };")]) "eval ok: 3"
  , expectEvalWithImports "qualified duplicate imports evaluate" "bring file.jbini; bring other.jbini; file@foo + other@foo" (Map.fromList [("file.jbini", "let foo = 1;"), ("other.jbini", "let foo = 2;")]) "eval ok: 3"
  , expectEvalPrefix "unhandled from-string fail" "'nope' from-string" "eval error:"
  ]

libraryAndSampleTests :: IO [String]
libraryAndSampleTests = do
  imports <- readLibraryImports
  runGroup "libraries and samples" $
    map (expectLibraryFileTypeOk imports) libraryImportFiles
      ++ map (expectSampleRunnable imports)
        [ "sample/asynchronous.jbini"
        , "sample/division-by-zero.jbini"
        , "sample/fizzbuzz.jbini"
        , "sample/lambda.jbini"
        , "sample/luck-game.jbini"
        , "sample/multishot.jbini"
        , "sample/scoreboard.jbini"
        ]

curriedPlus :: Expr
curriedPlus = EApply (EApply (EVar "+") (EInteger 1 :| [])) (EInteger 2 :| [])

expectParseOk :: String -> String -> Test
expectParseOk name source = pure $ case parse source of
  Right _ -> Nothing
  Left msg -> Just (name ++ ": parse failed: " ++ msg)

expectParseErr :: String -> String -> Test
expectParseErr name source = pure $ case parse source of
  Left _ -> Nothing
  Right _ -> Just (name ++ ": parse unexpectedly succeeded")

expectExpr :: String -> String -> Expr -> Test
expectExpr name source expected = pure $ case parse source of
  Right (Program [Let "_" Nothing actual]) | actual == expected -> Nothing
  Right actual -> Just (name ++ ": unexpected ast: " ++ show actual)
  Left msg -> Just (name ++ ": parse failed: " ++ msg)

expectEq :: String -> String -> String -> Test
expectEq name actual expected
  | expected `isSuffixOrExact` actual = pure Nothing
  | otherwise = pure (Just (name ++ ": expected " ++ show expected ++ ", got " ++ show actual))

expectTypeOk :: String -> String -> Test
expectTypeOk name source = expectEq name (check source) "type ok"

expectTypeErr :: String -> String -> Test
expectTypeErr name source = expectEq name (check source) "type error:"

expectTypeOkWithImports :: String -> String -> Map.Map String String -> Test
expectTypeOkWithImports name source imports = expectEq name (checkWithImports source imports) "type ok"

expectTypeErrWithImports :: String -> String -> Map.Map String String -> Test
expectTypeErrWithImports name source imports = expectEq name (checkWithImports source imports) "type error:"

expectRunnableOk :: String -> String -> Test
expectRunnableOk name source = expectEq name (checkRunnableWithImports source Map.empty) "type ok"

expectRunnableErr :: String -> String -> Test
expectRunnableErr name source = expectEq name (checkRunnableWithImports source Map.empty) "type error:"

expectEval :: String -> String -> String -> Test
expectEval name source expected = do
  actual <- evaluate source
  pure $ if actual == expected then Nothing else Just (name ++ ": expected " ++ show expected ++ ", got " ++ show actual)

expectEvalWithImports :: String -> String -> Map.Map String String -> String -> Test
expectEvalWithImports name source imports expected = do
  actual <- evaluateWithImports source imports
  pure $ if actual == expected then Nothing else Just (name ++ ": expected " ++ show expected ++ ", got " ++ show actual)

expectEvalPrefix :: String -> String -> String -> Test
expectEvalPrefix name source prefix = do
  actual <- evaluate source
  pure $ if prefix `isPrefixOf` actual then Nothing else Just (name ++ ": expected prefix " ++ show prefix ++ ", got " ++ show actual)

expectLibraryFileTypeOk :: Map.Map String String -> (FilePath, String) -> Test
expectLibraryFileTypeOk imports (path, _) = do
  source <- readFile path
  pure $ case checkWithImports source imports of
    "type ok" -> Nothing
    actual -> Just (path ++ ": expected type ok, got " ++ actual)

expectSampleRunnable :: Map.Map String String -> String -> Test
expectSampleRunnable imports path = do
  source <- readFile path
  pure $ case checkRunnableWithImports source imports of
    "type ok" -> Nothing
    actual -> Just (path ++ ": expected runnable type ok, got " ++ actual)

lookupTypeTest :: Test
lookupTypeTest = pure $ case contextOf "bring file.jbini;" imports of
  Left msg -> Just ("lookup imported value: " ++ msg)
  Right ctx -> case lookupEnv "foo" ctx of
    EnvFound (Forall _ _ ty) | showTy ty == "integer" -> Nothing
    other -> Just ("lookup imported value: unexpected lookup " ++ show other)
  where
    imports = Map.fromList [("file.jbini", "let foo: integer = 1;")]

canonicalTypeTest :: Test
canonicalTypeTest = pure $ case contextOf "bring file.jbini;" imports of
  Left msg -> Just ("canonical imported type: " ++ msg)
  Right ctx -> case canonicalTypeName "file-box" ctx of
    Just "file@file-box" -> Nothing
    other -> Just ("canonical imported type: unexpected result " ++ show other)
  where
    imports = Map.fromList [("file.jbini", "data file-box { box }")]

contextOf :: String -> Map.Map String String -> Either String TcContext
contextOf source imports = do
  program <- parse source
  case inferProgramContext program imports baseContext of
    TcOk ctx _ -> Right ctx
    TcErr msg -> Left msg

readLibraryImports :: IO (Map.Map String String)
readLibraryImports =
  Map.fromList <$> traverse readOne libraryImportFiles
  where
    readOne (path, name) = do
      source <- readFile path
      pure (name, source)

runGroup :: String -> [Test] -> IO [String]
runGroup group tests = do
  results <- sequence tests
  pure [group ++ "/" ++ msg | Just msg <- results]

isSuffixOrExact :: String -> String -> Bool
isSuffixOrExact expected actual =
  actual == expected || expected `isPrefixOf` actual || expected `isInfixOf` actual
