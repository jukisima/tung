-- checked interactive evaluation, strict order, dictionaries, and handlers.
module Test.Evaluate (group) where

import Control.Exception (bracket)
import Data.List (stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment qualified as Environment
import Test.Harness (Group, Test, evalErr, evalOk, evalOkWith, evalTypeErr, evalTypeErrWith)
import Test.Harness qualified as Harness
import Text.Read (readMaybe)
import Tung

group :: IO Group
group = do
  imports <- readBookhoardImports
  Harness.group "evaluate" (map evalCase deterministic ++ map runtimeErrorCase runtimeErrors ++ map typeErrorCase typeErrors ++ importedCases imports)

deterministic :: [(String, String, String)]
deterministic =
  [ ("integer arithmetic", "(1 + 2) × 3", "eval ok: 9")
  , ("float arithmetic", "let a = '0.5' from-string; let b = '0.25' from-string; a + b", "eval ok: 0.75")
  , ("curried partial application", "let add = { a, b | a + b }; let add-one = 1 add; 2 add-one", "eval ok: 3")
  , ("three curried arguments", "let add = { a, b, c | (a + b) + c }; 1 add 2 3", "eval ok: 6")
  , ("second-position function header", "let a add b = a + b; 1 add 2", "eval ok: 3")
  , ("dollar function segment", "let f = { x | x + 1 }; let h = { x, y, z | (x + y) × z }; 1 f $h 2 3", "eval ok: 12")
  , ("lexical closure", "let x = 1; let f = { _ | x }; (let x = 2; 0 f)", "eval ok: 1")
  , ("multi-scrutinee match", boolData ++ "match yea, nay { yea, yea | 1, yea, nay | 2, nay, yea | 3, nay, nay | 4 }", "eval ok: 2")
  , ("nested constructor match", boolData ++ optionData ++ "match nay some { yea some | 1, nay some | 2, none | 0 }", "eval ok: 2")
  , ("first matching arm wins", "match 1 { x | x, _ | 2 }", "eval ok: 1")
  , ("unchosen arm is not evaluated", boolData ++ "match yea { yea | 1, nay | 1 ÷ 0 }", "eval ok: 1")
  , ("let right side is strict", stopEffect ++ "try (let x = 1 stop; 2 stop) { x stop | x }", "eval ok: 1")
  , ("function expression runs before arguments", stopEffect ++ "try 0 (1 stop) (2 stop) { x stop | x }", "eval ok: 1")
  , ("arguments run left to right", stopEffect ++ "let choose = { x, _ | x }; try (1 stop) choose (2 stop) { return x | x, x stop | x }", "eval ok: 1")
  , ("record fields run left to right", stopEffect ++ "try [first = 1 stop, second = 2 stop] { return _ | 0, x stop | x }", "eval ok: 1")
  , ("record update evaluates base first", stopEffect ++ "let base = [field = 0]; try [= (let _ = 1 stop; base), field = 2 stop] { return _ | 0, x stop | x }", "eval ok: 1")
  , ("match scrutinees run left to right", stopEffect ++ "try match 1 stop, 2 stop { x, y | x } { x stop | x }", "eval ok: 1")
  , ("record update", "let person = [name = 'n', age = 1]; [= person, age = 2]", "eval ok: [age = 2, name = 'n']")
  , ("record access", "let person = [name = 'n', age = 1]; person@age", "eval ok: 1")
  , ("record removal", "let person = [name = 'n', age = 1]; [= person, - age]", "eval ok: [name = 'n']")
  , ("handled division failure", "try 1 ÷ 0 { fail | 9 }", "eval ok: 9")
  , ("operation payload", "deed e fail { e fail: a }; try 1 ÷ 0 { return n | n to-string, message fail | message }", "eval ok: 'division by zero'")
  , ("return clause maps normal answer", "try 2 + 3 { return n | n to-string }", "eval ok: '5'")
  , ("partial call performs only when saturated", "try 0 (1 ÷) { fail | 7 }", "eval ok: 7")
  , ("resume once", "deed ask { integer ask: integer }; try 10 ask { x ask | (x + 1) resume }", "eval ok: 11")
  , ("resume zero times", "deed ask { integer ask: integer }; try 10 ask { x ask | x + 1 }", "eval ok: 11")
  , ("deep resume handles later operation", unitData ++ "deed choice { 𝟙 pick: integer }; try (null pick) + (null pick) { pick | 1 resume }", "eval ok: 2")
  , ("multi-shot resume", unitData ++ "deed choice { 𝟙 pick: integer }; try (null pick) + (null pick) { pick | (1 resume) + (2 resume) }", "eval ok: 12")
  , ("return runs on each resumed branch", unitData ++ "deed choice { 𝟙 pick: integer }; try null pick { return n | n + 10, pick | (1 resume) + (2 resume) }", "eval ok: 23")
  , ("polymorphic state get", unitData ++ "deed a state { 𝟙 get: a, a set: 𝟙 }; try null get { get | 4 resume, x set | null resume }", "eval ok: 4")
  , ("effect-level clause catches either operation", duoEffect ++ "try null second { duo | null }", "eval ok: null")
  , ("nested handlers split operation coverage", duoEffect ++ "try (try null second { first | null }) { second | null }", "eval ok: null")
  , ("native sleep", unitData ++ "0 sleep", "eval ok: null")
  , ("shape laws are erased", "shape a identity { a identity: a; law (x: a): x identity ~ x }; fill integer identity { let x identity = x }; 2 identity", "eval ok: 2")
  , ("result-only member uses expected type", "shape a origin { origin: a }; fill integer origin { let origin = 7 }; let value: integer = origin; value", "eval ok: 7")
  , ("different fills dispatch by inferred type", "shape a label { a label: string }; fill integer label { let _ label = 'integer' }; fill string label { let _ label = 'string' }; 'x' label", "eval ok: 'string'")
  , ("graith function receives its caller dictionary", "shape a label { a label: string }; fill integer label { let _ label = 'integer' }; fill string label { let _ label = 'string' }; graith a label let (x: a) labelled: string = x label; 1 labelled", "eval ok: 'integer'")
  , ("child dictionary uses a direct parent fill", "shape a parent { a parent: a }; graith a parent shape a child { a child: a }; fill integer parent { let x parent = x }; fill integer child { let x child = x parent }; 4 child", "eval ok: 4")
  ]

runtimeErrors :: [(String, String)]
runtimeErrors =
  [ ("division failure is unhandled", "1 ÷ 0")
  , ("from-string failure is unhandled", "'not a float' from-string")
  , ("handler clause is outside its own handler", unitData ++ "deed pulse { 𝟙 pulse: 𝟙 }; try null pulse { pulse | null pulse }")
  , ("unhandled sibling operation escapes", duoEffect ++ "try null second { first | null }")
  ]

typeErrors :: [(String, String)]
typeErrors =
  [ ("unknown value", "missing")
  , ("non-function application", "1 2")
  , ("constructor overapplication", "kin a box { a box }; 1 box 2")
  , ("missing record field", "let value = [x = 1]; value@y")
  , ("unknown record removal", "let person = [name = 'n']; [= person, - age]")
  , ("update of non-record", "[= 1, field = 2]")
  , ("non-exhaustive match", boolData ++ "match nay { yea | 1 }")
  ]

importedCases :: Map.Map String String -> [Test]
importedCases imports =
  [ evalOkWith "imported call" "bring file.tung; 2 inc" (Map.insert "file.tung" "show let inc = { x | x + 1 };" imports) "eval ok: 3"
  , evalOkWith "qualified duplicate values" "bring left.tung; bring right.tung; left@foo + right@foo" duplicateImports "eval ok: 3"
  , evalOkWith "shared diamond import" "bring left.tung; bring right.tung; left@left + right@right" diamondImports "eval ok: 5"
  , evalOkWith "diamond import keeps one fill identity" "bring left.tung; bring right.tung; 3 left@identity" fillDiamondImports "eval ok: 3"
  , evalOkWith "qualified shapes from different modules stay distinct" "bring left.tung; bring right.tung; 3 left@identity" distinctShapeImports "eval ok: 4"
  , evalOkWith "value survives a named re-export" "bring middle.tung; value" reexportImports "eval ok: 7"
  , evalOkWith "shape fill survives an import" "bring identity.tung; 3 identity" identityImports "eval ok: 3"
  , evalOkWith "data constructor survives a named re-export" "bring data-middle.tung; 4 box" dataReexportImports "eval ok: (4 box)"
  , evalOkWith "qualified constructor pattern uses import alias" "bring alias.tung; match 4 alias@box { x alias@box | x }" (Map.insert "alias.tung" "show kin a box { a box };" imports) "eval ok: 4"
  , evalOkWith "effect operation survives a named re-export" "bring ask-middle.tung; try 3 ask { x ask | x }" effectReexportImports "eval ok: 3"
  , evalTypeErrWith "private qualified value is rejected before runtime" "bring private.tung; private@hidden" (Map.fromList [("private.tung", "let hidden = 1;")])
  , evalOkWith "imported fill does not shadow native integer order" "bring data/list.tung; 0 till 2" imports "eval ok: (0 (1 (2 empty .*) .*) .*)"
  , evalOkWith "list map pipeline keeps the list functor" "bring ground.tung; bring data/list.tung; 0 till 2 $ map { x | x + 1 } $ map to-string" imports "eval ok: ('1' ('2' ('3' empty .*) .*) .*)"
  , evalOkWith "pattern binders do not become constructor patterns" "bring ground.tung; bring data/list.tung; kin bit { off }; 0 till 2 $ map { _ | off } $ map { _ | 1 }" imports "eval ok: (1 (1 (1 empty .*) .*) .*)"
  , evalOkWith "control branch suspends actions" "bring ground.tung; nay branch { _ | 1 } { _ | 2 }" imports "eval ok: 2"
  , evalOkWith "option maybe maps present value" "bring ground.tung; bring data/option.tung; (3 some) maybe 0 { x | x + 1 }" imports "eval ok: 4"
  , evalOkWith "sum either consumes selected side" "bring ground.tung; bring data/sum.tung; (2 inject₁) either { x | x + 1 } { y | y × 3 }" imports "eval ok: 6"
  , evalOkWith "product bimap maps both fields" "bring ground.tung; bring data/product.tung; (1 ∏ 2) bimap { x | x + 1 } { y | y × 3 }" imports "eval ok: (2 6 ∏)"
  , evalOkWith "applicative lift2 for option" "bring ground.tung; bring data/option.tung; (1 some) lift₂ (2 some) +" imports "eval ok: (3 some)"
  , evalOkWith "monad void for option" "bring data/option.tung; bring collection/monad.tung; (1 some) void" imports "eval ok: (null some)"
  , evalOkWith "list take after drop" "bring data/list.tung; ((0 till 5) drop 2) take 2" imports "eval ok: (2 (3 empty .*) .*)"
  , evalOkWith "list find returns first match" "bring ground.tung; bring data/list.tung; (0 till 5) find { x | 3 ≤ x }" imports "eval ok: (3 some)"
  , evalTypeErrWith "missing import is rejected before runtime" "bring missing.tung;" Map.empty
  , evalTypeErrWith "import cycle is rejected before runtime" "bring left.tung;" cyclicImports
  , evalOkWith "arctan range is nonnegative" "bring _foreign.tung; 0.0 arctan-float -1.0" imports "eval ok: 4.71238898038469"
  , evalOkWith "synchronous fork and wait" "bring ground.tung; let work = { _ | 42 }; let task = work fork; task wait" imports "eval ok: 42"
  , randomRange imports
  , systemArguments imports
  , systemEnvironment imports
  , unixTime imports
  ]
 where
  duplicateImports = Map.fromList [("left.tung", "show let foo = 1;"), ("right.tung", "show let foo = 2;")]
  diamondImports =
    Map.fromList
      [ ("left.tung", "bring shared.tung; show let left = shared@base + 1;")
      , ("right.tung", "bring shared.tung; show let right = shared@base + 2;")
      , ("shared.tung", "show let base = 1;")
      ]
  fillDiamondImports =
    Map.fromList
      [ ("left.tung", "bring shared.tung; show shared@identity;")
      , ("right.tung", "bring shared.tung;")
      , ("shared.tung", "show shape a identity { a identity: a }; fill integer identity { let x identity = x };")
      ]
  distinctShapeImports =
    Map.fromList
      [ ("left.tung", "show shape a identity { a identity: a }; fill integer identity { let x identity = x + 1 };")
      , ("right.tung", "show shape a identity { a identity: a }; fill integer identity { let x identity = x + 2 };")
      ]
  reexportImports = Map.fromList [("base.tung", "show let value = 7;"), ("middle.tung", "bring base.tung; show base@value;")]
  identityImports = Map.fromList [("identity.tung", "show shape a identity { a identity: a }; fill integer identity { let x identity = x };")]
  dataReexportImports =
    Map.fromList
      [ ("data-base.tung", "show kin a box { a box };")
      , ("data-middle.tung", "bring data-base.tung; show-ilk data-base@box; show data-base@box;")
      ]
  effectReexportImports =
    Map.fromList
      [ ("ask-base.tung", "show deed ask { integer ask: integer };")
      , ("ask-middle.tung", "bring ask-base.tung; show ask-base@ask;")
      ]
  cyclicImports = Map.fromList [("left.tung", "bring right.tung;"), ("right.tung", "bring left.tung;")]

randomRange :: Map.Map String String -> Test
randomRange imports = do
  actual <- evaluateWithImports "bring ground.tung; null random" imports
  pure $ case stripPrefix "eval ok: " actual >>= readMaybe of
    Just value | value >= (0 :: Double) && value < 1 -> Nothing
    _ -> Just ("random result is in [0, 1): got " ++ actual)

systemArguments :: Map.Map String String -> Test
systemArguments imports = do
  actual <- evaluateWithArgsAndImports ["north", "two words"] "bring ground.tung; null arguments" imports
  pure $ if actual == "eval ok: ('north' ('two words' empty .*) .*)" then Nothing else Just ("system arguments: " ++ actual)

systemEnvironment :: Map.Map String String -> Test
systemEnvironment imports = bracket (Environment.lookupEnv name) restore $ \_ -> do
  Environment.setEnv name "seen"
  actual <- evaluateWithImports ("bring ground.tung; '" ++ name ++ "' environment") imports
  pure $ if actual == "eval ok: ('seen' some)" then Nothing else Just ("system environment: " ++ actual)
 where
  name = "TUNG_TEST_ENVIRONMENT"
  restore Nothing = Environment.unsetEnv name
  restore (Just value) = Environment.setEnv name value

unixTime :: Map.Map String String -> Test
unixTime imports = do
  before <- floor <$> getPOSIXTime
  actual <- evaluateWithImports "bring ground.tung; null unix-time" imports
  after <- floor <$> getPOSIXTime
  pure $ case stripPrefix "eval ok: " actual >>= readMaybe of
    Just value | value >= (before :: Int) && value <= after -> Nothing
    _ -> Just ("unix time is in the host clock range: got " ++ actual)

evalCase :: (String, String, String) -> Test
evalCase (name, source, expected) = evalOk name source expected

runtimeErrorCase :: (String, String) -> Test
runtimeErrorCase = uncurry evalErr

typeErrorCase :: (String, String) -> Test
typeErrorCase = uncurry evalTypeErr

unitData, boolData, optionData, stopEffect, duoEffect :: String
unitData = "kin 𝟙 { null }; "
boolData = "kin 𝟚 { yea, nay }; "
optionData = "kin a option { none, a some }; "
stopEffect = "deed e stop { e stop: a }; "
duoEffect = unitData ++ "deed duo { 𝟙 first: 𝟙, 𝟙 second: 𝟙 }; "
