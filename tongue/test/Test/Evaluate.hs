module Test.Evaluate (group) where

import Data.List (stripPrefix)
import Data.Map.Strict qualified as Map
import Test.Harness (Group, Test, evalErr, evalErrWith, evalOk, evalOkWith)
import Test.Harness qualified as Harness
import Text.Read (readMaybe)
import Tung

group :: IO Group
group = do
  imports <- readLibraryImports
  Harness.group "evaluate" (map evalCase deterministic ++ map evalError errors ++ importedCases imports)

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
  , ("arguments run left to right", stopEffect ++ "let pair = { x, y | x × 10 + y }; try (1 stop) pair (2 stop) { x stop | x }", "eval ok: 1")
  , ("record fields run left to right", stopEffect ++ "try [first = 1 stop, second = 2 stop] { x stop | x }", "eval ok: 1")
  , ("record update evaluates base first", stopEffect ++ "try [= (1 stop), field = 2 stop] { x stop | x }", "eval ok: 1")
  , ("match scrutinees run left to right", stopEffect ++ "try match 1 stop, 2 stop { x, y | x } { x stop | x }", "eval ok: 1")
  , ("record update", "let person = [name = 'n', age = 1]; [= person, age = 2]", "eval ok: [age = 2, name = 'n']")
  , ("record access", "let person = [name = 'n', age = 1]; person@age", "eval ok: 1")
  , ("record removal", "let person = [name = 'n', age = 1]; [= person, - age]", "eval ok: [name = 'n']")
  , ("handled division failure", "try 1 ÷ 0 { fail | 9 }", "eval ok: 9")
  , ("operation payload", "try 1 ÷ 0 { return n | n to-string, message fail | message }", "eval ok: 'division by zero'")
  , ("return clause maps normal answer", "try 2 + 3 { return n | n to-string }", "eval ok: '5'")
  , ("partial call performs only when saturated", "try 0 (1 ÷) { fail | 7 }", "eval ok: 7")
  , ("resume once", "deed ask { integer ask: integer }; try 10 ask { x ask | (x + 1) resume }", "eval ok: 11")
  , ("resume zero times", "deed ask { integer ask: integer }; try 10 ask { x ask | x + 1 }", "eval ok: 11")
  , ("deep resume handles later operation", unitData ++ "deed choice { 𝟙 pick: integer }; try (null pick) + (null pick) { pick | 1 resume }", "eval ok: 2")
  , ("multi-shot resume", unitData ++ "deed choice { 𝟙 pick: integer }; try (null pick) + (null pick) { pick | (1 resume) + (2 resume) }", "eval ok: 12")
  , ("return runs on each resumed branch", unitData ++ "deed choice { 𝟙 pick: integer }; try null pick { return n | n + 10, pick | (1 resume) + (2 resume) }", "eval ok: 23")
  , ("effect-level clause catches either operation", duoEffect ++ "try null second { duo | null }", "eval ok: null")
  , ("nested handlers split operation coverage", duoEffect ++ "try (try null second { first | null }) { second | null }", "eval ok: null")
  , ("native sleep", unitData ++ "0 sleep", "eval ok: null")
  ]

errors :: [(String, String)]
errors =
  [ ("division failure is unhandled", "1 ÷ 0")
  , ("from-string failure is unhandled", "'not a float' from-string")
  , ("unknown value", "missing")
  , ("non-function application", "1 2")
  , ("constructor overapplication", "choose a box { a box }; 1 box 2")
  , ("missing record field", "let value = [x = 1]; value@y")
  , ("unknown record removal", "let person = [name = 'n']; [= person, - age]")
  , ("update of non-record", "[= 1, field = 2]")
  , ("non-exhaustive unchecked match", boolData ++ "match nay { yea | 1 }")
  , ("handler clause is outside its own handler", unitData ++ "deed pulse { 𝟙 pulse: 𝟙 }; try null pulse { pulse | null pulse }")
  , ("unhandled sibling operation escapes", duoEffect ++ "try null second { first | null }")
  ]

importedCases :: Map.Map String String -> [Test]
importedCases imports =
  [ evalOkWith "imported call" "bring file.tung; 2 inc" (Map.insert "file.tung" "show let inc = { x | x + 1 };" imports) "eval ok: 3"
  , evalOkWith "qualified duplicate values" "bring left.tung; bring right.tung; left@foo + right@foo" duplicateImports "eval ok: 3"
  , evalOkWith "value survives a named re-export" "bring middle.tung; value" reexportImports "eval ok: 7"
  , evalOkWith "whole bring re-exports its shown surface" "bring umbrella.tung; value" wholeReexportImports "eval ok: 7"
  , evalOkWith "shape fill survives an import" "bring identity.tung; 3 identity" identityImports "eval ok: 3"
  , evalOkWith "data constructor survives a named re-export" "bring data-middle.tung; 4 box" dataReexportImports "eval ok: (4 box)"
  , evalOkWith "qualified constructor pattern uses import alias" "bring alias.tung; match 4 alias@box { x alias@box | x }" (Map.insert "alias.tung" "show choose a box { a box };" imports) "eval ok: 4"
  , evalOkWith "effect operation survives a named re-export" "bring ask-middle.tung; try 3 ask { x ask | x }" effectReexportImports "eval ok: 3"
  , evalErrWith "private qualified value is absent at runtime" "bring private.tung; private@hidden" (Map.fromList [("private.tung", "let hidden = 1;")])
  , evalOkWith "imported fill does not shadow native integer order" "bring list.tung; 0 till 2" imports "eval ok: (0 (1 (2 empty .*) .*) .*)"
  , evalErrWith "missing import fails at runtime" "bring missing.tung;" Map.empty
  , evalErrWith "import cycle fails at runtime" "bring left.tung;" cyclicImports
  , evalOkWith "arctan range is nonnegative" "bring foreign.tung; 0.0 arctan-float -1.0" imports "eval ok: 4.71238898038469"
  , evalOkWith "synchronous fork and wait" "bring ground.tung; let work = { _ | 42 }; let task = work fork; task wait" imports "eval ok: 42"
  , randomRange imports
  ]
 where
  duplicateImports = Map.fromList [("left.tung", "show let foo = 1;"), ("right.tung", "show let foo = 2;")]
  reexportImports = Map.fromList [("base.tung", "show let value = 7;"), ("middle.tung", "bring base.tung; show base@value;")]
  wholeReexportImports = Map.fromList [("base.tung", "show let value = 7;"), ("umbrella.tung", "show bring base.tung;")]
  identityImports = Map.fromList [("identity.tung", "show shape a identity { a identity: a }; fill integer identity { let x identity = x };")]
  dataReexportImports =
    Map.fromList
      [ ("data-base.tung", "show choose a box { a box };")
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

evalCase :: (String, String, String) -> Test
evalCase (name, source, expected) = evalOk name source expected

evalError :: (String, String) -> Test
evalError = uncurry evalErr

unitData, boolData, optionData, stopEffect, duoEffect :: String
unitData = "choose 𝟙 { null }; "
boolData = "choose 𝟚 { yea, nay }; "
optionData = "choose a option { none, a some }; "
stopEffect = "deed e stop { e stop: a }; "
duoEffect = unitData ++ "deed duo { 𝟙 first: 𝟙, 𝟙 second: 𝟙 }; "
