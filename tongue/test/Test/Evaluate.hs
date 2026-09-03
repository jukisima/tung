-- checked interactive evaluation, strict order, dictionaries, and handlers.
module Test.Evaluate (group) where

import Control.Exception (bracket)
import Data.List (stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment qualified as Environment
import Test.Harness (Group, Test, evalErr, evalOk, evalOkWith, evalTypeErr, evalTypeErrWith)
import Test.Harness qualified as Harness
import Test.QuickCheck qualified as QuickCheck
import Text.Read (readMaybe)
import Tung

group :: IO Group
group = do
  imports <- readBookhoardImports
  Harness.group "evaluate" (map evalCase deterministic ++ map runtimeErrorCase runtimeErrors ++ map typeErrorCase typeErrors ++ importedCases imports ++ handlerProperties)

handlerProperties :: [Test]
handlerProperties =
  [ Harness.propertyTest "property: a handler may call eftgin any generated number of times" $
      QuickCheck.forAll (QuickCheck.resize 6 (QuickCheck.listOf (QuickCheck.chooseInt (0, 9)))) \answers ->
        QuickCheck.ioProperty do
          actual <- evaluate (handlerSource answers)
          let expected = "eval ok: " ++ show (sum answers)
          pure (QuickCheck.counterexample (handlerSource answers ++ "\n" ++ actual) (actual QuickCheck.=== expected))
  ]

handlerSource :: [Int] -> String
handlerSource answers =
  unitData
    ++ "deed choice { 𝟙 pick: integer } yield try only pick { pick @ "
    ++ eftginSum answers
    ++ " }"

eftginSum :: [Int] -> String
eftginSum [] = "0"
eftginSum [answer] = show answer ++ " eftgin"
eftginSum (answer : answers) = "(" ++ show answer ++ " eftgin) + (" ++ eftginSum answers ++ ")"

deterministic :: [(String, String, String)]
deterministic =
  [ ("integer arithmetic", "yield (1 + 2) × 3", "eval ok: 9")
  , ("left-associated sequence", "yield <(-, 10, 3, 2)", "eval ok: 5")
  , ("right-associated sequence", "yield >(-, 10, 3, 2)", "eval ok: 9")
  , ("term type ascription is erased before evaluation", "yield (1 + 2: integer)", "eval ok: 3")
  , ("ascribed anonymous function remaineþ recursive", "let factorial = ({ 0 @ 1, n @ n × ((n - 1) factorial) }: integer → integer) yield 5 factorial", "eval ok: 120")
  , ("integer arithmetic is arbitrary precision", "yield 999999999999999999999999999999 + 1", "eval ok: 1000000000000000000000000000000")
  , ("fremmed key selecteþ a host function independently of the let name", "let plus: integer → integer → integer = 'add-integer' fremmed yield 1 plus 2", "eval ok: 3")
  , ("unicode text", "yield 'λ字'", "eval ok: 'λ字'")
  , ("unicode control rendering useþ decimal terminator", "yield `\\1;", "eval ok: `\\1;")
  , ("float arithmetic", "let a = '0.5' from-text let b = '0.25' from-text yield a + b", "eval ok: 0.75")
  , ("float division useþ the division sign", "yield 6.0 ÷ 4.0", "eval ok: 1.5")
  , ("float division by zero followeþ ieee", "yield 1.0 ÷ 0.0", "eval ok: Infinity")
  , ("float equality ignoreþ literal spelling", "ilk 𝟚 { yea, nay } yield 1.0 ≡ 1.00", "eval ok: yea")
  , ("standard boolean ABI ignoreþ constructor declaration order", "ilk 𝟚 { nay, yea } yield 1 ≡ 1", "eval ok: yea")
  , ("native booleans match exact standard local constructors", "ilk 𝟚 { yea, nay } yield match 1.0 ≡ 1.00 { yea @ 1, nay @ 0 }", "eval ok: 1")
  , ("native unit matcheþ the exact standard local constructor", "ilk 𝟙 { only } yield match 0 sleep { only @ 1 }", "eval ok: 1")
  , ("alpha-renamed standard list receiveþ a host list", "ilk x list { empty, x .* (x list) } let convert: text → unicode list = 'text-to-list' fremmed yield 'a' convert", "eval ok: (`a empty .*)")
  , ("alpha-renamed standard product receiveþ a host product", "ilk x ∏ y { x ∏ y } let divide: integer → integer → integer ∏ integer ! text fail = 'divide-remainder-integer' fremmed yield 5 divide 2", "eval ok: (2 1 ∏)")
  , ("nonstandard lookalike data remain nominally usable", "ilk 𝟚 { yes, no } yield match yes { yes @ 1, no @ 0 }", "eval ok: 1")
  , ("a constructor and an ordinary term retain distinct runtime identities", "ilk token { box } let token.box = 7 yield box", "eval ok: box")
  , ("float ordering is numeric", "ilk 𝟚 { yea, nay } yield 2.0 ≤ 10.0", "eval ok: yea")
  , ("curried partial application", "let a add b = a + b let add-one = 1 add yield 2 add-one", "eval ok: 3")
  , ("three curried arguments", "let a add b c = (a + b) + c yield 1 add 2 3", "eval ok: 6")
  , ("local recursive function", "ilk natural { zero, natural suc } let count = (let loop = { zero @ 0, n suc @ 1 + (n loop) } yield loop) yield ((zero suc) suc) count", "eval ok: 2")
  , ("evidence abstraction preserveþ local recursive capture", "frame a ident { let a ident: a } yield (let loop = { x @ (x loop) ident } yield 1)", "eval ok: 1")
  , ("second-position function header", "let a add b = a + b yield 1 add 2", "eval ok: 3")
  , ("dollar function segment", "let x f = x + 1 let x h y z = (x + y) × z yield 1 f $h 2 3", "eval ok: 12")
  , ("lexical closure", "let x = 1 let _ f = x yield (let x = 2 yield 0 f)", "eval ok: 1")
  , ("sequential shadowing preserveþ each preceding local", "yield (let value = 1 let value = value + 1 let value = value + 1 yield value)", "eval ok: 3")
  , ("nested same-text pattern binders remain distinct", "yield match 1 { value @ (match 2 { value @ value }) + value }", "eval ok: 3")
  , ("multi-scrutinee match", boolData ++ "yield match yea, nay { yea, yea @ 1, yea, nay @ 2, nay, yea @ 3, nay, nay @ 4 }", "eval ok: 2")
  , ("nested constructor match", boolData ++ optionData ++ "yield match nay some { yea some @ 1, nay some @ 2, none @ 0 }", "eval ok: 2")
  , ("first matching arm winneþ", boolData ++ "yield match yea { yea @ 1, _ @ 2 }", "eval ok: 1")
  , ("integer match patterns", "yield match -1 { 0 @ 10, -1 @ 20, _ @ 30 }", "eval ok: 20")
  , ("integer patterns fall through", "yield match 2 { 0 @ 10, 1 @ 20, _ @ 30 }", "eval ok: 30")
  , ("alternative patterns share one result", "yield match 2 { 0 | 1 | 2 @ 7, _ @ 0 }", "eval ok: 7")
  , ("alternative constructor patterns share bindings", "ilk a choice { a left, a right } yield match 2 right { x left | x right @ x }", "eval ok: 2")
  , ("integer function patterns", "let classify: integer → integer = { 0 @ 10, 1 @ 20, _ @ 30 } yield 1 classify", "eval ok: 20")
  , ("text match patterns", "yield match 'yes' { 'yes' @ 1, _ @ 0 }", "eval ok: 1")
  , ("text patterns fall through", "yield match 'no' { 'yes' @ 1, _ @ 0 }", "eval ok: 0")
  , ("text function patterns", "let classify: text → integer = { 'yes' @ 1, _ @ 0 } yield 'yes' classify", "eval ok: 1")
  , ("unchosen arm is not evaluated", boolData ++ "let _ ignore = 1 yield match yea { yea @ 1, nay @ (1 % 0) ignore }", "eval ok: 1")
  , ("let right side is strict", stopEffect ++ "yield try (let x = 1 stop yield 2 stop) { x stop @ x }", "eval ok: 1")
  , ("function expression runneþ before arguments", stopEffect ++ "yield try 0 (1 stop) (2 stop) { x stop @ x }", "eval ok: 1")
  , ("arguments run left to right", stopEffect ++ "let x choose _ = x yield try (1 stop) choose (2 stop) { yield x @ x, x stop @ x }", "eval ok: 1")
  , ("record fields run left to right", stopEffect ++ "yield try r(first = 1 stop, second = 2 stop) { yield _ @ 0, x stop @ x }", "eval ok: 1")
  , ("record update evaluateþ base first", stopEffect ++ "let base = r(field = 0) yield try r(= (let _ = 1 stop yield base), field = 2 stop) { yield _ @ 0, x stop @ x }", "eval ok: 1")
  , ("match scrutinees run left to right", stopEffect ++ "yield try match 1 stop, 2 stop { x, y @ x } { x stop @ x }", "eval ok: 1")
  , ("record update", "let person = r(name = 'n', age = 1) yield r(= person, age = 2)", "eval ok: r(age = 2, name = 'n')")
  , ("record access", "let person = r(name = 'n', age = 1) yield person@age", "eval ok: 1")
  , ("record removal", "let person = r(name = 'n', age = 1) yield r(= person, - age)", "eval ok: r(name = 'n')")
  , ("fremmed arity followeþ a transparent alias", "let-ilk binary = integer → integer → integer let plus: binary = 'add-integer' fremmed yield 1 plus 2", "eval ok: 3")
  , ("effect arity followeþ a transparent result alias", "let-ilk unary = integer → integer deed addition { integer add: unary } yield try 1 add 2 { x add y @ x + y }", "eval ok: 3")
  , ("handled division failure", "let _ ignore = 1 yield try (1 % 0) ignore { fail @ 9 }", "eval ok: 9")
  , ("operation payload", "deed e fail { e fail: a } let _ ignore = 'ok' yield try (1 % 0) ignore { yield text @ text, message fail @ message }", "eval ok: 'division by zero'")
  , ("a concrete-result fail lookalike remaineþ nominal", "ilk payload { payload } deed e fail { e fail: payload } yield try 'message' fail { _ fail @ payload }", "eval ok: payload")
  , ("yield clause mapeþ normal answer", "yield try 2 + 3 { yield n @ n to-text }", "eval ok: '5'")
  , ("partial call performeþ only when saturated", "let _ ignore = 0 yield try (0 (1 %)) ignore { fail @ 7 }", "eval ok: 7")
  , ("eftgin once", "deed ask { integer ask: integer } yield try 10 ask { x ask @ (x + 1) eftgin }", "eval ok: 11")
  , ("eftgin zero times", "deed ask { integer ask: integer } yield try 10 ask { x ask @ x + 1 }", "eval ok: 11")
  , ("deep eftgin handleþ later operation", unitData ++ "deed choice { 𝟙 pick: integer } yield try (only pick) + (only pick) { pick @ 1 eftgin }", "eval ok: 2")
  , ("multi-shot eftgin", unitData ++ "deed choice { 𝟙 pick: integer } yield try (only pick) + (only pick) { pick @ (1 eftgin) + (2 eftgin) }", "eval ok: 12")
  , ("yield runneþ on each continued branch", unitData ++ "deed choice { 𝟙 pick: integer } yield try only pick { yield n @ n + 10, pick @ (1 eftgin) + (2 eftgin) }", "eval ok: 23")
  , ("polymorphic state get", unitData ++ "deed a state { 𝟙 get: a, a set: 𝟙 } yield try only get { get @ 4 eftgin, x set @ only eftgin }", "eval ok: 4")
  , ("effect-level clause catches either operation", duoEffect ++ "yield try only second { duo @ only }", "eval ok: only")
  , ("nested handlers split operation coverage", duoEffect ++ "yield try (try only second { first @ only }) { second @ only }", "eval ok: only")
  , ("native sleep", unitData ++ "yield 0 sleep", "eval ok: only")
  , ("a custom system operation is not host-dispatched", unitData ++ "deed system { 𝟙 arguments: integer } yield try only arguments { arguments @ 42 }", "eval ok: 42")
  , ("an effect operation and an ordinary term retain distinct runtime identities", "deed s { integer op: integer } let s.op = 7 yield try 1 op { x op @ x }", "eval ok: 1")
  , ("frame laws are erased", "frame a identity { let a identity: a law (x: a): x identity ~ x } fill integer identity { let x identity = x } yield 2 identity", "eval ok: 2")
  , ("a frame selector and a constructor retain distinct runtime identities", "frame a s { let a op: a } fill integer s { let x op = x } ilk s { op } yield 1 op", "eval ok: 1")
  , ("result-only member useþ expected type", "frame a origin { let origin: a } fill integer origin { let origin = 7 } let value: integer = origin yield value", "eval ok: 7")
  , ("different fills dispatch by inferred type", "frame a label { let a label: text } fill integer label { let _ label = 'integer' } fill text label { let _ label = 'text' } yield 'x' label", "eval ok: 'text'")
  , ("graiþ function receiveþ its caller dictionary", "frame a label { let a label: text } fill integer label { let _ label = 'integer' } fill text label { let _ label = 'text' } graiþ a label let (x: a) labelled: text = x label yield 1 labelled", "eval ok: 'integer'")
  , ("child dictionary useþ a direct parent fill", "frame a parent { let a parent: a } graiþ a parent frame a child { let a child: a } fill integer parent { let x parent = x } fill integer child { let x child = x parent } yield 4 child", "eval ok: 4")
  , ("fill may precede its resolved frame declaration", "fill integer identity { let x identity = x } frame a identity { let a identity: a } yield 3 identity", "eval ok: 3")
  , ("an exact primitive frame schema receiveþ its host fill", "frame a add { let a + a: a } yield 1 + 2", "eval ok: 3")
  , ("fill graiþ remaineþ an external dictionary", "frame a combine { let a combine a: a } fill integer combine { let x combine y = x + y } ilk a box { a box } graiþ a combine fill (a box) combine { let (x box) combine (y box) = (x combine y) box } yield (1 box) combine (2 box)", "eval ok: (3 box)")
  ]

runtimeErrors :: [(String, String)]
runtimeErrors =
  [ ("division failure is unhandled", "yield 1 % 0")
  , ("from-text failure is unhandled", "yield 'not a float' from-text")
  , ("handler clause is outside its own handler", unitData ++ "deed pulse { 𝟙 pulse: 𝟙 } yield try only pulse { pulse @ only pulse }")
  , ("unhandled sibling operation escapeþ", duoEffect ++ "yield try only second { first @ only }")
  ]

typeErrors :: [(String, String)]
typeErrors =
  [ ("unknown value", "yield missing")
  , ("non-function application", "yield 1 2")
  , ("constructor overapplication", "ilk a box { a box } yield 1 box 2")
  , ("missing record field", "let value = r(x = 1) yield value@y")
  , ("unknown record removal", "let person = r(name = 'n') yield r(= person, - age)")
  , ("update of non-record", "yield r(= 1, field = 2)")
  , ("non-exhaustive match", boolData ++ "yield match nay { yea @ 1 }")
  , ("nonstandard local boolean cannot receive a native boolean", "ilk 𝟚 { yes, no } yield match 1 ≡ 1 { yes @ 1, no @ 0 }")
  , ("nonstandard local unit cannot receive native unit", "ilk 𝟙 { done } yield match 0 sleep { done @ 1 }")
  , ("a primitive namesake wiþ an incompatible schema hath no host fill", "frame a add { let a magic: a } graiþ a add let (x: a) apply: a = x magic yield 1 apply")
  , ("owned declaration components cannot collapse through qualification", "deed left { integer right.op: integer } deed left.right { integer op: integer } let x choose y = y yield try (try (2 op) choose (1 right.op) { x right.op @ 7 }) { x op @ 9 }")
  ]

importedCases :: Map.Map String String -> [Test]
importedCases imports =
  [ evalOkWith "imported call" "use file.tung yield 2 inc" (Map.insert "file.tung" "show let x inc = x + 1" imports) "eval ok: 3"
  , evalOkWith "a use declaration is initialised before a preceding dependent let" "let answer = dep.value use dep.tung yield answer" (Map.insert "dep.tung" "show let value = 7" imports) "eval ok: 7"
  , evalOkWith "a local and its imported predecessor retain distinct identities" "use source.tung let value = value + 1 yield value + source.value" (Map.insert "source.tung" "show let value = 2" imports) "eval ok: 5"
  , evalOkWith "a local value doth not hide an imported constructor pattern" "use ilk/two.tung let yea: 𝟚 = two.yea yield match yea { yea @ 1, nay @ 0 }" imports "eval ok: 1"
  , evalOkWith "an incompatible local value doth not hide an imported constructor pattern" "use ilk/two.tung let yea = 0 yield match two.yea { yea @ 1, nay @ 0 }" imports "eval ok: 1"
  , evalOkWith "an exact standard ABI is stable when imported" "use truth.tung yield truth.truth" (Map.insert "truth.tung" "show ilk 𝟚 { yea, nay } show let truth: 𝟚 = 1 ≡ 1" imports) "eval ok: yea"
  , evalOkWith "default use alias workeþ for values, frames, and constructor patterns" "use ilk/item.tung let value: item.item = item.empty yield match (value item.identity) { item.item @ 1 }" (Map.insert "ilk/item.tung" "show ilk item { item } show frame a identity { let a identity: a } fill item identity { let x identity = x } show let empty: item = item" imports) "eval ok: 1"
  , evalOkWith "use alias workeþ for effect operations and handlers" "use effect/ask.tung a yield try 3 a.ask { x a.ask @ x }" (Map.insert "effect/ask.tung" "show deed ask { integer ask: integer }" imports) "eval ok: 3"
  , evalOkWith "qualified handler target distinguishþ effect and operation names" "use effect/query.tung q yield try 3 q.ask { x q.ask @ x }" (Map.insert "effect/query.tung" "show deed query { integer ask: integer }" imports) "eval ok: 3"
  , evalOkWith "an imported module handleþ its own operation identity" "use query.tung yield value" (Map.insert "query.tung" "show deed query { integer ask: integer } show let value: integer = try 3 ask { x ask @ x }" imports) "eval ok: 3"
  , evalOkWith "an imported module handleþ its whole effect identity" "use query.tung yield value" (Map.insert "query.tung" "show deed query { integer ask: integer } show let value: integer = try 3 ask { query @ 7 }" imports) "eval ok: 7"
  , evalOkWith
      "an incompatible primitive-frame lookalike retaineþ its declared selector"
      "use algebra/arithmetic/semiring.tung yield 1 magic"
      (Map.insert "algebra/arithmetic/semiring.tung" "show frame a add { let a magic: a } fill integer add { let x magic = x }" imports)
      "eval ok: 1"
  , evalTypeErrWith "same-spelled effects in extension-sharing paths stay distinct" "use a.tung one use a.extra.tung two yield try 3 two.ask { x one.ask @ x }" samePrefixEffectImports
  , evalTypeErrWith "same-spelled effects beneath dotted directories stay distinct" "use pkg.one/query.tung one use pkg.two/query.tung two yield try 3 two.ask { x one.ask @ x }" dottedDirectoryEffectImports
  , evalOkWith "derived frame operations" "use ground.tung yield (1 < 2) ∧ (1 ≢ 2)" imports "eval ok: yea"
  , evalOkWith "false implieþ false" "use ground.tung yield nay ≤ nay" imports "eval ok: yea"
  , evalOkWith "true doth not imply false" "use ground.tung yield yea ≤ nay" imports "eval ok: nay"
  , evalOkWith "euclidean division returneþ quotient and remainder for a negative divisor" "use ground.tung use ilk/product.tung yield 5 % -3" imports "eval ok: (-1 2 ∏)"
  , evalOkWith "euclidean division returneþ quotient and remainder for a negative dividend" "use ground.tung use ilk/product.tung yield -5 % 3" imports "eval ok: (-2 1 ∏)"
  , evalOkWith "euclidean quotient and remainder reconstruct the dividend" "use ground.tung use ilk/product.tung let dividend = -5 let divisor = -3 yield match dividend % divisor { quotient ∏ rest @ (quotient × divisor) + rest }" imports "eval ok: -5"
  , evalOkWith "euclidean division faileþ at zero" "use ground.tung let _ ignore = 1 yield try (1 % 0) ignore { fail @ 9 }" imports "eval ok: 9"
  , evalOkWith "three-way comparison distinguishes each result" "use ground.tung use ilk/product.tung yield (0 compare 1) ∏ (1 compare 1) $∏ (2 compare 1)" imports "eval ok: ((fore mid ∏) aft ∏)"
  , evalOkWith "clamp keepeþ a value inside its bounds" "use ground.tung yield 0 clamp 10 12" imports "eval ok: 10"
  , evalOkWith "qualified duplicate values" "use left.tung use right.tung yield left.foo + right.foo" duplicateImports "eval ok: 3"
  , evalOkWith "shared diamond import" "use left.tung use right.tung yield left.left + right.right" diamondImports "eval ok: 5"
  , evalOkWith "diamond import keepeþ one fill identity" "use left.tung use right.tung yield 3 left.identity" fillDiamondImports "eval ok: 3"
  , evalOkWith "qualified frames from different modules stay distinct" "use left.tung use right.tung yield 3 left.identity" distinctShapeImports "eval ok: 4"
  , evalOkWith "fills in extension-sharing paths stay distinct" "use a.tung one use a.extra.tung two yield (1 one.identity) + (1 two.identity)" samePrefixFillImports "eval ok: 13"
  , evalOkWith "fills beneath dotted directories stay distinct" "use pkg.one/identity.tung one use pkg.two/identity.tung two yield (1 one.identity) + (1 two.identity)" dottedDirectoryFillImports "eval ok: 13"
  , evalOkWith "same-spelled imported data types dispatch through distinct fills" "use common.tung use left.tung use right.tung yield (left.value common.label) + (right.value common.label)" nominalTypeFillImports "eval ok: 11"
  , evalOkWith "an imported transparent alias useþ its underlying fill" "use base.tung use alias.tung yield alias.value base.label" transparentAliasFillImports "eval ok: 7"
  , evalOkWith "an exported alias retaineth its private nominal fill" "use opaque.tung opaque yield opaque.value opaque.label" privateAliasFillImports "eval ok: 7"
  , evalOkWith "a local frame and fill shadow an imported namesake" "use left.tung frame a identity { let a identity: a } fill integer identity { let x identity = x + 2 } yield (1 identity) + (1 left.identity)" distinctShapeImports "eval ok: 5"
  , evalTypeErrWith "local evidence cannot satisfy an imported namesake frame" "use left.tung frame a identity { let a identity: a } fill integer identity { let x identity = x } graiþ a left.identity let (x: a) imported-use: a = x left.identity graiþ a identity let (x: a) local-use: a = x imported-use yield 1 local-use" distinctShapeImports
  , evalTypeErrWith "an incompatible declaration at a primitive path cannot claim its host fill" "use algebra/arithmetic/semiring.tung yield 1 magic" (Map.insert "algebra/arithmetic/semiring.tung" "show frame a add { let a magic: a }" imports)
  , evalOkWith "value survives a named re-export" "use middle.tung yield value" reexportImports "eval ok: 7"
  , evalOkWith "qualified re-export outrankeþ a shadowing local" "use middle.tung let value = 3 yield middle.value + value" reexportImports "eval ok: 10"
  , evalOkWith "frame fill survives an import" "use identity.tung yield 3 identity" identityImports "eval ok: 3"
  , evalOkWith "specific fill outrankeþ a blanket fill" "frame a identity { let a identity: a } fill a identity { let x identity = x } fill integer identity { let x identity = x + 1 } yield 1 identity" imports "eval ok: 2"
  , evalOkWith "data constructor survives a named re-export" "use data-middle.tung yield 4 box" dataReexportImports "eval ok: (4 box)"
  , evalOkWith "constructor identity survives a different re-export path" "use data-base.tung use data-middle.tung yield match 4 data-middle.box { x data-base.box @ x }" dataReexportImports "eval ok: 4"
  , evalOkWith "constructor pattern surviveþ a middle-module re-export" "use middle.tung yield match middle.off { middle.off @ 1 }" constructorReexportImports "eval ok: 1"
  , evalOkWith "qualified constructor pattern useþ import alias" "use alias.tung yield match 4 alias.box { x alias.box @ x }" (Map.insert "alias.tung" "show ilk a box { a box }" imports) "eval ok: 4"
  , evalOkWith "qualified global outrankeþ record field fallback" "use public.tung let public = r(visible = 9) yield public.visible" (Map.insert "public.tung" "show let visible = 7" imports) "eval ok: 7"
  , evalOkWith "effect operation survives a named re-export" "use ask-middle.tung yield try 3 ask { x ask @ x }" effectReexportImports "eval ok: 3"
  , evalTypeErrWith "private qualified value is rejected before runtime" "use private.tung yield private.hidden" (Map.fromList [("private.tung", "let hidden = 1")])
  , evalOkWith "text and unicode-list round trip" "use ground.tung yield 'λ😀' text-to-list $ list-to-text" imports "eval ok: 'λ😀'"
  , evalOkWith "text converteþ to a unicode list" "use ground.tung yield 'λ😀' text-to-list" imports "eval ok: (`λ (`😀 empty .*) .*)"
  , evalOkWith "unicode converteþ to its scalar integer" "use ground.tung yield `😀 unicode-to-integer" imports "eval ok: 128512"
  , evalOkWith "scalar integer converteþ to unicode" "use ground.tung yield 128512 integer-to-unicode" imports "eval ok: `😀"
  , evalOkWith "negative integer cannot become unicode" "use ground.tung yield try -1 integer-to-unicode { yield _ @ 'valid', message fail @ message }" imports "eval ok: 'invalid unicode scalar value'"
  , evalOkWith "surrogate integer cannot become unicode" "use ground.tung yield try 55296 integer-to-unicode { yield _ @ 'valid', message fail @ message }" imports "eval ok: 'invalid unicode scalar value'"
  , evalOkWith "out-of-range integer cannot become unicode" "use ground.tung yield try 1114112 integer-to-unicode { yield _ @ 'valid', message fail @ message }" imports "eval ok: 'invalid unicode scalar value'"
  , evalOkWith "text behead exposeþ one code point" "use ground.tung use ilk/list.tung use ilk/option.tung use ilk/product.tung use algebra/total/magma.tung yield match 'λ字' behead-text { (c ∏ rest) option.some @ ((c .* empty) list-to-text) * rest, option.none @ '' }" imports "eval ok: 'λ字'"
  , evalOkWith "empty text hath no head" "use ground.tung use ilk/option.tung yield match '' behead-text { none @ 1, _ some @ 0 }" imports "eval ok: 1"
  , evalOkWith "generic text fold" "use ground.tung use ilk/list.tung use collection/catamorphism.tung yield ('north' .* ('南' .* empty)) fold" imports "eval ok: 'north南'"
  , evalOkWith "text frames dispatch" "use ground.tung use algebra/total/magma.tung yield (('a' * 'β') ≡ 'aβ') ∧ ('a' ≤ 'b')" imports "eval ok: yea"
  , evalOkWith "unicode frames dispatch" "use ground.tung yield (`a ≡ `a) ∧ (`a ≤ `b)" imports "eval ok: yea"
  , evalOkWith "imported fill doth not shadow native integer order" "use ilk/list.tung yield 0 till 2" imports "eval ok: (0 (1 (2 empty .*) .*) .*)"
  , evalOkWith "list map pipeline keepeþ the list functor" "use ground.tung use ilk/list.tung yield 0 till 2 $ map { x @ x + 1 } $ map to-text" imports "eval ok: ('1' ('2' ('3' empty .*) .*) .*)"
  , evalOkWith "list apply calleþ its separately filled map" "use ground.tung use ilk/list.tung let values = 1 .* (2 .* empty) let functions = { x @ x + 10 } .* ({ x @ x × 2 } .* empty) yield values apply functions" imports "eval ok: (11 (12 (2 (4 empty .*) .*) .*) .*)"
  , evalOkWith "list traversal sequences option values" "use ground.tung use ilk/list.tung use ilk/option.tung use collection/traverse.tung yield (1 .* (2 .* empty)) traverse { x @ x option.some }" imports "eval ok: ((1 (2 empty .*) .*) some)"
  , evalOkWith "fold-map combineþ mapped list values" "use ground.tung use ilk/list.tung use collection/catamorphism.tung yield (0 till 2) fold-map to-text" imports "eval ok: '012'"
  , evalOkWith "bounded conjunction folds values" "use ground.tung use ilk/list.tung use collection/catamorphism.tung yield yea .* (nay .* empty) $ …∧" imports "eval ok: nay"
  , evalOkWith "bounded disjunction folds values" "use ground.tung use ilk/list.tung use collection/catamorphism.tung yield nay .* (yea .* empty) $ …∨" imports "eval ok: yea"
  , evalOkWith "empty bounded conjunction useþ the upper endpoint" "use ground.tung use ilk/list.tung use collection/catamorphism.tung let values: 𝟚 list = empty yield values …∧" imports "eval ok: yea"
  , evalOkWith "empty bounded disjunction useþ the lower endpoint" "use ground.tung use ilk/list.tung use collection/catamorphism.tung let values: 𝟚 list = empty yield values …∨" imports "eval ok: nay"
  , evalOkWith "boolean complement negateþ" "use ground.tung yield yea ¬" imports "eval ok: nay"
  , evalTypeErrWith "bounded lattice need not have a complement" "use ground.tung yield mid ¬" imports
  , evalOkWith "integer lattice chooseþ the lesser value" "use ground.tung yield 3 ∧ 2" imports "eval ok: 2"
  , evalOkWith "total order supplieþ a distributive lattice" "use ground.tung graiþ a lattice-distributive let (x: a, y: a, z: a) distribute: a = x ∧ (y ∨ z) yield 3 distribute 2 4" imports "eval ok: 3"
  , evalOkWith "non-total powerset lattice joineþ predicates" "use ground.tung use ilk/list.tung use ilk/powerset.tung use collection/catamorphism.tung let one: integer powerset = { x @ x ≡ 1 } let two: integer powerset = { x @ x ≡ 2 } let joined = one .* (two .* list.empty) $ …∨ yield joined ∋ 2" imports "eval ok: yea"
  , evalOkWith "empty powerset meet yieldeþ the universal set" "use ground.tung use ilk/list.tung use ilk/powerset.tung use collection/catamorphism.tung let values: (integer powerset) list = list.empty let all = values …∧ yield all ∋ 42" imports "eval ok: yea"
  , evalOkWith "pattern binders do not become constructor patterns" "use ground.tung use ilk/list.tung ilk bit { off } yield 0 till 2 $ map { _ @ off } $ map { _ @ 1 }" imports "eval ok: (1 (1 (1 empty .*) .*) .*)"
  , evalOkWith "control branch suspends actions" "use ground.tung yield nay branch { _ @ 1 } { _ @ 2 }" imports "eval ok: 2"
  , evalOkWith "option maybe mapeþ present value" "use ground.tung use ilk/option.tung yield (3 some) maybe 0 { x @ x + 1 }" imports "eval ok: 4"
  , evalOkWith "sum eiþer consumeþ selected side" "use ground.tung use ilk/sum.tung yield (2 inject₁) eiþer { x @ x + 1 } { y @ y × 3 }" imports "eval ok: 6"
  , evalOkWith "sum bimap mapeþ þe selected side" "use ground.tung use ilk/sum.tung let value: integer ∐ text = 2 inject₀ yield match (value bimap { x @ x + 1 } { text @ text }) { x inject₀ @ x, _ inject₁ @ 0 }" imports "eval ok: 3"
  , evalOkWith "sum functor mapeþ the right side" "use ground.tung use ilk/sum.tung let value: text ∐ integer = 2 inject₁ yield match (value map { x @ x + 1 }) { _ inject₀ @ 0, x inject₁ @ x }" imports "eval ok: 3"
  , evalOkWith "sum applicative applieþ the right side" "use ground.tung use ilk/sum.tung let value: text ∐ integer = 2 inject₁ let f: text ∐ (integer → integer) = { x @ x + 3 } inject₁ yield match value apply f { _ inject₀ @ 0, x inject₁ @ x }" imports "eval ok: 5"
  , evalOkWith "sum monad preserveþ a left value" "use ground.tung use ilk/sum.tung let value: text ∐ integer = 'left' inject₀ yield match (value map-flat { x @ (x + 1) inject₁ }) { message inject₀ @ message, x inject₁ @ x to-text }" imports "eval ok: 'left'"
  , evalOkWith "sum catamorphism foldeþ the right side" "use ground.tung use ilk/sum.tung use collection/catamorphism.tung let value: text ∐ integer = 3 inject₁ yield value fold₁ 1 +" imports "eval ok: 4"
  , evalOkWith "sum traversal preserveþ its side" "use ground.tung use ilk/sum.tung use ilk/option.tung use collection/traverse.tung let value: text ∐ integer = 3 inject₁ yield value traverse { x @ (x + 1) some }" imports "eval ok: ((4 inject₁) some)"
  , evalOkWith "product bimap mapeþ both fields" "use ground.tung use ilk/product.tung yield (1 ∏ 2) bimap { x @ x + 1 } { y @ y × 3 }" imports "eval ok: (2 6 ∏)"
  , evalOkWith "product applicative accumulateþ value context first" "use ground.tung use ilk/product.tung let value = 'value:' ∏ 2 let f = 'function:' ∏ { x @ x + 1 } yield value apply f" imports "eval ok: ('value:function:' 3 ∏)"
  , evalOkWith "product monad accumulateþ outer context first" "use ground.tung use ilk/product.tung yield ('outer:' ∏ 2) map-flat { x @ 'inner:' ∏ (x + 1) }" imports "eval ok: ('outer:inner:' 3 ∏)"
  , evalOkWith "product catamorphism foldeþ the second field" "use ground.tung use ilk/product.tung use collection/catamorphism.tung yield ('ignored' ∏ 3) fold₁ 1 +" imports "eval ok: 4"
  , evalOkWith "product traversal preserveþ the first field" "use ground.tung use ilk/product.tung use ilk/option.tung use collection/traverse.tung yield ('context' ∏ 3) traverse { x @ (x + 1) some }" imports "eval ok: (('context' 4 ∏) some)"
  , evalOkWith "applicative lift2 for option" "use ground.tung use ilk/option.tung yield (1 some) lift₂ (2 some) +" imports "eval ok: (3 some)"
  , evalOkWith "monad void for option" "use ilk/option.tung use collection/monad.tung yield (1 some) void" imports "eval ok: (only some)"
  , evalOkWith "list behead exposeþ the head and tail" "use ilk/list.tung use ilk/option.tung use ilk/product.tung yield match (1 .* empty) behead { (head product.∏ _) option.some @ head, option.none @ 0 }" imports "eval ok: 1"
  , evalOkWith "list take after drop" "use ilk/list.tung yield ((0 till 5) drop 2) take 2" imports "eval ok: (2 (3 empty .*) .*)"
  , evalOkWith "list find returneþ first match" "use ground.tung use ilk/list.tung yield (0 till 5) find { x @ 3 ≤ x }" imports "eval ok: (3 some)"
  , evalOkWith "list find short-circuits effectful predicates" "use ground.tung use ilk/list.tung use ilk/option.tung deed late { 𝟙 late: 𝟚 } let values = 1 .* (2 .* empty) yield try (values find { x @ match x ≤ 1 { yea @ yea, nay @ only late } }) { late @ none }" imports "eval ok: (1 some)"
  , evalOkWith "list indexing is zero based" "use ilk/list.tung yield (10 .* (20 .* empty)) at 1" imports "eval ok: (20 some)"
  , evalOkWith "negative list index is absent" "use ground.tung use ilk/list.tung let index = 0 - 1 yield (10 .* empty) at index" imports "eval ok: none"
  , evalOkWith "list last returneþ the final value" "use ilk/list.tung yield (1 .* (2 .* empty)) last" imports "eval ok: (2 some)"
  , evalOkWith "list membership findeþ a value" "use ground.tung use ilk/list.tung yield (1 .* (2 .* empty)) ∋ 2" imports "eval ok: yea"
  , evalOkWith "list membership rejecteþ a missing value" "use ground.tung use ilk/list.tung yield (1 .* (2 .* empty)) ∋ 3" imports "eval ok: nay"
  , evalOkWith "list replicate useþ a nonnegative count" "use ilk/list.tung yield 'x' replicate 3" imports "eval ok: ('x' ('x' ('x' empty .*) .*) .*)"
  , evalOkWith "list partition preserveþ order" "use ground.tung use ilk/list.tung yield (0 till 4) partition { x @ 2 ≤ x }" imports "eval ok: ((2 (3 (4 empty .*) .*) .*) (0 (1 empty .*) .*) ∏)"
  , evalOkWith "list unzip preserveþ both sides" "use ilk/list.tung use ilk/product.tung yield ((1 ∏ 'a') .* ((2 ∏ 'b') .* empty)) unzip" imports "eval ok: ((1 (2 empty .*) .*) ('a' ('b' empty .*) .*) ∏)"
  , evalOkWith "n-ary sum addeþ a foldable collection" "use ground.tung use ilk/list.tung use collection/catamorphism.tung yield (1 .* (2 .* (3 .* empty))) …+" imports "eval ok: 6"
  , evalOkWith "n-ary sum of an empty collection is zero" "use ground.tung use ilk/list.tung use collection/catamorphism.tung let values: integer list = empty yield values …+" imports "eval ok: 0"
  , evalTypeErrWith "n-ary sum needeþ a catamorphism" "use ground.tung use collection/catamorphism.tung ilk a box { a box } let value: integer box = 1 box yield value …+" imports
  , evalOkWith "n-ary product multiplies a foldable collection" "use ground.tung use ilk/list.tung use collection/catamorphism.tung yield (2 .* (3 .* (4 .* empty))) …×" imports "eval ok: 24"
  , evalOkWith "n-ary product of an empty collection is one" "use ground.tung use ilk/list.tung use collection/catamorphism.tung let values: integer list = empty yield values …×" imports "eval ok: 1"
  , evalTypeErrWith "n-ary product needeþ a multiplicative monoid" "use ground.tung use ilk/list.tung use collection/catamorphism.tung yield ('a' .* empty) …×" imports
  , evalOkWith "boolean supremal wrapper useþ disjunction" "use ground.tung use ilk/list.tung use algebra/total/semigroup.tung use collection/catamorphism.tung let values = (nay supremal) .* ((yea supremal) .* list.empty) yield match values fold { result supremal @ result }" imports "eval ok: yea"
  , evalOkWith "empty boolean supremal fold yieldeþ nay" "use ground.tung use ilk/list.tung use algebra/total/semigroup.tung use collection/catamorphism.tung let values: (𝟚 supremal) list = list.empty yield match values fold { result supremal @ result }" imports "eval ok: nay"
  , evalOkWith "boolean infimal wrapper useþ conjunction" "use ground.tung use ilk/list.tung use algebra/total/semigroup.tung use collection/catamorphism.tung let values = (yea infimal) .* ((nay infimal) .* list.empty) yield match values fold { result infimal @ result }" imports "eval ok: nay"
  , evalOkWith "empty boolean infimal fold yieldeþ yea" "use ground.tung use ilk/list.tung use algebra/total/semigroup.tung use collection/catamorphism.tung let values: (𝟚 infimal) list = list.empty yield match values fold { result infimal @ result }" imports "eval ok: yea"
  , evalOkWith "natural semiring computeþ with both identities" "use ground.tung use ilk/natural.tung let two: natural = (natural.zero suc) suc let three = two suc yield ((two × three) + one) to-integer" imports "eval ok: 7"
  , evalOkWith "natural semiring supplieþ multiplicative monoid evidence" "use ground.tung use ilk/list.tung use ilk/natural.tung use collection/catamorphism.tung let two: natural = (natural.zero suc) suc let three = two suc yield ((two .* (three .* list.empty)) …×) to-integer" imports "eval ok: 6"
  , evalOkWith "nonnegative integer converteþ to natural" "use ground.tung use ilk/natural.tung yield 3 to-natural $ to-integer" imports "eval ok: 3"
  , evalOkWith "negative integer cannot become natural" "use ground.tung yield try -1 to-natural { yield _ @ 'valid', message fail @ message }" imports "eval ok: 'negative cannot be a natural'"
  , evalOkWith "complex sine of zero is zero" "use ground.tung use numeric/complex.tung yield ((0.0 complex 0.0) sine) real" imports "eval ok: 0.0"
  , evalOkWith "complex cosine of zero is one" "use ground.tung use numeric/complex.tung yield ((0.0 complex 0.0) cosine) real" imports "eval ok: 1.0"
  , evalOkWith "complex exponentiation scaleþ by the real exponent" "use ground.tung use numeric/complex.tung yield ((1.0 complex 0.0) exponent) real" imports "eval ok: 2.718281828459045"
  , evalOkWith "complex sine of unit imaginary yieldeþ hyperbolic sine" "use ground.tung use numeric/complex.tung yield ((0.0 complex 1.0) sine) imaginary" imports "eval ok: 1.1752011936438014"
  , evalOkWith "complex cosine of unit imaginary yieldeþ hyperbolic cosine" "use ground.tung use numeric/complex.tung yield ((0.0 complex 1.0) cosine) real" imports "eval ok: 1.5430806348152437"
  , evalOkWith "nonempty map preserveþ the first element" "use ground.tung use ilk/list.tung use ilk/nonempty.tung yield ((1 nonempty (2 .* empty)) map { x @ x + 1 }) to-list" imports "eval ok: (2 (3 empty .*) .*)"
  , evalOkWith "nonempty semigroup appendeþ without losing the head" "use ground.tung use ilk/list.tung use ilk/nonempty.tung use algebra/total/magma.tung yield ((1 nonempty (2 .* empty)) * (3 nonempty empty)) to-list" imports "eval ok: (1 (2 (3 empty .*) .*) .*)"
  , evalOkWith "nonempty applicative applieþ every function" "use ground.tung use ilk/list.tung use ilk/nonempty.tung let values = 1 nonempty (2 .* empty) let fs = { x @ x + 1 } nonempty ({ x @ x × 2 } .* empty) yield (values apply fs) to-list" imports "eval ok: (2 (3 (2 (4 empty .*) .*) .*) .*)"
  , evalOkWith "nonempty monad concatenateþ every result" "use ground.tung use ilk/list.tung use ilk/nonempty.tung let values = 1 nonempty (2 .* empty) yield (values map-flat { x @ x nonempty ((x + 10) .* empty) }) to-list" imports "eval ok: (1 (11 (2 (12 empty .*) .*) .*) .*)"
  , evalOkWith "nonempty catamorphism visiteþ the head first" "use ground.tung use ilk/list.tung use ilk/nonempty.tung use collection/catamorphism.tung yield (1 nonempty (2 .* empty)) fold₁ 0 +" imports "eval ok: 3"
  , evalOkWith "nonempty traversal keepeþ a nonempty result" "use ground.tung use ilk/list.tung use ilk/nonempty.tung use ilk/option.tung use collection/traverse.tung yield ((1 nonempty (2 .* empty)) traverse { x @ (x + 1) some }) map to-list" imports "eval ok: ((2 (3 empty .*) .*) some)"
  , evalOkWith "nonempty membership delegateþ to list" "use ground.tung use ilk/list.tung use ilk/nonempty.tung yield (1 nonempty (2 .* empty)) ∋ 2" imports "eval ok: yea"
  , evalOkWith "table put replaceþ a key" "use ground.tung use ilk/table.tung let table = (empty put 'a' 1) put 'a' 2 yield table lookup 'a'" imports "eval ok: (2 some)"
  , evalOkWith "table membership dispatcheþ through inhold" "use ground.tung use ilk/table.tung let table = empty put 'a' 1 yield table ∋ 'a'" imports "eval ok: yea"
  , evalOkWith "table constructor may be qualified" "use ground.tung use ilk/list.tung use ilk/product.tung use ilk/table.tung yield (('a' ∏ 2) .* list.empty) table.from-list $ lookup 'a'" imports "eval ok: (2 some)"
  , evalOkWith "table rid droppeþ a key" "use ground.tung use ilk/table.tung let table = empty put 'a' 1 yield (table rid 'a') lookup 'a'" imports "eval ok: none"
  , evalOkWith "set put keepeþ values unique" "use ground.tung use ilk/set.tung yield ((empty put 1) put 1) to-list" imports "eval ok: (1 empty .*)"
  , evalOkWith "set constructor may be qualified" "use ground.tung use ilk/list.tung use ilk/set.tung yield (1 .* (2 .* list.empty)) set.from-list $ to-list" imports "eval ok: (1 (2 empty .*) .*)"
  , evalOkWith "set rid droppeþ a value" "use ground.tung use ilk/set.tung let values = (empty put 1) put 2 yield (values rid 1) ∋ 1" imports "eval ok: nay"
  , evalOkWith "set supremum keepeþ unique values" "use ground.tung use ilk/set.tung let left = (empty put 1) put 2 let right = (empty put 2) put 3 yield (left ∨ right) to-list" imports "eval ok: (3 (2 (1 empty .*) .*) .*)"
  , evalOkWith "set infimum keepeþ shared values" "use ground.tung use ilk/set.tung let left = (empty put 1) put 2 let right = (empty put 2) put 3 yield (left ∧ right) to-list" imports "eval ok: (2 empty .*)"
  , evalOkWith "set equality ignoreþ insertion order" "use ground.tung use ilk/set.tung let left = (empty put 1) put 2 let right = (empty put 2) put 1 yield left ≡ right" imports "eval ok: yea"
  , evalOkWith "set partial order compareþ inclusion" "use ground.tung use ilk/set.tung let smaller = empty put 1 let larger = smaller put 2 yield (smaller ≤ larger) ∧ (smaller < larger)" imports "eval ok: yea"
  , evalOkWith "set inclusion rejecteþ a proper superset" "use ground.tung use ilk/set.tung let smaller = empty put 1 let larger = smaller put 2 yield larger ≤ smaller" imports "eval ok: nay"
  , evalOkWith "powerset addition is symmetric difference" "use ground.tung use ilk/powerset.tung let left: integer powerset = { value @ (value ≡ 1) ∨ (value ≡ 2) } let right: integer powerset = { value @ (value ≡ 2) ∨ (value ≡ 3) } yield ((left + right) ∋ 2)" imports "eval ok: nay"
  , evalOkWith "powerset addition keepeþ an unshared member" "use ground.tung use ilk/powerset.tung let left: integer powerset = { value @ value ≡ 1 } let right: integer powerset = { value @ value ≡ 2 } yield ((left + right) ∋ 1)" imports "eval ok: yea"
  , evalOkWith "powerset multiplication is intersection" "use ground.tung use ilk/powerset.tung let left: integer powerset = { value @ (value ≡ 1) ∨ (value ≡ 2) } let right: integer powerset = { value @ value ≡ 2 } yield ((left × right) ∋ 2)" imports "eval ok: yea"
  , evalOkWith "powerset supremum is union" "use ground.tung use ilk/powerset.tung let left: integer powerset = { value @ value ≡ 1 } let right: integer powerset = { value @ value ≡ 2 } yield (left ∨ right) ∋ 2" imports "eval ok: yea"
  , evalOkWith "powerset infimum is intersection" "use ground.tung use ilk/powerset.tung let left: integer powerset = { value @ (value ≡ 1) ∨ (value ≡ 2) } let right: integer powerset = { value @ value ≡ 2 } yield (left ∧ right) ∋ 1" imports "eval ok: nay"
  , evalOkWith "powerset complement excludeþ a former member" "use ground.tung use ilk/powerset.tung let one: integer powerset = { value @ value ≡ 1 } let outside: integer powerset = one ¬ yield outside ∋ 1" imports "eval ok: nay"
  , evalOkWith "powerset complement includeþ a former nonmember" "use ground.tung use ilk/powerset.tung let one: integer powerset = { value @ value ≡ 1 } let outside: integer powerset = one ¬ yield outside ∋ 2" imports "eval ok: yea"
  , evalOkWith "powerset subtraction is symmetric difference" "use ground.tung use ilk/powerset.tung let values: integer powerset = { value @ value ≡ 1 } yield ((values - values) ∋ 1)" imports "eval ok: nay"
  , evalOkWith "powerset zero is empty" "use ground.tung use ilk/powerset.tung let values: integer powerset = zero yield values ∋ 7" imports "eval ok: nay"
  , evalOkWith "powerset one is universal" "use ground.tung use ilk/powerset.tung let values: integer powerset = one yield values ∋ 7" imports "eval ok: yea"
  , evalOkWith "set becomeþ a powerset" "use ground.tung use ilk/set.tung use ilk/powerset.tung let finite = set.empty set.put 7 yield (finite to-powerset) ∋ 7" imports "eval ok: yea"
  , evalOkWith "powerset infimal wrapper useþ intersection" "use ground.tung use ilk/list.tung use ilk/powerset.tung use algebra/total/semigroup.tung use collection/catamorphism.tung let one: integer powerset = { value @ value ≡ 1 } let both: integer powerset = { value @ (value ≡ 1) ∨ (value ≡ 2) } let wrapped = (one infimal) .* ((both infimal) .* list.empty) yield match wrapped fold { result infimal @ result ∋ 2 }" imports "eval ok: nay"
  , evalOkWith "powerset supremal wrapper useþ union" "use ground.tung use ilk/list.tung use ilk/powerset.tung use algebra/total/semigroup.tung use collection/catamorphism.tung let one: integer powerset = { value @ value ≡ 1 } let two: integer powerset = { value @ value ≡ 2 } let wrapped = (one supremal) .* ((two supremal) .* list.empty) yield match wrapped fold { result supremal @ result ∋ 2 }" imports "eval ok: yea"
  , evalTypeErrWith "powerset hath no arbitrarily chosen monoid" "use ground.tung use ilk/list.tung use ilk/powerset.tung use algebra/total/semigroup.tung use collection/catamorphism.tung let values: (integer powerset) list = list.empty yield values fold" imports
  , evalOkWith "table functor mapeþ values" "use ground.tung use ilk/table.tung let table = (empty put 'a' 1) put 'b' 2 yield (table map { x @ x + 10 }) lookup 'a'" imports "eval ok: (11 some)"
  , evalOkWith "table filter keepeþ matching values" "use ground.tung use ilk/table.tung use collection/filter.tung let table = (empty put 'a' 1) put 'b' 2 yield (table filter { x @ 2 ≤ x }) lookup 'a'" imports "eval ok: none"
  , evalOkWith "table merge preferreþ right values" "use ground.tung use ilk/table.tung let left = empty put 'a' 1 let right = empty put 'a' 2 yield (left merge right) lookup 'a'" imports "eval ok: (2 some)"
  , evalOkWith "table adjust changeþ an existing value" "use ground.tung use ilk/table.tung let table = empty put 'a' 1 yield (table adjust 'a' { x @ x + 1 }) lookup 'a'" imports "eval ok: (2 some)"
  , evalOkWith "text operations use unicode code points" "use ground.tung use text/operation.tung yield ('aβ字' reverse-text) take-text 2" imports "eval ok: '字β'"
  , evalOkWith "text size counteþ unicode code points" "use ground.tung use text/operation.tung use collection/size.tung use ilk/natural.tung yield ('aβ字' size) to-integer" imports "eval ok: 3"
  , evalOkWith "text split and join preserve empty fields" "use ground.tung use text/operation.tung yield ('a,b,,c' split-text `,) join-wiþ-text '.'" imports "eval ok: 'a.b..c'"
  , evalOkWith "text words collapse whitespace runs" "use ground.tung use text/operation.tung yield ' red\\tblue\\nred ' words-text $ join-wiþ-text '.'" imports "eval ok: 'red.blue.red'"
  , evalOkWith "typed paþs join components" "use ground.tung use ilk/path.tung yield (('root' paþ) join-paþ ('leaf' paþ)) to-text" imports "eval ok: 'root/leaf'"
  , evalOkWith "time spans drive async sleep" "use ground.tung use ilk/duration.tung yield (0 milliseconds) sleep-for" imports "eval ok: only"
  , evalOkWith "process captureþ standard output" "use ground.tung use process.tung use process/result.tung use ilk/list.tung yield ('printf' run-process ('hello' .* empty)) process-output" imports "eval ok: 'hello'"
  , evalTypeErrWith "missing import is rejected before runtime" "use missing.tung" Map.empty
  , evalTypeErrWith "import cycle is rejected before runtime" "use left.tung" cyclicImports
  , evalOkWith "arctan range is nonnegative" "use _foreign.tung yield 0.0 arctan-float -1.0" imports "eval ok: 4.71238898038469"
  , evalOkWith "fork and wait" "use ground.tung let _ work = 42 let task = work fork yield task wait" imports "eval ok: 42"
  , evalOkWith "task result may be awaited repeatedly" "use ground.tung let _ work = 21 let task = work fork yield (task wait) + (task wait)" imports "eval ok: 42"
  , evalOkWith "task wait may time out" "use ground.tung let _ work = (let _ = 200 sleep yield 42) let task = work fork yield task wait-for 0" imports "eval ok: none"
  , evalOkWith "task wait-for returneþ a finished value" "use ground.tung let _ work = 42 let task = work fork yield task wait-for 1000" imports "eval ok: (42 some)"
  , evalOkWith "fordone task faileþ through the declared effect" "use ground.tung let _ work = (let _ = 1000 sleep yield 42) let task = work fork let _ = task fordo yield try task wait { yield _ @ 'finished', message fail @ message }" imports "eval ok: 'task cancelled'"
  , evalOkWith "cpu-bound task remaineþ cancellable" "use ground.tung let _ loop = only loop let spin = loop let task = spin fork let _ = task fordo yield try task wait { yield _ @ 'finished', message fail @ message }" imports "eval ok: 'task cancelled'"
  , evalTypeErrWith "task constructor is private" "use ground.tung yield 42 done" imports
  , concurrentForks imports
  , randomRange imports
  , systemArguments imports
  , systemEnvironment imports
  , unixTime imports
  ]
 where
  duplicateImports = Map.fromList [("left.tung", "show let foo = 1"), ("right.tung", "show let foo = 2")]
  diamondImports =
    Map.fromList
      [ ("left.tung", "use shared.tung show let left = shared.base + 1")
      , ("right.tung", "use shared.tung show let right = shared.base + 2")
      , ("shared.tung", "show let base = 1")
      ]
  fillDiamondImports =
    Map.fromList
      [ ("left.tung", "use shared.tung show shared.identity")
      , ("right.tung", "use shared.tung")
      , ("shared.tung", "show frame a identity { let a identity: a } fill integer identity { let x identity = x }")
      ]
  distinctShapeImports =
    Map.fromList
      [ ("left.tung", "show frame a identity { let a identity: a } fill integer identity { let x identity = x + 1 }")
      , ("right.tung", "show frame a identity { let a identity: a } fill integer identity { let x identity = x + 2 }")
      ]
  samePrefixFillImports = Map.fromList [("a.tung", identityFill 1), ("a.extra.tung", identityFill 10)]
  dottedDirectoryFillImports = Map.fromList [("pkg.one/identity.tung", identityFill 1), ("pkg.two/identity.tung", identityFill 10)]
  identityFill :: Int -> String
  identityFill increment = "show frame a identity { let a identity: a } fill integer identity { let x identity = x + " ++ show increment ++ " }"
  nominalTypeFillImports =
    Map.fromList
      [ ("common.tung", "show frame a label { let a label: integer }")
      , ("left.tung", "use common.tung show ilk token { token } fill token common.label { let _ label = 1 } show let value: token = token")
      , ("right.tung", "use common.tung show ilk token { token } fill token common.label { let _ label = 10 } show let value: token = token")
      ]
  transparentAliasFillImports =
    Map.fromList
      [ ("base.tung", "show ilk token { token } show frame a label { let a label: integer } fill token label { let _ label = 7 }")
      , ("alias.tung", "use base.tung show let-ilk alias = base.token show let value: alias = base.token")
      ]
  privateAliasFillImports =
    Map.singleton
      "opaque.tung"
      "ilk secret { secret } show let-ilk public = secret show frame a label { let a label: integer } fill secret label { let _ label = 7 } show let value: public = secret"
  reexportImports = Map.fromList [("base.tung", "show let value = 7"), ("middle.tung", "use base.tung show base.value")]
  identityImports = Map.fromList [("identity.tung", "show frame a identity { let a identity: a } fill integer identity { let x identity = x }")]
  dataReexportImports =
    Map.fromList
      [ ("data-base.tung", "show ilk a box { a box }")
      , ("data-middle.tung", "use data-base.tung show-ilk data-base.box show data-base.box")
      ]
  constructorReexportImports =
    Map.fromList
      [ ("base.tung", "show ilk bit { off }")
      , ("middle.tung", "use base.tung show-ilk base.bit show base.off")
      ]
  effectReexportImports =
    Map.fromList
      [ ("ask-base.tung", "show deed ask { integer ask: integer }")
      , ("ask-middle.tung", "use ask-base.tung show ask-base.ask")
      ]
  samePrefixEffectImports = Map.fromList [("a.tung", queryEffect), ("a.extra.tung", queryEffect)]
  dottedDirectoryEffectImports = Map.fromList [("pkg.one/query.tung", queryEffect), ("pkg.two/query.tung", queryEffect)]
  queryEffect = "show deed query { integer ask: integer }"
  cyclicImports = Map.fromList [("left.tung", "use right.tung"), ("right.tung", "use left.tung")]

randomRange :: Map.Map String String -> Test
randomRange imports = do
  actual <- evaluateWithImports "use ground.tung yield only random" imports
  pure $ case stripPrefix "eval ok: " actual >>= readMaybe of
    Just value | value >= (0 :: Double) && value < 1 -> Nothing
    _ -> Just ("random result is in [0, 1): got " ++ actual)

concurrentForks :: Map.Map String String -> Test
concurrentForks imports =
  evalOkWith "concurrent forks overlap while one is awaited" source imports "eval ok: (21 some)"
 where
  -- awaiting one equal-duration task giveþ the other time to finish. its
  -- already-available result testeþ overlap without timing the whole compiler.
  source =
    "use ground.tung "
      ++ "let _ work = (let _ = 600 sleep yield 21) "
      ++ "let left = work fork let right = work fork "
      ++ "let _ = left wait yield right wait-for 100"

systemArguments :: Map.Map String String -> Test
systemArguments imports = do
  actual <- evaluateWithArgsAndImports ["north", "two words"] "use ground.tung use system.tung yield only arguments" imports
  pure $ if actual == "eval ok: ('north' ('two words' empty .*) .*)" then Nothing else Just ("system arguments: " ++ actual)

systemEnvironment :: Map.Map String String -> Test
systemEnvironment imports = bracket (Environment.lookupEnv name) restore $ \_ -> do
  Environment.setEnv name "seen"
  actual <- evaluateWithImports ("use ground.tung use system.tung yield '" ++ name ++ "' environment") imports
  pure $ if actual == "eval ok: ('seen' some)" then Nothing else Just ("system environment: " ++ actual)
 where
  name = "TUNG_TEST_ENVIRONMENT"
  restore Nothing = Environment.unsetEnv name
  restore (Just value) = Environment.setEnv name value

unixTime :: Map.Map String String -> Test
unixTime imports = do
  before <- floor <$> getPOSIXTime
  actual <- evaluateWithImports "use ground.tung use clock.tung yield only unix-time" imports
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
unitData = "ilk 𝟙 { only } "
boolData = "ilk 𝟚 { yea, nay } "
optionData = "ilk a option { none, a some } "
stopEffect = "deed e stop { e stop: a } "
duoEffect = unitData ++ "deed duo { 𝟙 first: 𝟙, 𝟙 second: 𝟙 } "
