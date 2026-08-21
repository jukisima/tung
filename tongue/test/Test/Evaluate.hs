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
  [ Harness.propertyTest "property: a handler may resume a continuation any generated number of times" $
      QuickCheck.forAll (QuickCheck.resize 6 (QuickCheck.listOf (QuickCheck.chooseInt (0, 9)))) \answers ->
        QuickCheck.ioProperty do
          actual <- evaluate (handlerSource answers)
          let expected = "eval ok: " ++ show (sum answers)
          pure (QuickCheck.counterexample (handlerSource answers ++ "\n" ++ actual) (actual QuickCheck.=== expected))
  ]

handlerSource :: [Int] -> String
handlerSource answers =
  unitData
    ++ "deed choice { 𝟙 pick: integer }; try null pick { pick | "
    ++ resumeSum answers
    ++ " }"

resumeSum :: [Int] -> String
resumeSum [] = "0"
resumeSum [answer] = show answer ++ " resume"
resumeSum (answer : answers) = "(" ++ show answer ++ " resume) + (" ++ resumeSum answers ++ ")"

deterministic :: [(String, String, String)]
deterministic =
  [ ("integer arithmetic", "(1 + 2) × 3", "eval ok: 9")
  , ("term type ascription is erased before evaluation", "(1 + 2: integer)", "eval ok: 3")
  , ("ascribed anonymous function remaineth recursive", "let factorial = ({ 0 | 1, n | n × ((n - 1) factorial) }: integer → integer); 5 factorial", "eval ok: 120")
  , ("integer arithmetic is arbitrary precision", "999999999999999999999999999999 + 1", "eval ok: 1000000000000000000000000000000")
  , ("foreign let", "let add-integer: integer → integer → integer = foreign; 1 add-integer 2", "eval ok: 3")
  , ("unicode text", "'λ字'", "eval ok: 'λ字'")
  , ("unicode control rendering useth decimal terminator", "`\\1;", "eval ok: `\\1;")
  , ("float arithmetic", "let a = '0.5' from-text; let b = '0.25' from-text; a + b", "eval ok: 0.75")
  , ("float division useth the division slash", "6.0 ∕ 4.0", "eval ok: 1.5")
  , ("float division by zero followeth ieee", "1.0 ∕ 0.0", "eval ok: Infinity")
  , ("float equality ignoreth literal spelling", "kin 𝟚 { yea, nay }; 1.0 ≡ 1.00", "eval ok: yea")
  , ("float ordering is numeric", "kin 𝟚 { yea, nay }; 2.0 ≤ 10.0", "eval ok: yea")
  , ("curried partial application", "let a add b = a + b; let add-one = 1 add; 2 add-one", "eval ok: 3")
  , ("three curried arguments", "let a add b c = (a + b) + c; 1 add 2 3", "eval ok: 6")
  , ("local recursive function", "kin natural { zero, natural suc }; let count = (let loop = { zero | 0, n suc | 1 + (n loop) }; loop); ((zero suc) suc) count", "eval ok: 2")
  , ("second-position function header", "let a add b = a + b; 1 add 2", "eval ok: 3")
  , ("dollar function segment", "let x f = x + 1; let x h y z = (x + y) × z; 1 f $h 2 3", "eval ok: 12")
  , ("lexical closure", "let x = 1; let _ f = x; (let x = 2; 0 f)", "eval ok: 1")
  , ("multi-scrutinee match", boolData ++ "match yea, nay { yea, yea | 1, yea, nay | 2, nay, yea | 3, nay, nay | 4 }", "eval ok: 2")
  , ("nested constructor match", boolData ++ optionData ++ "match nay some { yea some | 1, nay some | 2, none | 0 }", "eval ok: 2")
  , ("first matching arm winneth", boolData ++ "match yea { yea | 1, _ | 2 }", "eval ok: 1")
  , ("integer match patterns", "match -1 { 0 | 10, -1 | 20, _ | 30 }", "eval ok: 20")
  , ("integer patterns fall through", "match 2 { 0 | 10, 1 | 20, _ | 30 }", "eval ok: 30")
  , ("integer function patterns", "let classify: integer → integer = { 0 | 10, 1 | 20, _ | 30 }; 1 classify", "eval ok: 20")
  , ("unchosen arm is not evaluated", boolData ++ "let _ ignore = 1; match yea { yea | 1, nay | (1 ÷ 0) ignore }", "eval ok: 1")
  , ("let right side is strict", stopEffect ++ "try (let x = 1 stop; 2 stop) { x stop | x }", "eval ok: 1")
  , ("function expression runneth before arguments", stopEffect ++ "try 0 (1 stop) (2 stop) { x stop | x }", "eval ok: 1")
  , ("arguments run left to right", stopEffect ++ "let x choose _ = x; try (1 stop) choose (2 stop) { return x | x, x stop | x }", "eval ok: 1")
  , ("record fields run left to right", stopEffect ++ "try [first = 1 stop, second = 2 stop] { return _ | 0, x stop | x }", "eval ok: 1")
  , ("record update evaluateth base first", stopEffect ++ "let base = [field = 0]; try [= (let _ = 1 stop; base), field = 2 stop] { return _ | 0, x stop | x }", "eval ok: 1")
  , ("match scrutinees run left to right", stopEffect ++ "try match 1 stop, 2 stop { x, y | x } { x stop | x }", "eval ok: 1")
  , ("record update", "let person = [name = 'n', age = 1]; [= person, age = 2]", "eval ok: [age = 2, name = 'n']")
  , ("record access", "let person = [name = 'n', age = 1]; person@age", "eval ok: 1")
  , ("record removal", "let person = [name = 'n', age = 1]; [= person, - age]", "eval ok: [name = 'n']")
  , ("handled division failure", "let _ ignore = 1; try (1 ÷ 0) ignore { fail | 9 }", "eval ok: 9")
  , ("operation payload", "deed e fail { e fail: a }; let _ ignore = 'ok'; try (1 ÷ 0) ignore { return text | text, message fail | message }", "eval ok: 'division by zero'")
  , ("return clause mapeth normal answer", "try 2 + 3 { return n | n to-text }", "eval ok: '5'")
  , ("partial call performeth only when saturated", "let _ ignore = 0; try (0 (1 ÷)) ignore { fail | 7 }", "eval ok: 7")
  , ("resume once", "deed ask { integer ask: integer }; try 10 ask { x ask | (x + 1) resume }", "eval ok: 11")
  , ("resume zero times", "deed ask { integer ask: integer }; try 10 ask { x ask | x + 1 }", "eval ok: 11")
  , ("deep resume handleth later operation", unitData ++ "deed choice { 𝟙 pick: integer }; try (null pick) + (null pick) { pick | 1 resume }", "eval ok: 2")
  , ("multi-shot resume", unitData ++ "deed choice { 𝟙 pick: integer }; try (null pick) + (null pick) { pick | (1 resume) + (2 resume) }", "eval ok: 12")
  , ("return runneth on each resumed branch", unitData ++ "deed choice { 𝟙 pick: integer }; try null pick { return n | n + 10, pick | (1 resume) + (2 resume) }", "eval ok: 23")
  , ("polymorphic state get", unitData ++ "deed a state { 𝟙 get: a, a set: 𝟙 }; try null get { get | 4 resume, x set | null resume }", "eval ok: 4")
  , ("effect-level clause catches either operation", duoEffect ++ "try null second { duo | null }", "eval ok: null")
  , ("nested handlers split operation coverage", duoEffect ++ "try (try null second { first | null }) { second | null }", "eval ok: null")
  , ("native sleep", unitData ++ "0 sleep", "eval ok: null")
  , ("shape laws are erased", "shape a identity { a identity: a; law (x: a): x identity ~ x }; fill integer identity { let x identity = x }; 2 identity", "eval ok: 2")
  , ("result-only member useth expected type", "shape a origin { origin: a }; fill integer origin { let origin = 7 }; let value: integer = origin; value", "eval ok: 7")
  , ("different fills dispatch by inferred type", "shape a label { a label: text }; fill integer label { let _ label = 'integer' }; fill text label { let _ label = 'text' }; 'x' label", "eval ok: 'text'")
  , ("graith function receiveth its caller dictionary", "shape a label { a label: text }; fill integer label { let _ label = 'integer' }; fill text label { let _ label = 'text' }; graith a label let (x: a) labelled: text = x label; 1 labelled", "eval ok: 'integer'")
  , ("child dictionary useth a direct parent fill", "shape a parent { a parent: a }; graith a parent shape a child { a child: a }; fill integer parent { let x parent = x }; fill integer child { let x child = x parent }; 4 child", "eval ok: 4")
  , ("shape default useth its child dictionary", "shape a source { source: a }; graith a source shape a derived { let derived: a = source }; fill integer source { let source = 7 }; fill integer derived {}; let result: integer = derived; result", "eval ok: 7")
  , ("fill graith remaineth an external dictionary", "shape a combine { a combine a: a }; fill integer combine { let x combine y = x + y }; kin a box { a box }; graith a combine fill (a box) combine { let (x box) combine (y box) = (x combine y) box }; (1 box) combine (2 box)", "eval ok: (3 box)")
  ]

runtimeErrors :: [(String, String)]
runtimeErrors =
  [ ("division failure is unhandled", "1 ÷ 0")
  , ("from-text failure is unhandled", "'not a float' from-text")
  , ("handler clause is outside its own handler", unitData ++ "deed pulse { 𝟙 pulse: 𝟙 }; try null pulse { pulse | null pulse }")
  , ("unhandled sibling operation escapeth", duoEffect ++ "try null second { first | null }")
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
  [ evalOkWith "imported call" "bring file.tung; 2 inc" (Map.insert "file.tung" "show let x inc = x + 1;" imports) "eval ok: 3"
  , evalOkWith "primitive shape defaults" "bring ground.tung; (1 < 2) ∧ (1 ≢ 2)" imports "eval ok: yea"
  , evalOkWith "false implieth false" "bring ground.tung; nay ≤ nay" imports "eval ok: yea"
  , evalOkWith "true doth not imply false" "bring ground.tung; yea ≤ nay" imports "eval ok: nay"
  , evalOkWith "euclidean division returneth quotient and remainder for a negative divisor" "bring ground.tung; bring data/product.tung; 5 ÷ -3" imports "eval ok: (-1 2 ∏)"
  , evalOkWith "euclidean division returneth quotient and remainder for a negative dividend" "bring ground.tung; bring data/product.tung; -5 ÷ 3" imports "eval ok: (-2 1 ∏)"
  , evalOkWith "euclidean quotient and remainder reconstruct the dividend" "bring ground.tung; bring data/product.tung; let dividend = -5; let divisor = -3; match dividend ÷ divisor { quotient ∏ rest | (quotient × divisor) + rest }" imports "eval ok: -5"
  , evalOkWith "euclidean division faileth at zero" "bring ground.tung; let _ ignore = 1; try (1 ÷ 0) ignore { fail | 9 }" imports "eval ok: 9"
  , evalOkWith "three-way comparison distinguishes each result" "bring ground.tung; bring data/product.tung; (0 compare 1) ∏ (1 compare 1) $∏ (2 compare 1)" imports "eval ok: ((fore mid ∏) aft ∏)"
  , evalOkWith "clamp keepeth a value inside its bounds" "bring ground.tung; 0 clamp 10 12" imports "eval ok: 10"
  , evalOkWith "qualified duplicate values" "bring left.tung; bring right.tung; left@foo + right@foo" duplicateImports "eval ok: 3"
  , evalOkWith "shared diamond import" "bring left.tung; bring right.tung; left@left + right@right" diamondImports "eval ok: 5"
  , evalOkWith "diamond import keepeth one fill identity" "bring left.tung; bring right.tung; 3 left@identity" fillDiamondImports "eval ok: 3"
  , evalOkWith "qualified shapes from different modules stay distinct" "bring left.tung; bring right.tung; 3 left@identity" distinctShapeImports "eval ok: 4"
  , evalOkWith "value survives a named re-export" "bring middle.tung; value" reexportImports "eval ok: 7"
  , evalOkWith "shape fill survives an import" "bring identity.tung; 3 identity" identityImports "eval ok: 3"
  , evalOkWith "data constructor survives a named re-export" "bring data-middle.tung; 4 box" dataReexportImports "eval ok: (4 box)"
  , evalOkWith "qualified constructor pattern useth import alias" "bring alias.tung; match 4 alias@box { x alias@box | x }" (Map.insert "alias.tung" "show kin a box { a box };" imports) "eval ok: 4"
  , evalOkWith "effect operation survives a named re-export" "bring ask-middle.tung; try 3 ask { x ask | x }" effectReexportImports "eval ok: 3"
  , evalTypeErrWith "private qualified value is rejected before runtime" "bring private.tung; private@hidden" (Map.fromList [("private.tung", "let hidden = 1;")])
  , evalOkWith "text and unicode-list round trip" "bring ground.tung; 'λ😀' text-to-list $ list-to-text" imports "eval ok: 'λ😀'"
  , evalOkWith "text converteth to a unicode list" "bring ground.tung; 'λ😀' text-to-list" imports "eval ok: (`λ (`😀 empty .*) .*)"
  , evalOkWith "unicode converteth to its scalar integer" "bring ground.tung; `😀 unicode-to-integer" imports "eval ok: 128512"
  , evalOkWith "scalar integer converteth to unicode" "bring ground.tung; 128512 integer-to-unicode" imports "eval ok: `😀"
  , evalOkWith "negative integer cannot become unicode" "bring ground.tung; try -1 integer-to-unicode { return _ | 'valid', message fail | message }" imports "eval ok: 'invalid unicode scalar value'"
  , evalOkWith "surrogate integer cannot become unicode" "bring ground.tung; try 55296 integer-to-unicode { return _ | 'valid', message fail | message }" imports "eval ok: 'invalid unicode scalar value'"
  , evalOkWith "out-of-range integer cannot become unicode" "bring ground.tung; try 1114112 integer-to-unicode { return _ | 'valid', message fail | message }" imports "eval ok: 'invalid unicode scalar value'"
  , evalOkWith "foreign text join" "bring ground.tung; 'left' join-text '右'" imports "eval ok: 'left右'"
  , evalOkWith "text behead exposeth one code point" "bring ground.tung; bring data/list.tung; bring data/option.tung; bring data/product.tung; bring algebra/total/magma.tung; match 'λ字' behead-text { (c ∏ rest) data/option@some | ((c .* empty) list-to-text) * rest, data/option@none | '' }" imports "eval ok: 'λ字'"
  , evalOkWith "empty text hath no head" "bring ground.tung; bring data/option.tung; match '' behead-text { none | 1, _ some | 0 }" imports "eval ok: 1"
  , evalOkWith "bulk text concatenation" "bring ground.tung; bring data/list.tung; ('north' .* ('南' .* empty)) fold-join-text" imports "eval ok: 'north南'"
  , evalOkWith "text shapes dispatch" "bring ground.tung; bring algebra/total/magma.tung; (('a' * 'β') ≡ 'aβ') ∧ ('a' ≤ 'b')" imports "eval ok: yea"
  , evalOkWith "unicode shapes dispatch" "bring ground.tung; (`a ≡ `a) ∧ (`a ≤ `b)" imports "eval ok: yea"
  , evalOkWith "imported fill doth not shadow native integer order" "bring data/list.tung; 0 till 2" imports "eval ok: (0 (1 (2 empty .*) .*) .*)"
  , evalOkWith "list map pipeline keepeth the list functor" "bring ground.tung; bring data/list.tung; 0 till 2 $ map { x | x + 1 } $ map to-text" imports "eval ok: ('1' ('2' ('3' empty .*) .*) .*)"
  , evalOkWith "list apply calleth its separately filled map" "bring ground.tung; bring data/list.tung; let values = 1 .* (2 .* empty); let functions = { x | x + 10 } .* ({ x | x × 2 } .* empty); values apply functions" imports "eval ok: (11 (12 (2 (4 empty .*) .*) .*) .*)"
  , evalOkWith "list traversal sequences option values" "bring ground.tung; bring data/list.tung; bring data/option.tung; bring collection/traverse.tung; (1 .* (2 .* empty)) traverse { x | x data/option@some }" imports "eval ok: ((1 (2 empty .*) .*) some)"
  , evalOkWith "fold-map combineth mapped list values" "bring ground.tung; bring data/list.tung; bring collection/catamorphism.tung; (0 till 2) fold-map to-text" imports "eval ok: '012'"
  , evalOkWith "bounded conjunction folds values" "bring ground.tung; bring data/list.tung; bring collection/catamorphism.tung; yea .* (nay .* empty) $ …∧" imports "eval ok: nay"
  , evalOkWith "bounded disjunction folds values" "bring ground.tung; bring data/list.tung; bring collection/catamorphism.tung; nay .* (yea .* empty) $ …∨" imports "eval ok: yea"
  , evalOkWith "empty bounded conjunction useth the upper endpoint" "bring ground.tung; bring data/list.tung; bring collection/catamorphism.tung; let values: 𝟚 list = empty; values …∧" imports "eval ok: yea"
  , evalOkWith "empty bounded disjunction useth the lower endpoint" "bring ground.tung; bring data/list.tung; bring collection/catamorphism.tung; let values: 𝟚 list = empty; values …∨" imports "eval ok: nay"
  , evalOkWith "boolean complement negateth" "bring ground.tung; yea ¬" imports "eval ok: nay"
  , evalTypeErrWith "bounded lattice need not have a complement" "bring ground.tung; mid ¬" imports
  , evalOkWith "integer lattice chooseth the lesser value" "bring ground.tung; 3 ∧ 2" imports "eval ok: 2"
  , evalOkWith "non-total powerset lattice joineth predicates" "bring ground.tung; bring data/list.tung; bring data/powerset.tung; bring collection/catamorphism.tung; let one: integer powerset = { x | x ≡ 1 }; let two: integer powerset = { x | x ≡ 2 }; let joined = one .* (two .* data/list@empty) $ …∨; joined ∋ 2" imports "eval ok: yea"
  , evalOkWith "empty powerset meet yieldeth the universal set" "bring ground.tung; bring data/list.tung; bring data/powerset.tung; bring collection/catamorphism.tung; let values: (integer powerset) list = data/list@empty; let all = values …∧; all ∋ 42" imports "eval ok: yea"
  , evalOkWith "pattern binders do not become constructor patterns" "bring ground.tung; bring data/list.tung; kin bit { off }; 0 till 2 $ map { _ | off } $ map { _ | 1 }" imports "eval ok: (1 (1 (1 empty .*) .*) .*)"
  , evalOkWith "control branch suspends actions" "bring ground.tung; nay branch { _ | 1 } { _ | 2 }" imports "eval ok: 2"
  , evalOkWith "option maybe mapeth present value" "bring ground.tung; bring data/option.tung; (3 some) maybe 0 { x | x + 1 }" imports "eval ok: 4"
  , evalOkWith "sum either consumeth selected side" "bring ground.tung; bring data/sum.tung; (2 inject₁) either { x | x + 1 } { y | y × 3 }" imports "eval ok: 6"
  , evalOkWith "sum functor mapeth the right side" "bring ground.tung; bring data/sum.tung; let value: text ∐ integer = 2 inject₁; match (value map { x | x + 1 }) { _ inject₀ | 0, x inject₁ | x }" imports "eval ok: 3"
  , evalOkWith "sum applicative applieth the right side" "bring ground.tung; bring data/sum.tung; let value: text ∐ integer = 2 inject₁; let f: text ∐ (integer → integer) = { x | x + 3 } inject₁; match value apply f { _ inject₀ | 0, x inject₁ | x }" imports "eval ok: 5"
  , evalOkWith "sum monad preserveth a left value" "bring ground.tung; bring data/sum.tung; let value: text ∐ integer = 'left' inject₀; match (value map-flat { x | (x + 1) inject₁ }) { message inject₀ | message, x inject₁ | x to-text }" imports "eval ok: 'left'"
  , evalOkWith "sum catamorphism foldeth the right side" "bring ground.tung; bring data/sum.tung; bring collection/catamorphism.tung; let value: text ∐ integer = 3 inject₁; value foldl 1 +" imports "eval ok: 4"
  , evalOkWith "sum traversal preserveth its side" "bring ground.tung; bring data/sum.tung; bring data/option.tung; bring collection/traverse.tung; let value: text ∐ integer = 3 inject₁; value traverse { x | (x + 1) some }" imports "eval ok: ((4 inject₁) some)"
  , evalOkWith "product bimap mapeth both fields" "bring ground.tung; bring data/product.tung; (1 ∏ 2) bimap { x | x + 1 } { y | y × 3 }" imports "eval ok: (2 6 ∏)"
  , evalOkWith "product applicative accumulateth value context first" "bring ground.tung; bring data/product.tung; let value = 'value:' ∏ 2; let f = 'function:' ∏ { x | x + 1 }; value apply f" imports "eval ok: ('value:function:' 3 ∏)"
  , evalOkWith "product monad accumulateth outer context first" "bring ground.tung; bring data/product.tung; ('outer:' ∏ 2) map-flat { x | 'inner:' ∏ (x + 1) }" imports "eval ok: ('outer:inner:' 3 ∏)"
  , evalOkWith "product catamorphism foldeth the second field" "bring ground.tung; bring data/product.tung; bring collection/catamorphism.tung; ('ignored' ∏ 3) foldl 1 +" imports "eval ok: 4"
  , evalOkWith "product traversal preserveth the first field" "bring ground.tung; bring data/product.tung; bring data/option.tung; bring collection/traverse.tung; ('context' ∏ 3) traverse { x | (x + 1) some }" imports "eval ok: (('context' 4 ∏) some)"
  , evalOkWith "applicative lift2 for option" "bring ground.tung; bring data/option.tung; (1 some) lift₂ (2 some) +" imports "eval ok: (3 some)"
  , evalOkWith "monad void for option" "bring data/option.tung; bring collection/monad.tung; (1 some) void" imports "eval ok: (null some)"
  , evalOkWith "list behead exposeth the head and tail" "bring data/list.tung; bring data/option.tung; bring data/product.tung; match (1 .* empty) behead { (head data/product@∏ _) data/option@some | head, data/option@none | 0 }" imports "eval ok: 1"
  , evalOkWith "list take after drop" "bring data/list.tung; ((0 till 5) drop 2) take 2" imports "eval ok: (2 (3 empty .*) .*)"
  , evalOkWith "list find returneth first match" "bring ground.tung; bring data/list.tung; (0 till 5) find { x | 3 ≤ x }" imports "eval ok: (3 some)"
  , evalOkWith "list find short-circuits effectful predicates" "bring ground.tung; bring data/list.tung; bring data/option.tung; deed late { 𝟙 late: 𝟚 }; let values = 1 .* (2 .* empty); try (values find { x | match x ≤ 1 { yea | yea, nay | null late } }) { late | none }" imports "eval ok: (1 some)"
  , evalOkWith "list indexing is zero based" "bring data/list.tung; (10 .* (20 .* empty)) at 1" imports "eval ok: (20 some)"
  , evalOkWith "negative list index is absent" "bring ground.tung; bring data/list.tung; let index = 0 - 1; (10 .* empty) at index" imports "eval ok: none"
  , evalOkWith "list last returneth the final value" "bring data/list.tung; (1 .* (2 .* empty)) last" imports "eval ok: (2 some)"
  , evalOkWith "list membership findeth a value" "bring ground.tung; bring data/list.tung; (1 .* (2 .* empty)) ∋ 2" imports "eval ok: yea"
  , evalOkWith "list membership rejecteth a missing value" "bring ground.tung; bring data/list.tung; (1 .* (2 .* empty)) ∋ 3" imports "eval ok: nay"
  , evalOkWith "list replicate useth a nonnegative count" "bring data/list.tung; 'x' replicate 3" imports "eval ok: ('x' ('x' ('x' empty .*) .*) .*)"
  , evalOkWith "list partition preserveth order" "bring ground.tung; bring data/list.tung; (0 till 4) partition { x | 2 ≤ x }" imports "eval ok: ((2 (3 (4 empty .*) .*) .*) (0 (1 empty .*) .*) ∏)"
  , evalOkWith "list unzip preserveth both sides" "bring data/list.tung; bring data/product.tung; ((1 ∏ 'a') .* ((2 ∏ 'b') .* empty)) unzip" imports "eval ok: ((1 (2 empty .*) .*) ('a' ('b' empty .*) .*) ∏)"
  , evalOkWith "n-ary sum addeth a foldable collection" "bring ground.tung; bring data/list.tung; bring collection/catamorphism.tung; (1 .* (2 .* (3 .* empty))) …+" imports "eval ok: 6"
  , evalOkWith "n-ary sum of an empty collection is zero" "bring ground.tung; bring data/list.tung; bring collection/catamorphism.tung; let values: integer list = empty; values …+" imports "eval ok: 0"
  , evalTypeErrWith "n-ary sum needeth a catamorphism" "bring ground.tung; bring collection/catamorphism.tung; kin a box { a box }; let value: integer box = 1 box; value …+" imports
  , evalOkWith "n-ary product multiplies a foldable collection" "bring ground.tung; bring data/list.tung; bring collection/catamorphism.tung; (2 .* (3 .* (4 .* empty))) …×" imports "eval ok: 24"
  , evalOkWith "n-ary product of an empty collection is one" "bring ground.tung; bring data/list.tung; bring collection/catamorphism.tung; let values: integer list = empty; values …×" imports "eval ok: 1"
  , evalTypeErrWith "n-ary product needeth a multiplicative monoid" "bring ground.tung; bring data/list.tung; bring collection/catamorphism.tung; ('a' .* empty) …×" imports
  , evalOkWith "boolean supremal wrapper useth disjunction" "bring ground.tung; bring data/list.tung; bring algebra/total/semigroup.tung; bring collection/catamorphism.tung; let values = (nay supremal) .* ((yea supremal) .* data/list@empty); match values fold { result supremal | result }" imports "eval ok: yea"
  , evalOkWith "empty boolean supremal fold yieldeth nay" "bring ground.tung; bring data/list.tung; bring algebra/total/semigroup.tung; bring collection/catamorphism.tung; let values: (𝟚 supremal) list = data/list@empty; match values fold { result supremal | result }" imports "eval ok: nay"
  , evalOkWith "boolean infimal wrapper useth conjunction" "bring ground.tung; bring data/list.tung; bring algebra/total/semigroup.tung; bring collection/catamorphism.tung; let values = (yea infimal) .* ((nay infimal) .* data/list@empty); match values fold { result infimal | result }" imports "eval ok: nay"
  , evalOkWith "empty boolean infimal fold yieldeth yea" "bring ground.tung; bring data/list.tung; bring algebra/total/semigroup.tung; bring collection/catamorphism.tung; let values: (𝟚 infimal) list = data/list@empty; match values fold { result infimal | result }" imports "eval ok: yea"
  , evalOkWith "natural semiring computeth with both identities" "bring ground.tung; bring data/natural.tung; let two: natural = (data/natural@zero suc) suc; let three = two suc; ((two × three) + one) natural-to-integer" imports "eval ok: 7"
  , evalOkWith "natural semiring supplieth multiplicative monoid evidence" "bring ground.tung; bring data/list.tung; bring data/natural.tung; bring collection/catamorphism.tung; let two: natural = (data/natural@zero suc) suc; let three = two suc; ((two .* (three .* data/list@empty)) …×) natural-to-integer" imports "eval ok: 6"
  , evalOkWith "nonnegative integer converteth to natural" "bring ground.tung; bring data/natural.tung; 3 integer-to-natural $ natural-to-integer" imports "eval ok: 3"
  , evalOkWith "negative integer cannot become natural" "bring ground.tung; try -1 integer-to-natural { return _ | 'valid', message fail | message }" imports "eval ok: 'negative cannot be a natural'"
  , evalOkWith "complex sine of zero is zero" "bring ground.tung; bring numeric/complex.tung; ((0.0 complex 0.0) sine) real" imports "eval ok: 0.0"
  , evalOkWith "complex cosine of zero is one" "bring ground.tung; bring numeric/complex.tung; ((0.0 complex 0.0) cosine) real" imports "eval ok: 1.0"
  , evalOkWith "complex exponentiation scaleth by the real exponent" "bring ground.tung; bring numeric/complex.tung; ((1.0 complex 0.0) exponent) real" imports "eval ok: 2.718281828459045"
  , evalOkWith "complex sine of unit imaginary yieldeth hyperbolic sine" "bring ground.tung; bring numeric/complex.tung; ((0.0 complex 1.0) sine) imaginary" imports "eval ok: 1.1752011936438014"
  , evalOkWith "complex cosine of unit imaginary yieldeth hyperbolic cosine" "bring ground.tung; bring numeric/complex.tung; ((0.0 complex 1.0) cosine) real" imports "eval ok: 1.5430806348152437"
  , evalOkWith "nonempty map preserveth the first element" "bring ground.tung; bring data/list.tung; bring data/nonempty.tung; ((1 nonempty (2 .* empty)) map { x | x + 1 }) nonempty-to-list" imports "eval ok: (2 (3 empty .*) .*)"
  , evalOkWith "nonempty semigroup appendeth without losing the head" "bring ground.tung; bring data/list.tung; bring data/nonempty.tung; bring algebra/total/magma.tung; ((1 nonempty (2 .* empty)) * (3 nonempty empty)) nonempty-to-list" imports "eval ok: (1 (2 (3 empty .*) .*) .*)"
  , evalOkWith "nonempty applicative applieth every function" "bring ground.tung; bring data/list.tung; bring data/nonempty.tung; let values = 1 nonempty (2 .* empty); let fs = { x | x + 1 } nonempty ({ x | x × 2 } .* empty); (values apply fs) nonempty-to-list" imports "eval ok: (2 (3 (2 (4 empty .*) .*) .*) .*)"
  , evalOkWith "nonempty monad concatenateth every result" "bring ground.tung; bring data/list.tung; bring data/nonempty.tung; let values = 1 nonempty (2 .* empty); (values map-flat { x | x nonempty ((x + 10) .* empty) }) nonempty-to-list" imports "eval ok: (1 (11 (2 (12 empty .*) .*) .*) .*)"
  , evalOkWith "nonempty catamorphism visiteth the head first" "bring ground.tung; bring data/list.tung; bring data/nonempty.tung; bring collection/catamorphism.tung; (1 nonempty (2 .* empty)) foldl 0 +" imports "eval ok: 3"
  , evalOkWith "nonempty traversal keepeth a nonempty result" "bring ground.tung; bring data/list.tung; bring data/nonempty.tung; bring data/option.tung; bring collection/traverse.tung; ((1 nonempty (2 .* empty)) traverse { x | (x + 1) some }) map nonempty-to-list" imports "eval ok: ((2 (3 empty .*) .*) some)"
  , evalOkWith "nonempty membership delegateth to list" "bring ground.tung; bring data/list.tung; bring data/nonempty.tung; (1 nonempty (2 .* empty)) ∋ 2" imports "eval ok: yea"
  , evalOkWith "table put replaceth a key" "bring ground.tung; bring data/table.tung; let table = (empty put 'a' 1) put 'a' 2; table lookup 'a'" imports "eval ok: (2 some)"
  , evalOkWith "table membership dispatcheth through inhold" "bring ground.tung; bring data/table.tung; let table = empty put 'a' 1; table ∋ 'a'" imports "eval ok: yea"
  , evalOkWith "table constructor may be qualified" "bring ground.tung; bring data/list.tung; bring data/product.tung; bring data/table.tung; (('a' ∏ 2) .* data/list@empty) data/table@from-list $ lookup 'a'" imports "eval ok: (2 some)"
  , evalOkWith "table rid droppeth a key" "bring ground.tung; bring data/table.tung; let table = empty put 'a' 1; (table rid 'a') lookup 'a'" imports "eval ok: none"
  , evalOkWith "set put keepeth values unique" "bring ground.tung; bring data/set.tung; ((empty put 1) put 1) to-list" imports "eval ok: (1 empty .*)"
  , evalOkWith "set constructor may be qualified" "bring ground.tung; bring data/list.tung; bring data/set.tung; (1 .* (2 .* data/list@empty)) data/set@from-list $ to-list" imports "eval ok: (1 (2 empty .*) .*)"
  , evalOkWith "set rid droppeth a value" "bring ground.tung; bring data/set.tung; let values = (empty put 1) put 2; (values rid 1) ∋ 1" imports "eval ok: nay"
  , evalOkWith "set supremum keepeth unique values" "bring ground.tung; bring data/set.tung; let left = (empty put 1) put 2; let right = (empty put 2) put 3; (left ∨ right) to-list" imports "eval ok: (3 (2 (1 empty .*) .*) .*)"
  , evalOkWith "set infimum keepeth shared values" "bring ground.tung; bring data/set.tung; let left = (empty put 1) put 2; let right = (empty put 2) put 3; (left ∧ right) to-list" imports "eval ok: (2 empty .*)"
  , evalOkWith "set equality ignoreth insertion order" "bring ground.tung; bring data/set.tung; let left = (empty put 1) put 2; let right = (empty put 2) put 1; left ≡ right" imports "eval ok: yea"
  , evalOkWith "set partial order compareth inclusion" "bring ground.tung; bring data/set.tung; let smaller = empty put 1; let larger = smaller put 2; (smaller ≤ larger) ∧ (smaller < larger)" imports "eval ok: yea"
  , evalOkWith "set inclusion rejecteth a proper superset" "bring ground.tung; bring data/set.tung; let smaller = empty put 1; let larger = smaller put 2; larger ≤ smaller" imports "eval ok: nay"
  , evalOkWith "powerset addition is symmetric difference" "bring ground.tung; bring data/powerset.tung; let left: integer powerset = { value | (value ≡ 1) ∨ (value ≡ 2) }; let right: integer powerset = { value | (value ≡ 2) ∨ (value ≡ 3) }; ((left + right) ∋ 2)" imports "eval ok: nay"
  , evalOkWith "powerset addition keepeth an unshared member" "bring ground.tung; bring data/powerset.tung; let left: integer powerset = { value | value ≡ 1 }; let right: integer powerset = { value | value ≡ 2 }; ((left + right) ∋ 1)" imports "eval ok: yea"
  , evalOkWith "powerset multiplication is intersection" "bring ground.tung; bring data/powerset.tung; let left: integer powerset = { value | (value ≡ 1) ∨ (value ≡ 2) }; let right: integer powerset = { value | value ≡ 2 }; ((left × right) ∋ 2)" imports "eval ok: yea"
  , evalOkWith "powerset supremum is union" "bring ground.tung; bring data/powerset.tung; let left: integer powerset = { value | value ≡ 1 }; let right: integer powerset = { value | value ≡ 2 }; (left ∨ right) ∋ 2" imports "eval ok: yea"
  , evalOkWith "powerset infimum is intersection" "bring ground.tung; bring data/powerset.tung; let left: integer powerset = { value | (value ≡ 1) ∨ (value ≡ 2) }; let right: integer powerset = { value | value ≡ 2 }; (left ∧ right) ∋ 1" imports "eval ok: nay"
  , evalOkWith "powerset complement excludeth a former member" "bring ground.tung; bring data/powerset.tung; let one: integer powerset = { value | value ≡ 1 }; let outside: integer powerset = one ¬; outside ∋ 1" imports "eval ok: nay"
  , evalOkWith "powerset complement includeth a former nonmember" "bring ground.tung; bring data/powerset.tung; let one: integer powerset = { value | value ≡ 1 }; let outside: integer powerset = one ¬; outside ∋ 2" imports "eval ok: yea"
  , evalOkWith "powerset subtraction is symmetric difference" "bring ground.tung; bring data/powerset.tung; let values: integer powerset = { value | value ≡ 1 }; ((values - values) ∋ 1)" imports "eval ok: nay"
  , evalOkWith "powerset zero is empty" "bring ground.tung; bring data/powerset.tung; let values: integer powerset = zero; values ∋ 7" imports "eval ok: nay"
  , evalOkWith "powerset one is universal" "bring ground.tung; bring data/powerset.tung; let values: integer powerset = one; values ∋ 7" imports "eval ok: yea"
  , evalOkWith "set becometh a powerset" "bring ground.tung; bring data/set.tung; bring data/powerset.tung; let finite = data/set@empty data/set@put 7; (finite to-powerset) ∋ 7" imports "eval ok: yea"
  , evalOkWith "powerset infimal wrapper useth intersection" "bring ground.tung; bring data/list.tung; bring data/powerset.tung; bring algebra/total/semigroup.tung; bring collection/catamorphism.tung; let one: integer powerset = { value | value ≡ 1 }; let both: integer powerset = { value | (value ≡ 1) ∨ (value ≡ 2) }; let wrapped = (one infimal) .* ((both infimal) .* data/list@empty); match wrapped fold { result infimal | result ∋ 2 }" imports "eval ok: nay"
  , evalOkWith "powerset supremal wrapper useth union" "bring ground.tung; bring data/list.tung; bring data/powerset.tung; bring algebra/total/semigroup.tung; bring collection/catamorphism.tung; let one: integer powerset = { value | value ≡ 1 }; let two: integer powerset = { value | value ≡ 2 }; let wrapped = (one supremal) .* ((two supremal) .* data/list@empty); match wrapped fold { result supremal | result ∋ 2 }" imports "eval ok: yea"
  , evalTypeErrWith "powerset hath no arbitrarily chosen monoid" "bring ground.tung; bring data/list.tung; bring data/powerset.tung; bring algebra/total/semigroup.tung; bring collection/catamorphism.tung; let values: (integer powerset) list = data/list@empty; values fold" imports
  , evalOkWith "table functor mapeth values" "bring ground.tung; bring data/table.tung; let table = (empty put 'a' 1) put 'b' 2; (table map { x | x + 10 }) lookup 'a'" imports "eval ok: (11 some)"
  , evalOkWith "table filter keepeth matching values" "bring ground.tung; bring data/table.tung; bring collection/filter.tung; let table = (empty put 'a' 1) put 'b' 2; (table filter { x | 2 ≤ x }) lookup 'a'" imports "eval ok: none"
  , evalOkWith "table merge preferreth right values" "bring ground.tung; bring data/table.tung; let left = empty put 'a' 1; let right = empty put 'a' 2; (left merge right) lookup 'a'" imports "eval ok: (2 some)"
  , evalOkWith "table adjust changeth an existing value" "bring ground.tung; bring data/table.tung; let table = empty put 'a' 1; (table adjust 'a' { x | x + 1 }) lookup 'a'" imports "eval ok: (2 some)"
  , evalOkWith "text operations use unicode code points" "bring ground.tung; bring text/operation.tung; ('aβ字' reverse-text) take-text 2" imports "eval ok: '字β'"
  , evalOkWith "text size counteth unicode code points" "bring ground.tung; bring text/operation.tung; bring collection/size.tung; bring data/natural.tung; ('aβ字' size) natural-to-integer" imports "eval ok: 3"
  , evalOkWith "text split and join preserve empty fields" "bring ground.tung; bring text/operation.tung; ('a,b,,c' split-text `,) join-with-text '|'" imports "eval ok: 'a|b||c'"
  , evalOkWith "text words collapse whitespace runs" "bring ground.tung; bring text/operation.tung; ' red\\tblue\\nred ' words-text $ join-with-text '|'" imports "eval ok: 'red|blue|red'"
  , evalOkWith "typed paths join components" "bring ground.tung; bring data/path.tung; (('root' path) join-path ('leaf' path)) to-text" imports "eval ok: 'root/leaf'"
  , evalOkWith "time spans drive async sleep" "bring ground.tung; bring data/time-span.tung; (0 milliseconds) sleep-for" imports "eval ok: null"
  , evalOkWith "process captureth standard output" "bring ground.tung; bring data/list.tung; ('printf' run-process ('hello' .* empty)) process-output" imports "eval ok: 'hello'"
  , evalTypeErrWith "missing import is rejected before runtime" "bring missing.tung;" Map.empty
  , evalTypeErrWith "import cycle is rejected before runtime" "bring left.tung;" cyclicImports
  , evalOkWith "arctan range is nonnegative" "bring _foreign.tung; 0.0 arctan-float -1.0" imports "eval ok: 4.71238898038469"
  , evalOkWith "fork and wait" "bring ground.tung; let _ work = 42; let task = work fork; task wait" imports "eval ok: 42"
  , evalOkWith "task result may be awaited repeatedly" "bring ground.tung; let _ work = 21; let task = work fork; (task wait) + (task wait)" imports "eval ok: 42"
  , evalOkWith "task wait may time out" "bring ground.tung; let _ work = (let _ = 200 sleep; 42); let task = work fork; task wait-for 0" imports "eval ok: none"
  , evalOkWith "task wait-for returneth a finished value" "bring ground.tung; let _ work = 42; let task = work fork; task wait-for 1000" imports "eval ok: (42 some)"
  , evalOkWith "fordone task faileth through the declared effect" "bring ground.tung; let _ work = (let _ = 1000 sleep; 42); let task = work fork; let _ = task fordo; try task wait { return _ | 'finished', message fail | message }" imports "eval ok: 'task cancelled'"
  , evalOkWith "cpu-bound task remaineth cancellable" "bring ground.tung; let _ loop = null loop; let spin = loop; let task = spin fork; let _ = task fordo; try task wait { return _ | 'finished', message fail | message }" imports "eval ok: 'task cancelled'"
  , evalTypeErrWith "task constructor is private" "bring ground.tung; 42 done" imports
  , concurrentForks imports
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

concurrentForks :: Map.Map String String -> Test
concurrentForks imports = do
  baselineStart <- getPOSIXTime
  _ <- evaluateWithImports baselineSource imports
  baseline <- subtract baselineStart <$> getPOSIXTime
  before <- getPOSIXTime
  actual <- evaluateWithImports source imports
  elapsed <- subtract before <$> getPOSIXTime
  pure $
    if actual == "eval ok: 42" && elapsed < baseline + 0.95
      then Nothing
      else Just ("concurrent forks should overlap: " ++ actual ++ " after " ++ show elapsed ++ " seconds; baseline " ++ show baseline)
 where
  baselineSource =
    "bring ground.tung; "
      ++ "let _ work = 21; "
      ++ "let left = work fork; let right = work fork; (left wait) + (right wait)"
  source =
    "bring ground.tung; "
      ++ "let _ work = (let _ = 600 sleep; 21); "
      ++ "let left = work fork; let right = work fork; (left wait) + (right wait)"

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
