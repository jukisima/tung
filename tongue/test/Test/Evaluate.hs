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
    ++ "deed choice { 𝟙 pick: integer } yield try null pick { pick | "
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
  , ("ascribed anonymous function remaineth recursive", "let factorial = ({ 0 | 1, n | n × ((n - 1) factorial) }: integer → integer) yield 5 factorial", "eval ok: 120")
  , ("integer arithmetic is arbitrary precision", "yield 999999999999999999999999999999 + 1", "eval ok: 1000000000000000000000000000000")
  , ("fremmed key selecteth a host function independently of the let name", "let plus: integer → integer → integer = 'add-integer' fremmed yield 1 plus 2", "eval ok: 3")
  , ("unicode text", "yield 'λ字'", "eval ok: 'λ字'")
  , ("unicode control rendering useth decimal terminator", "yield `\\1;", "eval ok: `\\1;")
  , ("float arithmetic", "let a = '0.5' from-text let b = '0.25' from-text yield a + b", "eval ok: 0.75")
  , ("float division useth the division slash", "yield 6.0 ∕ 4.0", "eval ok: 1.5")
  , ("float division by zero followeth ieee", "yield 1.0 ∕ 0.0", "eval ok: Infinity")
  , ("float equality ignoreth literal spelling", "kin 𝟚 { yea, nay } yield 1.0 ≡ 1.00", "eval ok: yea")
  , ("float ordering is numeric", "kin 𝟚 { yea, nay } yield 2.0 ≤ 10.0", "eval ok: yea")
  , ("curried partial application", "let a add b = a + b let add-one = 1 add yield 2 add-one", "eval ok: 3")
  , ("three curried arguments", "let a add b c = (a + b) + c yield 1 add 2 3", "eval ok: 6")
  , ("local recursive function", "kin natural { zero, natural suc } let count = (let loop = { zero | 0, n suc | 1 + (n loop) } yield loop) yield ((zero suc) suc) count", "eval ok: 2")
  , ("second-position function header", "let a add b = a + b yield 1 add 2", "eval ok: 3")
  , ("dollar function segment", "let x f = x + 1 let x h y z = (x + y) × z yield 1 f $h 2 3", "eval ok: 12")
  , ("lexical closure", "let x = 1 let _ f = x yield (let x = 2 yield 0 f)", "eval ok: 1")
  , ("multi-scrutinee match", boolData ++ "yield match yea, nay { yea, yea | 1, yea, nay | 2, nay, yea | 3, nay, nay | 4 }", "eval ok: 2")
  , ("nested constructor match", boolData ++ optionData ++ "yield match nay some { yea some | 1, nay some | 2, none | 0 }", "eval ok: 2")
  , ("first matching arm winneth", boolData ++ "yield match yea { yea | 1, _ | 2 }", "eval ok: 1")
  , ("integer match patterns", "yield match -1 { 0 | 10, -1 | 20, _ | 30 }", "eval ok: 20")
  , ("integer patterns fall through", "yield match 2 { 0 | 10, 1 | 20, _ | 30 }", "eval ok: 30")
  , ("integer function patterns", "let classify: integer → integer = { 0 | 10, 1 | 20, _ | 30 } yield 1 classify", "eval ok: 20")
  , ("unchosen arm is not evaluated", boolData ++ "let _ ignore = 1 yield match yea { yea | 1, nay | (1 ÷ 0) ignore }", "eval ok: 1")
  , ("let right side is strict", stopEffect ++ "yield try (let x = 1 stop yield 2 stop) { x stop | x }", "eval ok: 1")
  , ("function expression runneth before arguments", stopEffect ++ "yield try 0 (1 stop) (2 stop) { x stop | x }", "eval ok: 1")
  , ("arguments run left to right", stopEffect ++ "let x choose _ = x yield try (1 stop) choose (2 stop) { yield x | x, x stop | x }", "eval ok: 1")
  , ("record fields run left to right", stopEffect ++ "yield try r(first = 1 stop, second = 2 stop) { yield _ | 0, x stop | x }", "eval ok: 1")
  , ("record update evaluateth base first", stopEffect ++ "let base = r(field = 0) yield try r(= (let _ = 1 stop yield base), field = 2 stop) { yield _ | 0, x stop | x }", "eval ok: 1")
  , ("match scrutinees run left to right", stopEffect ++ "yield try match 1 stop, 2 stop { x, y | x } { x stop | x }", "eval ok: 1")
  , ("record update", "let person = r(name = 'n', age = 1) yield r(= person, age = 2)", "eval ok: r(age = 2, name = 'n')")
  , ("record access", "let person = r(name = 'n', age = 1) yield person@age", "eval ok: 1")
  , ("record removal", "let person = r(name = 'n', age = 1) yield r(= person, - age)", "eval ok: r(name = 'n')")
  , ("handled division failure", "let _ ignore = 1 yield try (1 ÷ 0) ignore { fail | 9 }", "eval ok: 9")
  , ("operation payload", "deed e fail { e fail: a } let _ ignore = 'ok' yield try (1 ÷ 0) ignore { yield text | text, message fail | message }", "eval ok: 'division by zero'")
  , ("yield clause mapeth normal answer", "yield try 2 + 3 { yield n | n to-text }", "eval ok: '5'")
  , ("partial call performeth only when saturated", "let _ ignore = 0 yield try (0 (1 ÷)) ignore { fail | 7 }", "eval ok: 7")
  , ("eftgin once", "deed ask { integer ask: integer } yield try 10 ask { x ask | (x + 1) eftgin }", "eval ok: 11")
  , ("eftgin zero times", "deed ask { integer ask: integer } yield try 10 ask { x ask | x + 1 }", "eval ok: 11")
  , ("deep eftgin handleth later operation", unitData ++ "deed choice { 𝟙 pick: integer } yield try (null pick) + (null pick) { pick | 1 eftgin }", "eval ok: 2")
  , ("multi-shot eftgin", unitData ++ "deed choice { 𝟙 pick: integer } yield try (null pick) + (null pick) { pick | (1 eftgin) + (2 eftgin) }", "eval ok: 12")
  , ("yield runneth on each continued branch", unitData ++ "deed choice { 𝟙 pick: integer } yield try null pick { yield n | n + 10, pick | (1 eftgin) + (2 eftgin) }", "eval ok: 23")
  , ("polymorphic state get", unitData ++ "deed a state { 𝟙 get: a, a set: 𝟙 } yield try null get { get | 4 eftgin, x set | null eftgin }", "eval ok: 4")
  , ("effect-level clause catches either operation", duoEffect ++ "yield try null second { duo | null }", "eval ok: null")
  , ("nested handlers split operation coverage", duoEffect ++ "yield try (try null second { first | null }) { second | null }", "eval ok: null")
  , ("native sleep", unitData ++ "yield 0 sleep", "eval ok: null")
  , ("shape laws are erased", "shape a identity { let a identity: a law (x: a): x identity ~ x } fill integer identity { let x identity = x } yield 2 identity", "eval ok: 2")
  , ("result-only member useth expected type", "shape a origin { let origin: a } fill integer origin { let origin = 7 } let value: integer = origin yield value", "eval ok: 7")
  , ("different fills dispatch by inferred type", "shape a label { let a label: text } fill integer label { let _ label = 'integer' } fill text label { let _ label = 'text' } yield 'x' label", "eval ok: 'text'")
  , ("graith function receiveth its caller dictionary", "shape a label { let a label: text } fill integer label { let _ label = 'integer' } fill text label { let _ label = 'text' } graith a label let (x: a) labelled: text = x label yield 1 labelled", "eval ok: 'integer'")
  , ("child dictionary useth a direct parent fill", "shape a parent { let a parent: a } graith a parent shape a child { let a child: a } fill integer parent { let x parent = x } fill integer child { let x child = x parent } yield 4 child", "eval ok: 4")
  , ("shape default useth its child dictionary", "shape a source { let source: a } graith a source shape a derived { let derived: a = source } fill integer source { let source = 7 } fill integer derived {} let result: integer = derived yield result", "eval ok: 7")
  , ("fill graith remaineth an external dictionary", "shape a combine { let a combine a: a } fill integer combine { let x combine y = x + y } kin a box { a box } graith a combine fill (a box) combine { let (x box) combine (y box) = (x combine y) box } yield (1 box) combine (2 box)", "eval ok: (3 box)")
  ]

runtimeErrors :: [(String, String)]
runtimeErrors =
  [ ("division failure is unhandled", "yield 1 ÷ 0")
  , ("from-text failure is unhandled", "yield 'not a float' from-text")
  , ("handler clause is outside its own handler", unitData ++ "deed pulse { 𝟙 pulse: 𝟙 } yield try null pulse { pulse | null pulse }")
  , ("unhandled sibling operation escapeth", duoEffect ++ "yield try null second { first | null }")
  ]

typeErrors :: [(String, String)]
typeErrors =
  [ ("unknown value", "yield missing")
  , ("non-function application", "yield 1 2")
  , ("constructor overapplication", "kin a box { a box } yield 1 box 2")
  , ("missing record field", "let value = r(x = 1) yield value@y")
  , ("unknown record removal", "let person = r(name = 'n') yield r(= person, - age)")
  , ("update of non-record", "yield r(= 1, field = 2)")
  , ("non-exhaustive match", boolData ++ "yield match nay { yea | 1 }")
  ]

importedCases :: Map.Map String String -> [Test]
importedCases imports =
  [ evalOkWith "imported call" "bring file.tung yield 2 inc" (Map.insert "file.tung" "show let x inc = x + 1" imports) "eval ok: 3"
  , evalOkWith "default bring alias worketh for values, shapes, and constructor patterns" "bring data/item.tung let value: item@item = item@empty yield match (value item@identity) { item@item | 1 }" (Map.insert "data/item.tung" "show kin item { item } show shape a identity { let a identity: a } fill item identity { let x identity = x } show let empty: item = item" imports) "eval ok: 1"
  , evalOkWith "bring alias worketh for effect operations and handlers" "bring effect/ask.tung a yield try 3 a@ask { x a@ask | x }" (Map.insert "effect/ask.tung" "show deed ask { integer ask: integer }" imports) "eval ok: 3"
  , evalOkWith "primitive shape defaults" "bring ground.tung yield (1 < 2) ∧ (1 ≢ 2)" imports "eval ok: yea"
  , evalOkWith "false implieth false" "bring ground.tung yield nay ≤ nay" imports "eval ok: yea"
  , evalOkWith "true doth not imply false" "bring ground.tung yield yea ≤ nay" imports "eval ok: nay"
  , evalOkWith "euclidean division returneth quotient and remainder for a negative divisor" "bring ground.tung bring data/product.tung yield 5 ÷ -3" imports "eval ok: (-1 2 ∏)"
  , evalOkWith "euclidean division returneth quotient and remainder for a negative dividend" "bring ground.tung bring data/product.tung yield -5 ÷ 3" imports "eval ok: (-2 1 ∏)"
  , evalOkWith "euclidean quotient and remainder reconstruct the dividend" "bring ground.tung bring data/product.tung let dividend = -5 let divisor = -3 yield match dividend ÷ divisor { quotient ∏ rest | (quotient × divisor) + rest }" imports "eval ok: -5"
  , evalOkWith "euclidean division faileth at zero" "bring ground.tung let _ ignore = 1 yield try (1 ÷ 0) ignore { fail | 9 }" imports "eval ok: 9"
  , evalOkWith "three-way comparison distinguishes each result" "bring ground.tung bring data/product.tung yield (0 compare 1) ∏ (1 compare 1) $∏ (2 compare 1)" imports "eval ok: ((fore mid ∏) aft ∏)"
  , evalOkWith "clamp keepeth a value inside its bounds" "bring ground.tung yield 0 clamp 10 12" imports "eval ok: 10"
  , evalOkWith "qualified duplicate values" "bring left.tung bring right.tung yield left@foo + right@foo" duplicateImports "eval ok: 3"
  , evalOkWith "shared diamond import" "bring left.tung bring right.tung yield left@left + right@right" diamondImports "eval ok: 5"
  , evalOkWith "diamond import keepeth one fill identity" "bring left.tung bring right.tung yield 3 left@identity" fillDiamondImports "eval ok: 3"
  , evalOkWith "qualified shapes from different modules stay distinct" "bring left.tung bring right.tung yield 3 left@identity" distinctShapeImports "eval ok: 4"
  , evalOkWith "value survives a named re-export" "bring middle.tung yield value" reexportImports "eval ok: 7"
  , evalOkWith "shape fill survives an import" "bring identity.tung yield 3 identity" identityImports "eval ok: 3"
  , evalOkWith "specific fill outranketh a blanket fill" "shape a identity { let a identity: a } fill a identity { let x identity = x } fill integer identity { let x identity = x + 1 } yield 1 identity" imports "eval ok: 2"
  , evalOkWith "data constructor survives a named re-export" "bring data-middle.tung yield 4 box" dataReexportImports "eval ok: (4 box)"
  , evalOkWith "qualified constructor pattern useth import alias" "bring alias.tung yield match 4 alias@box { x alias@box | x }" (Map.insert "alias.tung" "show kin a box { a box }" imports) "eval ok: 4"
  , evalOkWith "effect operation survives a named re-export" "bring ask-middle.tung yield try 3 ask { x ask | x }" effectReexportImports "eval ok: 3"
  , evalTypeErrWith "private qualified value is rejected before runtime" "bring private.tung yield private@hidden" (Map.fromList [("private.tung", "let hidden = 1")])
  , evalOkWith "text and unicode-list round trip" "bring ground.tung yield 'λ😀' text-to-list $ list-to-text" imports "eval ok: 'λ😀'"
  , evalOkWith "text converteth to a unicode list" "bring ground.tung yield 'λ😀' text-to-list" imports "eval ok: (`λ (`😀 empty .*) .*)"
  , evalOkWith "unicode converteth to its scalar integer" "bring ground.tung yield `😀 unicode-to-integer" imports "eval ok: 128512"
  , evalOkWith "scalar integer converteth to unicode" "bring ground.tung yield 128512 integer-to-unicode" imports "eval ok: `😀"
  , evalOkWith "negative integer cannot become unicode" "bring ground.tung yield try -1 integer-to-unicode { yield _ | 'valid', message fail | message }" imports "eval ok: 'invalid unicode scalar value'"
  , evalOkWith "surrogate integer cannot become unicode" "bring ground.tung yield try 55296 integer-to-unicode { yield _ | 'valid', message fail | message }" imports "eval ok: 'invalid unicode scalar value'"
  , evalOkWith "out-of-range integer cannot become unicode" "bring ground.tung yield try 1114112 integer-to-unicode { yield _ | 'valid', message fail | message }" imports "eval ok: 'invalid unicode scalar value'"
  , evalOkWith "text behead exposeth one code point" "bring ground.tung bring data/list.tung bring data/option.tung bring data/product.tung bring algebra/total/magma.tung yield match 'λ字' behead-text { (c ∏ rest) option@some | ((c .* empty) list-to-text) * rest, option@none | '' }" imports "eval ok: 'λ字'"
  , evalOkWith "empty text hath no head" "bring ground.tung bring data/option.tung yield match '' behead-text { none | 1, _ some | 0 }" imports "eval ok: 1"
  , evalOkWith "generic text fold" "bring ground.tung bring data/list.tung bring collection/catamorphism.tung yield ('north' .* ('南' .* empty)) fold" imports "eval ok: 'north南'"
  , evalOkWith "text shapes dispatch" "bring ground.tung bring algebra/total/magma.tung yield (('a' * 'β') ≡ 'aβ') ∧ ('a' ≤ 'b')" imports "eval ok: yea"
  , evalOkWith "unicode shapes dispatch" "bring ground.tung yield (`a ≡ `a) ∧ (`a ≤ `b)" imports "eval ok: yea"
  , evalOkWith "imported fill doth not shadow native integer order" "bring data/list.tung yield 0 till 2" imports "eval ok: (0 (1 (2 empty .*) .*) .*)"
  , evalOkWith "list map pipeline keepeth the list functor" "bring ground.tung bring data/list.tung yield 0 till 2 $ map { x | x + 1 } $ map to-text" imports "eval ok: ('1' ('2' ('3' empty .*) .*) .*)"
  , evalOkWith "list apply calleth its separately filled map" "bring ground.tung bring data/list.tung let values = 1 .* (2 .* empty) let functions = { x | x + 10 } .* ({ x | x × 2 } .* empty) yield values apply functions" imports "eval ok: (11 (12 (2 (4 empty .*) .*) .*) .*)"
  , evalOkWith "list traversal sequences option values" "bring ground.tung bring data/list.tung bring data/option.tung bring collection/traverse.tung yield (1 .* (2 .* empty)) traverse { x | x option@some }" imports "eval ok: ((1 (2 empty .*) .*) some)"
  , evalOkWith "fold-map combineth mapped list values" "bring ground.tung bring data/list.tung bring collection/catamorphism.tung yield (0 till 2) fold-map to-text" imports "eval ok: '012'"
  , evalOkWith "bounded conjunction folds values" "bring ground.tung bring data/list.tung bring collection/catamorphism.tung yield yea .* (nay .* empty) $ …∧" imports "eval ok: nay"
  , evalOkWith "bounded disjunction folds values" "bring ground.tung bring data/list.tung bring collection/catamorphism.tung yield nay .* (yea .* empty) $ …∨" imports "eval ok: yea"
  , evalOkWith "empty bounded conjunction useth the upper endpoint" "bring ground.tung bring data/list.tung bring collection/catamorphism.tung let values: 𝟚 list = empty yield values …∧" imports "eval ok: yea"
  , evalOkWith "empty bounded disjunction useth the lower endpoint" "bring ground.tung bring data/list.tung bring collection/catamorphism.tung let values: 𝟚 list = empty yield values …∨" imports "eval ok: nay"
  , evalOkWith "boolean complement negateth" "bring ground.tung yield yea ¬" imports "eval ok: nay"
  , evalTypeErrWith "bounded lattice need not have a complement" "bring ground.tung yield mid ¬" imports
  , evalOkWith "integer lattice chooseth the lesser value" "bring ground.tung yield 3 ∧ 2" imports "eval ok: 2"
  , evalOkWith "non-total powerset lattice joineth predicates" "bring ground.tung bring data/list.tung bring data/powerset.tung bring collection/catamorphism.tung let one: integer powerset = { x | x ≡ 1 } let two: integer powerset = { x | x ≡ 2 } let joined = one .* (two .* list@empty) $ …∨ yield joined ∋ 2" imports "eval ok: yea"
  , evalOkWith "empty powerset meet yieldeth the universal set" "bring ground.tung bring data/list.tung bring data/powerset.tung bring collection/catamorphism.tung let values: (integer powerset) list = list@empty let all = values …∧ yield all ∋ 42" imports "eval ok: yea"
  , evalOkWith "pattern binders do not become constructor patterns" "bring ground.tung bring data/list.tung kin bit { off } yield 0 till 2 $ map { _ | off } $ map { _ | 1 }" imports "eval ok: (1 (1 (1 empty .*) .*) .*)"
  , evalOkWith "control branch suspends actions" "bring ground.tung yield nay branch { _ | 1 } { _ | 2 }" imports "eval ok: 2"
  , evalOkWith "option maybe mapeth present value" "bring ground.tung bring data/option.tung yield (3 some) maybe 0 { x | x + 1 }" imports "eval ok: 4"
  , evalOkWith "sum either consumeth selected side" "bring ground.tung bring data/sum.tung yield (2 inject₁) either { x | x + 1 } { y | y × 3 }" imports "eval ok: 6"
  , evalOkWith "sum functor mapeth the right side" "bring ground.tung bring data/sum.tung let value: text ∐ integer = 2 inject₁ yield match (value map { x | x + 1 }) { _ inject₀ | 0, x inject₁ | x }" imports "eval ok: 3"
  , evalOkWith "sum applicative applieth the right side" "bring ground.tung bring data/sum.tung let value: text ∐ integer = 2 inject₁ let f: text ∐ (integer → integer) = { x | x + 3 } inject₁ yield match value apply f { _ inject₀ | 0, x inject₁ | x }" imports "eval ok: 5"
  , evalOkWith "sum monad preserveth a left value" "bring ground.tung bring data/sum.tung let value: text ∐ integer = 'left' inject₀ yield match (value map-flat { x | (x + 1) inject₁ }) { message inject₀ | message, x inject₁ | x to-text }" imports "eval ok: 'left'"
  , evalOkWith "sum catamorphism foldeth the right side" "bring ground.tung bring data/sum.tung bring collection/catamorphism.tung let value: text ∐ integer = 3 inject₁ yield value foldl 1 +" imports "eval ok: 4"
  , evalOkWith "sum traversal preserveth its side" "bring ground.tung bring data/sum.tung bring data/option.tung bring collection/traverse.tung let value: text ∐ integer = 3 inject₁ yield value traverse { x | (x + 1) some }" imports "eval ok: ((4 inject₁) some)"
  , evalOkWith "product bimap mapeth both fields" "bring ground.tung bring data/product.tung yield (1 ∏ 2) bimap { x | x + 1 } { y | y × 3 }" imports "eval ok: (2 6 ∏)"
  , evalOkWith "product applicative accumulateth value context first" "bring ground.tung bring data/product.tung let value = 'value:' ∏ 2 let f = 'function:' ∏ { x | x + 1 } yield value apply f" imports "eval ok: ('value:function:' 3 ∏)"
  , evalOkWith "product monad accumulateth outer context first" "bring ground.tung bring data/product.tung yield ('outer:' ∏ 2) map-flat { x | 'inner:' ∏ (x + 1) }" imports "eval ok: ('outer:inner:' 3 ∏)"
  , evalOkWith "product catamorphism foldeth the second field" "bring ground.tung bring data/product.tung bring collection/catamorphism.tung yield ('ignored' ∏ 3) foldl 1 +" imports "eval ok: 4"
  , evalOkWith "product traversal preserveth the first field" "bring ground.tung bring data/product.tung bring data/option.tung bring collection/traverse.tung yield ('context' ∏ 3) traverse { x | (x + 1) some }" imports "eval ok: (('context' 4 ∏) some)"
  , evalOkWith "applicative lift2 for option" "bring ground.tung bring data/option.tung yield (1 some) lift₂ (2 some) +" imports "eval ok: (3 some)"
  , evalOkWith "monad void for option" "bring data/option.tung bring collection/monad.tung yield (1 some) void" imports "eval ok: (null some)"
  , evalOkWith "list behead exposeth the head and tail" "bring data/list.tung bring data/option.tung bring data/product.tung yield match (1 .* empty) behead { (head product@∏ _) option@some | head, option@none | 0 }" imports "eval ok: 1"
  , evalOkWith "list take after drop" "bring data/list.tung yield ((0 till 5) drop 2) take 2" imports "eval ok: (2 (3 empty .*) .*)"
  , evalOkWith "list find returneth first match" "bring ground.tung bring data/list.tung yield (0 till 5) find { x | 3 ≤ x }" imports "eval ok: (3 some)"
  , evalOkWith "list find short-circuits effectful predicates" "bring ground.tung bring data/list.tung bring data/option.tung deed late { 𝟙 late: 𝟚 } let values = 1 .* (2 .* empty) yield try (values find { x | match x ≤ 1 { yea | yea, nay | null late } }) { late | none }" imports "eval ok: (1 some)"
  , evalOkWith "list indexing is zero based" "bring data/list.tung yield (10 .* (20 .* empty)) at 1" imports "eval ok: (20 some)"
  , evalOkWith "negative list index is absent" "bring ground.tung bring data/list.tung let index = 0 - 1 yield (10 .* empty) at index" imports "eval ok: none"
  , evalOkWith "list last returneth the final value" "bring data/list.tung yield (1 .* (2 .* empty)) last" imports "eval ok: (2 some)"
  , evalOkWith "list membership findeth a value" "bring ground.tung bring data/list.tung yield (1 .* (2 .* empty)) ∋ 2" imports "eval ok: yea"
  , evalOkWith "list membership rejecteth a missing value" "bring ground.tung bring data/list.tung yield (1 .* (2 .* empty)) ∋ 3" imports "eval ok: nay"
  , evalOkWith "list replicate useth a nonnegative count" "bring data/list.tung yield 'x' replicate 3" imports "eval ok: ('x' ('x' ('x' empty .*) .*) .*)"
  , evalOkWith "list partition preserveth order" "bring ground.tung bring data/list.tung yield (0 till 4) partition { x | 2 ≤ x }" imports "eval ok: ((2 (3 (4 empty .*) .*) .*) (0 (1 empty .*) .*) ∏)"
  , evalOkWith "list unzip preserveth both sides" "bring data/list.tung bring data/product.tung yield ((1 ∏ 'a') .* ((2 ∏ 'b') .* empty)) unzip" imports "eval ok: ((1 (2 empty .*) .*) ('a' ('b' empty .*) .*) ∏)"
  , evalOkWith "n-ary sum addeth a foldable collection" "bring ground.tung bring data/list.tung bring collection/catamorphism.tung yield (1 .* (2 .* (3 .* empty))) …+" imports "eval ok: 6"
  , evalOkWith "n-ary sum of an empty collection is zero" "bring ground.tung bring data/list.tung bring collection/catamorphism.tung let values: integer list = empty yield values …+" imports "eval ok: 0"
  , evalTypeErrWith "n-ary sum needeth a catamorphism" "bring ground.tung bring collection/catamorphism.tung kin a box { a box } let value: integer box = 1 box yield value …+" imports
  , evalOkWith "n-ary product multiplies a foldable collection" "bring ground.tung bring data/list.tung bring collection/catamorphism.tung yield (2 .* (3 .* (4 .* empty))) …×" imports "eval ok: 24"
  , evalOkWith "n-ary product of an empty collection is one" "bring ground.tung bring data/list.tung bring collection/catamorphism.tung let values: integer list = empty yield values …×" imports "eval ok: 1"
  , evalTypeErrWith "n-ary product needeth a multiplicative monoid" "bring ground.tung bring data/list.tung bring collection/catamorphism.tung yield ('a' .* empty) …×" imports
  , evalOkWith "boolean supremal wrapper useth disjunction" "bring ground.tung bring data/list.tung bring algebra/total/semigroup.tung bring collection/catamorphism.tung let values = (nay supremal) .* ((yea supremal) .* list@empty) yield match values fold { result supremal | result }" imports "eval ok: yea"
  , evalOkWith "empty boolean supremal fold yieldeth nay" "bring ground.tung bring data/list.tung bring algebra/total/semigroup.tung bring collection/catamorphism.tung let values: (𝟚 supremal) list = list@empty yield match values fold { result supremal | result }" imports "eval ok: nay"
  , evalOkWith "boolean infimal wrapper useth conjunction" "bring ground.tung bring data/list.tung bring algebra/total/semigroup.tung bring collection/catamorphism.tung let values = (yea infimal) .* ((nay infimal) .* list@empty) yield match values fold { result infimal | result }" imports "eval ok: nay"
  , evalOkWith "empty boolean infimal fold yieldeth yea" "bring ground.tung bring data/list.tung bring algebra/total/semigroup.tung bring collection/catamorphism.tung let values: (𝟚 infimal) list = list@empty yield match values fold { result infimal | result }" imports "eval ok: yea"
  , evalOkWith "natural semiring computeth with both identities" "bring ground.tung bring data/natural.tung let two: natural = (natural@zero suc) suc let three = two suc yield ((two × three) + one) to-integer" imports "eval ok: 7"
  , evalOkWith "natural semiring supplieth multiplicative monoid evidence" "bring ground.tung bring data/list.tung bring data/natural.tung bring collection/catamorphism.tung let two: natural = (natural@zero suc) suc let three = two suc yield ((two .* (three .* list@empty)) …×) to-integer" imports "eval ok: 6"
  , evalOkWith "nonnegative integer converteth to natural" "bring ground.tung bring data/natural.tung yield 3 integer-to-natural $ to-integer" imports "eval ok: 3"
  , evalOkWith "negative integer cannot become natural" "bring ground.tung yield try -1 integer-to-natural { yield _ | 'valid', message fail | message }" imports "eval ok: 'negative cannot be a natural'"
  , evalOkWith "complex sine of zero is zero" "bring ground.tung bring numeric/complex.tung yield ((0.0 complex 0.0) sine) real" imports "eval ok: 0.0"
  , evalOkWith "complex cosine of zero is one" "bring ground.tung bring numeric/complex.tung yield ((0.0 complex 0.0) cosine) real" imports "eval ok: 1.0"
  , evalOkWith "complex exponentiation scaleth by the real exponent" "bring ground.tung bring numeric/complex.tung yield ((1.0 complex 0.0) exponent) real" imports "eval ok: 2.718281828459045"
  , evalOkWith "complex sine of unit imaginary yieldeth hyperbolic sine" "bring ground.tung bring numeric/complex.tung yield ((0.0 complex 1.0) sine) imaginary" imports "eval ok: 1.1752011936438014"
  , evalOkWith "complex cosine of unit imaginary yieldeth hyperbolic cosine" "bring ground.tung bring numeric/complex.tung yield ((0.0 complex 1.0) cosine) real" imports "eval ok: 1.5430806348152437"
  , evalOkWith "nonempty map preserveth the first element" "bring ground.tung bring data/list.tung bring data/nonempty.tung yield ((1 nonempty (2 .* empty)) map { x | x + 1 }) nonempty-to-list" imports "eval ok: (2 (3 empty .*) .*)"
  , evalOkWith "nonempty semigroup appendeth without losing the head" "bring ground.tung bring data/list.tung bring data/nonempty.tung bring algebra/total/magma.tung yield ((1 nonempty (2 .* empty)) * (3 nonempty empty)) nonempty-to-list" imports "eval ok: (1 (2 (3 empty .*) .*) .*)"
  , evalOkWith "nonempty applicative applieth every function" "bring ground.tung bring data/list.tung bring data/nonempty.tung let values = 1 nonempty (2 .* empty) let fs = { x | x + 1 } nonempty ({ x | x × 2 } .* empty) yield (values apply fs) nonempty-to-list" imports "eval ok: (2 (3 (2 (4 empty .*) .*) .*) .*)"
  , evalOkWith "nonempty monad concatenateth every result" "bring ground.tung bring data/list.tung bring data/nonempty.tung let values = 1 nonempty (2 .* empty) yield (values map-flat { x | x nonempty ((x + 10) .* empty) }) nonempty-to-list" imports "eval ok: (1 (11 (2 (12 empty .*) .*) .*) .*)"
  , evalOkWith "nonempty catamorphism visiteth the head first" "bring ground.tung bring data/list.tung bring data/nonempty.tung bring collection/catamorphism.tung yield (1 nonempty (2 .* empty)) foldl 0 +" imports "eval ok: 3"
  , evalOkWith "nonempty traversal keepeth a nonempty result" "bring ground.tung bring data/list.tung bring data/nonempty.tung bring data/option.tung bring collection/traverse.tung yield ((1 nonempty (2 .* empty)) traverse { x | (x + 1) some }) map nonempty-to-list" imports "eval ok: ((2 (3 empty .*) .*) some)"
  , evalOkWith "nonempty membership delegateth to list" "bring ground.tung bring data/list.tung bring data/nonempty.tung yield (1 nonempty (2 .* empty)) ∋ 2" imports "eval ok: yea"
  , evalOkWith "table put replaceth a key" "bring ground.tung bring data/table.tung let table = (empty put 'a' 1) put 'a' 2 yield table lookup 'a'" imports "eval ok: (2 some)"
  , evalOkWith "table membership dispatcheth through inhold" "bring ground.tung bring data/table.tung let table = empty put 'a' 1 yield table ∋ 'a'" imports "eval ok: yea"
  , evalOkWith "table constructor may be qualified" "bring ground.tung bring data/list.tung bring data/product.tung bring data/table.tung yield (('a' ∏ 2) .* list@empty) table@from-list $ lookup 'a'" imports "eval ok: (2 some)"
  , evalOkWith "table rid droppeth a key" "bring ground.tung bring data/table.tung let table = empty put 'a' 1 yield (table rid 'a') lookup 'a'" imports "eval ok: none"
  , evalOkWith "set put keepeth values unique" "bring ground.tung bring data/set.tung yield ((empty put 1) put 1) to-list" imports "eval ok: (1 empty .*)"
  , evalOkWith "set constructor may be qualified" "bring ground.tung bring data/list.tung bring data/set.tung yield (1 .* (2 .* list@empty)) set@from-list $ to-list" imports "eval ok: (1 (2 empty .*) .*)"
  , evalOkWith "set rid droppeth a value" "bring ground.tung bring data/set.tung let values = (empty put 1) put 2 yield (values rid 1) ∋ 1" imports "eval ok: nay"
  , evalOkWith "set supremum keepeth unique values" "bring ground.tung bring data/set.tung let left = (empty put 1) put 2 let right = (empty put 2) put 3 yield (left ∨ right) to-list" imports "eval ok: (3 (2 (1 empty .*) .*) .*)"
  , evalOkWith "set infimum keepeth shared values" "bring ground.tung bring data/set.tung let left = (empty put 1) put 2 let right = (empty put 2) put 3 yield (left ∧ right) to-list" imports "eval ok: (2 empty .*)"
  , evalOkWith "set equality ignoreth insertion order" "bring ground.tung bring data/set.tung let left = (empty put 1) put 2 let right = (empty put 2) put 1 yield left ≡ right" imports "eval ok: yea"
  , evalOkWith "set partial order compareth inclusion" "bring ground.tung bring data/set.tung let smaller = empty put 1 let larger = smaller put 2 yield (smaller ≤ larger) ∧ (smaller < larger)" imports "eval ok: yea"
  , evalOkWith "set inclusion rejecteth a proper superset" "bring ground.tung bring data/set.tung let smaller = empty put 1 let larger = smaller put 2 yield larger ≤ smaller" imports "eval ok: nay"
  , evalOkWith "powerset addition is symmetric difference" "bring ground.tung bring data/powerset.tung let left: integer powerset = { value | (value ≡ 1) ∨ (value ≡ 2) } let right: integer powerset = { value | (value ≡ 2) ∨ (value ≡ 3) } yield ((left + right) ∋ 2)" imports "eval ok: nay"
  , evalOkWith "powerset addition keepeth an unshared member" "bring ground.tung bring data/powerset.tung let left: integer powerset = { value | value ≡ 1 } let right: integer powerset = { value | value ≡ 2 } yield ((left + right) ∋ 1)" imports "eval ok: yea"
  , evalOkWith "powerset multiplication is intersection" "bring ground.tung bring data/powerset.tung let left: integer powerset = { value | (value ≡ 1) ∨ (value ≡ 2) } let right: integer powerset = { value | value ≡ 2 } yield ((left × right) ∋ 2)" imports "eval ok: yea"
  , evalOkWith "powerset supremum is union" "bring ground.tung bring data/powerset.tung let left: integer powerset = { value | value ≡ 1 } let right: integer powerset = { value | value ≡ 2 } yield (left ∨ right) ∋ 2" imports "eval ok: yea"
  , evalOkWith "powerset infimum is intersection" "bring ground.tung bring data/powerset.tung let left: integer powerset = { value | (value ≡ 1) ∨ (value ≡ 2) } let right: integer powerset = { value | value ≡ 2 } yield (left ∧ right) ∋ 1" imports "eval ok: nay"
  , evalOkWith "powerset complement excludeth a former member" "bring ground.tung bring data/powerset.tung let one: integer powerset = { value | value ≡ 1 } let outside: integer powerset = one ¬ yield outside ∋ 1" imports "eval ok: nay"
  , evalOkWith "powerset complement includeth a former nonmember" "bring ground.tung bring data/powerset.tung let one: integer powerset = { value | value ≡ 1 } let outside: integer powerset = one ¬ yield outside ∋ 2" imports "eval ok: yea"
  , evalOkWith "powerset subtraction is symmetric difference" "bring ground.tung bring data/powerset.tung let values: integer powerset = { value | value ≡ 1 } yield ((values - values) ∋ 1)" imports "eval ok: nay"
  , evalOkWith "powerset zero is empty" "bring ground.tung bring data/powerset.tung let values: integer powerset = zero yield values ∋ 7" imports "eval ok: nay"
  , evalOkWith "powerset one is universal" "bring ground.tung bring data/powerset.tung let values: integer powerset = one yield values ∋ 7" imports "eval ok: yea"
  , evalOkWith "set becometh a powerset" "bring ground.tung bring data/set.tung bring data/powerset.tung let finite = set@empty set@put 7 yield (finite to-powerset) ∋ 7" imports "eval ok: yea"
  , evalOkWith "powerset infimal wrapper useth intersection" "bring ground.tung bring data/list.tung bring data/powerset.tung bring algebra/total/semigroup.tung bring collection/catamorphism.tung let one: integer powerset = { value | value ≡ 1 } let both: integer powerset = { value | (value ≡ 1) ∨ (value ≡ 2) } let wrapped = (one infimal) .* ((both infimal) .* list@empty) yield match wrapped fold { result infimal | result ∋ 2 }" imports "eval ok: nay"
  , evalOkWith "powerset supremal wrapper useth union" "bring ground.tung bring data/list.tung bring data/powerset.tung bring algebra/total/semigroup.tung bring collection/catamorphism.tung let one: integer powerset = { value | value ≡ 1 } let two: integer powerset = { value | value ≡ 2 } let wrapped = (one supremal) .* ((two supremal) .* list@empty) yield match wrapped fold { result supremal | result ∋ 2 }" imports "eval ok: yea"
  , evalTypeErrWith "powerset hath no arbitrarily chosen monoid" "bring ground.tung bring data/list.tung bring data/powerset.tung bring algebra/total/semigroup.tung bring collection/catamorphism.tung let values: (integer powerset) list = list@empty yield values fold" imports
  , evalOkWith "table functor mapeth values" "bring ground.tung bring data/table.tung let table = (empty put 'a' 1) put 'b' 2 yield (table map { x | x + 10 }) lookup 'a'" imports "eval ok: (11 some)"
  , evalOkWith "table filter keepeth matching values" "bring ground.tung bring data/table.tung bring collection/filter.tung let table = (empty put 'a' 1) put 'b' 2 yield (table filter { x | 2 ≤ x }) lookup 'a'" imports "eval ok: none"
  , evalOkWith "table merge preferreth right values" "bring ground.tung bring data/table.tung let left = empty put 'a' 1 let right = empty put 'a' 2 yield (left merge right) lookup 'a'" imports "eval ok: (2 some)"
  , evalOkWith "table adjust changeth an existing value" "bring ground.tung bring data/table.tung let table = empty put 'a' 1 yield (table adjust 'a' { x | x + 1 }) lookup 'a'" imports "eval ok: (2 some)"
  , evalOkWith "text operations use unicode code points" "bring ground.tung bring text/operation.tung yield ('aβ字' reverse-text) take-text 2" imports "eval ok: '字β'"
  , evalOkWith "text size counteth unicode code points" "bring ground.tung bring text/operation.tung bring collection/size.tung bring data/natural.tung yield ('aβ字' size) to-integer" imports "eval ok: 3"
  , evalOkWith "text split and join preserve empty fields" "bring ground.tung bring text/operation.tung yield ('a,b,,c' split-text `,) join-with-text '|'" imports "eval ok: 'a|b||c'"
  , evalOkWith "text words collapse whitespace runs" "bring ground.tung bring text/operation.tung yield ' red\\tblue\\nred ' words-text $ join-with-text '|'" imports "eval ok: 'red|blue|red'"
  , evalOkWith "typed paths join components" "bring ground.tung bring data/path.tung yield (('root' path) join-path ('leaf' path)) to-text" imports "eval ok: 'root/leaf'"
  , evalOkWith "time spans drive async sleep" "bring ground.tung bring data/time-span.tung yield (0 milliseconds) sleep-for" imports "eval ok: null"
  , evalOkWith "process captureth standard output" "bring ground.tung bring data/list.tung yield ('printf' run-process ('hello' .* empty)) process-output" imports "eval ok: 'hello'"
  , evalTypeErrWith "missing import is rejected before runtime" "bring missing.tung" Map.empty
  , evalTypeErrWith "import cycle is rejected before runtime" "bring left.tung" cyclicImports
  , evalOkWith "arctan range is nonnegative" "bring _foreign.tung yield 0.0 arctan-float -1.0" imports "eval ok: 4.71238898038469"
  , evalOkWith "fork and wait" "bring ground.tung let _ work = 42 let task = work fork yield task wait" imports "eval ok: 42"
  , evalOkWith "task result may be awaited repeatedly" "bring ground.tung let _ work = 21 let task = work fork yield (task wait) + (task wait)" imports "eval ok: 42"
  , evalOkWith "task wait may time out" "bring ground.tung let _ work = (let _ = 200 sleep yield 42) let task = work fork yield task wait-for 0" imports "eval ok: none"
  , evalOkWith "task wait-for returneth a finished value" "bring ground.tung let _ work = 42 let task = work fork yield task wait-for 1000" imports "eval ok: (42 some)"
  , evalOkWith "fordone task faileth through the declared effect" "bring ground.tung let _ work = (let _ = 1000 sleep yield 42) let task = work fork let _ = task fordo yield try task wait { yield _ | 'finished', message fail | message }" imports "eval ok: 'task cancelled'"
  , evalOkWith "cpu-bound task remaineth cancellable" "bring ground.tung let _ loop = null loop let spin = loop let task = spin fork let _ = task fordo yield try task wait { yield _ | 'finished', message fail | message }" imports "eval ok: 'task cancelled'"
  , evalTypeErrWith "task constructor is private" "bring ground.tung yield 42 done" imports
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
      [ ("left.tung", "bring shared.tung show let left = shared@base + 1")
      , ("right.tung", "bring shared.tung show let right = shared@base + 2")
      , ("shared.tung", "show let base = 1")
      ]
  fillDiamondImports =
    Map.fromList
      [ ("left.tung", "bring shared.tung show shared@identity")
      , ("right.tung", "bring shared.tung")
      , ("shared.tung", "show shape a identity { let a identity: a } fill integer identity { let x identity = x }")
      ]
  distinctShapeImports =
    Map.fromList
      [ ("left.tung", "show shape a identity { let a identity: a } fill integer identity { let x identity = x + 1 }")
      , ("right.tung", "show shape a identity { let a identity: a } fill integer identity { let x identity = x + 2 }")
      ]
  reexportImports = Map.fromList [("base.tung", "show let value = 7"), ("middle.tung", "bring base.tung show base@value")]
  identityImports = Map.fromList [("identity.tung", "show shape a identity { let a identity: a } fill integer identity { let x identity = x }")]
  dataReexportImports =
    Map.fromList
      [ ("data-base.tung", "show kin a box { a box }")
      , ("data-middle.tung", "bring data-base.tung show-ilk data-base@box show data-base@box")
      ]
  effectReexportImports =
    Map.fromList
      [ ("ask-base.tung", "show deed ask { integer ask: integer }")
      , ("ask-middle.tung", "bring ask-base.tung show ask-base@ask")
      ]
  cyclicImports = Map.fromList [("left.tung", "bring right.tung"), ("right.tung", "bring left.tung")]

randomRange :: Map.Map String String -> Test
randomRange imports = do
  actual <- evaluateWithImports "bring ground.tung yield null random" imports
  pure $ case stripPrefix "eval ok: " actual >>= readMaybe of
    Just value | value >= (0 :: Double) && value < 1 -> Nothing
    _ -> Just ("random result is in [0, 1): got " ++ actual)

concurrentForks :: Map.Map String String -> Test
concurrentForks imports =
  evalOkWith "concurrent forks overlap while one is awaited" source imports "eval ok: (21 some)"
 where
  -- awaiting one equal-duration task giveth the other time to finish. its
  -- already-available result testeth overlap without timing the whole compiler.
  source =
    "bring ground.tung "
      ++ "let _ work = (let _ = 600 sleep yield 21) "
      ++ "let left = work fork let right = work fork "
      ++ "let _ = left wait yield right wait-for 100"

systemArguments :: Map.Map String String -> Test
systemArguments imports = do
  actual <- evaluateWithArgsAndImports ["north", "two words"] "bring ground.tung yield null arguments" imports
  pure $ if actual == "eval ok: ('north' ('two words' empty .*) .*)" then Nothing else Just ("system arguments: " ++ actual)

systemEnvironment :: Map.Map String String -> Test
systemEnvironment imports = bracket (Environment.lookupEnv name) restore $ \_ -> do
  Environment.setEnv name "seen"
  actual <- evaluateWithImports ("bring ground.tung yield '" ++ name ++ "' environment") imports
  pure $ if actual == "eval ok: ('seen' some)" then Nothing else Just ("system environment: " ++ actual)
 where
  name = "TUNG_TEST_ENVIRONMENT"
  restore Nothing = Environment.unsetEnv name
  restore (Just value) = Environment.setEnv name value

unixTime :: Map.Map String String -> Test
unixTime imports = do
  before <- floor <$> getPOSIXTime
  actual <- evaluateWithImports "bring ground.tung yield null unix-time" imports
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
unitData = "kin 𝟙 { null } "
boolData = "kin 𝟚 { yea, nay } "
optionData = "kin a option { none, a some } "
stopEffect = "deed e stop { e stop: a } "
duoEffect = unitData ++ "deed duo { 𝟙 first: 𝟙, 𝟙 second: 𝟙 } "
