-- static semantics: positive inference and adversarial rejection cases live here.
module Test.Type (group) where

import Control.Monad (replicateM)
import Data.List (intercalate, isInfixOf, isPrefixOf, permutations)
import Data.Map.Strict qualified as Map
import Test.Harness (Group, Test, expect, expectEq, runnableErr, runnableOk, typeErr, typeErrContaining, typeErrWith, typeOk, typeOkWith)
import Test.Harness qualified as Harness
import Test.QuickCheck qualified as QuickCheck
import Tung (CoreProgram, check, checkRunnableWithImports, checkWithImports, elaborateProgramWithImports, parse, readBookhoardImports, typeOfWithImports)

group :: IO Group
group = do
  imports <- readBookhoardImports
  Harness.group "type" $
    map (uncurry typeOk) accepted
      ++ map (uncurry typeErr) rejected
      ++ importCases
      ++ tableAndSetCases imports
      ++ orderCases imports
      ++ boundCases imports
      ++ numericHierarchyCases imports
      ++ redundantFillGraithCases
      ++ generatedCoverageCases
      ++ redundantCoverageCases
      ++ generatedEffectCases
      ++ inferenceProperties
      ++ evidenceProperties
      ++ coverageProperties

accepted :: [(String, String)]
accepted =
  [ ("literal annotation", "let answer: ℤ = 42")
  , ("arbitrary term type ascription", "let answer = (1 + 2: ℤ)")
  , ("polymorphic term type ascription is reusable", "let identity = ({ x @ x }: a → a) let number: ℤ = 1 identity let word: text = 'x' identity")
  , ("ascribed anonymous function remaineþ recursive", "let factorial = ({ 0 @ 1, n @ n × ((n - 1) factorial) }: ℤ → ℤ) let answer: ℤ = 5 factorial")
  , ("type ascription preserveþ a graiþ", equalPrelude ++ "graiþ a equal let (x: a, y: a) same: 𝟚 = (x ≡ y: 𝟚)")
  , ("type ascription preserveþ an immediate effect", unitData ++ "deed pulse { 𝟙 pulse: ℤ } let (_: 𝟙) run: ℤ ! pulse = (only pulse: ℤ)")
  , ("unicode primitive", "let letter: unicode = `a")
  , ("text primitive", "let word: text = 'λ字'")
  , ("let polymorphism is reusable", "let x id = x let number: ℤ = 1 id let word: text = 'x' id")
  , ("annotated polymorphic identity", "let (x: a) id: a = x let number: ℤ = 1 id let word: text = 'x' id")
  , ("subscripted type variable", "let (x: a₀) id: a₀ = x let number: ℤ = 1 id let word: text = 'x' id")
  , ("higher-order effect polymorphism", "let (x: a, f: a → b ! e) call: b ! e = x f")
  , ("two effect rows compose", "let (f: a → b ! e0, g: b → c ! e1, x: a) compose: c ! e0, e1 = (x f) g")
  , ("subscripted effect rows compose", "let (f: a → b ! e₀, g: b → c ! e₁, x: a) compose: c ! e₀, e₁ = (x f) g")
  , ("effect-only parameter generaliseþ at a let", unitData ++ "deed a phantom { 𝟙 tick: 𝟙 } let operation = tick let first: 𝟙 → 𝟙 ! ℤ phantom = operation let second: 𝟙 → 𝟙 ! text phantom = operation")
  , ("pure partial application hideþ latent effect", "let half = 1 %")
  , ("type alias is transparent", "let-ilk count = ℤ let answer: count = 42")
  , ("forward type-alias chains resolve by dependency", "let-ilk first = second let-ilk second = token ilk token { token } let good: first = token")
  , ("type-alias parameters shadow nominal types", "ilk a { a } let-ilk a wrapper = a let good: ℤ wrapper = 1")
  , ("data parameters shadow type aliases", "let-ilk a = ℤ ilk a box { a box } let good: text box = 'x' box")
  , ("effect parameters shadow type aliases", "ilk 𝟙 { only } let-ilk a = ℤ deed a state { 𝟙 get: a } let good: 𝟙 → text ! text state = get")
  , ("frame parameters shadow type aliases", "let-ilk a = ℤ frame a identity { let a identity: a } fill text identity { let x identity = x } let good: text = 'x' identity")
  , ("effect operation type may be a transparent function alias", "let-ilk binary = ℤ → ℤ → ℤ deed addition { add: binary }")
  , ("unresolved raw primitive frame dependency stayeþ ABI-compatible", "frame a equal { let a ≡ a: 𝟚 } let result = 1 ≡ 1")
  , ("parameterised type alias is transparent", boolData ++ "let (_: ℤ) member: 𝟚 = yea let answer: 𝟚 = 1 member")
  , ("func equaleþ pure unary arrow", "let (x: ℤ) id: ℤ = x")
  , ("curried triple function", "let (x: ℤ, _: text, _: float) pick: ℤ = x")
  , ("constructor application is curried", "ilk a option { none, a some } let make = some let value: ℤ option = 1 make")
  , ("empty type eliminator", "ilk 𝟘 {} let initial: 𝟘 → a = {} let eliminate: 𝟘 → ℤ = initial")
  , ("multi-scrutinee exhaustive match", boolData ++ "let ok: ℤ = match yea, nay { yea, yea @ 1, yea, nay @ 2, nay, yea @ 3, nay, nay @ 4 }")
  , ("alternative rows contribute to coverage", boolData ++ "let ok: ℤ = match yea, nay { yea, _ | _, yea @ 1, nay, nay @ 0 }")
  , ("overlapping wildcards cover a product", boolData ++ "let ok: ℤ = match yea, nay { yea, _ @ 1, _, yea @ 2, nay, nay @ 3 }")
  , ("nested exhaustive match", boolData ++ optionData ++ "let ok: ℤ = match yea some { yea some @ 1, nay some @ 2, none @ 0 }")
  , ("variable pattern closeþ coverage", boolData ++ "let ok: ℤ = match yea { yea @ 1, other @ 0 }")
  , ("ℤ match with fallback", "let ok: ℤ = match 1 { 0 @ 0, 1 @ 1, _ @ 2 }")
  , ("ℤ patterns in a function", "let classify: ℤ → ℤ = { 0 @ 10, -1 @ 20, _ @ 30 }")
  , ("text match with fallback", "let ok: ℤ = match 'yes' { 'yes' @ 1, _ @ 0 }")
  , ("text patterns in a function", "let classify: text → ℤ = { 'yes' @ 1, _ @ 0 }")
  , ("closed record", "let person: r(age: ℤ, name: text) = r(name = 'n', age = 1)")
  , ("record access", "let person = r(name = 'n', age = 1) let age: ℤ = person.age")
  , ("chained record access", "let person = r(address = r(city = 'kyoto')) let city: text = person.address.city")
  , ("record update changeþ a field type", "let person = r(value = 1) let changed: r(value: text) = r(= person, value = 'one')")
  , ("record removal changeþ the row", "let person = r(name = 'n', age = 1) let public: r(name: text) = r(= person, - age)")
  , ("frame graiþ dischargeþ member need", equalPrelude ++ "graiþ a equal let (x: a, y: a) same: 𝟚 = x ≡ y")
  , ("fill context dischargeþ inner need", equalPrelude ++ boxData ++ "graiþ a equal fill (a box) equal { let (x box) ≡ (y box) = x ≡ y }")
  , ("frame law useþ its own methods", "frame a identity { let a identity: a law (x: a): x identity ~ x } fill ℤ identity { let x identity = x }")
  , ("well-typed unequal frame law remaineþ checked documentation", "frame a claimed { law (x: ℤ): x ~ 0 }")
  , ("frame member carrieþ its own graiþ", memberGraith ++ "fill option functor { let value map f = match value { none @ none, x some @ x f $ some } } fill option applicative { let pure = some let apply = { x some, f some @ x f $ some, _, _ @ none } } fill option traverse { let value traverse f = match value { none @ none pure, x some @ (x f) map some } } let lifted: (ℤ option) option = (1 some) traverse some")
  , ("existing parent fill satisfieþ child", classHierarchy ++ "fill ℤ parent { let x parent = x } fill ℤ child { let x child = x }")
  , ("specific fill outrankeþ a blanket fill", "frame a identity { let a identity: a } fill a identity { let x identity = x } fill ℤ identity { let x identity = x } let ok = 1 identity")
  , ("child fill may supply parent member", classHierarchy ++ "fill ℤ child { let x parent = x let x child = x } let ok: ℤ = 1 parent")
  , ("parametric child fill useþ its parent method", inheritedMethodHierarchy)
  , ("fill graiþ may construct a parent dictionary", parentDictionaryGraith)
  , ("parameterised state effect", unitData ++ "deed a state { 𝟙 get: a, a set: 𝟙 } let got: 𝟙 → ℤ ! ℤ state = get")
  , ("eftgin continueþ an operation handler", "deed ask { ℤ ask: ℤ } let ok: ℤ = try 10 ask { x ask @ x eftgin }")
  , ("handler changeþ answer through yield", "deed ask { ℤ ask: ℤ } let ok: text = try 10 ask { yield n @ n to-text, x ask @ x eftgin }")
  , ("all operation clauses remove an effect", duoEffect ++ "let (_: 𝟙) run: 𝟙 = try only first { first @ only, second @ only }")
  , ("effect clause removeþ every operation", duoEffect ++ "let (_: 𝟙) run: 𝟙 = try only first { duo @ only }")
  , ("handler clause effects escape handler", duoEffect ++ "let (_: 𝟙) run: 𝟙 ! duo = try only first { first @ only second, second @ only }")
  , ("operation keepeþ declared extra effect", unitData ++ "deed other { 𝟙 other: 𝟙 } deed mine { 𝟙 op: 𝟙 ! other } let (_: 𝟙) run: 𝟙 ! other = try only op { op @ only other }")
  , ("operation propagateþ a handler effect row", unitData ++ "ilk request { request } ilk response { response } deed e fail { e fail: a } deed clock { 𝟙 tick: 𝟙 } deed web { ℤ serve (request → response ! e): 𝟙 ! e, text fail } let (_: request) route: response ! clock = (let _ = only tick yield response) let (_: 𝟙) run: 𝟙 ! web, clock, text fail = 8080 serve route")
  , ("fremmed key entereþ value scope under another name", "let plus: ℤ → ℤ → ℤ = 'add-integer' fremmed let answer: ℤ = 1 plus 2")
  , ("declaration-only file is runnable", "let answer = 42")
  ]

rejected :: [(String, String)]
rejected =
  [ ("literal type mismatch", "let bad: ℤ = 'wrong'")
  , ("term type ascription rejecteþ a mismatch", "let bad = ('wrong': ℤ)")
  , ("parameterised type alias needeþ its argument", "let _ bad: predicate = 1")
  , ("parameterised type alias rejecteþ extra arguments", "let _ bad: ℤ ℤ predicate = 1")
  , ("recursive type aliases are rejected", "let-ilk first = second let-ilk second = first")
  , ("local type declarations cannot share a name", "let-ilk token = ℤ ilk token { token }")
  , ("occurs check rejecteþ self application", "let x omega = x x")
  , ("occurs check followeþ nested records", "let x omega = r(value = x) x")
  , ("annotation cannot claim false polymorphism", "let (_: a) bad: a = 1")
  , ("function branches must agree", "let bad = { yea @ 1, nay @ 'no' }")
  , ("function cases share arity", "let bad = { x @ x, x, y @ x }")
  , ("pattern binders are linear", "let bad = { x, x @ x }")
  , ("inhabited empty function", "let bad: ℤ → ℤ = {}")
  , ("unannotated empty function", "let bad = {}")
  , ("non-exhaustive data match", boolData ++ "let bad: ℤ = match yea { yea @ 1 }")
  , ("multi-match missing one product case", boolData ++ "let bad: ℤ = match yea, nay { yea, yea @ 1, yea, nay @ 2, nay, yea @ 3 }")
  , ("overlapping wildcards can miss a product", boolData ++ "let bad: ℤ = match yea, nay { yea, _ @ 1, _, yea @ 2 }")
  , ("nested match misses constructor", boolData ++ optionData ++ "let bad: ℤ = match yea some { yea some @ 1, none @ 0 }")
  , ("constructor pattern arity", optionData ++ "let bad = match 1 some { some @ 1, none @ 0 }")
  , ("ℤ pattern needeþ ℤ input", "let bad = match 'one' { 1 @ 1, _ @ 0 }")
  , ("finite ℤ patterns are not exhaustive", "let bad = match 1 { 0 @ 0, 1 @ 1 }")
  , ("text pattern needeþ text input", "let bad = match 1 { 'one' @ 1, _ @ 0 }")
  , ("finite text patterns are not exhaustive", "let bad = match 'one' { 'one' @ 1 }")
  , ("closed record rejecteþ extra field", "let bad: r(name: text) = r(name = 'n', age = 1)")
  , ("missing record field", "let person = r(name = 'n') let bad = person.age")
  , ("unknown record removal", "let person = r(name = 'n') let bad = r(= person, - age)")
  , ("record update needeþ a record", "let bad = r(= 1, value = 2)")
  , ("missing function graiþ", equalPrelude ++ "let (x: a, y: a) same: 𝟚 = x ≡ y")
  , ("insufficient graiþ", equalPrelude ++ "graiþ a equal frame a order-partial { let a ≤ a: 𝟚 } graiþ a equal let (x: a, y: a) leq: 𝟚 = x ≤ y")
  , ("unknown fill frame", "fill ℤ missing { let x missing = x }")
  , ("fill evidence cannot be shown", "show fill ℤ missing {}")
  , ("fill misses required member", "frame a identity { let a identity: a } fill ℤ identity {}")
  , ("fill rejecteþ unknown member", "frame a identity { let a identity: a } fill ℤ identity { let x other = x }")
  , ("fill member hath declared type", "frame a identity { let a identity: a } fill ℤ identity { let x identity = 'wrong' }")
  , ("duplicate fill is incoherent", "frame a identity { let a identity: a } fill ℤ identity { let x identity = x } fill ℤ identity { let x identity = x }")
  , ("incomparable fills are ambiguous when used", "frame a identity { let a identity: a } ilk a box { a box } fill (ℤ f) identity { let x identity = x } fill (a box) identity { let x identity = x } let bad = (1 box) identity")
  , ("frame law sides share a value type", "frame a bad { law (x: a): x ~ 1 }")
  , ("frame law sides share an effect row", unitData ++ "deed pulse { 𝟙 pulse: 𝟙 } frame a bad { law (x: 𝟙): x ~ only pulse }")
  , ("frame law rejecteþ an unbound value", "frame a bad { law (x: a): x ~ missing }")
  , ("frame law needeþ must be declared", equalPrelude ++ "frame a bad { law (x: a): x ≡ x ~ x ≡ x }")
  , ("frame member graiþ is required at use", memberGraith ++ "fill box traverse { let (x box) traverse f = (x f) map box } let bad: (ℤ box) box = (1 box) traverse { x @ x box }")
  , ("fill arity followeþ frame", "frame a convert b { let a convert: b } fill ℤ convert { let x convert = x }")
  , ("effect operation must be a function", "deed bad { tick: ℤ }")
  , ("primitive frame rejecteþ a nominal result lookalike", "ilk 𝟚 { maybe } frame a equal { let a ≡ a: 𝟚 } let result: 𝟚 = 1 ≡ 1")
  , ("primitive frame rejecteþ a nominal effect lookalike", "deed e fail { e nope: a } frame a from-text { let text from-text: a ! text fail } let (x: text) parse: float ! text fail = x from-text")
  , ("state parameter mismatch", unitData ++ "deed a state { 𝟙 get: a, a set: 𝟙 } let got: 𝟙 → ℤ ! text state = get")
  , ("one operation clause cannot erase sibling operations", duoEffect ++ "let (_: 𝟙) run: 𝟙 = try only second { first @ only }")
  , ("partial coverage doth not combine across handlers", duoEffect ++ "let (_: 𝟙) run: 𝟙 = try (try only second { first @ only }) { second @ only }")
  , ("handler for absent effect", "let bad: ℤ = try 1 { fail @ 2 }")
  , ("ordinary function is not an operation", "deed ask { ℤ ask: ℤ } let (x: ℤ) f: ℤ ! ask = x ask let bad = try 1 f { x f @ x }")
  , ("eftgin argument hath operation result type", "deed ask { ℤ ask: ℤ } let bad: ℤ = try 10 ask { x ask @ 'bad' eftgin }")
  , ("eftgin is scoped to operation clauses", "let bad = 1 eftgin")
  , ("handler yield pattern must be irrefutable", boolData ++ "let bad = try nay { yield yea @ 1 }")
  , ("handler yield ℤ pattern is refutable", "let bad = try 1 { yield 1 @ 1 }")
  , ("handler operation patterns must be irrefutable", boolData ++ "deed choose { 𝟚 choose: ℤ } let bad = try nay choose { yea choose @ 1 }")
  , ("effect handler cannot bind operation arguments", duoEffect ++ "let bad: 𝟙 = try only first { x duo @ only }")
  , ("effectful let result stayeþ monomorphic", unitData ++ "deed a state { 𝟙 get: a } let _ run = (let value = only get let number: ℤ = value let word: text = value yield only)")
  , ("bookhoard let cannot run immediate effects", "let answer = 'hello' write")
  , ("fremmed let requireþ an annotation", "let plus = 'add-integer' fremmed")
  , ("fremmed key requireþ a host implementation", "let unknown: ℤ → ℤ = 'unknown' fremmed")
  , ("fremmed key must match its host signature", "let plus: ℤ → ℤ = 'add-integer' fremmed")
  , ("fremmed marker cannot be nested", "let bad: r(value: ℤ) = r(value = 'add-integer' fremmed)")
  , ("fremmed let must be file-level", "let _ outer = (let plus: ℤ → ℤ → ℤ = 'add-integer' fremmed yield 1)")
  ]

importCases :: [Test]
importCases =
  [ typeOkWith "shown value and type cross a use" "use file.tung let value: box = box" shown
  , typeOkWith "default use namespace covereþ terms, types, and frames" "use ilk/item.tung let value: item~item = item~empty let same: item~item = value let result = same item~identity" aliased
  , typeOkWith "custom use alias surviveþ resolved frame inheritance" "use dep.tung d graiþ a d~parent frame a child { let a child: a }" (Map.singleton "dep.tung" "show frame a parent { let a parent: a }")
  , expect "re-exported nominal mismatch nameþ boþ outer aliases" $
      let message = checkWithImports "use middle-left.tung left use middle-right.tung right let bad: left~token = right~value" reexportMismatchImports
       in "left~token" `isInfixOf` message && "right~token" `isInfixOf` message
  , expect "concrete aliases are not rendered as applied constructors" $
      let message = checkWithImports "use middle.tung outer let bad: text = outer~value" concreteAliasProjection
       in "type error:" `isPrefixOf` message && not ("outer~boxed<ℤ>" `isInfixOf` message)
  , expect "re-exported effect mismatch nameþ boþ outer aliases" $
      let message = checkWithImports "use middle-left.tung left use middle-right.tung right let (x: ℤ) bad: ℤ ! left~pulse = x right~ask" reexportEffectImports
       in "left~pulse" `isInfixOf` message && "right~pulse" `isInfixOf` message
  , expect "re-exported graiþ mismatch nameþ boþ outer aliases" $
      let message = checkWithImports "use middle-left.tung left use middle-right.tung right graiþ a left~identity let (x: a) bad: a = x right~identity" reexportShapeImports
       in "left~identity" `isInfixOf` message && "right~identity" `isInfixOf` message
  , expect "imported frame-member mismatch nameþ boþ type aliases" $
      let message = checkWithImports "use left-data.tung left use right-data.tung right fill ℤ left~maker { let maker = right~token }" fixedShapeTypeImports
       in "left~token" `isInfixOf` message && "right~token" `isInfixOf` message
  , expect "imported fill graiþ useþ þe visible alias" $
      let message = checkWithImports "use dep.tung left let bad = left~value left~identity" nestedFillImport
       in "left~token" `isInfixOf` message && "left~label" `isInfixOf` message
  , expect "duplicate imported fill preferreþ þe requested alias" $
      let message = checkWithImports "use base.tung first use base.tung second let bad = first~value first~child" duplicatedFillImport
       in "first~token" `isInfixOf` message && "first~parent" `isInfixOf` message && not ("second~" `isInfixOf` message)
  , typeOkWith "two aliases of one module shareþ data identity" "use shared-data.tung first use shared-data.tung second let left: first~token = second~token let right: second~token = first~token" sharedTypeAlias
  , typeErrWith "unknown qualified imported type is rejected" "use shared-data.tung shared let bad: shared~missing = shared~token" sharedTypeAlias
  , typeErrWith "private imported type spelling stayeþ hidden" "use opaque.tung opaque let bad: opaque~secret = opaque~value" opaqueTypeImport
  , typeOkWith "value with a private imported type remaineth inferable" "use opaque.tung opaque let value = opaque~value" opaqueTypeImport
  , typeOkWith "type identity surviveþ a re-export" "use base-data.tung original use middle-data.tung reexport let from-original: reexport~token = original~value let from-reexport: original~token = reexport~value" reexportedTypeImport
  , typeErrWith "host data schema rejecteþ nested nominal lookalikes" nestedTableLookalike rawTableImport
  , typeOkWith "unresolved raw host data dependencies remain ABI-compatible" "use ilk/table.tung standard ilk k table v { ((k ∏ v) list) from-list } let cast: ℤ table text → ℤ standard~table text = { x @ x }" rawTableImport
  , typeOkWith "coverage separateþ same-spelled imported data" "use left-token.tung left use right-token.tung right let result: ℤ = match left~value { left~zero @ 0, left~one @ 1 }" nominalCoverageImports
  , typeErrWith "use aliases cannot collide" "use left.tung common use right.tung common" duplicateAliases
  , typeErrWith "default use aliases cannot collide" "use ilk/item.tung use syntax/item.tung" defaultAliasCollisionImports
  , typeOkWith "explicit use aliases override colliding defaults" "use ilk/item.tung ilk-item use syntax/item.tung syntax-item let result = ilk-item~value + syntax-item~value" defaultAliasCollisionImports
  , expect "ambiguous imports suggest surface namespaces" $
      let message = checkWithImports "use ilk/natural.tung use algebra/arithmetic/semiring.tung let bad = zero" ambiguousZeros
       in "natural~zero" `isInfixOf` message && "semiring~zero" `isInfixOf` message && not ("/" `isInfixOf` message)
  , typeErrWith "unshown value stayeþ hidden" "use file.tung let bad = hidden" shown
  , typeErrWith "a transitive frame requireþ an explicit re-export" "use middle.tung graiþ a hidden frame a child { let a child: a }" privateShapeImports
  , typeErrWith "a transitive frame cannot be reached through its private namespace" "use middle.tung graiþ a leaf~hidden frame a child { let a child: a }" privateShapeImports
  , typeErrWith "a reserved incompatible data lookalike cannot receive a native value" "use ilk/two.tung let truth: two~𝟚 = 1 ≡ 1" reservedTwoLookalike
  , typeOkWith "a reserved incompatible data lookalike retaineþ its own type" "use ilk/two.tung let value: two~𝟚 = two~yea" reservedTwoLookalike
  , typeErrWith "missing use is rejected" "use missing.tung" Map.empty
  , typeErrWith "bad imported source is rejected" "use bad.tung" (Map.singleton "bad.tung" "let =")
  , typeErrWith "self use cycle is rejected" "use self.tung" (Map.singleton "self.tung" "use self.tung")
  , typeErrWith "mutual use cycle is rejected" "use left.tung" cyclic
  , runnableErr "module without main is not runnable" "let answer = 42"
  , runnableOk "inferred pure main is runnable" (unitData ++ "let _ main = only")
  , runnableOk "partial effectful call is pure at boundary" (unitData ++ "let half = 1 % let (_: 𝟙) main: 𝟙 = only")
  , runnableOk "runner accepteþ unit and runner effects" runnableOkSource
  , runnableOk "host effect operation order is schema-insensitive" (unitData ++ "deed console { 𝟙 read: text, text write: 𝟙 } let (_: 𝟙) main: 𝟙 ! console = 'ok' write")
  , runnableOk "host effect rows are schema-insensitive sets" reorderedWebSource
  , expect "host effect schema rejecteþ a nominal unit lookalike" $
      "unsupported effects {console}" `isInfixOf` checkRunnableWithImports nominalConsoleLookalike (Map.singleton "ilk/one.tung" "show ilk 𝟙 { only }")
  , expect "host effect schema rejecteþ a nested nominal effect lookalike" $
      "unsupported effects {web}" `isInfixOf` checkRunnableWithImports nominalWebFailLookalike Map.empty
  , runnableErr "main rejecteþ a nonstandard unit lookalike" "ilk 𝟙 { done } let (done: 𝟙) main: 𝟙 = done"
  , runnableErr "main must be a function" (unitData ++ "let main = only")
  , runnableErr "main takeþ one unit argument" (unitData ++ "let (_: ℤ) main: 𝟙 = only")
  , runnableErr "main returneþ unit" (unitData ++ "let (_: 𝟙) main: ℤ = 42")
  , runnableErr "fail is not a main effect" (unitData ++ "let (_: 𝟙) main: 𝟙 ! text fail = (let _ = 1 % 0 yield only)")
  , runnableErr "custom effect is not a main effect" (unitData ++ "deed ask { 𝟙 ask: 𝟙 } let (_: 𝟙) main: 𝟙 ! ask = only ask")
  , runnableErr "parameterised runner-looking effect is rejected" (unitData ++ "deed a console { 𝟙 fake: 𝟙 } let (_: 𝟙) main: 𝟙 ! 𝟙 console = only fake")
  , runnableErr "effects cannot run before main" (unitData ++ "let _ = 'early' write let (_: 𝟙) main: 𝟙 = only")
  , expectEq "reporteþ an inferred source type" (Right "m1 → m1") (typeOfWithImports "show let x identity = x" Map.empty "identity")
  , expectEq "reporteþ an unknown inspected name" (Left "unknown name 'missing'") (typeOfWithImports "let answer = 42" Map.empty "missing")
  , expect "elaboration resolveþ every type-class evidence hole" evidenceIsResolved
  , typeOkWith "shared diamond imports" "use left.tung use right.tung yield left~left + right~right" sharedImports
  ]
 where
  shown = Map.fromList [("file.tung", "show ilk box { box } let hidden = 1")]
  aliased = Map.singleton "ilk/item.tung" "show ilk item { item } show frame a identity { let a identity: a } fill item identity { let x identity = x } show let empty: item = item"
  reexportMismatchImports =
    Map.fromList
      [ ("base-left.tung", "show ilk token { token } show let value: token = token")
      , ("base-right.tung", "show ilk token { token } show let value: token = token")
      , ("middle-left.tung", "use base-left.tung base show-ilk base~token show base~value")
      , ("middle-right.tung", "use base-right.tung base show-ilk base~token show base~value")
      ]
  concreteAliasProjection =
    Map.fromList
      [ ("base.tung", "show ilk a box { a box }")
      , ("middle.tung", "use base.tung base show let-ilk boxed = ℤ base~box show let value: boxed = 1 base~box")
      ]
  reexportEffectImports =
    Map.fromList
      [ ("base-left.tung", "show deed pulse { ℤ ask: ℤ }")
      , ("base-right.tung", "show deed pulse { ℤ ask: ℤ }")
      , ("middle-left.tung", "use base-left.tung base-left show-ilk base-left~pulse show base-left~ask")
      , ("middle-right.tung", "use base-right.tung base-right show-ilk base-right~pulse show base-right~ask")
      ]
  reexportShapeImports =
    Map.fromList
      [ ("base-left.tung", "show frame a identity { let a identity: a }")
      , ("base-right.tung", "show frame a identity { let a identity: a }")
      , ("middle-left.tung", "use base-left.tung base-left show-ilk base-left~identity show base-left~identity")
      , ("middle-right.tung", "use base-right.tung base-right show-ilk base-right~identity show base-right~identity")
      ]
  fixedShapeTypeImports =
    Map.fromList
      [ ("left-data.tung", "show ilk token { token } show frame a maker { let maker: token }")
      , ("right-data.tung", "show ilk token { token }")
      ]
  nestedFillImport =
    Map.singleton
      "dep.tung"
      "show ilk token { token } show frame a identity { let a identity: a } show frame a label { let a label: a } graiþ token label fill token identity { let x identity = x label } show let value: token = token"
  duplicatedFillImport =
    Map.singleton
      "base.tung"
      "show ilk token { token } show frame a parent { let a parent: ℤ } show frame a child { let a child: ℤ } graiþ token parent fill token child { let x child = x parent } show let value: token = token"
  sharedTypeAlias = Map.singleton "shared-data.tung" "show ilk token { token }"
  opaqueTypeImport = Map.singleton "opaque.tung" "ilk secret { secret } show let value: secret = secret"
  reexportedTypeImport =
    Map.fromList
      [ ("base-data.tung", "show ilk token { token } show let value: token = token")
      , ("middle-data.tung", "use base-data.tung base show-ilk base~token show base~value")
      ]
  nominalCoverageImports =
    Map.fromList
      [ ("left-token.tung", "show ilk token { zero, one } show let value: token = zero")
      , ("right-token.tung", "show ilk token { only } show let value: token = only")
      ]
  duplicateAliases = Map.fromList [("left.tung", "show let left = 1"), ("right.tung", "show let right = 2")]
  defaultAliasCollisionImports = Map.fromList [("ilk/item.tung", "show let value = 1"), ("syntax/item.tung", "show let value = 2")]
  ambiguousZeros = Map.fromList [("ilk/natural.tung", "show let zero = 0"), ("algebra/arithmetic/semiring.tung", "show let zero = 1")]
  privateShapeImports = Map.fromList [("leaf.tung", "show frame a hidden { let a hidden: a }"), ("middle.tung", "use leaf.tung")]
  reservedTwoLookalike = Map.singleton "ilk/two.tung" "show ilk 𝟚 { yea, maybe }"
  rawTableImport = Map.singleton "ilk/table.tung" "show ilk k table v { ((k ∏ v) list) from-list }"
  nestedTableLookalike =
    "use ilk/table.tung standard "
      ++ "ilk a ∏ b { pair } ilk a list { empty } ilk k table v { ((k ∏ v) list) from-list } "
      ++ "let local: ℤ table text = empty from-list let escaped: ℤ standard~table text = local"
  cyclic = Map.fromList [("left.tung", "use right.tung"), ("right.tung", "use left.tung")]
  sharedImports =
    Map.fromList
      [ ("left.tung", "use shared.tung show let left = shared~base + 1")
      , ("right.tung", "use shared.tung show let right = shared~base + 2")
      , ("shared.tung", "show let base = 1")
      ]
  evidenceIsResolved = case parse (equalPrelude ++ "fill ℤ equal { let x ≡ y = yea } let answer = 1 ≡ 1") >>= (\program -> elaborateProgramWithImports program Map.empty False) of
    Left _ -> False
    Right program -> dictionariesAreResolved program

tableAndSetCases :: Map.Map String String -> [Test]
tableAndSetCases imports =
  [ typeOkWith "table, set, and powerset may qualify shared names" (source ++ qualifiedValues) imports
  , typeErrWith "table, set, powerset, and list empty values are ambiguous bare" (source ++ "let bad = empty") imports
  , typeErrWith "table and set from-list constructors are ambiguous bare" (source ++ "let values: ℤ list = list~empty let bad = values from-list") imports
  ]
 where
  source = "use ilk/list.tung use ilk/table.tung use ilk/set.tung use ilk/powerset.tung "
  qualifiedValues =
    "let table-value: ℤ table text = table~empty "
      ++ "let set-value: ℤ set = set~empty "
      ++ "let powerset-value: ℤ powerset = powerset~empty "
      ++ "let table-built: ℤ table text = list~empty table~from-list "
      ++ "let set-built: ℤ set = list~empty set~from-list"

orderCases :: Map.Map String String -> [Test]
orderCases imports =
  [ typeOkWith "partial order supplieþ strict comparison" (partialPrefix ++ "let good = lesser < greater") imports
  , typeErrWith "partial order doth not supply compare" (partialPrefix ++ "let bad = lesser compare greater") imports
  , typeErrWith "partial order doth not supply clamp" (partialPrefix ++ "let bad = lesser clamp greater lesser") imports
  , typeOkWith "total order inherits partial operations" (totalPrefix ++ "let before = lesser < greater let compared = lesser compare greater let bounded = lesser clamp greater greater") imports
  , typeErrWith "total order fill requireþ its partial parent" (rankPrefix ++ "fill rank order-total {}") imports
  , typeOkWith "finite set supplieþ partial order" (setPrefix ++ "let included = smaller ≤ larger") imports
  , typeErrWith "finite set doth not claim total order" (setPrefix ++ "let bad = smaller compare larger") imports
  ]
 where
  rankPrefix =
    "use ilk/two.tung use equal.tung use order.tung "
      ++ "ilk rank { lesser, greater } "
      ++ "fill rank equal { let ≡ = { lesser, lesser @ yea, greater, greater @ yea, _, _ @ nay } } "
  partialPrefix =
    rankPrefix
      ++ "fill rank order-partial { let ≤ = { lesser, _ @ yea, greater, greater @ yea, greater, lesser @ nay } } "
  totalPrefix = partialPrefix ++ "fill rank order-total {} "
  setPrefix =
    "use ground.tung use ilk/set.tung "
      ++ "let smaller: ℤ set = empty put 1 "
      ++ "let larger: ℤ set = smaller put 2 "

boundCases :: Map.Map String String -> [Test]
boundCases imports =
  [ typeOkWith "finite orders provide both endpoints" (prefix ++ "let low: 𝟚 = ⟂ let high: 𝟛 = ⊤") imports
  , typeOkWith "natural supporteþ the lower-bound fold" (prefix ++ "let value: ℕ = (natural~zero _* list~empty) …∨") imports
  , typeErrWith "natural doth not claim an upper bound" (prefix ++ "let value: ℕ = (natural~zero _* list~empty) …∧") imports
  , typeOkWith "powerset is a bounded lattice without total order" (powersetPrefix ++ "let joined: ℤ powerset = (empty-set _* (full-set _* list~empty)) …∨") imports
  , typeErrWith "lattice doth not imply total order" (powersetPrefix ++ "let bad = empty-set ≤ full-set") imports
  , typeErrWith "float doth not claim a lawful lattice" "use ground.tung let bad: float = 1.0 ∧ 2.0" imports
  , typeOkWith "infimum semilattice stands without supremum" (semilatticePrefix ++ "let good = item ∧ item") imports
  , typeErrWith "infimum semilattice doth not supply supremum" (semilatticePrefix ++ "let bad = item ∨ item") imports
  , typeOkWith "supremum semilattice stands without infimum" (supremumPrefix ++ "let good = item ∨ item") imports
  , typeErrWith "supremum semilattice doth not supply infimum" (supremumPrefix ++ "let bad = item ∧ item") imports
  ]
 where
  prefix = "use ground.tung use ilk/list.tung use ilk/natural.tung use collection/catamorphism.tung "
  powersetPrefix = "use ground.tung use ilk/list.tung use ilk/powerset.tung use collection/catamorphism.tung let empty-set: ℤ powerset = powerset~empty let full-set: ℤ powerset = universe "
  semilatticePrefix = "use order/lattice.tung ilk one-sided { item } fill one-sided semilattice-infimal { let _ ∧ _ = item } "
  supremumPrefix = "use order/lattice.tung ilk one-sided { item } fill one-sided semilattice-supremal { let _ ∨ _ = item } "

numericHierarchyCases :: Map.Map String String -> [Test]
numericHierarchyCases imports =
  [ typeOkWith "ℤ supplieþ euclidean division" "use ground.tung use ilk/product.tung graiþ a euclidean let (x: a, y: a) divide-with-rest: a ∏ a ! text fail = x % y let value: ℤ ∏ ℤ = try -5 divide-with-rest 3 { fail @ 0 ∏ 0 }" imports
  , typeOkWith "float retaineþ raw numeric and comparison operations" "use ground.tung let sum: float = 1.0 + 2.0 let quotient: float = 1.0 ÷ 2.0 let compared: 𝟚 = 1.0 ≤ 2.0" imports
  , typeErrWith "float doth not claim exact semiring laws" "use ground.tung graiþ a semiring let (x: a) twice: a = x + x let bad: float = 1.0 twice" imports
  , typeErrWith "float doth not claim exact field laws" "use ground.tung graiþ a field let (x: a) reciprocal: a = one ÷ x let bad: float = 2.0 reciprocal" imports
  , typeErrWith "float doth not claim partial-order laws" "use ground.tung graiþ a order-partial let (x: a) reflexive: 𝟚 = x ≤ x let bad: 𝟚 = 1.0 reflexive" imports
  ]

redundantFillGraithCases :: [Test]
redundantFillGraithCases =
  [ typeErrContaining
      "unused fill graiþ is rejected"
      "fill 'box equal' hath redundant graiþ a equal"
      (equalPrelude ++ boxData ++ "graiþ a equal fill (a box) equal { let _ ≡ _ = yea }")
  , typeErrContaining
      "duplicate fill graiþ is rejected"
      "fill 'box equal' hath redundant graiþ a equal"
      (equalPrelude ++ boxData ++ "graiþ a equal, a equal fill (a box) equal { let (x box) ≡ (y box) = x ≡ y }")
  , typeErrContaining
      "fill graiþ hidden behind a stronger collection graiþ is rejected"
      "fill 'set inhold a' hath redundant graiþ a equal"
      redundantCollectionGraith
  ]

generatedCoverageCases :: [Test]
generatedCoverageCases = complete ++ incomplete
 where
  complete =
    [ typeOk ("generated exhaustive match of arity " ++ show arity) (matchSource arity Nothing)
    | arity <- [1 .. 3]
    ]
  incomplete =
    [ typeErr
        ("generated match of arity " ++ show arity ++ " misses " ++ intercalate "," missing)
        (matchSource arity (Just missing))
    | arity <- [1 .. 3]
    , missing <- booleanRows arity
    ]

redundantCoverageCases :: [Test]
redundantCoverageCases =
  [ typeErrContaining "duplicate ℤ pattern is reported" "redundant match case 2; pattern 0 is unreachable" "let bad = match 1 { 0 @ 0, 0 @ 1, _ @ 2 }"
  , typeErrContaining "duplicate text pattern is reported" "redundant match case 2; pattern 'same' is unreachable" "let bad = match 'same' { 'same' @ 0, 'same' @ 1, _ @ 2 }"
  , typeErrContaining "wildcard hideþ constructor pattern" "redundant match case 2; pattern yea is unreachable" (boolData ++ "let bad = match yea { _ @ 0, yea @ 1 }")
  , typeErrContaining "constructor pattern hidden inside product" "redundant match case 3" (boolData ++ "let bad = match yea, nay { yea, _ @ 1, nay, _ @ 2, _, yea @ 3 }")
  , typeErrContaining "nested constructor pattern is reported" "redundant match case 2" (boolData ++ optionData ++ "let bad = match yea some { _ some @ 1, yea some @ 2, none @ 0 }")
  , typeErrContaining "pattern over empty type is unreachable" "redundant match case 1" "ilk 𝟘 {} let bad: 𝟘 → ℤ = { _ @ 1 }"
  ]

memberGraith :: String
memberGraith =
  "ilk a option { none, a some } ilk a box { a box } "
    ++ "frame f functor { let (a f) map (a → b): b f } "
    ++ "graiþ f functor frame f applicative { let a pure: a f let (a f) apply ((a → b) f): b f } "
    ++ "frame f traverse { graiþ m applicative let (a f) traverse (a → b m): (b f) m } "

matchSource :: Int -> Maybe [String] -> String
matchSource arity omitted =
  boolData
    ++ "let checked: ℤ = match "
    ++ intercalate ", " (replicate arity "yea")
    ++ " { "
    ++ intercalate
      ", "
      [ intercalate ", " row ++ " @ " ++ show result
      | (result, row) <- zip [0 :: Int ..] (booleanRows arity)
      , Just row /= omitted
      ]
    ++ " }"

booleanRows :: Int -> [[String]]
booleanRows arity = replicateM arity ["yea", "nay"]

generatedEffectCases :: [Test]
generatedEffectCases = reordered ++ omitted
 where
  effects = ["pulse", "spark", "glow"]
  reordered =
    [ typeOk
        ("generated effect row order " ++ intercalate "," row)
        (effectSource effects row)
    | row <- permutations effects
    ]
  omitted =
    [ typeErr
        ("generated effect row misses " ++ missing)
        (effectSource effects (filter (/= missing) effects))
    | missing <- effects
    ]

effectSource :: [String] -> [String] -> String
effectSource performed annotated =
  unitData
    ++ concatMap (\name -> "deed " ++ name ++ " { 𝟙 " ++ name ++ ": 𝟙 } ") performed
    ++ "let run: 𝟙 → 𝟙 ! "
    ++ intercalate ", " annotated
    ++ " = { _ @ "
    ++ foldr (\name body -> "(let _ = only " ++ name ++ " yield " ++ body ++ ")") "only" performed
    ++ " }"

inferenceProperties :: [Test]
inferenceProperties =
  [ Harness.propertyTest "property: polymorphic identity inferreþ every generated primitive use" $
      QuickCheck.forAll (QuickCheck.listOf1 (QuickCheck.elements primitiveValues)) \values ->
        let declarations =
              [ "let value" ++ show index ++ ": " ++ ty ++ " = " ++ literal ++ " identity "
              | (index, (ty, literal)) <- zip [(0 :: Int) ..] (take 12 values)
              ]
            source = "let x identity = x " ++ concat declarations
         in QuickCheck.counterexample source (check source QuickCheck.=== "type ok")
  , Harness.propertyTest "property: primitive annotations rejecteþ a different inferred type" $
      QuickCheck.forAll (QuickCheck.elements primitiveValues) \(actualType, literal) ->
        QuickCheck.forAll (QuickCheck.elements [ty | (ty, _) <- primitiveValues, ty /= actualType]) \claimedType ->
          let source = "let value: " ++ claimedType ++ " = " ++ literal
           in QuickCheck.counterexample source ("type error:" `isPrefixOf` check source)
  ]

primitiveValues :: [(String, String)]
primitiveValues =
  [ ("ℤ", "42")
  , ("float", "1.5")
  , ("text", "'word'")
  , ("unicode", "`x")
  ]

evidenceProperties :: [Test]
evidenceProperties =
  [ Harness.propertyTest "property: unrelated fill order keepeþ evidence coherent" $
      QuickCheck.forAll (QuickCheck.shuffle evidenceCases) \orderedCases ->
        let source = equalPrelude ++ concatMap evidenceFill orderedCases ++ concatMap evidenceUse orderedCases
         in QuickCheck.counterexample source (QuickCheck.conjoin [check source QuickCheck.=== "type ok", QuickCheck.property (evidenceResolved source)])
  ]

data EvidenceCase = EvidenceCase String String
  deriving stock (Show)

evidenceCases :: [EvidenceCase]
evidenceCases =
  [ EvidenceCase "ℤ" "1"
  , EvidenceCase "float" "1.5"
  , EvidenceCase "text" "'word'"
  , EvidenceCase "unicode" "`x"
  ]

evidenceFill :: EvidenceCase -> String
evidenceFill (EvidenceCase ty _) = "fill " ++ ty ++ " equal { let x ≡ y = yea }"

evidenceUse :: EvidenceCase -> String
evidenceUse (EvidenceCase ty literal) = "let " ++ ty ++ "-evidence: 𝟚 = " ++ literal ++ " ≡ " ++ literal ++ " "

evidenceResolved :: String -> Bool
evidenceResolved source = case parse source >>= (\program -> elaborateProgramWithImports program Map.empty False) of
  Left _ -> False
  Right program -> dictionariesAreResolved program

dictionariesAreResolved :: CoreProgram -> Bool
dictionariesAreResolved program =
  let rendered = show program
   in "CoreDictionary" `isInfixOf` rendered
        && not ("EWithEvidence" `isInfixOf` rendered)
        && not ("EDictionary" `isInfixOf` rendered)
        && not ("ElaboratedFill" `isInfixOf` rendered)

coverageProperties :: [Test]
coverageProperties =
  [ Harness.propertyTest "property: generated product coverage accepteþ all rows and rejecteþ one omission" $
      QuickCheck.forAll (QuickCheck.chooseInt (1, 4)) \arity ->
        QuickCheck.forAll (QuickCheck.elements (booleanRows arity)) \omitted ->
          let complete = matchSource arity Nothing
              incomplete = matchSource arity (Just omitted)
           in QuickCheck.counterexample incomplete (check complete QuickCheck.=== "type ok" QuickCheck..&&. ("type error:" `isPrefixOf` check incomplete))
  ]

unitData, boolData, optionData, boxData, equalPrelude, classHierarchy, inheritedMethodHierarchy, parentDictionaryGraith, redundantCollectionGraith, duoEffect, runnableOkSource, webData, reorderedWebSource, nominalConsoleLookalike, nominalWebFailLookalike :: String
unitData = "ilk 𝟙 { only } "
boolData = "ilk 𝟚 { yea, nay } "
optionData = "ilk a option { none, a some } "
boxData = "ilk a box { a box } "
equalPrelude = boolData ++ "frame a equal { let a ≡ a: 𝟚 } "
classHierarchy = "frame a parent { let a parent: a } graiþ a parent frame a child { let a child: a } "
inheritedMethodHierarchy =
  "frame a magma { let a combine a: a } "
    ++ "graiþ a magma frame a semigroup {} "
    ++ "ilk a option { none, a some } "
    ++ "graiþ a semigroup fill (a option) semigroup { "
    ++ "let combine = { none, b @ b, a, none @ a, a some, b some @ a combine b $ some } "
    ++ "}"
parentDictionaryGraith =
  "frame a parent { let a parent: a } "
    ++ "graiþ a parent frame a child {} "
    ++ boxData
    ++ "graiþ a parent fill (a box) parent { let (x box) parent = (x parent) box } "
    ++ "graiþ a parent fill (a box) child {}"
redundantCollectionGraith =
  equalPrelude
    ++ "frame a inhold b { let a ∋ b: 𝟚 } "
    ++ "ilk a list { empty, a cons (a list) } "
    ++ "graiþ a equal fill (a list) inhold a { let list ∋ value = match list { empty @ nay, candidate cons _ @ value ≡ candidate } } "
    ++ "ilk a set { (a list) set } "
    ++ "graiþ a equal, (a list) inhold a fill (a set) inhold a { let (list set) ∋ value = list ∋ value }"
duoEffect = unitData ++ "deed duo { 𝟙 first: 𝟙, 𝟙 second: 𝟙 } "
runnableOkSource = unitData ++ "let (_: 𝟙) main: 𝟙 ! console = 'ok' write"
reorderedWebSource =
  webData
    ++ "deed e fail { e fail: a } "
    ++ "deed web { ℤ serve (request → response ! e): 𝟙 ! text fail, e } "
    ++ "let (_: request) route: response = 200 response (empty from-list) '' "
    ++ "let (_: 𝟙) main: 𝟙 ! web = try 8080 serve route { _ fail @ only }"
webData =
  unitData
    ++ "ilk a list { empty, a _* (a list) } "
    ++ "ilk k table v { ((k ∏ v) list) from-list } "
    ++ "ilk request { text request text (text table text) text } "
    ++ "ilk response { ℤ response (text table text) text } "
nominalConsoleLookalike =
  "use ilk/one.tung one ilk 𝟙 { done } deed console { text write: 𝟙, 𝟙 read: text } "
    ++ "let (_: one~𝟙) main: one~𝟙 ! console = (let _ = 'ok' write yield one~only)"
nominalWebFailLookalike =
  webData
    ++ "deed e fail { e nope: a } "
    ++ "deed web { ℤ serve (request → response ! e): 𝟙 ! e, text fail } "
    ++ "let (_: 𝟙) main: 𝟙 ! web = only"
