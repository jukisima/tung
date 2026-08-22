-- static semantics: positive inference and adversarial rejection cases live here.
module Test.Type (group) where

import Control.Monad (replicateM)
import Data.List (intercalate, isInfixOf, isPrefixOf, permutations)
import Data.Map.Strict qualified as Map
import Test.Harness (Group, Test, expect, expectEq, runnableErr, runnableOk, typeErr, typeErrContaining, typeErrWith, typeOk, typeOkWith)
import Test.Harness qualified as Harness
import Test.QuickCheck qualified as QuickCheck
import Tung (check, elaborateProgramWithImports, parse, readBookhoardImports, typeOfWithImports)

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
  [ ("literal annotation", "let answer: integer = 42")
  , ("arbitrary term type ascription", "let answer = (1 + 2: integer)")
  , ("polymorphic term type ascription is reusable", "let identity = ({ x | x }: a → a) let number: integer = 1 identity let word: text = 'x' identity")
  , ("ascribed anonymous function remaineth recursive", "let factorial = ({ 0 | 1, n | n × ((n - 1) factorial) }: integer → integer) let answer: integer = 5 factorial")
  , ("type ascription preserveth a graith", equalPrelude ++ "graith a equal let (x: a, y: a) same: 𝟚 = (x ≡ y: 𝟚)")
  , ("type ascription preserveth an immediate effect", unitData ++ "deed pulse { 𝟙 pulse: integer } let (_: 𝟙) run: integer ! pulse = (null pulse: integer)")
  , ("unicode primitive", "let letter: unicode = `a")
  , ("text primitive", "let word: text = 'λ字'")
  , ("let polymorphism is reusable", "let x id = x let number: integer = 1 id let word: text = 'x' id")
  , ("annotated polymorphic identity", "let (x: a) id: a = x let number: integer = 1 id let word: text = 'x' id")
  , ("subscripted type variable", "let (x: a₀) id: a₀ = x let number: integer = 1 id let word: text = 'x' id")
  , ("higher-order effect polymorphism", "let (x: a, f: a → b ! e) call: b ! e = x f")
  , ("two effect rows compose", "let (f: a → b ! e0, g: b → c ! e1, x: a) compose: c ! e0, e1 = (x f) g")
  , ("subscripted effect rows compose", "let (f: a → b ! e₀, g: b → c ! e₁, x: a) compose: c ! e₀, e₁ = (x f) g")
  , ("pure partial application hideth latent effect", "let half = 1 ÷")
  , ("type alias is transparent", "let-ilk count = integer let answer: count = 42")
  , ("parameterised type alias is transparent", boolData ++ "let (_: integer) member: 𝟚 = yea let answer: 𝟚 = 1 member")
  , ("func equaleth pure unary arrow", "let (x: integer) id: integer = x")
  , ("curried triple function", "let (x: integer, _: text, _: float) pick: integer = x")
  , ("constructor application is curried", "kin a option { none, a some } let make = some let value: integer option = 1 make")
  , ("empty type eliminator", "kin 𝟘 {} let initial: 𝟘 → a = {} let use: 𝟘 → integer = initial")
  , ("multi-scrutinee exhaustive match", boolData ++ "let ok: integer = match yea, nay { yea, yea | 1, yea, nay | 2, nay, yea | 3, nay, nay | 4 }")
  , ("overlapping wildcards cover a product", boolData ++ "let ok: integer = match yea, nay { yea, _ | 1, _, yea | 2, nay, nay | 3 }")
  , ("nested exhaustive match", boolData ++ optionData ++ "let ok: integer = match yea some { yea some | 1, nay some | 2, none | 0 }")
  , ("variable pattern closeth coverage", boolData ++ "let ok: integer = match yea { yea | 1, other | 0 }")
  , ("integer match with fallback", "let ok: integer = match 1 { 0 | 0, 1 | 1, _ | 2 }")
  , ("integer patterns in a function", "let classify: integer → integer = { 0 | 10, -1 | 20, _ | 30 }")
  , ("closed record", "let person: [age: integer, name: text] = [name = 'n', age = 1]")
  , ("record access", "let person = [name = 'n', age = 1] let age: integer = person@age")
  , ("record update changeth a field type", "let person = [value = 1] let changed: [value: text] = [= person, value = 'one']")
  , ("record removal changeth the row", "let person = [name = 'n', age = 1] let public: [name: text] = [= person, - age]")
  , ("shape graith dischargeth member need", equalPrelude ++ "graith a equal let (x: a, y: a) same: 𝟚 = x ≡ y")
  , ("fill context dischargeth inner need", equalPrelude ++ boxData ++ "graith a equal fill (a box) equal { let (x box) ≡ (y box) = x ≡ y }")
  , ("shape default supplieth a member", "shape a identity { let (x: a) identity: a = x } fill integer identity {}")
  , ("shape law useth its own methods", "shape a identity { let a identity: a law (x: a): x identity ~ x } fill integer identity { let x identity = x }")
  , ("well-typed unequal shape law remaineth checked documentation", "shape a claimed { law (x: integer): x ~ 0 }")
  , ("shape member carrieth its own graith", memberGraith ++ "fill option functor { let value map f = match value { none | none, x some | x f $ some } } fill option applicative { let pure = some let apply = { x some, f some | x f $ some, _, _ | none } } fill option traverse { let value traverse f = match value { none | none pure, x some | (x f) map some } } let lifted: (integer option) option = (1 some) traverse some")
  , ("existing parent fill satisfieth child", classHierarchy ++ "fill integer parent { let x parent = x } fill integer child { let x child = x }")
  , ("child fill may supply parent member", classHierarchy ++ "fill integer child { let x parent = x let x child = x } let ok: integer = 1 parent")
  , ("parametric child fill useth its parent method", inheritedMethodHierarchy)
  , ("fill graith may construct a parent dictionary", parentDictionaryGraith)
  , ("parameterised state effect", unitData ++ "deed a state { 𝟙 get: a, a set: 𝟙 } let got: 𝟙 → integer ! integer state = get")
  , ("operation handler resumeth", "deed ask { integer ask: integer } let ok: integer = try 10 ask { x ask | x resume }")
  , ("handler changeth answer through return", "deed ask { integer ask: integer } let ok: text = try 10 ask { return n | n to-text, x ask | x resume }")
  , ("all operation clauses remove an effect", duoEffect ++ "let (_: 𝟙) run: 𝟙 = try null first { first | null, second | null }")
  , ("effect clause removeth every operation", duoEffect ++ "let (_: 𝟙) run: 𝟙 = try null first { duo | null }")
  , ("handler clause effects escape handler", duoEffect ++ "let (_: 𝟙) run: 𝟙 ! duo = try null first { first | null second, second | null }")
  , ("operation keepeth declared extra effect", unitData ++ "deed other { 𝟙 other: 𝟙 } deed mine { 𝟙 op: 𝟙 ! other } let (_: 𝟙) run: 𝟙 ! other = try null op { op | null other }")
  , ("foreign let entereth value scope", "let add-integer: integer → integer → integer = foreign let answer: integer = 1 add-integer 2")
  , ("declaration-only file is runnable", "let answer = 42")
  ]

rejected :: [(String, String)]
rejected =
  [ ("literal type mismatch", "let bad: integer = 'wrong'")
  , ("term type ascription rejecteth a mismatch", "let bad = ('wrong': integer)")
  , ("parameterised type alias needeth its argument", "let _ bad: predicate = 1")
  , ("parameterised type alias rejecteth extra arguments", "let _ bad: integer integer predicate = 1")
  , ("occurs check rejecteth self application", "let x omega = x x")
  , ("occurs check followeth nested records", "let x omega = [value = x] x")
  , ("annotation cannot claim false polymorphism", "let (_: a) bad: a = 1")
  , ("function branches must agree", "let bad = { yea | 1, nay | 'no' }")
  , ("function cases share arity", "let bad = { x | x, x, y | x }")
  , ("pattern binders are linear", "let bad = { x, x | x }")
  , ("inhabited empty function", "let bad: integer → integer = {}")
  , ("unannotated empty function", "let bad = {}")
  , ("non-exhaustive data match", boolData ++ "let bad: integer = match yea { yea | 1 }")
  , ("multi-match missing one product case", boolData ++ "let bad: integer = match yea, nay { yea, yea | 1, yea, nay | 2, nay, yea | 3 }")
  , ("overlapping wildcards can miss a product", boolData ++ "let bad: integer = match yea, nay { yea, _ | 1, _, yea | 2 }")
  , ("nested match misses constructor", boolData ++ optionData ++ "let bad: integer = match yea some { yea some | 1, none | 0 }")
  , ("constructor pattern arity", optionData ++ "let bad = match 1 some { some | 1, none | 0 }")
  , ("integer pattern needeth integer input", "let bad = match 'one' { 1 | 1, _ | 0 }")
  , ("finite integer patterns are not exhaustive", "let bad = match 1 { 0 | 0, 1 | 1 }")
  , ("closed record rejecteth extra field", "let bad: [name: text] = [name = 'n', age = 1]")
  , ("missing record field", "let person = [name = 'n'] let bad = person@age")
  , ("unknown record removal", "let person = [name = 'n'] let bad = [= person, - age]")
  , ("record update needeth a record", "let bad = [= 1, value = 2]")
  , ("missing function graith", equalPrelude ++ "let (x: a, y: a) same: 𝟚 = x ≡ y")
  , ("insufficient graith", equalPrelude ++ "graith a equal shape a order-partial { let a ≤ a: 𝟚 } graith a equal let (x: a, y: a) leq: 𝟚 = x ≤ y")
  , ("unknown fill shape", "fill integer missing { let x missing = x }")
  , ("fill evidence cannot be shown", "show fill integer missing {}")
  , ("fill misses required member", "shape a identity { let a identity: a } fill integer identity {}")
  , ("fill rejecteth unknown member", "shape a identity { let a identity: a } fill integer identity { let x other = x }")
  , ("fill member hath declared type", "shape a identity { let a identity: a } fill integer identity { let x identity = 'wrong' }")
  , ("duplicate fill is incoherent", "shape a identity { let a identity: a } fill integer identity { let x identity = x } fill integer identity { let x identity = x }")
  , ("overlapping fills are ambiguous when used", "shape a identity { let a identity: a } fill a identity { let x identity = x } fill integer identity { let x identity = x } let bad = 1 identity")
  , ("shape law sides share a value type", "shape a bad { law (x: a): x ~ 1 }")
  , ("shape law sides share an effect row", unitData ++ "deed pulse { 𝟙 pulse: 𝟙 } shape a bad { law (x: 𝟙): x ~ null pulse }")
  , ("shape law rejecteth an unbound value", "shape a bad { law (x: a): x ~ missing }")
  , ("shape law needeth must be declared", equalPrelude ++ "shape a bad { law (x: a): x ≡ x ~ x ≡ x }")
  , ("shape member graith is required at use", memberGraith ++ "fill box traverse { let (x box) traverse f = (x f) map box } let bad: (integer box) box = (1 box) traverse { x | x box }")
  , ("fill arity followeth shape", "shape a convert b { let a convert: b } fill integer convert { let x convert = x }")
  , ("effect operation must be a function", "deed bad { tick: integer }")
  , ("state parameter mismatch", unitData ++ "deed a state { 𝟙 get: a, a set: 𝟙 } let got: 𝟙 → integer ! text state = get")
  , ("one operation clause cannot erase sibling operations", duoEffect ++ "let (_: 𝟙) run: 𝟙 = try null second { first | null }")
  , ("partial coverage doth not combine across handlers", duoEffect ++ "let (_: 𝟙) run: 𝟙 = try (try null second { first | null }) { second | null }")
  , ("handler for absent effect", "let bad: integer = try 1 { fail | 2 }")
  , ("ordinary function is not an operation", "deed ask { integer ask: integer } let (x: integer) f: integer ! ask = x ask let bad = try 1 f { x f | x }")
  , ("resume argument hath operation result type", "deed ask { integer ask: integer } let bad: integer = try 10 ask { x ask | 'bad' resume }")
  , ("resume is scoped to operation clauses", "let bad = 1 resume")
  , ("handler return pattern must be irrefutable", boolData ++ "let bad = try nay { return yea | 1 }")
  , ("handler return integer pattern is refutable", "let bad = try 1 { return 1 | 1 }")
  , ("handler operation patterns must be irrefutable", boolData ++ "deed choose { 𝟚 choose: integer } let bad = try nay choose { yea choose | 1 }")
  , ("effect handler cannot bind operation arguments", duoEffect ++ "let bad: 𝟙 = try null first { x duo | null }")
  , ("effectful let result stayeth monomorphic", unitData ++ "deed a state { 𝟙 get: a } let _ run = (let value = null get let number: integer = value let word: text = value yield null)")
  , ("bookhoard let cannot run immediate effects", "let answer = 'hello' write")
  , ("foreign let requireth an annotation", "let add-integer = foreign")
  , ("foreign let requireth a host implementation", "let unknown: integer → integer = foreign")
  , ("foreign let must match its host signature", "let add-integer: integer → integer = foreign")
  , ("foreign marker cannot be nested", "let bad: [value: integer] = [value = foreign]")
  , ("foreign let must be file-level", "let _ outer = (let add-integer: integer → integer → integer = foreign yield 1)")
  ]

importCases :: [Test]
importCases =
  [ typeOkWith "shown value and type cross a bring" "bring file.tung let value: box = box" shown
  , typeErrWith "unshown value stayeth hidden" "bring file.tung let bad = hidden" shown
  , typeErrWith "missing bring is rejected" "bring missing.tung" Map.empty
  , typeErrWith "bad brought source is rejected" "bring bad.tung" (Map.singleton "bad.tung" "let =")
  , typeErrWith "self bring cycle is rejected" "bring self.tung" (Map.singleton "self.tung" "bring self.tung")
  , typeErrWith "mutual bring cycle is rejected" "bring left.tung" cyclic
  , runnableErr "module without main is not runnable" "let answer = 42"
  , runnableOk "inferred pure main is runnable" (unitData ++ "let _ main = null")
  , runnableOk "partial effectful call is pure at boundary" (unitData ++ "let half = 1 ÷ let (_: 𝟙) main: 𝟙 = null")
  , runnableOk "runner accepteth unit and runner effects" runnableOkSource
  , runnableErr "main must be a function" (unitData ++ "let main = null")
  , runnableErr "main taketh one unit argument" (unitData ++ "let (_: integer) main: 𝟙 = null")
  , runnableErr "main returneth unit" (unitData ++ "let (_: 𝟙) main: integer = 42")
  , runnableErr "fail is not a main effect" (unitData ++ "let (_: 𝟙) main: 𝟙 ! text fail = (let _ = 1 ÷ 0 yield null)")
  , runnableErr "custom effect is not a main effect" (unitData ++ "deed ask { 𝟙 ask: 𝟙 } let (_: 𝟙) main: 𝟙 ! ask = null ask")
  , runnableErr "parameterised runner-looking effect is rejected" (unitData ++ "deed a console { 𝟙 fake: 𝟙 } let (_: 𝟙) main: 𝟙 ! 𝟙 console = null fake")
  , runnableErr "effects cannot run before main" (unitData ++ "let _ = 'early' write let (_: 𝟙) main: 𝟙 = null")
  , expectEq "reporteth an inferred source type" (Right "m1 → m1") (typeOfWithImports "show let x identity = x" Map.empty "identity")
  , expectEq "reporteth an unknown inspected name" (Left "unknown name 'missing'") (typeOfWithImports "let answer = 42" Map.empty "missing")
  , expect "elaboration resolveth every type-class evidence hole" evidenceIsResolved
  , typeOkWith "shared diamond imports" "bring left.tung bring right.tung yield left@left + right@right" sharedImports
  ]
 where
  shown = Map.fromList [("file.tung", "show kin box { box } let hidden = 1")]
  cyclic = Map.fromList [("left.tung", "bring right.tung"), ("right.tung", "bring left.tung")]
  sharedImports =
    Map.fromList
      [ ("left.tung", "bring shared.tung show let left = shared@base + 1")
      , ("right.tung", "bring shared.tung show let right = shared@base + 2")
      , ("shared.tung", "show let base = 1")
      ]
  evidenceIsResolved = case parse (equalPrelude ++ "fill integer equal { let x ≡ y = yea } let answer = 1 ≡ 1") >>= (\program -> elaborateProgramWithImports program Map.empty False) of
    Left _ -> False
    Right program -> "EvidenceFill" `isInfixOf` show program && not ("EvidenceHole" `isInfixOf` show program)

tableAndSetCases :: Map.Map String String -> [Test]
tableAndSetCases imports =
  [ typeOkWith "table, set, and powerset may qualify shared names" (source ++ qualifiedValues) imports
  , typeErrWith "table, set, powerset, and list empty values are ambiguous bare" (source ++ "let bad = empty") imports
  , typeErrWith "table and set from-list constructors are ambiguous bare" (source ++ "let values: integer list = data/list@empty let bad = values from-list") imports
  ]
 where
  source = "bring data/list.tung bring data/table.tung bring data/set.tung bring data/powerset.tung "
  qualifiedValues =
    "let table-value: integer table text = data/table@empty "
      ++ "let set-value: integer set = data/set@empty "
      ++ "let powerset-value: integer powerset = data/powerset@empty "
      ++ "let table-built: integer table text = data/list@empty data/table@from-list "
      ++ "let set-built: integer set = data/list@empty data/set@from-list"

orderCases :: Map.Map String String -> [Test]
orderCases imports =
  [ typeOkWith "partial order supplieth strict comparison" (partialPrefix ++ "let good = lesser < greater") imports
  , typeErrWith "partial order doth not supply compare" (partialPrefix ++ "let bad = lesser compare greater") imports
  , typeErrWith "partial order doth not supply clamp" (partialPrefix ++ "let bad = lesser clamp greater lesser") imports
  , typeOkWith "total order inherits partial operations" (totalPrefix ++ "let before = lesser < greater let compared = lesser compare greater let bounded = lesser clamp greater greater") imports
  , typeErrWith "total order fill requireth its partial parent" (rankPrefix ++ "fill rank order-total {}") imports
  , typeOkWith "finite set supplieth partial order" (setPrefix ++ "let included = smaller ≤ larger") imports
  , typeErrWith "finite set doth not claim total order" (setPrefix ++ "let bad = smaller compare larger") imports
  ]
 where
  rankPrefix =
    "bring data/two.tung bring equal.tung bring order.tung "
      ++ "kin rank { lesser, greater } "
      ++ "fill rank equal { let ≡ = { lesser, lesser | yea, greater, greater | yea, _, _ | nay } } "
  partialPrefix =
    rankPrefix
      ++ "fill rank order-partial { let ≤ = { lesser, _ | yea, greater, greater | yea, greater, lesser | nay } } "
  totalPrefix = partialPrefix ++ "fill rank order-total {} "
  setPrefix =
    "bring ground.tung bring data/set.tung "
      ++ "let smaller: integer set = empty put 1 "
      ++ "let larger: integer set = smaller put 2 "

boundCases :: Map.Map String String -> [Test]
boundCases imports =
  [ typeOkWith "finite orders provide both endpoints" (prefix ++ "let low: 𝟚 = ⟂ let high: 𝟛 = ⊤") imports
  , typeOkWith "natural supporteth the lower-bound fold" (prefix ++ "let value: natural = (data/natural@zero .* data/list@empty) …∨") imports
  , typeErrWith "natural doth not claim an upper bound" (prefix ++ "let value: natural = (data/natural@zero .* data/list@empty) …∧") imports
  , typeOkWith "powerset is a bounded lattice without total order" (powersetPrefix ++ "let joined: integer powerset = (empty-set .* (full-set .* data/list@empty)) …∨") imports
  , typeErrWith "lattice doth not imply total order" (powersetPrefix ++ "let bad = empty-set ≤ full-set") imports
  , typeErrWith "float doth not claim a lawful lattice" "bring ground.tung let bad: float = 1.0 ∧ 2.0" imports
  , typeOkWith "infimum semilattice stands without supremum" (semilatticePrefix ++ "let good = item ∧ item") imports
  , typeErrWith "infimum semilattice doth not supply supremum" (semilatticePrefix ++ "let bad = item ∨ item") imports
  , typeOkWith "supremum semilattice stands without infimum" (supremumPrefix ++ "let good = item ∨ item") imports
  , typeErrWith "supremum semilattice doth not supply infimum" (supremumPrefix ++ "let bad = item ∧ item") imports
  ]
 where
  prefix = "bring ground.tung bring data/list.tung bring data/natural.tung bring collection/catamorphism.tung "
  powersetPrefix = "bring ground.tung bring data/list.tung bring data/powerset.tung bring collection/catamorphism.tung let empty-set: integer powerset = data/powerset@empty let full-set: integer powerset = universe "
  semilatticePrefix = "bring order/lattice.tung kin one-sided { item } fill one-sided semilattice-infimal { let _ ∧ _ = item } "
  supremumPrefix = "bring order/lattice.tung kin one-sided { item } fill one-sided semilattice-supremal { let _ ∨ _ = item } "

numericHierarchyCases :: Map.Map String String -> [Test]
numericHierarchyCases imports =
  [ typeOkWith "integer supplieth euclidean division" "bring ground.tung bring data/product.tung graith a euclidean let (x: a, y: a) divide-with-rest: a ∏ a ! text fail = x ÷ y let value: integer ∏ integer = try -5 divide-with-rest 3 { fail | 0 ∏ 0 }" imports
  , typeOkWith "float retaineth raw numeric and comparison operations" "bring ground.tung let sum: float = 1.0 + 2.0 let quotient: float = 1.0 ∕ 2.0 let compared: 𝟚 = 1.0 ≤ 2.0" imports
  , typeErrWith "float doth not claim exact semiring laws" "bring ground.tung graith a semiring let (x: a) twice: a = x + x let bad: float = 1.0 twice" imports
  , typeErrWith "float doth not claim exact field laws" "bring ground.tung graith a field let (x: a) reciprocal: a = one ∕ x let bad: float = 2.0 reciprocal" imports
  , typeErrWith "float doth not claim partial-order laws" "bring ground.tung graith a order-partial let (x: a) reflexive: 𝟚 = x ≤ x let bad: 𝟚 = 1.0 reflexive" imports
  ]

redundantFillGraithCases :: [Test]
redundantFillGraithCases =
  [ typeErrContaining
      "unused fill graith is rejected"
      "fill 'box equal' hath redundant graith a equal"
      (equalPrelude ++ boxData ++ "graith a equal fill (a box) equal { let _ ≡ _ = yea }")
  , typeErrContaining
      "duplicate fill graith is rejected"
      "fill 'box equal' hath redundant graith a equal"
      (equalPrelude ++ boxData ++ "graith a equal, a equal fill (a box) equal { let (x box) ≡ (y box) = x ≡ y }")
  , typeErrContaining
      "fill graith hidden behind a stronger collection graith is rejected"
      "fill 'set inhold a' hath redundant graith a equal"
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
  [ typeErrContaining "duplicate integer pattern is reported" "redundant match case 2; pattern 0 is unreachable" "let bad = match 1 { 0 | 0, 0 | 1, _ | 2 }"
  , typeErrContaining "wildcard hideth constructor pattern" "redundant match case 2; pattern yea is unreachable" (boolData ++ "let bad = match yea { _ | 0, yea | 1 }")
  , typeErrContaining "constructor pattern hidden inside product" "redundant match case 3" (boolData ++ "let bad = match yea, nay { yea, _ | 1, nay, _ | 2, _, yea | 3 }")
  , typeErrContaining "nested constructor pattern is reported" "redundant match case 2" (boolData ++ optionData ++ "let bad = match yea some { _ some | 1, yea some | 2, none | 0 }")
  , typeErrContaining "pattern over empty type is unreachable" "redundant match case 1" "kin 𝟘 {} let bad: 𝟘 → integer = { _ | 1 }"
  ]

memberGraith :: String
memberGraith =
  "kin a option { none, a some } kin a box { a box } "
    ++ "shape f functor { let (a f) map (a → b): b f } "
    ++ "graith f functor shape f applicative { let a pure: a f let (a f) apply ((a → b) f): b f } "
    ++ "shape f traverse { graith m applicative let (a f) traverse (a → b m): (b f) m } "

matchSource :: Int -> Maybe [String] -> String
matchSource arity omitted =
  boolData
    ++ "let checked: integer = match "
    ++ intercalate ", " (replicate arity "yea")
    ++ " { "
    ++ intercalate
      ", "
      [ intercalate ", " row ++ " | " ++ show result
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
    ++ " = { _ | "
    ++ foldr (\name body -> "(let _ = null " ++ name ++ " yield " ++ body ++ ")") "null" performed
    ++ " }"

inferenceProperties :: [Test]
inferenceProperties =
  [ Harness.propertyTest "property: polymorphic identity inferreth every generated primitive use" $
      QuickCheck.forAll (QuickCheck.listOf1 (QuickCheck.elements primitiveValues)) \values ->
        let declarations =
              [ "let value" ++ show index ++ ": " ++ ty ++ " = " ++ literal ++ " identity "
              | (index, (ty, literal)) <- zip [(0 :: Int) ..] (take 12 values)
              ]
            source = "let x identity = x " ++ concat declarations
         in QuickCheck.counterexample source (check source QuickCheck.=== "type ok")
  , Harness.propertyTest "property: primitive annotations rejecteth a different inferred type" $
      QuickCheck.forAll (QuickCheck.elements primitiveValues) \(actualType, literal) ->
        QuickCheck.forAll (QuickCheck.elements [ty | (ty, _) <- primitiveValues, ty /= actualType]) \claimedType ->
          let source = "let value: " ++ claimedType ++ " = " ++ literal
           in QuickCheck.counterexample source ("type error:" `isPrefixOf` check source)
  ]

primitiveValues :: [(String, String)]
primitiveValues =
  [ ("integer", "42")
  , ("float", "1.5")
  , ("text", "'word'")
  , ("unicode", "`x")
  ]

evidenceProperties :: [Test]
evidenceProperties =
  [ Harness.propertyTest "property: unrelated fill order keepeth evidence coherent" $
      QuickCheck.forAll (QuickCheck.shuffle evidenceCases) \orderedCases ->
        let source = equalPrelude ++ concatMap evidenceFill orderedCases ++ concatMap evidenceUse orderedCases
         in QuickCheck.counterexample source (QuickCheck.conjoin [check source QuickCheck.=== "type ok", QuickCheck.property (evidenceResolved source)])
  ]

data EvidenceCase = EvidenceCase String String
  deriving stock (Show)

evidenceCases :: [EvidenceCase]
evidenceCases =
  [ EvidenceCase "integer" "1"
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
  Right program -> "EvidenceFill" `isInfixOf` show program && not ("EvidenceHole" `isInfixOf` show program)

coverageProperties :: [Test]
coverageProperties =
  [ Harness.propertyTest "property: generated product coverage accepteth all rows and rejecteth one omission" $
      QuickCheck.forAll (QuickCheck.chooseInt (1, 4)) \arity ->
        QuickCheck.forAll (QuickCheck.elements (booleanRows arity)) \omitted ->
          let complete = matchSource arity Nothing
              incomplete = matchSource arity (Just omitted)
           in QuickCheck.counterexample incomplete (check complete QuickCheck.=== "type ok" QuickCheck..&&. ("type error:" `isPrefixOf` check incomplete))
  ]

unitData, boolData, optionData, boxData, equalPrelude, classHierarchy, inheritedMethodHierarchy, parentDictionaryGraith, redundantCollectionGraith, duoEffect, runnableOkSource :: String
unitData = "kin 𝟙 { null } "
boolData = "kin 𝟚 { yea, nay } "
optionData = "kin a option { none, a some } "
boxData = "kin a box { a box } "
equalPrelude = boolData ++ "shape a equal { let a ≡ a: 𝟚 } "
classHierarchy = "shape a parent { let a parent: a } graith a parent shape a child { let a child: a } "
inheritedMethodHierarchy =
  "shape a magma { let a combine a: a } "
    ++ "graith a magma shape a semigroup {} "
    ++ "kin a option { none, a some } "
    ++ "graith a semigroup fill (a option) semigroup { "
    ++ "let combine = { none, b | b, a, none | a, a some, b some | a combine b $ some } "
    ++ "}"
parentDictionaryGraith =
  "shape a parent { let a parent: a } "
    ++ "graith a parent shape a child {} "
    ++ boxData
    ++ "graith a parent fill (a box) parent { let (x box) parent = (x parent) box } "
    ++ "graith a parent fill (a box) child {}"
redundantCollectionGraith =
  equalPrelude
    ++ "shape a inhold b { let a ∋ b: 𝟚 } "
    ++ "kin a list { empty, a cons (a list) } "
    ++ "graith a equal fill (a list) inhold a { let list ∋ value = match list { empty | nay, candidate cons _ | value ≡ candidate } } "
    ++ "kin a set { (a list) set } "
    ++ "graith a equal, (a list) inhold a fill (a set) inhold a { let (list set) ∋ value = list ∋ value }"
duoEffect = unitData ++ "deed duo { 𝟙 first: 𝟙, 𝟙 second: 𝟙 } "
runnableOkSource = unitData ++ "let (_: 𝟙) main: 𝟙 ! console = 'ok' write"
