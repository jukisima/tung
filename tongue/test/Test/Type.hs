-- static semantics: positive inference and adversarial rejection cases live here.
module Test.Type (group) where

import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Test.Harness (Group, Test, expect, expectEq, runnableErr, runnableOk, typeErr, typeErrWith, typeOk, typeOkWith)
import Test.Harness qualified as Harness
import Tung (elaborateProgramWithImports, parse, typeOfWithImports)

group :: IO Group
group =
  Harness.group "type" $
    map (uncurry typeOk) accepted
      ++ map (uncurry typeErr) rejected
      ++ importCases

accepted :: [(String, String)]
accepted =
  [ ("literal annotation", "let answer: integer = 42;")
  , ("character primitive", "let letter: character = `a;")
  , ("let polymorphism is reusable", "let id = { x | x }; let number: integer = 1 id; let text: string = 'x' id;")
  , ("annotated polymorphic identity", "let id: a → a = { x | x }; let number: integer = 1 id; let text: string = 'x' id;")
  , ("higher-order effect polymorphism", "let (x: a, f: a → b ! e) call: b ! e = x f;")
  , ("two effect rows compose", "let (f: a → b ! e0, g: b → c ! e1) compose: a → c ! e0, e1 = { x | (x f) g };")
  , ("pure partial application hides latent effect", "let half: integer → integer ! string fail = 1 ÷;")
  , ("type alias is transparent", "let-ilk count = integer; let answer: count = 42;")
  , ("func equals pure unary arrow", "let id: integer func integer = { x | x };")
  , ("curried triple function", "let pick: integer → string → float → integer = { x, _, _ | x };")
  , ("constructor application is curried", "kin a option { none, a some }; let make = some; let value: integer option = 1 make;")
  , ("empty type eliminator", "kin 𝟘 {}; let initial: 𝟘 → a = {}; let use: 𝟘 → integer = initial;")
  , ("multi-scrutinee exhaustive match", boolData ++ "let ok: integer = match yea, nay { yea, yea | 1, yea, nay | 2, nay, yea | 3, nay, nay | 4 };")
  , ("nested exhaustive match", boolData ++ optionData ++ "let ok: integer = match yea some { yea some | 1, nay some | 2, none | 0 };")
  , ("variable pattern closes coverage", boolData ++ "let ok: integer = match yea { yea | 1, other | 0 };")
  , ("closed record", "let person: [age: integer, name: string] = [name = 'n', age = 1];")
  , ("record access", "let person = [name = 'n', age = 1]; let age: integer = person@age;")
  , ("record update changes a field type", "let person = [value = 1]; let changed: [value: string] = [= person, value = 'one'];")
  , ("record removal changes the row", "let person = [name = 'n', age = 1]; let public: [name: string] = [= person, - age];")
  , ("shape graith discharges member need", equalPrelude ++ "graith a equal let (x: a, y: a) same: 𝟚 = x ≡ y;")
  , ("fill context discharges inner need", equalPrelude ++ boxData ++ "graith a equal fill (a box) equal { let (x box) ≡ (y box) = x ≡ y };")
  , ("shape default supplies a member", "shape a identity { let (x: a) identity: a = x }; fill integer identity {};")
  , ("shape law uses its own methods", "shape a identity { a identity: a; law (x: a): x identity ~ x }; fill integer identity { let x identity = x };")
  , ("existing parent fill satisfies child", classHierarchy ++ "fill integer parent { let x parent = x }; fill integer child { let x child = x };")
  , ("child fill may supply parent member", classHierarchy ++ "fill integer child { let x parent = x; let x child = x }; let ok: integer = 1 parent;")
  , ("parametric child fill uses its parent method", inheritedMethodHierarchy)
  , ("parameterised state effect", unitData ++ "deed a state { 𝟙 get: a, a set: 𝟙 }; let got: 𝟙 → integer ! integer state = get;")
  , ("operation handler resumes", "deed ask { integer ask: integer }; let ok: integer = try 10 ask { x ask | x resume };")
  , ("handler changes answer through return", "deed ask { integer ask: integer }; let ok: string = try 10 ask { return n | n to-string, x ask | x resume };")
  , ("all operation clauses remove an effect", duoEffect ++ "let run: 𝟙 → 𝟙 = { _ | try null first { first | null, second | null } };")
  , ("effect clause removes every operation", duoEffect ++ "let run: 𝟙 → 𝟙 = { _ | try null first { duo | null } };")
  , ("handler clause effects escape handler", duoEffect ++ "let run: 𝟙 → 𝟙 ! duo = { _ | try null first { first | null second, second | null } };")
  , ("operation keeps declared extra effect", unitData ++ "deed other { 𝟙 other: 𝟙 }; deed mine { 𝟙 op: 𝟙 ! other }; let run: 𝟙 → 𝟙 ! other = { _ | try null op { op | null other } };")
  , ("foreign declaration enters value scope", "class foreign { integer add-integer integer: integer }; let answer: integer = 1 add-integer 2;")
  , ("declaration-only file is runnable", "let answer = 42;")
  ]

rejected :: [(String, String)]
rejected =
  [ ("literal type mismatch", "let bad: integer = 'wrong';")
  , ("occurs check rejects self application", "let omega = { x | x x };")
  , ("annotation cannot claim false polymorphism", "let bad: a → a = { _ | 1 };")
  , ("function branches must agree", "let bad = { yea | 1, nay | 'no' };")
  , ("function cases share arity", "let bad = { x | x, x, y | x };")
  , ("pattern binders are linear", "let bad = { x, x | x };")
  , ("inhabited empty function", "let bad: integer → integer = {};")
  , ("unannotated empty function", "let bad = {};")
  , ("non-exhaustive data match", boolData ++ "let bad: integer = match yea { yea | 1 };")
  , ("multi-match missing one product case", boolData ++ "let bad: integer = match yea, nay { yea, yea | 1, yea, nay | 2, nay, yea | 3 };")
  , ("nested match misses constructor", boolData ++ optionData ++ "let bad: integer = match yea some { yea some | 1, none | 0 };")
  , ("constructor pattern arity", optionData ++ "let bad = match 1 some { some | 1, none | 0 };")
  , ("closed record rejects extra field", "let bad: [name: string] = [name = 'n', age = 1];")
  , ("missing record field", "let person = [name = 'n']; let bad = person@age;")
  , ("unknown record removal", "let person = [name = 'n']; let bad = [= person, - age];")
  , ("record update needs a record", "let bad = [= 1, value = 2];")
  , ("missing function graith", equalPrelude ++ "let (x: a, y: a) same: 𝟚 = x ≡ y;")
  , ("insufficient graith", equalPrelude ++ "graith a equal shape a order { a ≤ a: 𝟚 }; graith a equal let (x: a, y: a) leq: 𝟚 = x ≤ y;")
  , ("unknown fill shape", "fill integer missing { let x missing = x };")
  , ("fill evidence cannot be shown", "show fill integer missing {};")
  , ("fill misses required member", "shape a identity { a identity: a }; fill integer identity {};")
  , ("fill rejects unknown member", "shape a identity { a identity: a }; fill integer identity { let x other = x };")
  , ("fill member has declared type", "shape a identity { a identity: a }; fill integer identity { let x identity = 'wrong' };")
  , ("duplicate fill is incoherent", "shape a identity { a identity: a }; fill integer identity { let x identity = x }; fill integer identity { let x identity = x };")
  , ("overlapping fills are ambiguous when used", "shape a identity { a identity: a }; fill a identity { let x identity = x }; fill integer identity { let x identity = x }; let bad = 1 identity;")
  , ("shape law sides share a value type", "shape a bad { law (x: a): x ~ 1 };")
  , ("shape law sides share an effect row", unitData ++ "deed pulse { 𝟙 pulse: 𝟙 }; shape a bad { law (x: 𝟙): x ~ null pulse };")
  , ("shape law rejects an unbound value", "shape a bad { law (x: a): x ~ missing };")
  , ("shape law needs must be declared", equalPrelude ++ "shape a bad { law (x: a): x ≡ x ~ x ≡ x };")
  , ("fill arity follows shape", "shape a b convert { a convert: b }; fill integer convert { let x convert = x };")
  , ("effect operation must be a function", "deed bad { tick: integer };")
  , ("state parameter mismatch", unitData ++ "deed a state { 𝟙 get: a, a set: 𝟙 }; let got: 𝟙 → integer ! string state = get;")
  , ("one operation clause cannot erase sibling operations", duoEffect ++ "let run: 𝟙 → 𝟙 = { _ | try null second { first | null } };")
  , ("partial coverage does not combine across handlers", duoEffect ++ "let run: 𝟙 → 𝟙 = { _ | try (try null second { first | null }) { second | null } };")
  , ("handler for absent effect", "let bad: integer = try 1 { fail | 2 };")
  , ("ordinary function is not an operation", "deed ask { integer ask: integer }; let f: integer → integer ! ask = { x | x ask }; let bad = try 1 f { x f | x };")
  , ("resume argument has operation result type", "deed ask { integer ask: integer }; let bad: integer = try 10 ask { x ask | 'bad' resume };")
  , ("resume is scoped to operation clauses", "let bad = 1 resume;")
  , ("effect handler cannot bind operation arguments", duoEffect ++ "let bad: 𝟙 = try null first { x duo | null };")
  , ("effectful let result stays monomorphic", unitData ++ "deed a state { 𝟙 get: a }; let run = { _ | (let value = null get; let number: integer = value; let text: string = value; null) };")
  , ("bookhoard let cannot run immediate effects", "let answer = 'hello' write;")
  ]

importCases :: [Test]
importCases =
  [ typeOkWith "shown value and type cross a bring" "bring file.tung; let value: box = box;" shown
  , typeErrWith "unshown value stays hidden" "bring file.tung; let bad = hidden;" shown
  , typeErrWith "missing bring is rejected" "bring missing.tung;" Map.empty
  , typeErrWith "bad brought source is rejected" "bring bad.tung;" (Map.singleton "bad.tung" "let =")
  , typeErrWith "self bring cycle is rejected" "bring self.tung;" (Map.singleton "self.tung" "bring self.tung;")
  , typeErrWith "mutual bring cycle is rejected" "bring left.tung;" cyclic
  , runnableErr "module without main is not runnable" "let answer = 42;"
  , runnableOk "inferred pure main is runnable" (unitData ++ "let main = { _ | null };")
  , runnableOk "partial effectful call is pure at boundary" (unitData ++ "let half = 1 ÷; let main: 𝟙 → 𝟙 = { _ | null };")
  , runnableOk "runner accepts unit and runner effects" runnableOkSource
  , runnableErr "main must be a function" (unitData ++ "let main = null;")
  , runnableErr "main takes one unit argument" (unitData ++ "let main: integer → 𝟙 = { _ | null };")
  , runnableErr "main returns unit" (unitData ++ "let main: 𝟙 → integer = { _ | 42 };")
  , runnableErr "fail is not a main effect" (unitData ++ "let main: 𝟙 → 𝟙 ! string fail = { _ | (let _ = 1 ÷ 0; null) };")
  , runnableErr "custom effect is not a main effect" (unitData ++ "deed ask { 𝟙 ask: 𝟙 }; let main: 𝟙 → 𝟙 ! ask = { _ | null ask };")
  , runnableErr "parameterised runner-looking effect is rejected" (unitData ++ "deed a console { 𝟙 fake: 𝟙 }; let main: 𝟙 → 𝟙 ! 𝟙 console = { _ | null fake };")
  , runnableErr "effects cannot run before main" (unitData ++ "let _ = 'early' write; let main: 𝟙 → 𝟙 = { _ | null };")
  , expectEq "reports an inferred source type" (Right "m1 → m1") (typeOfWithImports "show let identity = { x | x };" Map.empty "identity")
  , expectEq "reports an unknown inspected name" (Left "unknown name 'missing'") (typeOfWithImports "let answer = 42;" Map.empty "missing")
  , expect "elaboration resolves every type-class evidence hole" evidenceIsResolved
  , typeOkWith "shared diamond imports" "bring left.tung; bring right.tung; left@left + right@right" sharedImports
  ]
 where
  shown = Map.fromList [("file.tung", "show kin box { box }; let hidden = 1;")]
  cyclic = Map.fromList [("left.tung", "bring right.tung;"), ("right.tung", "bring left.tung;")]
  sharedImports =
    Map.fromList
      [ ("left.tung", "bring shared.tung; show let left = shared@base + 1;")
      , ("right.tung", "bring shared.tung; show let right = shared@base + 2;")
      , ("shared.tung", "show let base = 1;")
      ]
  evidenceIsResolved = case parse (equalPrelude ++ "fill integer equal { let x ≡ y = yea }; let answer = 1 ≡ 1;") >>= (\program -> elaborateProgramWithImports program Map.empty False) of
    Left _ -> False
    Right program -> "EvidenceFill" `isInfixOf` show program && not ("EvidenceHole" `isInfixOf` show program)

unitData, boolData, optionData, boxData, equalPrelude, classHierarchy, inheritedMethodHierarchy, duoEffect, runnableOkSource :: String
unitData = "kin 𝟙 { null }; "
boolData = "kin 𝟚 { yea, nay }; "
optionData = "kin a option { none, a some }; "
boxData = "kin a box { a box }; "
equalPrelude = boolData ++ "shape a equal { a ≡ a: 𝟚 }; "
classHierarchy = "shape a parent { a parent: a }; graith a parent shape a child { a child: a }; "
inheritedMethodHierarchy =
  "shape a magma { a combine a: a }; "
    ++ "graith a magma shape a semigroup {}; "
    ++ "kin a option { none, a some }; "
    ++ "graith a semigroup fill (a option) semigroup { "
    ++ "let combine = { none, b | b, a, none | a, a some, b some | a combine b $ some } "
    ++ "};"
duoEffect = unitData ++ "deed duo { 𝟙 first: 𝟙, 𝟙 second: 𝟙 }; "
runnableOkSource = unitData ++ "let main: 𝟙 → 𝟙 ! console = { _ | 'ok' write };"
