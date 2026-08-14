module Test.Type (group) where

import Data.Map.Strict qualified as Map
import Jbini (typeOfWithImports)
import Test.Harness (Group, Test, expectEq, runnableErr, runnableOk, typeErr, typeErrWith, typeOk, typeOkWith)
import Test.Harness qualified as Harness

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
  , ("constructor application is curried", "choose a option { none, a some }; let make = some; let value: integer option = 1 make;")
  , ("empty type eliminator", "choose 𝟘 {}; let initial: 𝟘 → a = {}; let use: 𝟘 → integer = initial;")
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
  , ("existing parent fill satisfies child", classHierarchy ++ "fill integer parent { let x parent = x }; fill integer child { let x child = x };")
  , ("child fill may supply parent member", classHierarchy ++ "fill integer child { let x parent = x; let x child = x }; let ok: integer = 1 parent;")
  , ("parameterized state effect", unitData ++ "deed a state { 𝟙 get: a, a set: 𝟙 }; let got: 𝟙 → integer ! integer state = get;")
  , ("operation handler resumes", "deed ask { integer ask: integer }; let ok: integer = try 10 ask { x ask | x resume };")
  , ("handler changes answer through return", "deed ask { integer ask: integer }; let ok: string = try 10 ask { return n | n to-string, x ask | x resume };")
  , ("all operation clauses remove an effect", duoEffect ++ "let run: 𝟙 → 𝟙 = { _ | try null first { first | null, second | null } };")
  , ("effect clause removes every operation", duoEffect ++ "let run: 𝟙 → 𝟙 = { _ | try null first { duo | null } };")
  , ("handler clause effects escape handler", duoEffect ++ "let run: 𝟙 → 𝟙 ! duo = { _ | try null first { first | null second, second | null } };")
  , ("operation keeps declared extra effect", unitData ++ "deed other { 𝟙 other: 𝟙 }; deed mine { 𝟙 op: 𝟙 ! other }; let run: 𝟙 → 𝟙 ! other = { _ | try null op { op | null } };")
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
  , ("library let cannot run immediate effects", "let answer = 'hello' write;")
  ]

importCases :: [Test]
importCases =
  [ typeOkWith "shown value and type cross a bring" "bring file.tung; let value: box = box;" shown
  , typeErrWith "unshown value stays hidden" "bring file.tung; let bad = hidden;" shown
  , typeErrWith "missing bring is rejected" "bring missing.tung;" Map.empty
  , typeErrWith "bad brought source is rejected" "bring bad.tung;" (Map.singleton "bad.tung" "let =")
  , typeErrWith "self bring cycle is rejected" "bring self.tung;" (Map.singleton "self.tung" "bring self.tung;")
  , typeErrWith "mutual bring cycle is rejected" "bring left.tung;" cyclic
  , runnableOk "partial effectful call is pure at boundary" "let half = 1 ÷;"
  , runnableOk "runner accepts unit and runner effects" runnableOkSource
  , runnableErr "runnable value must be unit" "42"
  , runnableErr "fail is not a runner effect" "let _ = 1 ÷ 0;"
  , runnableErr "custom effect is not a runner effect" (unitData ++ "deed ask { 𝟙 ask: 𝟙 }; let _ = null ask;")
  , runnableErr "parameterized runner-looking effect is rejected" (unitData ++ "deed a console { 𝟙 fake: 𝟙 }; let _ = null fake;")
  , expectEq "reports an inferred source type" (Right "m1 → m1") (typeOfWithImports "show let identity = { x | x };" Map.empty "identity")
  , expectEq "reports an unknown inspected name" (Left "unknown name 'missing'") (typeOfWithImports "let answer = 42;" Map.empty "missing")
  ]
 where
  shown = Map.fromList [("file.tung", "show choose box { box }; let hidden = 1;")]
  cyclic = Map.fromList [("left.tung", "bring right.tung;"), ("right.tung", "bring left.tung;")]

unitData, boolData, optionData, boxData, equalPrelude, classHierarchy, duoEffect, runnableOkSource :: String
unitData = "choose 𝟙 { null }; "
boolData = "choose 𝟚 { yea, nay }; "
optionData = "choose a option { none, a some }; "
boxData = "choose a box { a box }; "
equalPrelude = boolData ++ "shape a equal { a ≡ a: 𝟚 }; "
classHierarchy = "shape a parent { a parent: a }; graith a parent shape a child { a child: a }; "
duoEffect = unitData ++ "deed duo { 𝟙 first: 𝟙, 𝟙 second: 𝟙 }; "
runnableOkSource = unitData ++ "let _ = 'ok' write;"
