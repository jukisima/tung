-- static semantics: positive inference and adversarial rejection cases live here.
module Test.Type (group) where

import Control.Monad (replicateM)
import Data.List (intercalate, isInfixOf, isPrefixOf, permutations, subsequences)
import Data.Map.Strict qualified as Map
import Test.Harness (Group, Test, expect, expectEq, runnableErr, runnableOk, typeErr, typeErrContaining, typeErrWith, typeOk, typeOkWith)
import Test.Harness qualified as Harness
import Test.QuickCheck qualified as QuickCheck
import Tung (CoreProgram, check, checkRunnableWithImports, checkWithImports, elaborateProgramWithImports, parse, readLibraryImports, typeOfWithImports)

group :: IO Group
group = do
  imports <- readLibraryImports "." >>= either fail pure
  Harness.group "type" $
    map (uncurry typeOk) accepted
      ++ map (uncurry typeErr) rejected
      ++ importCases
      ++ tableAndSetCases imports
      ++ orderCases imports
      ++ boundCases imports
      ++ numericHierarchyCases imports
      ++ kindCases
      ++ annotationScopeCases
      ++ redundantInstanceConstraintCases
      ++ generatedCoverageCases
      ++ redundantCoverageCases
      ++ generatedEffectCases
      ++ inferenceProperties
      ++ evidenceProperties
      ++ coverageProperties

kindCases :: [Test]
kindCases =
  [ typeOk "requirement inferreþ a higher kind" "flock functor [f:[*, *]] {} flock applicative [byzen f functor] {}",
    typeOk "requirements reuse an owner parameter" "flock equal [a:*] {} flock owner [a:*] { let id [byzen a equal, a, a] }",
    typeOk "explicit requirement parameter may shadow a visible type" "ilk a [*] {} flock same [x:*] {} let keep [byzen a same, @a:*, x:a, a] x",
    typeOk "compound requirement retaineth an undetermined kind annotation" "flock same [x:*] {} flock higher [byzen (a f) same, a:[*, *]] { let keep [a f, a f] }",
    typeOk "requirement parameter order supporteþ explicit type input" "flock same [a:*] {} bizen:ℤ same {} let keep [byzen a same, @b:*, x:a, _:b, a] x let result [ℤ] 1 keep @ℤ @text 'x'",
    typeOk "compound requirement followeþ source parameter order" "ilk pair [a:*, b:*, *] { pair [a, b, a pair b] } flock same [a:*] {} bizen:(ℤ pair text) same {} let keep [byzen (a f b) same, x:a f b, a f b] x let result [ℤ pair text] (1 pair 'x') keep @ℤ @pair @text",
    typeOk "forward class requirements resolve kinds" "flock applicative [byzen f functor] {} flock functor [f:[*, *]] {}",
    typeOk "parent member kinds resolve before child requirements" "flock child [byzen a parent f] {} flock parent [byzen (a f) same] { let value [ℤ f, ℤ f] } flock same [a:*] {}",
    typeErrContaining "parent member kinds make inherited annotations redundant" "redundant type parameter 'a'" "flock child [byzen a parent f, a:*] {} flock parent [byzen (a f) same] { let value [ℤ f, ℤ f] } flock same [a:*] {}",
    typeOk "expected GADT index floweþ through a local block" "ilk tag [a:*, *] { integer [ℤ, ℤ tag], word [text, text tag] } let get [@a:*, x:a tag, a] (let y [] x yield match y { integer n ^ n, word s ^ s })",
    typeOk "expected GADT index floweþ through a record field" "ilk tag [a:*, *] { integer [ℤ, ℤ tag], word [text, text tag] } let get [@a:*, x:a tag, r{value:a}] r{value = match x { integer n ^ n, word s ^ s }}",
    typeErr "an annotated strict initializer cannot refer to itself" "let value [ℤ] value",
    typeOk "parameterised effect aliases are transparent in rows" "deed a state { get [ℤ, a] } let row [a:*, *] a state let run [x:ℤ, text; text row] x get",
    typeErr "ordinary types cannot be effect labels" "let identity [x:ℤ, ℤ; ℤ] x",
    typeErr "data constructors cannot be effect labels" "ilk marker [*] { marker } let identity [x:ℤ, ℤ; marker] x",
    typeErr "explicit row input must denote an effect" "let call [@e:*, x:ℤ, f:[ℤ, ℤ; e], ℤ; e] x f let identity [x:ℤ, ℤ] x let bad [ℤ] 1 call @text identity",
    typeOkWith "imported class supplyeþ parameter kinds" "use classes.tung c flock applicative [byzen f c~functor] {}" (Map.singleton "classes.tung" "show flock functor [f:[*, *]] {}"),
    typeErr "ordinary type parameter cannot be a constructor" "let bad [@a:*, @f:*, a f, a f] {x ^ x}",
    typeErr "requirements reject conflicting kinds" "flock same [a:*] {} flock functor [f:[*, *]] {} flock bad [byzen a same, byzen a functor] {}",
    typeErrContaining "explicit flock kind is redundant" "redundant type parameter 'f'" "flock functor [f:[*, *]] {} flock bad [byzen f functor, f:[*, *]] {}",
    typeErrContaining "forward inherited kind declaration is redundant" "redundant type parameter 'f'" "flock bad [byzen f derived, f:[*, *]] {} flock derived [byzen f functor] {} flock functor [f:[*, *]] {}",
    typeErrContaining "explicit kind after requirement is redundant" "redundant type parameter 'f'" "flock functor [f:[*, *]] {} let bad [byzen f functor, @f:[*, *], @a:*, x:a f, a f] x",
    typeErrContaining "instance kind declaration is redundant" "redundant type parameter 'a'" "flock same [a:*] {} bizen [byzen a same, @a:*]:a same {}"
  ]
    ++ explicitKindCases
    ++ map
      (uncurry typeErr)
      [ ("higher-kinded parameter cannot be a value type", "ilk wrong [f:[*, *], *] { make [f, f wrong] }"),
        ("ordinary ilk parameter cannot be applied", "ilk box [a:*, *] { make [a a, a box] }"),
        ("primitive type cannot bizen a higher kind parameter", "ilk free [f:[*, *], a:*, *] { bad [ℤ free a] }"),
        ("flock member cannot use a constructor parameter as a value type", "flock bad [f:[*, *]] { let wrong [f, f] }"),
        ("flock member cannot apply an ordinary type parameter", "flock bad [a:*] { let wrong [ℤ a, ℤ] }")
      ]

-- unused binders still have kinds. exercise the same rule through binding,
-- ascription, higher-rank input, constructors, and imported schemes.
explicitKindCases :: [Test]
explicitKindCases =
  concat
    [ [ test (label ++ " accepts its declared kind") (source valid),
        reject (label ++ " rejects a different kind") (source invalid)
      ]
    | (label, test, reject, source, valid, invalid) <-
        [ ("unused ordinary binder", typeOk, typeErr, \ty -> "let keep [@a:*, x:ℤ, ℤ] x let value [ℤ] 1 keep @" ++ ty, "text", "→"),
          ("unused constructor binder", typeOk, typeErr, \ty -> "let keep [@f:[*, *, *], x:ℤ, ℤ] x let value [ℤ] 1 keep @" ++ ty, "→", "ℤ"),
          ("ascribed binder", typeOk, typeErr, \ty -> "let keep [] ({x ^ x}:[@f:[*, *, *], ℤ, ℤ]) let value [ℤ] 1 keep @" ++ ty, "→", "ℤ"),
          ("higher-rank binder", typeOk, typeErr, \ty -> "let call [f:[@g:[*, *, *], ℤ, ℤ], ℤ] 1 f @" ++ ty, "→", "ℤ"),
          ("constructor binder", typeOk, typeErr, \ty -> "ilk box [f:[*, *, *], *] { box [ℤ, f box] } let value [] 1 box @" ++ ty, "→", "ℤ"),
          ("imported binder", \label source -> typeOkWith label source imports, \label source -> typeErrWith label source imports, \ty -> "use keep.tung let value [ℤ] 1 keep @" ++ ty, "→", "ℤ")
        ]
    ]
  where
    imports = Map.singleton "keep.tung" "show let keep [@f:[*, *, *], x:ℤ, ℤ] x"

annotationScopeCases :: [Test]
annotationScopeCases =
  [ typeErrContaining label ("undeclared type '" ++ name ++ "'") source
  | (label, name, source) <-
      [ ("annotation requireþ an explicit parameter", "a", "let id [x:a, a] x"),
        ("nested annotation cannot introduce a free variable", "b", "let apply [@a:*, f:[a, b], x:a, a] x"),
        ("ascription requireþ a scoped type", "a", "let x id [] (x:a)"),
        ("effect row requireþ a declared parameter", "e", "let id [x:ℤ, ℤ; e] x"),
        ("type constructor variable requireþ a declaration", "f", "let id [x:ℤ f, ℤ f] x"),
        ("constructor existential requireþ a declaration", "a", "ilk packed [*] { pack [a, packed] }"),
        ("instance head requireþ declared variables", "a", "ilk box [a:*, *] { box [a, a box] } flock same [a:*] {} bizen:(a box) same {}"),
        ("law requireþ declared variables", "b", "flock same [a:*] { law [x:b] x = x }"),
        ("instance member annotation cannot declare a free variable", "b", "flock same [a:*] { let same [a, a] } bizen:ℤ same { let same [x:b, b] x }")
      ]
  ]

accepted :: [(String, String)]
accepted =
  [ ("literal annotation", "let answer [ℤ] 42"),
    ("bracketed function value checks its own binder", "let identity [@a:*, a, a] { x ^ x } let answer [ℤ] 1 identity"),
    ("constructor-first header pattern binds an argument", "ilk box [a:*, *] { a box } let unbox [@a:*, box x: a box, a] x let answer [ℤ] (1 box) unbox"),
    ("second-position header pattern binds an argument", "ilk box [a:*, *] { a box } let unbox [@a:*, x box: a box, a] x let answer [ℤ] (1 box) unbox"),
    ("second-position header pattern binds several fields", "ilk pair [*] { ℤ pair text } let first [number pair word: pair, ℤ] number let answer [ℤ] (1 pair 'x') first"),
    ("constructor-first law pattern binds an argument", "ilk box [a:*, *] { a box } flock identity [a:*] { law [box x: a box] x = x }"),
    ("second-position law patterns bind distinct arguments", "ilk box [a:*, *] { a box } flock identity [a:*] { law [x box:a box, y box:a box] x = y }"),
    ("bare argument after named prefix is supplied by þe body", "let add-after [x: ℤ, ℤ, ℤ] { y ^ x + y } let answer [ℤ] 1 add-after 2"),
    ("untyped positional argument infers result", "let x identity [] x let answer [ℤ] 1 identity"),
    ("positional argument joins bracketed argument", "let x add-after [y: ℤ, ℤ] x + y let answer [ℤ] 1 add-after 2"),
    ("positional argument joins bracketed result", "let x identity [ℤ] x let answer [ℤ] 1 identity"),
    ("term name may share a type name", "let count [*] ℤ let x count [] x let answer [ℤ] 1 count let value [count] 2"),
    ("local alias constraineþ its captured parameter", "let x add-one [] (let y [] x let number [ℤ] y yield number + 1) let result [ℤ] 2 add-one"),
    ("independent local function remaineþ polymorphic", "let x keep [] (let y identity [] y let number [ℤ] 1 identity let word [text] 'x' identity yield x)"),
    ("constructor variables remain linked through composition", "ilk box [a:*, *] { a box } let lift [@a:*, @f:[*, *], x: a f, a f] x let x twice [] (x lift) lift let good [ℤ box] (1 box) twice"),
    ("annotated global function may call itself", "let factorial [ℤ, ℤ] { 0 ^ 1, n ^ n × ((n - 1) factorial) } let answer [ℤ] 5 factorial"),
    ("annotated local initializer seeþ its preceding binding", "let answer [text] (let value [] 1 let value [text] match value { 0 ^ 'zero', _ ^ 'other' } yield value)"),
    ("arbitrary term type ascription", "let answer [] (1 + 2: ℤ)"),
    ("polymorphic term type ascription is reusable", "let identity [] ({ x ^ x }: [@a:*, a, a]) let number [ℤ] 1 identity let word [text] 'x' identity"),
    ("ascribed anonymous function remaineþ recursive", "let factorial [] ({ 0 ^ 1, n ^ n × ((n - 1) factorial) }: [ℤ, ℤ]) let answer [ℤ] 5 factorial"),
    ("type ascription preserveþ a constraint", equalPrelude ++ "let same [byzen a equal, x: a, y: a, 𝟚] (x ≡ y: 𝟚)"),
    ("type ascription preserveþ an immediate effect", unitData ++ "deed pulse { pulse [𝟙, ℤ] } let run [_: 𝟙, ℤ; pulse] (only pulse: ℤ)"),
    ("unicode primitive", "let letter [unicode] `a"),
    ("text primitive", "let word [text] 'λ字'"),
    ("let polymorphism is reusable", "let x id [] x let number [ℤ] 1 id let word [text] 'x' id"),
    ("annotated polymorphic identity", "let id [@a:*, x: a, a] x let number [ℤ] 1 id let word [text] 'x' id"),
    ("declared type parameters remain inferred at calls", "let constant [@a:*, @b:*, x:a, _:b, a] x let number [ℤ] 1 constant 'x' let word [text] 'x' constant 1"),
    ("explicit type arguments select declaration order around value arguments", "ilk list [a:*, *] { empty [a list] } let f [@a:*, @b:*, x:a, _:b, a] x let inferred [text] 'a' f empty let partial [text] 'a' f @text empty let explicit [text] 'a' f @text @(text list) empty"),
    ("declared type parameters are in scope in þe body", "let id [@a:*, x:a, a] x let forward [@a:*, x:a, a] x id @a let answer [ℤ] 1 forward @ℤ"),
    ("outer parameter scopes a local annotation", "let id [@a:*, x:a, a] (let y [a] x yield y)"),
    ("explicit higher-kinded parameters remain usable", "ilk box [a:*, *] { box [a, a box] } let id [@f:[*, *], @a:*, x:a f, a f] x let answer [ℤ box] (1 box) id @box @ℤ"),
    ("law may declare a local parameter", "flock same [a:*] { law [@b:*, x:b] x = x }"),
    ("method quantifiers cannot capture instance parameters", "ilk product [a:*, b:*, *] { pair [a, b, a product b] } flock mapper [f:[*, *]] { let map [@a:*, @b:*, a f, [a, b], b f] } bizen [@a:*]:(a product) mapper { let (x pair y) map f [] x pair (y f) } let good [ℤ product text] (1 pair 2) map to-text"),
    ("instance parameter scopes explicit type inputs", "ilk box [a:*, *] { box [a, a box] } let id [@a:*, x:a, a] x flock copy [a:*] { let copy [a, a] } bizen [@a:*]:(a box) copy { let (box x) copy [] (x id @a) box } let answer [ℤ box] (1 box) copy"),
    ("instance method annotation scopes its own parameters", "let id [@a:*, x:a, a] x flock owner [a:*] { let copy [@b:*, a, b, b] } bizen:ℤ owner { let copy [@c:*, _:ℤ, x:c, c] x id @c } let answer [text] 1 copy 'x'"),
    ("polymorphic argument accepteth explicit type input", "let call [f:[@a:*, a, a], ℤ] 1 f @ℤ let x id [] x let answer [ℤ] id call"),
    ("explicit type application may precede value application", "let keep [@a:*, x:a, a] x let copy [] (keep @ℤ) let answer [ℤ] 1 copy"),
    ("a quantified ascription retaineth explicit input after let binding", "let id [] ({x ^ x}:[@a:*, a, a]) let answer [ℤ] 1 id @ℤ"),
    ("constructor fields preserve polymorphism through a match", "ilk holder [*] { hold [[@a:*, a, a], holder] } let unpack [h:holder, [@a:*, a, a]] match h { hold f ^ f } let id [@a:*, x:a, a] x let answer [ℤ] 1 ((id hold) unpack) @ℤ"),
    ("higher-rank argument instantiateþ at distinct types", polymorphicPrelude ++ "let x identity [] x let good [] identity both let lambda [] {x ^ x} both"),
    ("rank-three annotation floweþ into a lambda argument", "let run [k:[[@a:*, a, a], ℤ], ℤ] {x ^ x} k let good [ℤ] {f ^ 1 f} run"),
    ("polymorphic result is checked and reused", polymorphicPrelude ++ "let make [_:ℤ, [@a:*, a, a]] {x ^ x} let good [] (1 make) both"),
    ("explicit polymorphic ascription retaineth its quantifier", polymorphicPrelude ++ "let good [] ({x ^ x}:[@t1:*, t1, t1]) both"),
    ("quantified alias accepteth higher-kinded parameters", "ilk box [a:*, *] { box [a, a box] } let ⇒ [f:[*, *], g:[*, *], *] [@a:*, a f, a g] let lift [box ⇒ box] {x ^ x}"),
    ("nested quantifiers shadow outer type variables", "let first [@a:*, x:a, [@a:*, a, a]] {y ^ y} let good [ℤ] 1 (('x' first):[@b:*, b, b])"),
    ("subscripted type variable", "let id [@a₀:*, x: a₀, a₀] x let number [ℤ] 1 id let word [text] 'x' id"),
    ("higher-order effect polymorphism", "let call [@a:*, @b:*, @e:*, x: a, f: [a, b; e], b; e] x f"),
    ("two effect rows compose", "let compose [@a:*, @b:*, @e0:*, @c:*, @e1:*, f: [a, b; e0], g: [b, c; e1], x: a, c; e0, e1] (x f) g"),
    ("subscripted effect rows compose", "let compose [@a:*, @b:*, @e₀:*, @c:*, @e₁:*, f: [a, b; e₀], g: [b, c; e₁], x: a, c; e₀, e₁] (x f) g"),
    ("effect-only parameter generaliseþ at a let", unitData ++ "deed a phantom { tick [𝟙, 𝟙] } let operation [] tick let first [𝟙, 𝟙; ℤ phantom] operation let second [𝟙, 𝟙; text phantom] operation"),
    ("pure partial application hideþ latent effect", "let half [] 1 %"),
    ("type alias is transparent", "let count [*] ℤ let answer [count] 42"),
    ("type alias accepteth higher-kinded parameters", "ilk box [a:*, *] { box [a, a box] } let applied [f:[*, *], a:*, *] a f let value [box applied ℤ] 1 box"),
    ("forward type-alias chains resolve by dependency", "let first [*] second let second [*] token ilk token [*] { token } let good [first] token"),
    ("type-alias parameters shadow nominal types", "ilk a [*] { a } let wrapper [a:*, *] a let good [ℤ wrapper] 1"),
    ("data parameters shadow type aliases", "let a [*] ℤ ilk box [a:*, *] { a box } let good [text box] 'x' box"),
    ("effect parameters shadow type aliases", "ilk 𝟙 [*] { only } let a [*] ℤ deed a state { get [𝟙, a] } let good [𝟙, text; text state] get"),
    ("flock parameters shadow type aliases", "let a [*] ℤ flock identity [a:*] { let identity [a, a] } bizen:text identity { let x identity [] x } let good [text] 'x' identity"),
    ("effect operation type may be a transparent function alias", "let binary [*] [ℤ, ℤ, ℤ] deed addition { add [binary] }"),
    ("declared primitive flock dependency stayeþ ABI-compatible", boolData ++ "flock equal [a:*] { let ≡ [a, a, 𝟚] } let result [] 1 ≡ 1"),
    ("parameterised type alias is transparent", boolData ++ "let predicate [a:*, *] [a, 𝟚] let member [ℤ predicate] { _ ^ yea } let answer [𝟚] 1 member"),
    ("arrow equaleþ a pure unary function type", "let id [x: ℤ, ℤ] x let same [ℤ → ℤ] id"),
    ("arrow type associateþ to þe right", "let choose [ℤ → text → ℤ] { x, y ^ x }"),
    ("a pure arrow may return an effectful function", "deed ask { ask [ℤ, ℤ] } let relay [@a:*, @f:[*, *], value: a f, a f] value let maker [] { x ^ { y ^ (x + y) ask } } let result [ℤ, [ℤ, ℤ; ask]] maker relay"),
    ("curried triple function", "let pick [x: ℤ, _: text, _: float, ℤ] x"),
    ("constructor application is curried", "ilk option [a:*, *] { none, a some } let make [] some let value [ℤ option] 1 make"),
    ("kinded data constructor hath an explicit result", "ilk box [a:*, *] { box [a, a box] } let value [ℤ box] 1 box"),
    ("nullary constructor hath an explicit result", "ilk ℕ [*] { zero [ℕ] suc [ℕ, ℕ] } let value [ℕ] zero suc"),
    ("indexed constructor refines a scrutinee in each branch", "ilk tag [a:*, *] { integer [ℤ tag] textual [text tag] } let inspect [@a:*, x:a tag, ℤ] match x { integer ^ 1, textual ^ 2 }"),
    ("indexed payload returneþ the refined result type", "ilk tag [a:*, *] { integer [ℤ, ℤ tag] textual [text, text tag] } let unpack [@a:*, x:a tag, a] match x { integer n ^ n, textual s ^ s }"),
    ("concrete GADT index excludeþ other constructors from coverage", "ilk tag [a:*, *] { integer [ℤ tag] textual [text tag] } let inspect [x:ℤ tag, ℤ] match x { integer ^ 1 }"),
    ("higher-kinded GADT index refineþ a type constructor", "ilk list [a:*, *] { empty } ilk option [a:*, *] { none } ilk tag [f:[*, *], *] { listed [list tag] optional [option tag] } let choose [@f:[*, *], @a:*, x:f tag, a f] match x { listed ^ empty, optional ^ none }"),
    ("existential payload may be inspected without escaping", "ilk packed [*] { pack [@a:*, a, packed] } let discard [x:packed, ℤ] match x { pack value ^ 1 }"),
    ("empty type eliminator", "ilk 𝟘 [*] {} let initial [@a:*, 𝟘, a] {} let eliminate [𝟘, ℤ] initial"),
    ("empty explicit match preserveþ its scrutinee effect", "ilk 𝟘 [*] {} deed source { obtain [ℤ, 𝟘] } let eliminate [@a:*, x: ℤ, a; source] match (x obtain) {}"),
    ("multi-scrutinee exhaustive match", boolData ++ "let ok [ℤ] match yea, nay { yea, yea ^ 1, yea, nay ^ 2, nay, yea ^ 3, nay, nay ^ 4 }"),
    ("alternative rows contribute to coverage", boolData ++ "let ok [ℤ] match yea, nay { yea, _ | _, yea ^ 1, nay, nay ^ 0 }"),
    ("overlapping wildcards cover a product", boolData ++ "let ok [ℤ] match yea, nay { yea, _ ^ 1, _, yea ^ 2, nay, nay ^ 3 }"),
    ("nested exhaustive match", boolData ++ optionData ++ "let ok [ℤ] match yea some { yea some ^ 1, nay some ^ 2, none ^ 0 }"),
    ("variable pattern closeþ coverage", boolData ++ "let ok [ℤ] match yea { yea ^ 1, other ^ 0 }"),
    ("ℤ match with fallback", "let ok [ℤ] match 1 { 0 ^ 0, 1 ^ 1, _ ^ 2 }"),
    ("ℤ patterns in a function", "let classify [ℤ, ℤ] { 0 ^ 10, -1 ^ 20, _ ^ 30 }"),
    ("text match with fallback", "let ok [ℤ] match 'yes' { 'yes' ^ 1, _ ^ 0 }"),
    ("text patterns in a function", "let classify [text, ℤ] { 'yes' ^ 1, _ ^ 0 }"),
    ("closed record", "let person [r{age: ℤ, name: text}] r{name = 'n', age = 1}"),
    ("record access", "let person [] r{name = 'n', age = 1} let age [ℤ] person.age"),
    ("chained record access", "let person [] r{address = r{city = 'kyoto'}} let city [text] person.address.city"),
    ("record update changeþ a field type", "let person [] r{value = 1} let changed [r{value: text}] r{= person, value = 'one'}"),
    ("record removal changeþ the row", "let person [] r{name = 'n', age = 1} let public [r{name: text}] r{= person, - age}"),
    ("flock constraint dischargeþ member need", equalPrelude ++ "let same [byzen a equal, x: a, y: a, 𝟚] x ≡ y"),
    ("bizen context dischargeþ inner need", equalPrelude ++ boxData ++ "bizen [byzen a equal]:(a box) equal { let (x box) ≡ (y box) [] x ≡ y }"),
    ("flock law useþ its own methods", "flock identity [a:*] { let identity [a, a] law [x: a] x identity = x } bizen:ℤ identity { let x identity [] x }"),
    ("well-typed unequal flock law remaineþ checked documentation", "flock claimed [a:*] { law [x: ℤ] x = 0 }"),
    ("flock member carrieþ its own constraint", memberConstraint ++ "bizen:option functor { let value map f [] match value { none ^ none, x some ^ x f $ some } } bizen:option applicative { let pure [] some let apply [] { x some, f some ^ x f $ some, _, _ ^ none } } bizen:option traverse { let value traverse f [] match value { none ^ none pure, x some ^ (x f) map some } } let lifted [(ℤ option) option] (1 some) traverse some"),
    ("existing parent bizen satisfieþ child", classHierarchy ++ "bizen:ℤ parent { let x parent [] x } bizen:ℤ child { let x child [] x }"),
    ("specific bizen outrankeþ a blanket instance", "flock identity [a:*] { let identity [a, a] } bizen [@a:*]:a identity { let x identity [] x } bizen:ℤ identity { let x identity [] x } let ok [] 1 identity"),
    ("specific bizen winneþ when blanket with constraint cometh later", instancePriorityPrefix ++ specificIdentityInstance ++ blanketIdentityInstance ++ "let answer [ℤ] 1 identity"),
    ("specific bizen winneþ when blanket with constraint cometh earlier", instancePriorityPrefix ++ blanketIdentityInstance ++ specificIdentityInstance ++ "let answer [ℤ] 1 identity"),
    ("inferred need chooseþ specific bizen before later blanket", instancePriorityPrefix ++ specificIdentityInstance ++ blanketIdentityInstance ++ "let answer [] 1 identity"),
    ("inferred need chooseþ specific bizen after earlier blanket", instancePriorityPrefix ++ blanketIdentityInstance ++ specificIdentityInstance ++ "let answer [] 1 identity"),
    ("child bizen may supply parent member", classHierarchy ++ "bizen:ℤ child { let x parent [] x let x child [] x } let ok [ℤ] 1 parent"),
    ("parametric child bizen useþ its parent method", inheritedMethodHierarchy),
    ("bizen constraint may construct a parent dictionary", parentDictionaryConstraint),
    ("parameterised state effect", unitData ++ "deed a state { get [𝟙, a], set [a, 𝟙] } let got [𝟙, ℤ; ℤ state] get"),
    ("eftgin continueþ an operation handler", "deed ask { ask [ℤ, ℤ] } let ok [ℤ] try 10 ask { x ask ^ x eftgin }"),
    ("handler changeþ answer through yield", "deed ask { ask [ℤ, ℤ] } let ok [text] try 10 ask { yield n ^ n to-text, x ask ^ x eftgin }"),
    ("all operation clauses remove an effect", duoEffect ++ "let run [_: 𝟙, 𝟙] try only first { first ^ only, second ^ only }"),
    ("effect clause removeþ every operation", duoEffect ++ "let run [_: 𝟙, 𝟙] try only first { duo ^ only }"),
    ("handler clause effects escape handler", duoEffect ++ "let run [_: 𝟙, 𝟙; duo] try only first { first ^ only second, second ^ only }"),
    ("operation keepeþ declared extra effect", unitData ++ "deed other { other [𝟙, 𝟙] } deed mine { op [𝟙, 𝟙; other] } let run [_: 𝟙, 𝟙; other] try only op { op ^ only other }"),
    ("operation propagateþ a handler effect row", unitData ++ "ilk request [*] { request } ilk response [*] { response } deed e fail { fail [@a:*, e, a] } deed clock { tick [𝟙, 𝟙] } deed web { serve [@e:*, ℤ, [request, response; e], 𝟙; e, text fail] } let route [_: request, response; clock] (let _ [] only tick yield response) let run [_: 𝟙, 𝟙; web, clock, text fail] 8080 serve route"),
    ("fremmed key entereþ value scope under another name", "let plus [ℤ, ℤ, ℤ] 'add-integer' fremmed let answer [ℤ] 1 plus 2"),
    ("declaration-only file is runnable", "let answer [] 42")
  ]
  where
    instancePriorityPrefix = "flock label [a:*] { let label [a, a] } flock identity [a:*] { let identity [a, a] } "
    specificIdentityInstance = "bizen:ℤ identity { let x identity [] x } "
    blanketIdentityInstance = "bizen [byzen a label]:a identity { let x identity [] x label } "

rejected :: [(String, String)]
rejected =
  [ ("literal type mismatch", "let bad [ℤ] 'wrong'"),
    ("instance annotation must satisfy its interface", "flock copy [a:*] { let copy [a, a] } bizen:ℤ copy { let copy [x:text, text] x }"),
    ("instance annotation cannot claim false polymorphism", "flock copy [a:*] { let copy [a, a] } bizen:ℤ copy { let copy [@a:*, x:a, a] x + 1 }"),
    ("explicit type argument cannot contradict a value argument", "let id [@a:*, x:a, a] x let bad [] 1 id @text"),
    ("explicit type arguments cannot exceed declared parameters", "let id [@a:*, x:a, a] x let bad [] 1 id @ℤ @text"),
    ("explicit type input requireþ a declared parameter", "let x id [] x let bad [] 1 id @ℤ"),
    ("explicit type input cannot be impredicative", "let id [@a:*, x:a, a] x let bad [] id @[@b:*, b, b]"),
    ("monomorphic function cannot satisfy a polymorphic argument", polymorphicPrelude ++ "let numeric [x:ℤ, ℤ] x let bad [] numeric both"),
    ("captured inference variable cannot escape a quantified check", polymorphicPrelude ++ "let f bad [] f both"),
    ("a local declared quantifier cannot capture an outer variable", "let x bad [] (let value [@a:*, _:ℤ, a] x yield 1 value @ℤ)"),
    ("explicit quantifiers are rigid even wiþ a hole-like name", "let bad [[@t1:*, t1, t1]] {_ ^ 1}"),
    ("polymorphic result cannot capture an outer argument", "let make [@a:*, x:a, [@b:*, b, b]] {_ ^ x}"),
    ("polymorphic alias cannot merge a free and a bound variable", "let alias [a:*, *] [@b:*, b, a] let bad [@a:*, x:a, a alias] {y ^ y}"),
    ("inference doth not invent an impredicative instantiation", "let id [@a:*, x:a, a] x let bad [[@b:*, b, b], [@c:*, c, c]] id"),
    ("bare bracket argument doth not bind implicitly", "let bad [ℤ, ℤ] 1"),
    ("a later local function shadoweþ an earlier one in application", "let foo [x: text, text] x let foo [x: ℤ, ℤ] x + 1 let bad [text] 'x' foo"),
    ("captured local alias cannot become polymorphic", "let x bad [] (let y [] x let number [ℤ] y yield number + 1) yield 'oops' bad"),
    ("nested function cannot generalise a captured variable", "let x bad [] (let _ capture [] x let number [ℤ] 0 capture let word [text] 0 capture yield number)"),
    ("constructor unification cannot change a nominal head", "ilk box [a:*, *] { a box } ilk option [a:*, *] { a some } let lift [@a:*, @f:[*, *], x: a f, a f] x let x twice [] (x lift) lift let bad [ℤ option] (1 box) twice"),
    ("an effectful outer function cannot be an arrow", "deed ask { ask [ℤ, ℤ] } let relay [@a:*, @f:[*, *], value: a f, a f] value let maker [] { x ^ (let value [] x ask yield { y ^ value + y }) } let result [] maker relay"),
    ("old func type operator is unavailable", "let id [x: ℤ, ℤ] x let same [ℤ func ℤ] id"),
    ("constructor result must be its data type", "ilk box [a:*, *] { box [a, ℤ] }"),
    ("indexed constructor cannot inhabit a different index", "ilk tag [a:*, *] { integer [ℤ tag] textual [text tag] } let wrong [ℤ tag] textual"),
    ("GADT branch cannot return a wrong indexed type", "ilk tag [a:*, *] { integer [ℤ tag] textual [text tag] } let wrong [@a:*, x:a tag, a] match x { integer ^ 'no', textual ^ 'yes' }"),
    ("existential constructor field cannot escape its branch", "ilk packed [*] { pack [@a:*, a, packed] } let leak [@a:*, x:packed, a] match x { pack value ^ value }"),
    ("inferred existential constructor field cannot escape", "ilk packed [*] { pack [@a:*, a, packed] } let leak [] { pack value ^ value }"),
    ("higher-kinded existential head cannot escape", "ilk packed [*] { pack [@a:*, @g:[*, *], a g, packed] } let leak [] { pack value ^ value }"),
    ("quantification cannot hide an escaping constructor existential", "ilk packed [*] { pack [@f:[*, *], [@a:*, a f, a f], packed] } let leak [] { pack value ^ value }"),
    ("existential effect row cannot escape", "ilk packed [*] { pack [@e:*, [ℤ, ℤ; e], packed] } let leak [] { pack value ^ value }"),
    ("term type ascription rejecteþ a mismatch", "let bad [] ('wrong': ℤ)"),
    ("parameterised type alias needeþ its argument", "let predicate [a:*, *] [a, ℤ] let _ bad [predicate] 1"),
    ("parameterised type alias rejecteþ extra arguments", "let predicate [a:*, *] [a, ℤ] let _ bad [ℤ predicate ℤ] 1"),
    ("recursive type aliases are rejected", "let first [*] second let second [*] first"),
    ("local type declarations cannot share a name", "let token [*] ℤ ilk token [*] { token }"),
    ("occurs check rejecteþ self application", "let x omega [] x x"),
    ("occurs check followeþ nested records", "let x omega [] r{value = x} x"),
    ("annotation cannot claim false polymorphism", "let bad [@a:*, _: a, a] 1"),
    ("function branches must agree", boolData ++ "let bad [] { yea ^ 1, nay ^ 'no' }"),
    ("function cases share arity", "let bad [] { x ^ x, x, y ^ x }"),
    ("pattern binders are linear", "let bad [] { x, x ^ x }"),
    ("inhabited empty function", "let bad [ℤ, ℤ] {}"),
    ("unannotated empty function", "let bad [] {}"),
    ("non-exhaustive data match", boolData ++ "let bad [ℤ] match yea { yea ^ 1 }"),
    ("multi-match missing one product case", boolData ++ "let bad [ℤ] match yea, nay { yea, yea ^ 1, yea, nay ^ 2, nay, yea ^ 3 }"),
    ("overlapping wildcards can miss a product", boolData ++ "let bad [ℤ] match yea, nay { yea, _ ^ 1, _, yea ^ 2 }"),
    ("nested match misses constructor", boolData ++ optionData ++ "let bad [ℤ] match yea some { yea some ^ 1, none ^ 0 }"),
    ("constructor pattern arity", optionData ++ "let bad [] match 1 some { some ^ 1, none ^ 0 }"),
    ("ℤ pattern needeþ ℤ input", "let bad [] match 'one' { 1 ^ 1, _ ^ 0 }"),
    ("finite ℤ patterns are not exhaustive", "let bad [] match 1 { 0 ^ 0, 1 ^ 1 }"),
    ("text pattern needeþ text input", "let bad [] match 1 { 'one' ^ 1, _ ^ 0 }"),
    ("finite text patterns are not exhaustive", "let bad [] match 'one' { 'one' ^ 1 }"),
    ("closed record rejecteþ extra field", "let bad [r{name: text}] r{name = 'n', age = 1}"),
    ("missing record field", "let person [] r{name = 'n'} let bad [] person.age"),
    ("unknown record removal", "let person [] r{name = 'n'} let bad [] r{= person, - age}"),
    ("record update needeþ a record", "let bad [] r{= 1, value = 2}"),
    ("missing function constraint", equalPrelude ++ "let same [@a:*, x: a, y: a, 𝟚] x ≡ y"),
    ("insufficient constraint", equalPrelude ++ "flock order-partial [byzen a equal] { let ≤ [a, a, 𝟚] } let leq [byzen a equal, x: a, y: a, 𝟚] x ≤ y"),
    ("unknown bizen class", "bizen:ℤ missing { let x missing [] x }"),
    ("bizen evidence cannot be shown", "show bizen:ℤ missing {}"),
    ("bizen misses required member", "flock identity [a:*] { let identity [a, a] } bizen:ℤ identity {}"),
    ("bizen rejecteþ unknown member", "flock identity [a:*] { let identity [a, a] } bizen:ℤ identity { let x other [] x }"),
    ("bizen member hath declared type", "flock identity [a:*] { let identity [a, a] } bizen:ℤ identity { let x identity [] 'wrong' }"),
    ("duplicate bizen is incoherent", "flock identity [a:*] { let identity [a, a] } bizen:ℤ identity { let x identity [] x } bizen:ℤ identity { let x identity [] x }"),
    ("incomparable instances are ambiguous when used", "flock identity [a:*] { let identity [a, a] } ilk box [a:*, *] { a box } bizen [@f:[*, *]]:(ℤ f) identity { let x identity [] x } bizen [@a:*]:(a box) identity { let x identity [] x } let bad [] (1 box) identity"),
    ("flock law sides share a value type", "flock bad [a:*] { law [x: a] x = 1 }"),
    ("flock law sides share an effect row", unitData ++ "deed pulse { pulse [𝟙, 𝟙] } flock bad [a:*] { law [x: 𝟙] x = only pulse }"),
    ("flock law rejecteþ an unbound value", "flock bad [a:*] { law [x: a] x = missing }"),
    ("flock law needeþ must be declared", equalPrelude ++ "flock bad [a:*] { law [x: a] x ≡ x = x ≡ x }"),
    ("flock member constraint is required at use", memberConstraint ++ "bizen:box traverse { let (x box) traverse f [] (x f) map box } let bad [(ℤ box) box] (1 box) traverse { x ^ x box }"),
    ("bizen arity followeþ class", "flock convert [a:*, b:*] { let convert [a, b] } bizen:ℤ convert { let x convert [] x }"),
    ("effect operation must be a function", "deed bad { tick [ℤ] }"),
    ("primitive flock rejecteþ a nominal result lookalike", "ilk 𝟚 [*] { maybe } flock equal [a:*] { let ≡ [a, a, 𝟚] } let result [𝟚] 1 ≡ 1"),
    ("primitive flock rejecteþ a nominal effect lookalike", "deed e fail { nope [@a:*, e, a] } flock from-text [a:*] { let from-text [text, a; text fail] } let parse [x: text, float; text fail] x from-text"),
    ("state parameter mismatch", unitData ++ "deed a state { get [𝟙, a], set [a, 𝟙] } let got [𝟙, ℤ; text state] get"),
    ("one operation clause cannot erase sibling operations", duoEffect ++ "let run [_: 𝟙, 𝟙] try only second { first ^ only }"),
    ("partial coverage doth not combine across handlers", duoEffect ++ "let run [_: 𝟙, 𝟙] try (try only second { first ^ only }) { second ^ only }"),
    ("handler for absent effect", "let bad [ℤ] try 1 { fail ^ 2 }"),
    ("ordinary function is not an operation", "deed ask { ask [ℤ, ℤ] } let f [x: ℤ, ℤ; ask] x ask let bad [] try 1 f { x f ^ x }"),
    ("eftgin argument hath operation result type", "deed ask { ask [ℤ, ℤ] } let bad [ℤ] try 10 ask { x ask ^ 'bad' eftgin }"),
    ("eftgin is scoped to operation clauses", "let bad [] 1 eftgin"),
    ("handler yield pattern must be irrefutable", boolData ++ "let bad [] try nay { yield yea ^ 1 }"),
    ("handler yield ℤ pattern is refutable", "let bad [] try 1 { yield 1 ^ 1 }"),
    ("handler operation patterns must be irrefutable", boolData ++ "deed choose { choose [𝟚, ℤ] } let bad [] try nay choose { yea choose ^ 1 }"),
    ("effect handler cannot bind operation arguments", duoEffect ++ "let bad [𝟙] try only first { x duo ^ only }"),
    ("effectful let result stayeþ monomorphic", unitData ++ "deed a state { get [𝟙, a] } let _ run [] (let value [] only get let number [ℤ] value let word [text] value yield only)"),
    ("bookhoard let cannot run immediate effects", "let answer [] 'hello' write"),
    ("fremmed let requireþ an annotation", "let plus [] 'add-integer' fremmed"),
    ("fremmed key requireþ a host implementation", "let unknown [ℤ, ℤ] 'unknown' fremmed"),
    ("fremmed key must match its host signature", "let plus [ℤ, ℤ] 'add-integer' fremmed"),
    ("fremmed marker cannot be nested", "let bad [r{value: ℤ}] r{value = 'add-integer' fremmed}"),
    ("fremmed let must be file-level", "let _ outer [] (let plus [ℤ, ℤ, ℤ] 'add-integer' fremmed yield 1)")
  ]

importCases :: [Test]
importCases =
  [ typeOkWith "shown value and type cross a use" "use file.tung let value [box] box" shown,
    typeErrWith "a local function shadoweþ an imported one in application" "use source.tung let foo [x: text, text] x let bad [ℤ] 1 foo" shadowedFunctionImport,
    typeOkWith "qualification still reacheþ a shadowed import" "use source.tung let foo [x: text, text] x let good [ℤ] 1 source~foo" shadowedFunctionImport,
    typeOkWith "default use namespace covereþ terms, types, and classes" "use ilk/item.tung let value [item~item] item~empty let same [item~item] value let result [] same item~identity" aliased,
    typeOkWith "custom use alias surviveþ resolved flock inheritance" "use dep.tung d flock child [byzen a d~parent] { let child [a, a] }" (Map.singleton "dep.tung" "show flock parent [a:*] { let parent [a, a] }"),
    expect "re-exported nominal mismatch nameþ boþ outer aliases" $
      let message = checkWithImports "use middle-left.tung left use middle-right.tung right let bad [left~token] right~value" reexportMismatchImports
       in "left~token" `isInfixOf` message && "right~token" `isInfixOf` message,
    expect "concrete aliases are not rendered as applied constructors" $
      let message = checkWithImports "use middle.tung outer let bad [text] outer~value" concreteAliasProjection
       in "type error:" `isPrefixOf` message && not ("outer~boxed<ℤ>" `isInfixOf` message),
    expect "re-exported effect mismatch nameþ boþ outer aliases" $
      let message = checkWithImports "use middle-left.tung left use middle-right.tung right let bad [x: ℤ, ℤ; left~pulse] x right~ask" reexportEffectImports
       in "left~pulse" `isInfixOf` message && "right~pulse" `isInfixOf` message,
    expect "a hidden operation still counteth toward handler coverage" $
      let message = checkWithImports "use middle.tung let run [_: 𝟙, 𝟙] try only trigger { first ^ only }" hiddenEffectOperationImports
       in "type error:" `isPrefixOf` message && "cannot unify effects" `isInfixOf` message,
    typeOkWith "an effect-level handler covereth hidden operations" "use middle.tung let run [_: 𝟙, 𝟙] try only trigger { duo ^ only }" hiddenEffectOperationImports,
    expect "re-exported constraint mismatch nameþ boþ outer aliases" $
      let message = checkWithImports "use middle-left.tung left use middle-right.tung right let bad [byzen a left~identity, x: a, a] x right~identity" reexportClassImports
       in "left~identity" `isInfixOf` message && "right~identity" `isInfixOf` message,
    expect "imported class-member mismatch nameþ boþ type aliases" $
      let message = checkWithImports "use left-data.tung left use right-data.tung right bizen:ℤ left~maker { let maker [] right~token }" fixedClassTypeImports
       in "left~token" `isInfixOf` message && "right~token" `isInfixOf` message,
    expect "imported bizen constraint useþ þe visible alias" $
      let message = checkWithImports "use dep.tung left let bad [] left~value left~identity" nestedInstanceImport
       in "left~token" `isInfixOf` message && "left~label" `isInfixOf` message,
    expect "duplicate imported bizen preferreþ þe requested alias" $
      let message = checkWithImports "use base.tung first use base.tung second let bad [] first~value first~child" duplicatedInstanceImport
       in "first~token" `isInfixOf` message && "first~parent" `isInfixOf` message && not ("second~" `isInfixOf` message),
    typeOkWith "two aliases of one module shareþ data identity" "use shared-data.tung first use shared-data.tung second let left [first~token] second~token let right [second~token] first~token" sharedTypeAlias,
    typeErrWith "unknown qualified imported type is rejected" "use shared-data.tung shared let bad [shared~missing] shared~token" sharedTypeAlias,
    typeErrWith "private imported type spelling stayeþ hidden" "use opaque.tung opaque let bad [opaque~secret] opaque~value" opaqueTypeImport,
    typeOkWith "value with a private imported type remaineth inferable" "use opaque.tung opaque let value [] opaque~value" opaqueTypeImport,
    typeOkWith "type identity surviveþ a re-export" "use base-data.tung original use middle-data.tung reexport let from-original [reexport~token] original~value let from-reexport [original~token] reexport~value" reexportedTypeImport,
    typeErrWith "host data schema rejecteþ nested nominal lookalikes" nestedTableLookalike rawTableImport,
    typeOkWith "declared host data dependencies remain ABI-compatible" "use ilk/table.tung standard ilk ∏ [a:*, b:*, *] { a ∏ b } ilk list [a:*, *] { empty, a _* (a list) } ilk table [k:*, v:*, *] { ((k ∏ v) list) from-list } let cast [ℤ table text, ℤ standard~table text] { x ^ x }" rawTableImport,
    typeOkWith "coverage separateþ same-spelled imported data" "use left-token.tung left use right-token.tung right let result [ℤ] match left~value { left~zero ^ 0, left~one ^ 1 }" nominalCoverageImports,
    typeErrWith "use aliases cannot collide" "use left.tung common use right.tung common" duplicateAliases,
    typeErrWith "default use aliases cannot collide" "use ilk/item.tung use syntax/item.tung" defaultAliasCollisionImports,
    typeOkWith "explicit use aliases override colliding defaults" "use ilk/item.tung ilk-item use syntax/item.tung syntax-item let result [] ilk-item~value + syntax-item~value" defaultAliasCollisionImports,
    expect "ambiguous imports suggest surface namespaces" $
      let message = checkWithImports "use ilk/natural.tung use flock/algebra/arithmetic/semiring.tung let bad [] zero" ambiguousZeros
       in "natural~zero" `isInfixOf` message && "semiring~zero" `isInfixOf` message && not ("/" `isInfixOf` message),
    typeErrWith "unshown value stayeþ hidden" "use file.tung let bad [] hidden" shown,
    typeErrWith "a transitive flock requireþ an explicit re-export" "use middle.tung flock child [byzen a hidden] { let child [a, a] }" privateClassImports,
    typeErrWith "a transitive flock cannot be reached through its private namespace" "use middle.tung flock child [byzen a leaf~hidden] { let child [a, a] }" privateClassImports,
    typeErrWith "a reserved incompatible data lookalike cannot receive a native value" "use ilk/two.tung let truth [two~𝟚] 1 ≡ 1" reservedTwoLookalike,
    typeOkWith "a reserved incompatible data lookalike retaineþ its own type" "use ilk/two.tung let value [two~𝟚] two~yea" reservedTwoLookalike,
    typeErrWith "missing use is rejected" "use missing.tung" Map.empty,
    typeErrWith "bad imported source is rejected" "use bad.tung" (Map.singleton "bad.tung" "let []"),
    typeErrWith "self use cycle is rejected" "use self.tung" (Map.singleton "self.tung" "use self.tung"),
    typeErrWith "mutual use cycle is rejected" "use left.tung" cyclic,
    runnableErr "module without main is not runnable" "let answer [] 42",
    runnableOk "inferred pure main is runnable" (unitData ++ "let _ main [] only"),
    runnableOk "partial effectful call is pure at boundary" (unitData ++ "let half [] 1 % let main [_: 𝟙, 𝟙] only"),
    runnableOk "runner accepteþ unit and runner effects" runnableOkSource,
    runnableOk "host effect operation order is schema-insensitive" (unitData ++ "deed console { read [𝟙, text], write [text, 𝟙] } let main [_: 𝟙, 𝟙; console] 'ok' write"),
    runnableOk "host effect rows are schema-insensitive sets" reorderedWebSource,
    expect "host effect schema rejecteþ a nominal unit lookalike" $
      "unsupported effects {console}" `isInfixOf` checkRunnableWithImports nominalConsoleLookalike (Map.singleton "ilk/one.tung" "show ilk 𝟙 [*] { only }"),
    expect "host effect schema rejecteþ a nested nominal effect lookalike" $
      "unsupported effects {web}" `isInfixOf` checkRunnableWithImports nominalWebFailLookalike Map.empty,
    runnableErr "main rejecteþ a nonstandard unit lookalike" "ilk 𝟙 [*] { done } let main [done: 𝟙, 𝟙] done",
    runnableErr "main must be a function" (unitData ++ "let main [] only"),
    runnableErr "main takeþ one unit argument" (unitData ++ "let main [_: ℤ, 𝟙] only"),
    runnableErr "main returneþ unit" (unitData ++ "let main [_: 𝟙, ℤ] 42"),
    runnableErr "fail is not a main effect" (unitData ++ "let main [_: 𝟙, 𝟙; text fail] (let _ [] 1 % 0 yield only)"),
    runnableErr "custom effect is not a main effect" (unitData ++ "deed ask { ask [𝟙, 𝟙] } let main [_: 𝟙, 𝟙; ask] only ask"),
    runnableErr "parameterised runner-looking effect is rejected" (unitData ++ "deed a console { fake [𝟙, 𝟙] } let main [_: 𝟙, 𝟙; 𝟙 console] only fake"),
    runnableErr "effects cannot run before main" (unitData ++ "let _ [] 'early' write let main [_: 𝟙, 𝟙] only"),
    expectEq "reporteþ an inferred source type" (Right "[m1, m1]") (typeOfWithImports "show let x identity [] x" Map.empty "identity"),
    expectEq "type display preserveþ the effectful function boundary" (Right "[ℤ, [ℤ, ℤ]; ask]") (typeOfWithImports "deed ask { ask [ℤ, ℤ] } let maker [] { x ^ (let value [] x ask yield { y ^ value + y }) }" Map.empty "maker"),
    expectEq "reporteþ an unknown inspected name" (Left "unknown name 'missing'") (typeOfWithImports "let answer [] 42" Map.empty "missing"),
    expect "elaboration resolveþ every type-class evidence hole" evidenceIsResolved,
    typeOkWith "shared diamond imports" "use left.tung use right.tung yield left~left + right~right" sharedImports
  ]
  where
    shown = Map.fromList [("file.tung", "show ilk box [*] { box } let hidden [] 1")]
    shadowedFunctionImport = Map.singleton "source.tung" "show let foo [x: ℤ, ℤ] x + 1"
    aliased = Map.singleton "ilk/item.tung" "show ilk item [*] { item } show flock identity [a:*] { let identity [a, a] } bizen:item identity { let x identity [] x } show let empty [item] item"
    reexportMismatchImports =
      Map.fromList
        [ ("base-left.tung", "show ilk token [*] { token } show let value [token] token"),
          ("base-right.tung", "show ilk token [*] { token } show let value [token] token"),
          ("middle-left.tung", "use base-left.tung base show-ilk base~token show base~value"),
          ("middle-right.tung", "use base-right.tung base show-ilk base~token show base~value")
        ]
    concreteAliasProjection =
      Map.fromList
        [ ("base.tung", "show ilk box [a:*, *] { a box }"),
          ("middle.tung", "use base.tung base show let boxed [*] ℤ base~box show let value [boxed] 1 base~box")
        ]
    reexportEffectImports =
      Map.fromList
        [ ("base-left.tung", "show deed pulse { ask [ℤ, ℤ] }"),
          ("base-right.tung", "show deed pulse { ask [ℤ, ℤ] }"),
          ("middle-left.tung", "use base-left.tung base-left show-ilk base-left~pulse show base-left~ask"),
          ("middle-right.tung", "use base-right.tung base-right show-ilk base-right~pulse show base-right~ask")
        ]
    hiddenEffectOperationImports =
      Map.fromList
        [ ("base.tung", "show ilk 𝟙 [*] { only } show deed duo { first [𝟙, 𝟙], second [𝟙, 𝟙] } show let trigger [_: 𝟙, 𝟙; duo] only second"),
          ("middle.tung", "use base.tung show-ilk base~𝟙 show-ilk base~duo show base~only show base~first show base~trigger")
        ]
    reexportClassImports =
      Map.fromList
        [ ("base-left.tung", "show flock identity [a:*] { let identity [a, a] }"),
          ("base-right.tung", "show flock identity [a:*] { let identity [a, a] }"),
          ("middle-left.tung", "use base-left.tung base-left show-ilk base-left~identity show base-left~identity"),
          ("middle-right.tung", "use base-right.tung base-right show-ilk base-right~identity show base-right~identity")
        ]
    fixedClassTypeImports =
      Map.fromList
        [ ("left-data.tung", "show ilk token [*] { token } show flock maker [a:*] { let maker [token] }"),
          ("right-data.tung", "show ilk token [*] { token }")
        ]
    nestedInstanceImport =
      Map.singleton
        "dep.tung"
        "show ilk token [*] { token } show flock identity [a:*] { let identity [a, a] } show flock label [a:*] { let label [a, a] } bizen [byzen token label]:token identity { let x identity [] x label } show let value [token] token"
    duplicatedInstanceImport =
      Map.singleton
        "base.tung"
        "show ilk token [*] { token } show flock parent [a:*] { let parent [a, ℤ] } show flock child [a:*] { let child [a, ℤ] } bizen [byzen token parent]:token child { let x child [] x parent } show let value [token] token"
    sharedTypeAlias = Map.singleton "shared-data.tung" "show ilk token [*] { token }"
    opaqueTypeImport = Map.singleton "opaque.tung" "ilk secret [*] { secret } show let value [secret] secret"
    reexportedTypeImport =
      Map.fromList
        [ ("base-data.tung", "show ilk token [*] { token } show let value [token] token"),
          ("middle-data.tung", "use base-data.tung base show-ilk base~token show base~value")
        ]
    nominalCoverageImports =
      Map.fromList
        [ ("left-token.tung", "show ilk token [*] { zero, one } show let value [token] zero"),
          ("right-token.tung", "show ilk token [*] { only } show let value [token] only")
        ]
    duplicateAliases = Map.fromList [("left.tung", "show let left [] 1"), ("right.tung", "show let right [] 2")]
    defaultAliasCollisionImports = Map.fromList [("ilk/item.tung", "show let value [] 1"), ("syntax/item.tung", "show let value [] 2")]
    ambiguousZeros = Map.fromList [("ilk/natural.tung", "show let zero [] 0"), ("flock/algebra/arithmetic/semiring.tung", "show let zero [] 1")]
    privateClassImports = Map.fromList [("leaf.tung", "show flock hidden [a:*] { let hidden [a, a] }"), ("middle.tung", "use leaf.tung")]
    reservedTwoLookalike = Map.singleton "ilk/two.tung" "show ilk 𝟚 [*] { yea, maybe }"
    rawTableImport = Map.singleton "ilk/table.tung" "ilk ∏ [a:*, b:*, *] { a ∏ b } ilk list [a:*, *] { empty, a _* (a list) } show ilk table [k:*, v:*, *] { ((k ∏ v) list) from-list }"
    nestedTableLookalike =
      "use ilk/table.tung standard "
        ++ "ilk ∏ [a:*, b:*, *] { pair } ilk list [a:*, *] { empty } ilk table [k:*, v:*, *] { ((k ∏ v) list) from-list } "
        ++ "let local [ℤ table text] empty from-list let escaped [ℤ standard~table text] local"
    cyclic = Map.fromList [("left.tung", "use right.tung"), ("right.tung", "use left.tung")]
    sharedImports =
      Map.fromList
        [ ("left.tung", "use shared.tung show let left [] shared~base + 1"),
          ("right.tung", "use shared.tung show let right [] shared~base + 2"),
          ("shared.tung", "show let base [] 1")
        ]
    evidenceIsResolved = case parse (equalPrelude ++ "bizen:ℤ equal { let x ≡ y [] yea } let answer [] 1 ≡ 1") >>= (\program -> elaborateProgramWithImports program Map.empty False) of
      Left _ -> False
      Right program -> dictionariesAreResolved program

tableAndSetCases :: Map.Map String String -> [Test]
tableAndSetCases imports =
  [ typeOkWith "table, set, and powerset may qualify shared names" (source ++ qualifiedValues) imports,
    typeErrWith "table, set, powerset, and list empty values are ambiguous bare" (source ++ "let bad [] empty") imports,
    typeErrWith "table and set from-list constructors are ambiguous bare" (source ++ "let values [ℤ list] list~empty let bad [] values from-list") imports
  ]
  where
    source = "use ilk/list.tung use ilk/table.tung use ilk/set.tung use ilk/powerset.tung "
    qualifiedValues =
      "let table-value [ℤ table text] table~empty "
        ++ "let set-value [ℤ set] set~empty "
        ++ "let powerset-value [ℤ powerset] powerset~empty "
        ++ "let table-built [ℤ table text] list~empty table~from-list "
        ++ "let set-built [ℤ set] list~empty set~from-list"

orderCases :: Map.Map String String -> [Test]
orderCases imports =
  [ typeOkWith "partial order supplieþ strict comparison" (partialPrefix ++ "let good [] lesser < greater") imports,
    typeErrWith "partial order doth not supply compare" (partialPrefix ++ "let bad [] lesser compare greater") imports,
    typeErrWith "partial order doth not supply clamp" (partialPrefix ++ "let bad [] lesser clamp greater lesser") imports,
    typeOkWith "total order inherits partial operations" (totalPrefix ++ "let before [] lesser < greater let compared [] lesser compare greater let bounded [] lesser clamp greater greater") imports,
    typeErrWith "total order bizen requireþ its partial parent" (rankPrefix ++ "bizen:rank order-total {}") imports,
    typeOkWith "finite set supplieþ partial order" (setPrefix ++ "let included [] smaller ≤ larger") imports,
    typeErrWith "finite set doth not claim total order" (setPrefix ++ "let bad [] smaller compare larger") imports
  ]
  where
    rankPrefix =
      "use ilk/two.tung use flock/equal.tung use flock/order/less-equal.tung use flock/order/order-partial.tung use flock/order/order-total.tung "
        ++ "ilk rank [*] { lesser, greater } "
        ++ "bizen:rank equal { let ≡ [] { lesser, lesser ^ yea, greater, greater ^ yea, _, _ ^ nay } } "
    partialPrefix =
      rankPrefix
        ++ "bizen:rank order-partial { let ≤ [] { lesser, _ ^ yea, greater, greater ^ yea, greater, lesser ^ nay } } "
    totalPrefix = partialPrefix ++ "bizen:rank order-total {} "
    setPrefix =
      "use ground.tung use ilk/set.tung "
        ++ "let smaller [ℤ set] empty put 1 "
        ++ "let larger [ℤ set] smaller put 2 "

boundCases :: Map.Map String String -> [Test]
boundCases imports =
  [ typeOkWith "finite orders provide both endpoints" (prefix ++ "let low [𝟚] ⟂ let high [𝟛] ⊤") imports,
    typeOkWith "natural supporteþ the lower-bound fold" (prefix ++ "let value [ℕ] (natural~zero _* list~empty) …∨") imports,
    typeErrWith "natural doth not claim an upper bound" (prefix ++ "let value [ℕ] (natural~zero _* list~empty) …∧") imports,
    typeOkWith "powerset is a bounded lattice without total order" (powersetPrefix ++ "let joined [ℤ powerset] (empty-set _* (full-set _* list~empty)) …∨") imports,
    typeErrWith "lattice doth not imply total order" (powersetPrefix ++ "let bad [] empty-set ≤ full-set") imports,
    typeErrWith "float doth not claim a lawful lattice" "use ground.tung let bad [float] 1.0 ∧ 2.0" imports,
    typeOkWith "infimum semilattice stands without supremum" (semilatticePrefix ++ "let good [] item ∧ item") imports,
    typeErrWith "infimum semilattice doth not supply supremum" (semilatticePrefix ++ "let bad [] item ∨ item") imports,
    typeOkWith "supremum semilattice stands without infimum" (supremumPrefix ++ "let good [] item ∨ item") imports,
    typeErrWith "supremum semilattice doth not supply infimum" (supremumPrefix ++ "let bad [] item ∧ item") imports
  ]
  where
    prefix = "use ground.tung use ilk/list.tung use ilk/natural.tung use flock/functor/cata.tung "
    powersetPrefix = "use ground.tung use ilk/list.tung use ilk/powerset.tung use flock/functor/cata.tung let empty-set [ℤ powerset] powerset~empty let full-set [ℤ powerset] universe "
    semilatticePrefix = "use flock/order/semilattice-lower.tung ilk one-sided [*] { item } bizen:one-sided semilattice-lower { let _ ∧ _ [] item } "
    supremumPrefix = "use flock/order/semilattice-upper.tung ilk one-sided [*] { item } bizen:one-sided semilattice-upper { let _ ∨ _ [] item } "

numericHierarchyCases :: Map.Map String String -> [Test]
numericHierarchyCases imports =
  [ typeOkWith "ℤ supplieþ euclidean division" "use ground.tung use ilk/product.tung let divide-with-rest [byzen a euclidean, x: a, y: a, a ∏ a; text fail] x % y let value [ℤ ∏ ℤ] try -5 divide-with-rest 3 { fail ^ 0 ∏ 0 }" imports,
    typeOkWith "float retaineþ raw numeric and comparison operations" "use ground.tung let sum [float] 1.0 + 2.0 let quotient [float] 1.0 ÷ 2.0 let compared [𝟚] 1.0 ≤ 2.0" imports,
    typeErrWith "float doth not claim exact semiring laws" "use ground.tung let twice [byzen a semiring, x: a, a] x + x let bad [float] 1.0 twice" imports,
    typeErrWith "float doth not claim exact field laws" "use ground.tung let reciprocal [byzen a field, x: a, a] one ÷ x let bad [float] 2.0 reciprocal" imports,
    typeErrWith "float doth not claim partial-order laws" "use ground.tung let reflexive [byzen a order-partial, x: a, 𝟚] x ≤ x let bad [𝟚] 1.0 reflexive" imports
  ]

redundantInstanceConstraintCases :: [Test]
redundantInstanceConstraintCases =
  [ typeErrContaining
      "unused bizen constraint is rejected"
      "bizen 'box equal' hath redundant constraint a equal"
      (equalPrelude ++ boxData ++ "bizen [byzen a equal]:(a box) equal { let _ ≡ _ [] yea }"),
    typeErrContaining
      "duplicate bizen constraint is rejected"
      "bizen 'box equal' hath redundant constraint a equal"
      (equalPrelude ++ boxData ++ "bizen [byzen a equal, byzen a equal]:(a box) equal { let (x box) ≡ (y box) [] x ≡ y }"),
    typeErrContaining
      "bizen constraint hidden behind a stronger collection constraint is rejected"
      "bizen 'set inhold a' hath redundant constraint a equal"
      redundantCollectionConstraint
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
      | arity <- [1 .. 3],
        missing <- booleanRows arity
      ]

redundantCoverageCases :: [Test]
redundantCoverageCases =
  [ typeErrContaining "duplicate ℤ pattern is reported" "redundant match case 2; pattern 0 is unreachable" "let bad [] match 1 { 0 ^ 0, 0 ^ 1, _ ^ 2 }",
    typeErrContaining "duplicate text pattern is reported" "redundant match case 2; pattern 'same' is unreachable" "let bad [] match 'same' { 'same' ^ 0, 'same' ^ 1, _ ^ 2 }",
    typeErrContaining "wildcard hideþ constructor pattern" "redundant match case 2; pattern yea is unreachable" (boolData ++ "let bad [] match yea { _ ^ 0, yea ^ 1 }"),
    typeErrContaining "constructor pattern hidden inside product" "redundant match case 3" (boolData ++ "let bad [] match yea, nay { yea, _ ^ 1, nay, _ ^ 2, _, yea ^ 3 }"),
    typeErrContaining "nested constructor pattern is reported" "redundant match case 2" (boolData ++ optionData ++ "let bad [] match yea some { _ some ^ 1, yea some ^ 2, none ^ 0 }"),
    typeErrContaining "pattern over empty type is unreachable" "redundant match case 1" "ilk 𝟘 [*] {} let bad [𝟘, ℤ] { _ ^ 1 }"
  ]

memberConstraint :: String
memberConstraint =
  "ilk option [a:*, *] { none, a some } ilk box [a:*, *] { a box } "
    ++ "flock functor [f:[*, *]] { let map [@a:*, @b:*, a f, [a, b], b f] } "
    ++ "flock applicative [byzen f functor] { let pure [@a:*, a, a f] let apply [@a:*, @b:*, a f, [a, b] f, b f] } "
    ++ "flock traverse [f:[*, *]] { let traverse [byzen m applicative, @a:*, @b:*, a f, [a, b m], (b f) m] } "

matchSource :: Int -> Maybe [String] -> String
matchSource arity omitted =
  boolData
    ++ "let checked [ℤ] match "
    ++ intercalate ", " (replicate arity "yea")
    ++ " { "
    ++ intercalate
      ", "
      [ intercalate ", " row ++ " ^ " ++ show result
      | (result, row) <- zip [0 :: Int ..] (booleanRows arity),
        Just row /= omitted
      ]
    ++ " }"

booleanRows :: Int -> [[String]]
booleanRows arity = replicateM arity ["yea", "nay"]

generatedEffectCases :: [Test]
generatedEffectCases = reordered ++ closed ++ unions ++ parameterized ++ hostHandlers
  where
    effects = ["pulse", "spark", "glow"]
    rows = subsequences ["pulse", "spark"]
    declarations = unitData ++ concatMap (\name -> "deed " ++ name ++ " { " ++ name ++ " [𝟙, 𝟙] } ") effects
    reordered =
      [ typeOk ("effect row order and duplication " ++ show row) (declarations ++ effectFunction "run" effects row)
      | row <- permutations effects ++ [effects ++ effects]
      ]
    closed =
      [ checkRow (rule ++ show (left, right)) permitted (declarations ++ body) Map.empty
      | left <- rows,
        right <- rows,
        (rule, permitted, body) <-
          [ ("body effects are a subset ", all (`elem` right) left, effectFunction "run" left right),
            ("function rows are equal ", left == right, effectFunction "first" left left ++ "let second [𝟙, 𝟙" ++ effectHeader right ++ "] first")
          ]
      ]
    -- chain equateþ the first row with the union: right must be a subset of left.
    -- reversing the chain also checkeþ that solving doth not depend on equation order.
    unions =
      [ checkRow ("inferred union " ++ show (body, imported, left, right)) (all (`elem` left) right) source imports
      | body <- ["f chain (f compose g)", "(f compose g) chain f"],
        let functions =
              "let chain [@a:*, @b:*, @e:*, @c:*, f: [a, b; e], g: [b, c; e], x: a, c; e] (x f) g "
                ++ "let compose [@a:*, @b:*, @e0:*, @c:*, @e1:*, f: [a, b; e0], g: [b, c; e1], x: a, c; e0, e1] (x f) g "
                ++ "show let f combined g [] "
                ++ body
                ++ " ",
        imported <- [False, True],
        left <- rows,
        right <- rows,
        let imports = if imported then Map.singleton "rows.tung" functions else Map.empty
            source =
              (if imported then "use rows.tung " else functions)
                ++ declarations
                ++ effectFunction "first" left left
                ++ effectFunction "second" right right
                ++ "let result [] first "
                ++ (if imported then "rows~combined" else "combined")
                ++ " second"
      ]
    parameterized =
      [ typeErrContaining
          "one inferred row cannot contain conflicting parameters of the same effect"
          "cannot unify"
          (askPrelude ++ "let _ mixed [] (let word [] (only ask: text) let number [] (only ask: ℤ) yield only)"),
        typeErrContaining
          "one operation handler cannot erase conflicting effect parameters"
          "cannot unify"
          (askPrelude ++ "let bad [_: 𝟙, text] try (let word [] (only ask: text) let number [] (only ask: ℤ) yield word) { x ask ^ 1 eftgin }"),
        typeErrContaining
          "parameterized operation handler cannot assume an open row excludes its effect"
          "cannot handle parameterized effect 'source' across an open effect row"
          (askPrelude ++ "let run [@e:*, f: [𝟙, text; e], text; e] try (let number [] (only ask: ℤ) let word [] only f yield word) { ask ^ 1 eftgin }"),
        typeErrContaining
          "whole-effect clause with an operation name cannot receive eftgin"
          "unknown name 'eftgin'"
          ("ilk 𝟙 [*] { only } deed duo { duo [𝟙, ℤ], other [𝟙, ℤ] } let bad [_: 𝟙, ℤ] try only other { duo ^ 1 eftgin }")
      ]
    askPrelude = "ilk 𝟙 [*] { only } deed a source { ask [𝟙, a] } "
    hostHandlers =
      [ typeErrContaining
          "a source handler cannot suppress an effect run by the host"
          "runner effect 'console' cannot have a source handler"
          "ilk 𝟙 [*] { only } let _ [] try 'should-be-handled' write { console ^ only }",
        typeOk
          "state remaineþ handled in source despite its standard schema"
          "ilk 𝟙 [*] { only } deed a state { get [𝟙, a], set [a, 𝟙] } let answer [ℤ] try (only get: ℤ) { state ^ 0 }"
      ]
    checkRow name permitted source imports =
      let actual = checkWithImports source imports
       in if permitted
            then expectEq name "type ok" actual
            else expect (name ++ ": " ++ actual) ("type error:" `isPrefixOf` actual && "cannot unify effects" `isInfixOf` actual)

effectFunction :: String -> [String] -> [String] -> String
effectFunction name performed annotated =
  "let "
    ++ name
    ++ " [𝟙, 𝟙"
    ++ effectHeader annotated
    ++ "] {_ ^ "
    ++ foldr (\name body -> "(let _ [] only " ++ name ++ " yield " ++ body ++ ")") "only" performed
    ++ "} "

effectHeader :: [String] -> String
effectHeader [] = ""
effectHeader row = "; " ++ intercalate ", " row

inferenceProperties :: [Test]
inferenceProperties =
  [ Harness.propertyTest "property: polymorphic identity inferreþ every generated primitive use" $
      QuickCheck.forAll (QuickCheck.listOf1 (QuickCheck.elements primitiveValues)) \values ->
        let declarations =
              [ "let value" ++ show index ++ " [" ++ ty ++ "] " ++ literal ++ " identity "
              | (index, (ty, literal)) <- zip [(0 :: Int) ..] (take 12 values)
              ]
            source = "let x identity [] x " ++ concat declarations
         in QuickCheck.counterexample source (check source QuickCheck.=== "type ok"),
    Harness.propertyTest "property: primitive annotations rejecteþ a different inferred type" $
      QuickCheck.forAll (QuickCheck.elements primitiveValues) \(actualType, literal) ->
        QuickCheck.forAll (QuickCheck.elements [ty | (ty, _) <- primitiveValues, ty /= actualType]) \claimedType ->
          let source = "let value [" ++ claimedType ++ "] " ++ literal
           in QuickCheck.counterexample source ("type error:" `isPrefixOf` check source)
  ]

primitiveValues :: [(String, String)]
primitiveValues =
  [ ("ℤ", "42"),
    ("float", "1.5"),
    ("text", "'word'"),
    ("unicode", "`x")
  ]

evidenceProperties :: [Test]
evidenceProperties =
  [ Harness.propertyTest "property: unrelated bizen order keepeþ evidence coherent" $
      QuickCheck.forAll (QuickCheck.shuffle evidenceCases) \orderedCases ->
        let source = equalPrelude ++ concatMap evidenceInstance orderedCases ++ concatMap evidenceUse orderedCases
         in QuickCheck.counterexample source (QuickCheck.conjoin [check source QuickCheck.=== "type ok", QuickCheck.property (evidenceResolved source)])
  ]

data EvidenceCase = EvidenceCase String String
  deriving stock (Show)

evidenceCases :: [EvidenceCase]
evidenceCases =
  [ EvidenceCase "ℤ" "1",
    EvidenceCase "float" "1.5",
    EvidenceCase "text" "'word'",
    EvidenceCase "unicode" "`x"
  ]

evidenceInstance :: EvidenceCase -> String
evidenceInstance (EvidenceCase ty _) = "bizen:" ++ ty ++ " equal { let x ≡ y [] yea }"

evidenceUse :: EvidenceCase -> String
evidenceUse (EvidenceCase ty literal) = "let " ++ ty ++ "-evidence [𝟚] " ++ literal ++ " ≡ " ++ literal ++ " "

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
        && not ("ElaboratedInstance" `isInfixOf` rendered)

coverageProperties :: [Test]
coverageProperties =
  [ Harness.propertyTest "property: generated product coverage accepteþ all rows and rejecteþ one omission" $
      QuickCheck.forAll (QuickCheck.chooseInt (1, 4)) \arity ->
        QuickCheck.forAll (QuickCheck.elements (booleanRows arity)) \omitted ->
          let complete = matchSource arity Nothing
              incomplete = matchSource arity (Just omitted)
           in QuickCheck.counterexample incomplete (check complete QuickCheck.=== "type ok" QuickCheck..&&. ("type error:" `isPrefixOf` check incomplete))
  ]

polymorphicPrelude :: String
polymorphicPrelude = "let both [f:[@a:*, a, a], r{number:ℤ, word:text}] r{number=1 f, word='x' f} "

unitData, boolData, optionData, boxData, equalPrelude, classHierarchy, inheritedMethodHierarchy, parentDictionaryConstraint, redundantCollectionConstraint, duoEffect, runnableOkSource, webData, reorderedWebSource, nominalConsoleLookalike, nominalWebFailLookalike :: String
unitData = "ilk 𝟙 [*] { only } "
boolData = "ilk 𝟚 [*] { yea, nay } "
optionData = "ilk option [a:*, *] { none, a some } "
boxData = "ilk box [a:*, *] { a box } "
equalPrelude = boolData ++ "flock equal [a:*] { let ≡ [a, a, 𝟚] } "
classHierarchy = "flock parent [a:*] { let parent [a, a] } flock child [byzen a parent] { let child [a, a] } "
inheritedMethodHierarchy =
  "flock magma [a:*] { let combine [a, a, a] } "
    ++ "flock semigroup [byzen a magma] {} "
    ++ "ilk option [a:*, *] { none, a some } "
    ++ "bizen [byzen a semigroup]:(a option) semigroup { "
    ++ "let combine [] { none, b ^ b, a, none ^ a, a some, b some ^ a combine b $ some } "
    ++ "}"
parentDictionaryConstraint =
  "flock parent [a:*] { let parent [a, a] } "
    ++ "flock child [byzen a parent] {} "
    ++ boxData
    ++ "bizen [byzen a parent]:(a box) parent { let (x box) parent [] (x parent) box } "
    ++ "bizen [byzen a parent]:(a box) child {}"
redundantCollectionConstraint =
  equalPrelude
    ++ "flock inhold [a:*, b:*] { let ∋ [a, b, 𝟚] } "
    ++ "ilk list [a:*, *] { empty, a cons (a list) } "
    ++ "bizen [byzen a equal]:(a list) inhold a { let list ∋ value [] match list { empty ^ nay, candidate cons _ ^ value ≡ candidate } } "
    ++ "ilk set [a:*, *] { (a list) set } "
    ++ "bizen [byzen a equal, byzen (a list) inhold a]:(a set) inhold a { let (list set) ∋ value [] list ∋ value }"
duoEffect = unitData ++ "deed duo { first [𝟙, 𝟙], second [𝟙, 𝟙] } "
runnableOkSource = unitData ++ "deed console { write [text, 𝟙], read [𝟙, text] } let main [_: 𝟙, 𝟙; console] 'ok' write"
reorderedWebSource =
  webData
    ++ "deed e fail { fail [@a:*, e, a] } "
    ++ "deed web { serve [@e:*, ℤ, [request, response; e], 𝟙; text fail, e] } "
    ++ "let route [_: request, response] 200 response (empty from-list) '' "
    ++ "let main [_: 𝟙, 𝟙; web] try 8080 serve route { _ fail ^ only }"
webData =
  unitData
    ++ "ilk ∏ [a:*, b:*, *] { a ∏ b } "
    ++ "ilk list [a:*, *] { empty, a _* (a list) } "
    ++ "ilk table [k:*, v:*, *] { ((k ∏ v) list) from-list } "
    ++ "ilk request [*] { text request text (text table text) text } "
    ++ "ilk response [*] { ℤ response (text table text) text } "
nominalConsoleLookalike =
  "use ilk/one.tung one ilk 𝟙 [*] { done } deed console { write [text, 𝟙], read [𝟙, text] } "
    ++ "let main [_: one~𝟙, one~𝟙; console] (let _ [] 'ok' write yield one~only)"
nominalWebFailLookalike =
  webData
    ++ "deed e fail { nope [@a:*, e, a] } "
    ++ "deed web { serve [@e:*, ℤ, [request, response; e], 𝟙; e, text fail] } "
    ++ "let main [_: 𝟙, 𝟙; web] only"
