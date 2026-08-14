module Test.Name (group) where

import Data.Map.Strict qualified as Map
import Test.Harness (Group, Test, expect, expectEq, typeErrWith, typeOkWith)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = Harness.group "name" (resolutionCases ++ [lookupCase, canonicalTypeCase])

resolutionCases :: [Test]
resolutionCases =
  [ typeOkWith "unique imported value may be bare" "bring left.tung; let ok: integer = foo;" leftOnly
  , typeErrWith "two imported values are ambiguous" "bring left.tung; bring right.tung; let bad: integer = foo;" both
  , typeOkWith "qualification resolves value ambiguity" "bring left.tung; bring right.tung; let ok: integer = left@foo + right@foo;" both
  , typeErrWith "unknown qualified namespace" "bring left.tung; let bad = right@foo;" leftOnly
  , typeOkWith "local value shadows imported values" "bring left.tung; bring right.tung; let foo = 3; let ok: integer = foo;" both
  , typeOkWith "unique imported constructor may be bare" "bring left.tung; let ok: left@box = box;" leftOnly
  , typeErrWith "constructors are ambiguous too" "bring left.tung; bring right.tung; let bad = box;" both
  , typeOkWith "qualified constructor resolves ambiguity" "bring left.tung; bring right.tung; let ok: left@box = left@box;" both
  , typeErrWith "bare imported types are ambiguous" "bring left.tung; bring right.tung; let bad: box = left@box;" both
  , typeOkWith "qualified type resolves ambiguity" "bring left.tung; bring right.tung; let ok: left@box = left@box;" both
  , typeErrWith "exports hide unshown values" "bring public.tung; let bad = hidden;" publicOnly
  , typeErrWith "private values stay hidden when qualified" "bring public.tung; let bad = public@hidden;" publicOnly
  , typeErrWith "private types stay hidden when qualified" "bring private-type.tung; let bad: private-type@secret = 1;" privateType
  , typeOkWith "shown values stay bare" "bring public.tung; let ok: integer = visible;" publicOnly
  , typeOkWith "qualified lookup wins before field access" "bring public.tung; let public = [visible = 'record']; let ok: integer = public@visible;" publicOnly
  , typeOkWith "field access is fallback lookup" "let value = [field = 1]; let ok: integer = value@field;" Map.empty
  , typeOkWith "a qualified name can be re-exported" "bring middle.tung; let ok: integer = value + middle@value;" reexported
  , typeErrWith "an ordinary bring does not re-export" "bring closed.tung; let bad = value;" notReexported
  , typeErrWith "an unknown re-export is rejected" "show missing;" Map.empty
  , typeErrWith "an unknown type re-export is rejected" "show-ilk missing;" Map.empty
  , typeErrWith "an ambiguous bare re-export is rejected" "bring left.tung; bring right.tung; show foo;" both
  , typeErrWith "bare imported type aliases are ambiguous" "bring integer-type.tung; bring string-type.tung; let id: token → token = { x | x };" aliasImports
  , typeOkWith "qualification resolves type alias ambiguity" "bring integer-type.tung; bring string-type.tung; let integer-id: integer-type@token → integer-type@token = { x | x }; let string-id: string-type@token → string-type@token = { x | x };" aliasImports
  , typeErrWith "an ambiguous type re-export is rejected" "bring integer-type.tung; bring string-type.tung; show-ilk token;" aliasImports
  , typeOkWith "a type and constructor can be re-exported separately" "bring data-middle.tung; let ok: integer box = 1 box;" dataReexport
  , typeOkWith "show-ilk exports the type namespace" "bring type-only.tung; let ok: token = 1;" selectiveReexports
  , typeErrWith "show-ilk does not export a same-spelled term" "bring type-only.tung; let bad = token;" selectiveReexports
  , typeOkWith "show exports the term namespace" "bring term-only.tung; let ok: integer = token;" selectiveReexports
  , typeErrWith "show does not export a same-spelled type" "bring term-only.tung; let bad: token = 1;" selectiveReexports
  , typeOkWith "a shape export carries methods and fill evidence" "bring identity.tung; let ok: integer = 1 identity;" identityExport
  , typeOkWith "an effect export carries its operations" "bring ask.tung; let run: integer → integer = { x | try x ask { y ask | y } };" effectExport
  , typeOkWith "an effect export carries its type-level name" "bring ask.tung; let run: integer → integer ! ask = { x | x ask };" effectExport
  , typeOkWith "show-ilk re-exports an effect" "bring ask-middle.tung; let run: integer → integer ! ask = { x | x ask };" effectReexport
  , typeErrWith "effects and data types share the type namespace" "bring data.tung; bring effect.tung; let run: integer → integer ! signal = { x | x effect@signal };" typeKindCollision
  , typeOkWith "qualification resolves an effect and data type collision" "bring data.tung; bring effect.tung; let run: integer → integer ! effect@signal = { x | x effect@signal };" typeKindCollision
  , typeOkWith "show bring re-exports the shown surface" "bring umbrella.tung; let ok: integer = value + umbrella@value;" wholeReexport
  ]
 where
  leftOnly = Map.fromList [("left.tung", moduleSource 1)]
  both = Map.fromList [("left.tung", moduleSource 1), ("right.tung", moduleSource 2)]
  publicOnly = Map.fromList [("public.tung", "let hidden = 0; show let visible = 1;")]
  privateType = Map.fromList [("private-type.tung", "let-ilk secret = integer;")]
  reexported =
    Map.fromList
      [ ("base.tung", "show let value = 1;")
      , ("middle.tung", "bring base.tung; show base@value;")
      ]
  notReexported =
    Map.fromList
      [ ("base.tung", "show let value = 1;")
      , ("closed.tung", "bring base.tung;")
      ]
  dataReexport =
    Map.fromList
      [ ("data-base.tung", "show choose a box { a box };")
      , ("data-middle.tung", "bring data-base.tung; show-ilk data-base@box; show data-base@box;")
      ]
  selectiveReexports =
    Map.fromList
      [ ("type-only.tung", "let-ilk token = integer; let token = 1; show-ilk token;")
      , ("term-only.tung", "let-ilk token = integer; let token = 1; show token;")
      ]
  identityExport =
    Map.fromList
      [ ("identity.tung", "show shape a identity { a identity: a }; fill integer identity { let x identity = x };")
      ]
  effectExport = Map.fromList [("ask.tung", "show deed ask { integer ask: integer };")]
  effectReexport =
    Map.fromList
      [ ("ask-base.tung", "show deed ask { integer ask: integer };")
      , ("ask-middle.tung", "bring ask-base.tung; show-ilk ask-base@ask; show ask-base@ask;")
      ]
  typeKindCollision =
    Map.fromList
      [ ("data.tung", "show choose signal { signal-value };")
      , ("effect.tung", "show deed signal { integer signal: integer };")
      ]
  aliasImports =
    Map.fromList
      [ ("integer-type.tung", "show let-ilk token = integer;")
      , ("string-type.tung", "show let-ilk token = string;")
      ]
  wholeReexport =
    Map.fromList
      [ ("whole-base.tung", "show let value = 1;")
      , ("umbrella.tung", "show bring whole-base.tung;")
      ]

moduleSource :: Int -> String
moduleSource value = "show choose box { box }; show let foo: integer = " ++ show value ++ ";"

lookupCase :: Test
lookupCase = case contextOf "bring left.tung;" imports of
  Left message -> pure (Just ("lookup imported value: " ++ message))
  Right ctx -> case lookupEnv "foo" ctx of
    EnvFound (Forall _ _ ty) -> expectEq "lookup imported value" "integer" (showTy ty)
    other -> pure (Just ("lookup imported value: " ++ show other))
 where
  imports = Map.fromList [("left.tung", moduleSource 1)]

canonicalTypeCase :: Test
canonicalTypeCase = case contextOf "bring left.tung;" imports of
  Left message -> pure (Just ("canonical imported type: " ++ message))
  Right ctx -> expect "canonical imported type" (canonicalTypeName "box" ctx == Just "left@box")
 where
  imports = Map.fromList [("left.tung", moduleSource 1)]

contextOf :: String -> Map.Map String String -> Either String TcContext
contextOf source imports = do
  program <- parse source
  case inferProgramContext program imports baseContext of
    TcOk ctx _ -> Right ctx
    TcErr message -> Left message
