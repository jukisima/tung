-- bare and qualified lookup, ambiguity, exports, shadowing, and field fallback.
module Test.Name (group) where

import Data.Map.Strict qualified as Map
import Test.Harness (Group, Test, expect, expectEq, typeErrWith, typeOkWith)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = Harness.group "name" (resolutionCases ++ [lookupCase, canonicalTypeCase, dottedFileCanonicalTypeCase, dottedDirectoryCanonicalTypeCase])

resolutionCases :: [Test]
resolutionCases =
  [ typeOkWith "unique imported value may be bare" "use left.tung let ok: integer = foo" leftOnly
  , typeErrWith "two imported values are ambiguous" "use left.tung use right.tung let bad: integer = foo" both
  , typeOkWith "qualification resolveþ value ambiguity" "use left.tung use right.tung let ok: integer = left@foo + right@foo" both
  , typeErrWith "unknown qualified namespace" "use left.tung let bad = right@foo" leftOnly
  , typeOkWith "local value shadows imported values" "use left.tung use right.tung let foo = 3 let ok: integer = foo" both
  , typeOkWith "unique imported constructor may be bare" "use left.tung let ok: left@box = box" leftOnly
  , typeErrWith "constructors are ambiguous too" "use left.tung use right.tung let bad = box" both
  , typeOkWith "qualified constructor resolveþ ambiguity" "use left.tung use right.tung let ok: left@box = left@box" both
  , typeErrWith "bare imported types are ambiguous" "use left.tung use right.tung let bad: box = left@box" both
  , typeOkWith "qualified type resolveþ ambiguity" "use left.tung use right.tung let ok: left@box = left@box" both
  , typeErrWith "exports hide unshown values" "use public.tung let bad = hidden" publicOnly
  , typeErrWith "private values stay hidden when qualified" "use public.tung let bad = public@hidden" publicOnly
  , typeErrWith "private types stay hidden when qualified" "use private-type.tung let bad: private-type@secret = 1" privateType
  , typeOkWith "shown values stay bare" "use public.tung let ok: integer = visible" publicOnly
  , typeOkWith "qualified lookup winneþ before field access" "use public.tung let public = r(visible = 'record') let ok: integer = public@visible" publicOnly
  , typeOkWith "field access is fallback lookup" "let value = r(field = 1) let ok: integer = value@field" Map.empty
  , typeOkWith "a qualified name can be re-exported" "use middle.tung let ok: integer = value + middle@value" reexported
  , typeErrWith "an ordinary use doth not re-export" "use closed.tung let bad = value" notReexported
  , typeErrWith "an unknown re-export is rejected" "show missing" Map.empty
  , typeErrWith "an unknown type re-export is rejected" "show-ilk missing" Map.empty
  , typeErrWith "an ambiguous bare re-export is rejected" "use left.tung use right.tung show foo" both
  , typeErrWith "bare imported type aliases are ambiguous" "use integer-type.tung use text-type.tung let (x: token) id: token = x" aliasImports
  , typeOkWith "qualification resolveþ type alias ambiguity" "use integer-type.tung use text-type.tung let (x: integer-type@token) integer-id: integer-type@token = x let (x: text-type@token) text-id: text-type@token = x" aliasImports
  , typeErrWith "an ambiguous type re-export is rejected" "use integer-type.tung use text-type.tung show-ilk token" aliasImports
  , typeOkWith "a type and constructor can be re-exported separately" "use data-middle.tung let ok: integer box = 1 box" dataReexport
  , typeOkWith "show-ilk exports the type namespace" "use type-only.tung let ok: token = 1" selectiveReexports
  , typeErrWith "show-ilk doth not export a same-spelled term" "use type-only.tung let bad = token" selectiveReexports
  , typeOkWith "show exports the term namespace" "use term-only.tung let ok: integer = token" selectiveReexports
  , typeErrWith "show doth not export a same-spelled type" "use term-only.tung let bad: token = 1" selectiveReexports
  , typeOkWith "a frame export carrieþ methods and fill evidence" "use identity.tung let ok: integer = 1 identity" identityExport
  , typeOkWith "show-ilk explicitly re-exporteþ a frame" "use identity-middle.tung let ok: integer = 1 identity" shapeReexport
  , typeOkWith "an effect export carrieþ its operations" "use ask.tung let (x: integer) run: integer = try x ask { y ask | y }" effectExport
  , typeOkWith "an effect export carrieþ its type-level name" "use ask.tung let (x: integer) run: integer ! ask = x ask" effectExport
  , typeOkWith "show-ilk re-exports an effect" "use ask-middle.tung let (x: integer) run: integer ! ask = x ask" effectReexport
  , typeErrWith "effects and data types share the type namespace" "use data.tung use effect.tung let (x: integer) run: integer ! signal = x effect@signal" typeKindCollision
  , typeOkWith "qualification resolveþ an effect and data type collision" "use data.tung use effect.tung let (x: integer) run: integer ! effect@signal = x effect@signal" typeKindCollision
  , typeOkWith "constructor pattern is exhaustive after re-export" "use data-middle.tung let unbox: integer box → integer = { x box | x }" dataWholeReexport
  , typeOkWith "dotted filename preserveþ its short default alias" "use a.extra.tung let ok: integer = a@foo" dottedFileImport
  , typeOkWith "a dotted directory doth not alter the default basename alias" "use pkg.one/query.tung let ok: integer = query@foo" dottedDirectoryImport
  ]
 where
  leftOnly = Map.fromList [("left.tung", moduleSource 1)]
  both = Map.fromList [("left.tung", moduleSource 1), ("right.tung", moduleSource 2)]
  publicOnly = Map.fromList [("public.tung", "let hidden = 0 show let visible = 1")]
  privateType = Map.fromList [("private-type.tung", "let-ilk secret = integer")]
  reexported =
    Map.fromList
      [ ("base.tung", "show let value = 1")
      , ("middle.tung", "use base.tung show base@value")
      ]
  notReexported =
    Map.fromList
      [ ("base.tung", "show let value = 1")
      , ("closed.tung", "use base.tung")
      ]
  dataReexport =
    Map.fromList
      [ ("data-base.tung", "show kin a box { a box }")
      , ("data-middle.tung", "use data-base.tung show-ilk data-base@box show data-base@box")
      ]
  dataWholeReexport =
    Map.fromList
      [ ("data-base.tung", "show kin a box { a box }")
      , ("data-middle.tung", "use data-base.tung show-ilk data-base@box show data-base@box")
      ]
  selectiveReexports =
    Map.fromList
      [ ("type-only.tung", "let-ilk token = integer let token = 1 show-ilk token")
      , ("term-only.tung", "let-ilk token = integer let token = 1 show token")
      ]
  identityExport =
    Map.fromList
      [ ("identity.tung", "show frame a identity { let a identity: a } fill integer identity { let x identity = x }")
      ]
  shapeReexport =
    Map.fromList
      [ ("identity-base.tung", "show frame a identity { let a identity: a } fill integer identity { let x identity = x }")
      , ("identity-middle.tung", "use identity-base.tung show-ilk identity-base@identity show identity-base@identity")
      ]
  effectExport = Map.fromList [("ask.tung", "show deed ask { integer ask: integer }")]
  effectReexport =
    Map.fromList
      [ ("ask-base.tung", "show deed ask { integer ask: integer }")
      , ("ask-middle.tung", "use ask-base.tung show-ilk ask-base@ask show ask-base@ask")
      ]
  typeKindCollision =
    Map.fromList
      [ ("data.tung", "show kin signal { signal-value }")
      , ("effect.tung", "show deed signal { integer signal: integer }")
      ]
  aliasImports =
    Map.fromList
      [ ("integer-type.tung", "show let-ilk token = integer")
      , ("text-type.tung", "show let-ilk token = text")
      ]
  dottedFileImport = Map.singleton "a.extra.tung" (moduleSource 1)
  dottedDirectoryImport = Map.singleton "pkg.one/query.tung" (moduleSource 1)
moduleSource :: Int -> String
moduleSource value = "show kin box { box } show let foo: integer = " ++ show value

lookupCase :: Test
lookupCase = case contextOf "use left.tung" imports of
  Left message -> pure (Just ("lookup imported value: " ++ message))
  Right ctx -> case lookupEnv "foo" ctx of
    EnvFound (Forall _ _ ty) -> expectEq "lookup imported value" "integer" (showTy ty)
    other -> pure (Just ("lookup imported value: " ++ show other))
 where
  imports = Map.fromList [("left.tung", moduleSource 1)]

canonicalTypeCase :: Test
canonicalTypeCase = case contextOf "use left.tung" imports of
  Left message -> pure (Just ("canonical imported type: " ++ message))
  Right ctx -> expect "canonical imported type" (canonicalTypeName "box" ctx == Just "left@box")
 where
  imports = Map.fromList [("left.tung", moduleSource 1)]

dottedFileCanonicalTypeCase :: Test
dottedFileCanonicalTypeCase = case contextOf "use a.extra.tung" imports of
  Left message -> pure (Just ("dotted filename canonical type: " ++ message))
  Right ctx -> expect "dotted filename canonical type" (canonicalTypeName "a@box" ctx == Just "a.extra@box")
 where
  imports = Map.singleton "a.extra.tung" (moduleSource 1)

dottedDirectoryCanonicalTypeCase :: Test
dottedDirectoryCanonicalTypeCase = case contextOf "use pkg.one/item.tung item-one" imports of
  Left message -> pure (Just ("dotted directory canonical type: " ++ message))
  Right ctx -> expect "dotted directory canonical type" (canonicalTypeName "item-one@box" ctx == Just "pkg.one/item@box")
 where
  imports = Map.singleton "pkg.one/item.tung" (moduleSource 1)

contextOf :: String -> Map.Map String String -> Either String TcContext
contextOf source imports = do
  program <- parse source
  case inferProgramContext program imports baseContext of
    TcOk ctx _ -> Right ctx
    TcErr message -> Left message
