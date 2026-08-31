-- checked-program generation guards the elaborated core boundary from crashes
-- and evaluator failures which should have been rejected statically.
module Test.Core (group) where

import Control.Exception qualified as Exception
import Control.Monad (replicateM)
import Data.List (intercalate, isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Test.Harness (Group, Test)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = do
  imports <- readBookhoardImports
  Harness.group "core" (map (integerCase imports) integerTerms ++ map (languageCase imports) (languagePrograms ++ booleanProductPrograms) ++ [checkedImportGraph, checkedModuleInterface, checkedImportInterface, checkedReExportInterface, checkedReservedDataInterface, checkedAliasedShapeNeed, checkedVisibleShapeNeed, checkedMainCore])

checkedImportGraph :: Test
checkedImportGraph = case parse source >>= (\program -> elaborateInteractiveProgramWithImports program imports) of
  Left message -> pure (Just ("checked import graph: elaboration failed: " ++ message))
  Right program -> evaluateCoreProgram program >>= Harness.expectEq "checked import graph evaluateþ without source imports" "eval ok: 7"
 where
  source = "use middle.tung yield middle@value"
  imports =
    Map.fromList
      [ ("middle.tung", "use leaf.tung show let value = leaf@value + 1")
      , ("leaf.tung", "show let value = 6")
      ]

checkedAliasedShapeNeed :: Test
checkedAliasedShapeNeed = checkedShapeNeed "checked frame need retaineþ a custom use alias" "child" (SymbolId (SourceModule "dep.tung") "parent") source imports
 where
  source = "use dep.tung d graiþ a d@parent frame a child { let a child: a }"
  imports = Map.singleton "dep.tung" "show frame a parent { let a parent: a }"

checkedVisibleShapeNeed :: Test
checkedVisibleShapeNeed = checkedShapeNeed "checked frame need cannot select a hidden collision" "child" (SymbolId (SourceModule "public.tung") "parent") source imports
 where
  source = "use public.tung use private.tung graiþ a parent frame a child { let a child: a }"
  imports =
    Map.fromList
      [ ("public.tung", "show frame a parent { let a parent: a }")
      , ("private.tung", "frame a parent { let a parent: a }")
      ]

checkedShapeNeed :: String -> String -> SymbolId -> String -> Map.Map String String -> Test
checkedShapeNeed name shapeName parent source imports = case parse source >>= (\program -> elaborateInteractiveProgramWithImports program imports) of
  Left message -> pure (Just (name ++ ": elaboration failed: " ++ message))
  Right program ->
    let target = SymbolId RootModule shapeName
        fragment = "CoreShape (" ++ show target ++ ") [" ++ show parent ++ "]"
        rendered = show program
     in pure $ if fragment `isInfixOf` rendered then Nothing else Just (name ++ ": missing " ++ fragment ++ " in " ++ rendered)

checkedMainCore :: Test
checkedMainCore = case parse source >>= (\program -> elaborateProgramWithImports program Map.empty True) of
  Left message -> pure (Just ("checked main core: elaboration failed: " ++ message))
  Right program -> evaluateMainCoreProgram program >>= Harness.expectEq "checked main core evaluateþ without surface syntax" "eval ok: null"
 where
  source = "ilk 𝟙 { null } let (_: 𝟙) main: 𝟙 = null"

checkedModuleInterface :: Test
checkedModuleInterface = case parse source >>= (\program -> elaborateInteractiveProgramWithImports program Map.empty) of
  Left message -> pure (Just ("checked module interface: elaboration failed: " ++ message))
  Right program ->
    let ModuleInterface{interfaceModule, interfaceTerms} = coreInterface program
        actual = (interfaceModule, interfaceTerms)
        symbol = SymbolId RootModule
        exported target kind = TermExport (symbol target) kind
        expectedTerms =
          Map.fromList
            [ (symbol "box", exported "box@box" (ConstructorTerm 0))
            , (symbol "identity", exported "identity@identity" (ShapeMemberTerm (symbol "identity")))
            , (symbol "value", exported "value" OrdinaryTerm)
            ]
        expected = (RootModule, expectedTerms)
     in Harness.expectEq "checked module interface recordeþ public runtime terms" expected actual
 where
  source = "show ilk box { box } show frame a identity { let a identity: a } fill box identity { let x identity = x } show let value = box"

checkedImportInterface :: Test
checkedImportInterface = case parse source >>= (\program -> elaborateInteractiveProgramWithImports program imports) of
  Left message -> pure (Just ("checked import interface: elaboration failed: " ++ message))
  Right program -> case coreImportInterface "dep.tung" program of
    Nothing -> pure (Just "checked import interface: missing dep.tung interface")
    Just imported ->
      let depSymbol = SymbolId (SourceModule "dep.tung")
          importedSummary = (interfaceModule imported, interfaceTerms imported)
          exported target kind = TermExport (depSymbol target) kind
          expectedTerms =
            Map.fromList
              [ (depSymbol "box", exported "box@box" (ConstructorTerm 0))
              , (depSymbol "identity", exported "identity@identity" (ShapeMemberTerm (depSymbol "identity")))
              , (depSymbol "value", exported "value" OrdinaryTerm)
              ]
          expected = (SourceModule "dep.tung", expectedTerms)
       in Harness.expectEq "checked import interface retaineþ its resolved module identity" expected importedSummary
 where
  source = "use dep.tung let same: dep@box = dep@value dep@identity"
  imports = Map.singleton "dep.tung" "show ilk box { box } show frame a identity { let a identity: a } fill box identity { let x identity = x } show let value = box"

checkedReExportInterface :: Test
checkedReExportInterface = case parse source >>= (\program -> elaborateInteractiveProgramWithImports program imports) of
  Left message -> pure (Just ("checked re-export interface: elaboration failed: " ++ message))
  Right program -> case coreImportInterface "middle.tung" program of
    Nothing -> pure (Just "checked re-export interface: missing middle.tung interface")
    Just imported ->
      let middle = SymbolId (SourceModule "middle.tung")
          base = SymbolId (SourceModule "base.tung")
          expected =
            Map.fromList
              [ (middle "off", TermExport (base "bit@off") (ConstructorTerm 0))
              , (middle "value", TermExport (base "value") OrdinaryTerm)
              ]
       in Harness.expectEq "checked re-exports retain defining term identities" expected (interfaceTerms imported)
 where
  source = "use middle.tung yield middle@value"
  imports =
    Map.fromList
      [ ("base.tung", "show ilk bit { off } show let value = 7")
      , ("middle.tung", "use base.tung show base@off show base@value")
      ]

checkedReservedDataInterface :: Test
checkedReservedDataInterface = case parse source >>= (\program -> elaborateInteractiveProgramWithImports program imports) of
  Left message -> pure (Just ("checked reserved data interface: elaboration failed: " ++ message))
  Right program -> case coreImportInterface path program of
    Nothing -> pure (Just ("checked reserved data interface: missing " ++ path ++ " interface"))
    Just imported ->
      let symbol = SymbolId (SourceModule path)
          expected = Just (TermExport (symbol "$nominal@𝟚@yea") (ConstructorTerm 0))
       in Harness.expectEq "checked incompatible reserved data retaineþ a nominal constructor identity" expected (Map.lookup (symbol "yea") (interfaceTerms imported))
 where
  path = "data/two.tung"
  source = "use data/two.tung let value: two@𝟚 = two@yea"
  imports = Map.singleton path "show ilk 𝟚 { yea, maybe }"

integerCase :: Map.Map String String -> (String, Integer) -> Test
integerCase imports (source, expected) = safeResult imports ("generated integer " ++ source) ("yield " ++ source) ("eval ok: " ++ show expected)

languageCase :: Map.Map String String -> (String, String, String) -> Test
languageCase imports (name, source, expected) = safeResult imports name source expected

safeResult :: Map.Map String String -> String -> String -> String -> Test
safeResult imports name source expected = do
  outcome <- Exception.try (evaluateWithImports source imports) :: IO (Either Exception.SomeException String)
  pure $ case outcome of
    Left exception -> Just (name ++ ": host exception: " ++ Exception.displayException exception)
    Right actual
      | actual == expected -> Nothing
      | otherwise -> Just (name ++ ": expected " ++ expected ++ ", got " ++ actual)

integerTerms :: [(String, Integer)]
integerTerms = atoms ++ take 90 firstLevel ++ take 90 secondLevel
 where
  atoms = [(show value, value) | value <- [-4 .. 4]]
  firstLevel = combine atoms atoms
  secondLevel = combine (take 18 firstLevel) atoms
  combine leftTerms rightTerms =
    [ ("(" ++ leftSource ++ " " ++ operator ++ " " ++ rightSource ++ ")", operation left right)
    | (operator, operation) <- [("+", (+)), ("-", (-)), ("×", (*))]
    , (leftSource, left) <- leftTerms
    , (rightSource, right) <- rightTerms
    ]

languagePrograms :: [(String, String, String)]
languagePrograms =
  [ ("checked record access", "let value = r(left = 1, right = 2) yield value@right", "eval ok: 2")
  , ("checked record update", "let value = r(left = 1, right = 2) yield r(= value, right = 3, - left)", "eval ok: r(right = 3)")
  , ("checked exhaustive match", "ilk 𝟚 { yea, nay } yield match nay { yea | 1, nay | 2 }", "eval ok: 2")
  , ("checked integer match", "yield match 1 { 0 | 10, 1 | 20, _ | 30 }", "eval ok: 20")
  , ("checked class evidence", "frame a identity { let a identity: a } fill integer identity { let x identity = x } yield 7 identity", "eval ok: 7")
  , ("checked fill member calleþ sibling", "frame a linked { let a first: a let a second: a } fill integer linked { let x first = x let x second = x first } yield 7 second", "eval ok: 7")
  , ("checked empty effect", "deed marker {} yield 1", "eval ok: 1")
  , ("checked multi-shot handler", "ilk 𝟙 { null } deed choice { 𝟙 choose: integer } yield try null choose { choose | (1 eftgin) + (2 eftgin) }", "eval ok: 3")
  , ("checked non-finite floor failure", "use ground.tung yield try ('Infinity' from-text $ ⌊) { _ fail | 0 }", "eval ok: 0")
  ]

booleanProductPrograms :: [(String, String, String)]
booleanProductPrograms =
  [ ( "checked boolean product " ++ unwords input
    , boolData ++ "yield match " ++ intercalate ", " input ++ " { " ++ cases ++ " }"
    , "eval ok: " ++ show expected
    )
  | arity <- [1 .. 3]
  , input <- booleanRows arity
  , let rows = booleanRows arity
        expected = fromMaybe 0 (lookup input (zip rows [0 :: Int ..]))
        cases = intercalate ", " [intercalate ", " row ++ " | " ++ show result | (row, result) <- zip rows [0 :: Int ..]]
  ]
 where
  boolData = "ilk 𝟚 { yea, nay } "
  booleanRows arity = replicateM arity ["yea", "nay"]
