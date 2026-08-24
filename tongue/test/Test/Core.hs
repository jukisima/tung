-- checked-program generation guards the elaborated core boundary from crashes
-- and evaluator failures which should have been rejected statically.
module Test.Core (group) where

import Control.Exception qualified as Exception
import Control.Monad (replicateM)
import Data.List (intercalate, isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Test.Harness (Group, Test)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = do
  imports <- readBookhoardImports
  Harness.group "core" (map (integerCase imports) integerTerms ++ map (languageCase imports) (languagePrograms ++ booleanProductPrograms) ++ map (rejectedCase imports) rejectedPrograms ++ [checkedImportGraph, checkedMainCore])

checkedImportGraph :: Test
checkedImportGraph = case parse source >>= (\program -> elaborateInteractiveProgramWithImports program imports) of
  Left message -> pure (Just ("checked import graph: elaboration failed: " ++ message))
  Right program -> evaluateCoreProgram program >>= Harness.expectEq "checked import graph evaluateth without source imports" "eval ok: 7"
 where
  source = "bring middle.tung yield middle@value"
  imports =
    Map.fromList
      [ ("middle.tung", "bring leaf.tung show let value = leaf@value + 1")
      , ("leaf.tung", "show let value = 6")
      ]

checkedMainCore :: Test
checkedMainCore = case parse source >>= (\program -> elaborateProgramWithImports program Map.empty True) of
  Left message -> pure (Just ("checked main core: elaboration failed: " ++ message))
  Right program -> evaluateMainCoreProgram program >>= Harness.expectEq "checked main core evaluateth without surface syntax" "eval ok: null"
 where
  source = "kin 𝟙 { null } let (_: 𝟙) main: 𝟙 = null"

integerCase :: Map.Map String String -> (String, Integer) -> Test
integerCase imports (source, expected) = safeResult imports ("generated integer " ++ source) ("yield " ++ source) ("eval ok: " ++ show expected)

languageCase :: Map.Map String String -> (String, String, String) -> Test
languageCase imports (name, source, expected) = safeResult imports name source expected

rejectedCase :: Map.Map String String -> (String, String) -> Test
rejectedCase imports (name, source) = do
  outcome <- safeEvaluate imports source
  pure $ case outcome of
    Left message -> Just (name ++ ": host exception: " ++ message)
    Right result
      | "eval error: type error:" `isPrefixOf` result -> Nothing
      | otherwise -> Just (name ++ ": unsafe program reached evaluation: " ++ result)

safeResult :: Map.Map String String -> String -> String -> String -> Test
safeResult imports name source expected = do
  outcome <- safeEvaluate imports source
  pure $ case outcome of
    Left message -> Just (name ++ ": host exception: " ++ message)
    Right actual
      | actual == expected -> Nothing
      | otherwise -> Just (name ++ ": expected " ++ expected ++ ", got " ++ actual)

safeEvaluate :: Map.Map String String -> String -> IO (Either String String)
safeEvaluate imports source = do
  result <- Exception.try (evaluateWithImports source imports) :: IO (Either Exception.SomeException String)
  pure (either (Left . Exception.displayException) Right result)

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
  , ("checked exhaustive match", "kin 𝟚 { yea, nay } yield match nay { yea | 1, nay | 2 }", "eval ok: 2")
  , ("checked integer match", "yield match 1 { 0 | 10, 1 | 20, _ | 30 }", "eval ok: 20")
  , ("checked class evidence", "shape a identity { let a identity: a } fill integer identity { let x identity = x } yield 7 identity", "eval ok: 7")
  , ("checked fill member calleth sibling", "shape a linked { let a first: a let a second: a } fill integer linked { let x first = x let x second = x first } yield 7 second", "eval ok: 7")
  , ("checked multi-shot handler", "kin 𝟙 { null } deed choice { 𝟙 choose: integer } yield try null choose { choose | (1 eftgin) + (2 eftgin) }", "eval ok: 3")
  , ("checked non-finite floor failure", "bring ground.tung yield try ('Infinity' from-text $ ⌊) { _ fail | 0 }", "eval ok: 0")
  ]

rejectedPrograms :: [(String, String)]
rejectedPrograms =
  [ ("reject non-function application", "yield 1 2")
  , ("reject constructor overapplication", "kin a box { a box } yield 1 box 2")
  , ("reject missing record field", "let value = r(field = 1) yield value@missing")
  , ("reject non-exhaustive match", "kin 𝟚 { yea, nay } yield match nay { yea | 1 }")
  , ("reject refutable handler yield", "kin 𝟚 { yea, nay } yield try nay { yield yea | 1 }")
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
  boolData = "kin 𝟚 { yea, nay } "
  booleanRows arity = replicateM arity ["yea", "nay"]
