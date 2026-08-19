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
  Harness.group "core" (map (integerCase imports) integerTerms ++ map (languageCase imports) (languagePrograms ++ booleanProductPrograms) ++ map (rejectedCase imports) rejectedPrograms)

integerCase :: Map.Map String String -> (String, Integer) -> Test
integerCase imports (source, expected) = safeResult imports ("generated integer " ++ source) source ("eval ok: " ++ show expected)

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
    | (operator, operation, accepts) <- [("+", (+), const True), ("-", (-), const True), ("×", (*), const True), ("÷", div, (/= 0))]
    , (leftSource, left) <- leftTerms
    , (rightSource, right) <- rightTerms
    , accepts right
    ]

languagePrograms :: [(String, String, String)]
languagePrograms =
  [ ("checked record access", "let value = [left = 1, right = 2]; value@right", "eval ok: 2")
  , ("checked record update", "let value = [left = 1, right = 2]; [= value, right = 3, - left]", "eval ok: [right = 3]")
  , ("checked exhaustive match", "kin 𝟚 { yea, nay }; match nay { yea | 1, nay | 2 }", "eval ok: 2")
  , ("checked integer match", "match 1 { 0 | 10, 1 | 20, _ | 30 }", "eval ok: 20")
  , ("checked class evidence", "shape a identity { a identity: a }; fill integer identity { let x identity = x }; 7 identity", "eval ok: 7")
  , ("checked multi-shot handler", "kin 𝟙 { null }; deed choice { 𝟙 choose: integer }; try null choose { choose | (1 resume) + (2 resume) }", "eval ok: 3")
  , ("checked non-finite floor failure", "bring ground.tung; try ('Infinity' from-text $ ⌊) { _ fail | 0 }", "eval ok: 0")
  ]

rejectedPrograms :: [(String, String)]
rejectedPrograms =
  [ ("reject non-function application", "1 2")
  , ("reject constructor overapplication", "kin a box { a box }; 1 box 2")
  , ("reject missing record field", "let value = [field = 1]; value@missing")
  , ("reject non-exhaustive match", "kin 𝟚 { yea, nay }; match nay { yea | 1 }")
  , ("reject refutable handler return", "kin 𝟚 { yea, nay }; try nay { return yea | 1 }")
  ]

booleanProductPrograms :: [(String, String, String)]
booleanProductPrograms =
  [ ( "checked boolean product " ++ unwords input
    , boolData ++ "match " ++ intercalate ", " input ++ " { " ++ cases ++ " }"
    , "eval ok: " ++ show expected
    )
  | arity <- [1 .. 3]
  , input <- booleanRows arity
  , let rows = booleanRows arity
        expected = fromMaybe 0 (lookup input (zip rows [0 :: Int ..]))
        cases = intercalate ", " [intercalate ", " row ++ " | " ++ show result | (row, result) <- zip rows [0 :: Int ..]]
  ]
 where
  boolData = "kin 𝟚 { yea, nay }; "
  booleanRows arity = replicateM arity ["yea", "nay"]
