-- bookhoard registry, dependency, ownership, export, and type-checking invariants.
-- the graph must be acyclic, dependencies may not bring ground, and each source
-- owns at most one algebraic data declaration.
module Test.Bookhoard (group) where

import Data.Foldable (traverse_)
import Data.List (intercalate, nub, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Harness (Group, Test)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = do
  imports <- readBookhoardImports
  registry <- registryCase
  Harness.group "bookhoard" $
    registry
      : acyclicCase imports
      : noUmbrellaDependencyCase
      : groundEffectExportsCase imports
      : runnerEffectsCase imports
      : map (typeCase imports) bookhoardImportFiles
      ++ map oneDataTypeCase (nub (map fst bookhoardImportFiles))

registryCase :: IO Test
registryCase = do
  files <- discover "bookhoard"
  let registered = sort (nub (map fst bookhoardImportFiles))
      actual = sort (filter ((== ".tung") . takeExtension) files)
  pure . pure $
    if registered == actual
      then Nothing
      else Just ("bookhoard registry differs from source tree: " ++ intercalate ", " (registered `symmetricDifference` actual))

discover :: FilePath -> IO [FilePath]
discover directory = do
  entries <- listDirectory directory
  concat <$> traverse visit entries
 where
  visit entry = do
    let path = directory </> entry
    isDirectory <- doesDirectoryExist path
    if isDirectory then discover path else pure [path]

symmetricDifference :: (Eq a) => [a] -> [a] -> [a]
symmetricDifference left right = filter (`notElem` right) left ++ filter (`notElem` left) right

typeCase :: Map.Map String String -> (FilePath, String) -> Test
typeCase imports (path, importName) = do
  source <- readFile path
  pure $ case checkWithImports source imports of
    "type ok" -> Nothing
    actual -> Just ("type " ++ importName ++ ": " ++ actual)

acyclicCase :: Map.Map String String -> Test
acyclicCase imports = pure $ case traverse_ (walk []) (Map.keys imports) of
  Left message -> Just message
  Right () -> Nothing
 where
  walk stack path = do
    nextStack <- enterImport stack path
    source <- maybe (Left ("missing bring '" ++ path ++ "'")) Right (Map.lookup path imports)
    Program declarations <- either (Left . (("parse " ++ path ++ ": ") ++)) Right (parse source)
    traverse_ (walk nextStack) (concatMap importedPaths declarations)

noUmbrellaDependencyCase :: Test
noUmbrellaDependencyCase = listToMaybe . catMaybes <$> traverse checkModule (nub (map fst bookhoardImportFiles))
 where
  checkModule "bookhoard/ground.tung" = pure Nothing
  checkModule path = do
    source <- readFile path
    pure $ case parse source of
      Left message -> Just ("parse " ++ path ++ ": " ++ message)
      Right (Program declarations)
        | "ground.tung" `elem` concatMap importedPaths declarations -> Just (path ++ " brings the ground umbrella")
        | otherwise -> Nothing

groundEffectExportsCase :: Map.Map String String -> Test
groundEffectExportsCase imports =
  pure $ case checkWithImports source imports of
    "type ok" -> Nothing
    actual -> Just ("ground effect exports: " ++ actual)
 where
  source = "bring ground.tung; show-ilk ground@fail, ground@console, ground@random, ground@state, ground@async, ground@file, ground@system, ground@clock;"

runnerEffectsCase :: Map.Map String String -> Test
runnerEffectsCase imports =
  pure $ case checkRunnableWithImports source imports of
    "type ok" -> Nothing
    actual -> Just ("system and clock runner effects: " ++ actual)
 where
  source =
    "bring ground.tung; bring data/list.tung; bring data/option.tung; "
      ++ "let main: 𝟙 → 𝟙 ! system, clock = { _ | ("
      ++ "let args = null arguments; "
      ++ "let setting = 'TUNG_SETTING' environment; "
      ++ "let stamp = null unix-time; null) };"

oneDataTypeCase :: FilePath -> Test
oneDataTypeCase path = do
  source <- readFile path
  pure $ case parse source of
    Left message -> Just ("parse " ++ path ++ ": " ++ message)
    Right (Program declarations) ->
      let count = length [() | declaration <- declarations, isDataDecl declaration]
       in if count <= 1 then Nothing else Just (path ++ " owns " ++ show count ++ " data types")

importedPaths :: Decl -> [String]
importedPaths (Import path) = [path]
importedPaths (Export declaration) = importedPaths declaration
importedPaths _ = []

isDataDecl :: Decl -> Bool
isDataDecl DataDecl{} = True
isDataDecl (Export declaration) = isDataDecl declaration
isDataDecl _ = False
