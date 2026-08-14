module Test.Library (group) where

import Data.Foldable (traverse_)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe)
import Jbini
import Test.Harness (Group, Test)
import Test.Harness qualified as Harness

group :: IO Group
group = do
  imports <- readLibraryImports
  Harness.group "library" $
    acyclicCase imports
      : noUmbrellaDependencyCase
      : groundEffectExportsCase imports
      : map (typeCase imports) libraryImportFiles
      ++ map oneDataTypeCase (nub (map fst libraryImportFiles))

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
noUmbrellaDependencyCase = listToMaybe . catMaybes <$> traverse checkModule (nub (map fst libraryImportFiles))
 where
  checkModule "library/ground.tung" = pure Nothing
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
  source = "bring ground.tung; show-ilk ground@fail, ground@console, ground@random, ground@state, ground@async, ground@file;"

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
