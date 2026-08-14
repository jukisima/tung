module Main where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Jbini
import System.Directory (doesFileExist, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath (normalise, takeDirectory, (</>))
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--check-stdin"] -> checkStdin checkWithImports
    ["--check-runnable-stdin"] -> checkStdin checkRunnableWithImports
    ["--check-editor-stdin"] -> checkStdin checkEditorWithImports
    ["--check-editor-stdin", path] -> checkFileStdin path
    ["--check-editor-bundle-stdin"] -> checkBundleStdin checkEditorWithImports
    ["--type-of-stdin", name] -> typeOfStdin name Nothing
    ["--type-of-stdin", name, path] -> typeOfStdin name (Just path)
    ["--type-of-bundle-stdin", name] -> typeOfBundleStdin name
    [path] -> do
      imports <- readLibraryImports
      source <- readFile path
      case checkRunnableWithImports source imports of
        "type ok" -> evaluateWithImports source imports >>= putStrLn
        msg -> putStrLn msg
    _ -> putStrLn "usage: cabal run tung -- <file.tung>"

checkStdin :: (String -> Map.Map String String -> String) -> IO ()
checkStdin checkSource = do
  imports <- readLibraryImports
  source <- getContents
  putStrLn (checkSource source imports)

checkFileStdin :: FilePath -> IO ()
checkFileStdin path = do
  source <- getContents
  libraries <- readLibraryImports
  local <- readLocalImports (takeDirectory path) source
  putStrLn (checkEditorWithImports source (Map.union local libraries))

typeOfStdin :: String -> Maybe FilePath -> IO ()
typeOfStdin name sourcePath = do
  source <- getContents
  libraries <- readLibraryImports
  local <- maybe (pure Map.empty) (\path -> readLocalImports (takeDirectory path) source) sourcePath
  putStrLn $ case typeOfWithImports source (Map.union local libraries) name of
    Right ty -> "type: " ++ ty
    Left message -> "type error: " ++ message

checkBundleStdin :: (String -> Map.Map String String -> String) -> IO ()
checkBundleStdin checkSource = do
  bundle <- getContents
  libraries <- readLibraryImports
  putStrLn $ case readMaybe bundle of
    Nothing -> "parse error: invalid editor source bundle"
    Just (source, imports) -> checkSource source (Map.union (Map.fromList imports) libraries)

typeOfBundleStdin :: String -> IO ()
typeOfBundleStdin name = do
  bundle <- getContents
  libraries <- readLibraryImports
  putStrLn $ case readMaybe bundle of
    Nothing -> "type error: invalid editor source bundle"
    Just (source, imports) -> case typeOfWithImports source (Map.union (Map.fromList imports) libraries) name of
      Right ty -> "type: " ++ ty
      Left message -> "type error: " ++ message

readLocalImports :: FilePath -> String -> IO (Map.Map String String)
readLocalImports root source = walk Set.empty root (sourceImports source)
 where
  walk _ _ [] = pure Map.empty
  walk seen base (importPath : rest) = do
    let candidate = normalise (base </> importPath)
    absolute <- normalise <$> makeAbsolute candidate
    exists <- doesFileExist absolute
    if not exists || Set.member absolute seen
      then walk seen base rest
      else do
        imported <- readFile absolute
        nested <- walk (Set.insert absolute seen) (takeDirectory absolute) (sourceImports imported)
        siblings <- walk (Set.insert absolute seen) base rest
        pure (Map.insert importPath imported (Map.union nested siblings))

sourceImports :: String -> [String]
sourceImports source = case parse source of
  Left _ -> []
  Right (Program declarations) -> concatMap declarationImports declarations

declarationImports :: Decl -> [String]
declarationImports = \case
  Import path -> [path]
  Export declaration -> declarationImports declaration
  _ -> []
