-- the cli compiles runnable files once. the stdin commands are the stable bridge
-- used by the editor; local import discovery remains tolerant of broken buffers.
module Main where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.Directory (doesFileExist, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath (normalise, takeDirectory, (</>))
import Text.Read (readMaybe)
import Tung

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
    path : programArgs -> do
      imports <- readBookhoardImports
      source <- readFile path
      evaluateMainWithArgsAndImports programArgs source imports >>= putStrLn
    _ -> putStrLn "usage: cabal run tung -- <file.tung> [arguments...]"

checkStdin :: (String -> Map.Map String String -> String) -> IO ()
checkStdin checkSource = do
  imports <- readBookhoardImports
  source <- getContents
  putStrLn (checkSource source imports)

checkFileStdin :: FilePath -> IO ()
checkFileStdin path = do
  source <- getContents
  bookhoards <- readBookhoardImports
  local <- readLocalImports (takeDirectory path) source
  putStrLn (checkEditorWithImports source (Map.union local bookhoards))

typeOfStdin :: String -> Maybe FilePath -> IO ()
typeOfStdin name sourcePath = do
  source <- getContents
  bookhoards <- readBookhoardImports
  local <- maybe (pure Map.empty) (\path -> readLocalImports (takeDirectory path) source) sourcePath
  putStrLn $ case typeOfWithImports source (Map.union local bookhoards) name of
    Right ty -> "type: " ++ ty
    Left message -> "type error: " ++ message

checkBundleStdin :: (String -> Map.Map String String -> String) -> IO ()
checkBundleStdin checkSource = do
  bundle <- getContents
  bookhoards <- readBookhoardImports
  putStrLn $ case readMaybe bundle of
    Nothing -> "parse error: invalid editor source bundle"
    Just (source, imports) -> checkSource source (Map.union (Map.fromList imports) bookhoards)

typeOfBundleStdin :: String -> IO ()
typeOfBundleStdin name = do
  bundle <- getContents
  bookhoards <- readBookhoardImports
  putStrLn $ case readMaybe bundle of
    Nothing -> "type error: invalid editor source bundle"
    Just (source, imports) -> case typeOfWithImports source (Map.union (Map.fromList imports) bookhoards) name of
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
