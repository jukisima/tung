module Main where

import qualified Data.Map.Strict as Map
import Jbini
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      imports <- readLibraryImports
      source <- readFile path
      case checkRunnableWithImports source imports of
        "type ok" -> evaluateWithImports source imports >>= putStrLn
        msg -> putStrLn msg
    _ -> putStrLn "usage: cabal run jbini -- <file.jbini>"

readLibraryImports :: IO (Map.Map String String)
readLibraryImports =
  Map.fromList <$> traverse readOne libraryImportFiles
  where
    readOne (path, name) = do
      source <- readFile path
      pure (name, source)
