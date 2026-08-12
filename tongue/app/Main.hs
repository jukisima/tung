module Main where

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
