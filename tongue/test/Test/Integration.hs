module Test.Integration (group) where

import Control.Monad (when)
import Data.Map.Strict qualified as Map
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import Test.Harness (Group, Test)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = do
  imports <- readLibraryImports
  Harness.group "integration" $
    map (sampleCase imports) samples
      ++ [fileRoundTrip imports]

samples :: [FilePath]
samples =
  [ "../sample/asynchronous.tung"
  , "../sample/fizzbuzz.tung"
  , "../sample/lambda.tung"
  , "../sample/luck.tung"
  , "../sample/multishot.tung"
  , "../sample/scoreboard.tung"
  ]

sampleCase :: Imports -> FilePath -> Test
sampleCase imports path = do
  source <- readFile path
  pure $ case checkRunnableWithImports source imports of
    "type ok" -> Nothing
    actual -> Just ("sample " ++ path ++ ": " ++ actual)

fileRoundTrip :: Imports -> Test
fileRoundTrip imports = do
  directory <- getTemporaryDirectory
  let path = directory ++ "/tung-file-effect-test.txt"
      source = "bring ground.tung; let _ = '" ++ path ++ "' write-file 'hello'; let _ = '" ++ path ++ "' append-file ' world'; '" ++ path ++ "' read-file"
  removeIfPresent path
  actual <- evaluateWithImports source imports
  removeIfPresent path
  pure $ if actual == "eval ok: 'hello world'" then Nothing else Just ("file round trip: " ++ actual)

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = doesFileExist path >>= \present -> when present (removeFile path)

type Imports = Map.Map String String
