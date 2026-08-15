-- runnable byspels and host-backed effects cross the full compiler boundary here.
module Test.Integration (group) where

import Control.Monad (filterM, when)
import Data.List (isPrefixOf, sort)
import Data.Map.Strict qualified as Map
import System.Directory (doesFileExist, getTemporaryDirectory, listDirectory, removeFile)
import System.FilePath (takeExtension, (</>))
import Test.Harness (Group, Test)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = do
  imports <- readBookhoardImports
  byspels <- byspelFiles
  Harness.group "integration" $
    map (byspelCase imports) byspels
      ++ [mainEntry imports, rejectedMain imports, fileRoundTrip imports]

byspelFiles :: IO [FilePath]
byspelFiles = do
  let directory = "../byspel"
  entries <- listDirectory directory
  sort <$> filterM doesFileExist [directory </> entry | entry <- entries, takeExtension entry == ".tung"]

byspelCase :: Imports -> FilePath -> Test
byspelCase imports path = do
  source <- readFile path
  pure $ case checkRunnableWithImports source imports of
    "type ok" -> Nothing
    actual -> Just ("byspel " ++ path ++ ": " ++ actual)

mainEntry :: Imports -> Test
mainEntry imports = do
  actual <- evaluateMainWithImports "bring ground.tung; let main: 𝟙 → 𝟙 = { _ | null }; 42" imports
  pure $ if actual == "eval ok: null" then Nothing else Just ("main entry: " ++ actual)

rejectedMain :: Imports -> Test
rejectedMain imports = do
  actual <- evaluateMainWithImports "bring ground.tung; let main: 𝟙 → 𝟙 = { _ | missing };" imports
  pure $ if "type error:" `isPrefixOf` actual then Nothing else Just ("rejected main reached evaluation: " ++ actual)

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
