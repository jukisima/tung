-- the cli shares one project loader across running, checking, and inspection.
-- one persistent stdin session remaineþ the stable bridge used by the editor.
module Main where

import Control.Concurrent (MVar, ThreadId, forkIO, killThread, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Exception qualified as Exception
import Control.Monad (unless)
import Data.List (intercalate, isPrefixOf)
import Data.Map.Strict qualified as Map
import System.Environment (getArgs)
import System.Environment qualified as Environment
import System.Exit (exitFailure)
import System.FilePath (splitSearchPath)
import System.IO (hFlush, isEOF, stdout)
import Text.Read (readMaybe)
import Tung

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--check-stdin"] -> checkStdin checkWithImports
    ["--editor-session"] -> editorSession
    ["--language-metadata"] -> putStr languageMetadata
    ["--check", path] -> checkFile True path
    ["--check-module", path] -> checkFile False path
    ["--type-of", name, path] -> typeOfFile name path
    ["--format-stdin"] -> getContents >>= putStr . formatSource
    "--format" : paths | not (null paths) -> mapM_ formatFile paths
    ["--help"] -> putStr usage
    path : programArgs
      | not ("-" `isPrefixOf` path) -> runFile path programArgs
    _ -> putStr usage >> exitFailure

formatFile :: FilePath -> IO ()
formatFile path = do
  source <- readFile path
  _ <- Exception.evaluate (length source)
  let formatted = formatSource source
  unless (formatted == source) (writeFile path formatted)

runFile :: FilePath -> [String] -> IO ()
runFile path programArgs = withProject path \Project{projectSource, projectImports} -> do
  result <- evaluateMainWithArgsAndImports programArgs projectSource projectImports
  putStrLn result
  unless ("eval ok:" `isPrefixOf` result) exitFailure

checkFile :: Bool -> FilePath -> IO ()
checkFile runnable path = withProject path \Project{projectPath, projectSource, projectImports, projectImportPaths} ->
  case checkDiagnosticWithImports runnable projectSource projectImports of
    Nothing -> putStrLn "type ok"
    Just diagnostic@Diagnostic{diagnosticPath} -> do
      let ownerPath = maybe projectPath (\owner -> Map.findWithDefault owner owner projectImportPaths) diagnosticPath
          ownerSource = maybe projectSource (\owner -> Map.findWithDefault projectSource owner projectImports) diagnosticPath
      putStrLn (renderFileDiagnostic ownerPath ownerSource diagnostic)
      exitFailure

typeOfFile :: String -> FilePath -> IO ()
typeOfFile name path = withProject path \Project{projectSource, projectImports} ->
  case typeOfWithImports projectSource projectImports name of
    Right ty -> putStrLn ("type: " ++ ty)
    Left message -> putStrLn ("type error: " ++ message) >> exitFailure

withProject :: FilePath -> (Project -> IO ()) -> IO ()
withProject path action = do
  bookhoard <- readBookhoardImports
  loadConfiguredFile bookhoard path >>= \case
    Left message -> putStrLn ("project error: " ++ message) >> exitFailure
    Right project -> action project

checkStdin :: (String -> Map.Map String String -> String) -> IO ()
checkStdin checkSource = do
  imports <- readBookhoardImports
  source <- getContents
  putStrLn (checkSource source imports)

type EditorRequest = (Int, Int, String, String, (String, [(String, String)]))

editorSession :: IO ()
editorSession = do
  bookhoard <- readBookhoardImports
  workers <- newMVar Map.empty
  outputLock <- newMVar ()
  sessionLoop bookhoard workers outputLock

sessionLoop :: Map.Map String String -> MVar (Map.Map Int ThreadId) -> MVar () -> IO ()
sessionLoop bookhoard workers outputLock = do
  eof <- isEOF
  unless eof do
    line <- getLine
    case readMaybe line of
      Nothing -> pure ()
      Just request -> dispatchEditorRequest bookhoard workers outputLock request
    sessionLoop bookhoard workers outputLock

dispatchEditorRequest :: Map.Map String String -> MVar (Map.Map Int ThreadId) -> MVar () -> EditorRequest -> IO ()
dispatchEditorRequest bookhoard workers outputLock request@(requestId, version, command, _, _)
  | command == "cancel" = cancelEditorRequest workers requestId
  | otherwise = do
      gate <- newEmptyMVar
      thread <- forkIO $ do
        takeMVar gate
        run `Exception.finally` modifyMVar_ workers (pure . Map.delete requestId)
      modifyMVar_ workers (pure . Map.insert requestId thread)
      putMVar gate ()
 where
  run =
    sendEditorResponse outputLock requestId version (editorResponse bookhoard request)
      `Exception.catch` \exception ->
        case Exception.fromException exception of
          Just Exception.ThreadKilled -> pure ()
          _ -> sendEditorResponse outputLock requestId version ("checker session failed: " ++ Exception.displayException exception)

cancelEditorRequest :: MVar (Map.Map Int ThreadId) -> Int -> IO ()
cancelEditorRequest workers requestId = do
  thread <- modifyMVar workers \running -> pure (Map.delete requestId running, Map.lookup requestId running)
  mapM_ killThread thread

editorResponse :: Map.Map String String -> EditorRequest -> String
editorResponse bookhoard (_, _, command, name, (source, imports)) =
  let allImports = Map.union (Map.fromList imports) bookhoard
   in case command of
        "check" -> maybe "tung-ok" renderDiagnostic (checkEditorDiagnosticWithImports source allImports)
        "type" -> case typeOfWithImports source allImports name of
          Right ty -> "type: " ++ ty
          Left message -> "type error: " ++ message
        "format" -> "tung-format\n" ++ formatSource source
        _ -> "checker session failed: unknown command '" ++ command ++ "'"

sendEditorResponse :: MVar () -> Int -> Int -> String -> IO ()
sendEditorResponse outputLock requestId version output = do
  size <- Exception.evaluate (length output)
  withMVar outputLock \_ -> do
    putStrLn (intercalate "\t" ["tung-response", show requestId, show version, show size])
    putStr output
    putChar '\n'
    hFlush stdout

loadConfiguredFile :: Map.Map String String -> FilePath -> IO (Either String Project)
loadConfiguredFile bookhoard path = moduleRoots >>= \roots -> loadProjectFileWithRoots bookhoard roots path

moduleRoots :: IO [FilePath]
moduleRoots = maybe [] splitSearchPath <$> Environment.lookupEnv "TUNG_PATH"

usage :: String
usage =
  unlines
    [ "usage: tung <file.tung> [arguments...]"
    , "       tung --check <file.tung>"
    , "       tung --check-module <file.tung>"
    , "       tung --type-of <name> <file.tung>"
    , "       tung --format <file.tung>..."
    , "       tung --format-stdin"
    , "       tung --language-metadata"
    ]
