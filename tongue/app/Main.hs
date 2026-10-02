-- the cli shares one project loader across running, checking, and inspection.
-- one persistent stdin session remaineþ the stable bridge used by the editor.
module Main where

import Control.Concurrent (MVar, ThreadId, forkIO, killThread, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Exception qualified as Exception
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.List (intercalate, isPrefixOf)
import Data.Map.Strict qualified as Map
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, isEOF, stdout)
import Text.Read (readMaybe)
import Tung

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--check-stdin"] -> checkStdin
    ["--editor-session"] -> editorSession
    ["--language-metadata"] -> putStr languageMetadata
    ["--library-paths"] -> getCurrentDirectory >>= listLibraryPaths
    ["--library-paths", path] -> listLibraryPaths path
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
runFile path programArgs = withProject path \Project {projectBundle} -> do
  result <- evaluateMainBundleWithArgs programArgs projectBundle
  putStrLn result
  unless ("eval ok:" `isPrefixOf` result) exitFailure

checkFile :: Bool -> FilePath -> IO ()
checkFile runnable path = withProject path \project@Project {projectPath, projectImportPaths, projectBundle} ->
  case checkBundleDiagnostic runnable projectBundle of
    Nothing -> putStrLn "type ok"
    Just diagnostic@Diagnostic {diagnosticPath} -> do
      let ownerPath = maybe projectPath (\owner -> Map.findWithDefault owner owner projectImportPaths) diagnosticPath
          source = projectSource project
          ownerSource = maybe source (\owner -> Map.findWithDefault source owner (projectImports project)) diagnosticPath
      putStrLn (renderFileDiagnostic ownerPath ownerSource diagnostic)
      exitFailure

typeOfFile :: String -> FilePath -> IO ()
typeOfFile name path = withProject path \Project {projectBundle} ->
  case typeOfBundle projectBundle name of
    Right ty -> putStrLn ("type: " ++ ty)
    Left message -> putStrLn ("type error: " ++ message) >> exitFailure

withProject :: FilePath -> (Project -> IO ()) -> IO ()
withProject path action = do
  resolveLibraryRoots path >>= \case
    Left message -> projectFailure message
    Right roots ->
      loadProjectFileWithRoots Map.empty roots path >>= \case
        Left message -> projectFailure message
        Right project -> action project

checkStdin :: IO ()
checkStdin = do
  source <- getContents
  current <- getCurrentDirectory
  resolveLibraryRoots current >>= \case
    Left message -> projectFailure message
    Right roots ->
      loadProjectSourceWithRoots Map.empty roots (current </> "<stdin>.tung") source >>= \case
        Left message -> projectFailure message
        Right Project {projectBundle} -> putStrLn (maybe "type ok" diagnosticMessage (checkBundleDiagnostic False projectBundle))

listLibraryPaths :: FilePath -> IO ()
listLibraryPaths path =
  resolveLibraryRoots path >>= \case
    Left message -> projectFailure message
    Right roots -> mapM_ putStrLn roots

projectFailure :: String -> IO a
projectFailure message = putStrLn ("project error: " ++ message) >> exitFailure

type EditorRequest = (Int, Int, String, String, (String, [(String, String)]))

editorSession :: IO ()
editorSession = do
  workers <- newMVar Map.empty
  outputLock <- newMVar ()
  sessionLoop workers outputLock

sessionLoop :: MVar (Map.Map Int ThreadId) -> MVar () -> IO ()
sessionLoop workers outputLock = do
  eof <- isEOF
  unless eof do
    line <- getLine
    for_ (readMaybe line) (dispatchEditorRequest workers outputLock)
    sessionLoop workers outputLock

dispatchEditorRequest :: MVar (Map.Map Int ThreadId) -> MVar () -> EditorRequest -> IO ()
dispatchEditorRequest workers outputLock request@(requestId, version, command, _, _)
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
      sendEditorResponse outputLock requestId version (editorResponse request)
        `Exception.catch` \exception ->
          case Exception.fromException exception of
            Just Exception.ThreadKilled -> pure ()
            _ -> sendEditorResponse outputLock requestId version ("checker session failed: " ++ Exception.displayException exception)

cancelEditorRequest :: MVar (Map.Map Int ThreadId) -> Int -> IO ()
cancelEditorRequest workers requestId = do
  thread <- modifyMVar workers \running -> pure (Map.delete requestId running, Map.lookup requestId running)
  mapM_ killThread thread

editorResponse :: EditorRequest -> String
editorResponse (_, _, command, name, (source, imports)) =
  let sourceImports = Map.fromList imports
   in case command of
        "check" -> maybe "tung-ok" renderDiagnostic (checkEditorDiagnosticWithImports source sourceImports)
        "type" -> case typeOfWithImports source sourceImports name of
          Right ty -> "type: " ++ ty
          Left message -> "type error: " ++ message
        "format" -> "tung-format\n" ++ formatSource source
        "highlight" -> highlightSource source
        _ -> "checker session failed: unknown command '" ++ command ++ "'"

sendEditorResponse :: MVar () -> Int -> Int -> String -> IO ()
sendEditorResponse outputLock requestId version output = do
  size <- Exception.evaluate (length output)
  withMVar outputLock \_ -> do
    putStrLn (intercalate "\t" ["tung-response", show requestId, show version, show size])
    putStr output
    putChar '\n'
    hFlush stdout

usage :: String
usage =
  unlines
    [ "usage: tung <file.tung> [arguments...]",
      "       tung --check <file.tung>",
      "       tung --check-module <file.tung>",
      "       tung --type-of <name> <file.tung>",
      "       tung --format <file.tung>...",
      "       tung --format-stdin",
      "       tung --language-metadata",
      "       tung --library-paths [project-path]"
    ]
