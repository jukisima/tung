-- the cli shares one project loader across running, checking, inspection, and
-- the repl. stdin commands remain the stable bridge used by the editor.
module Main where

import Control.Concurrent (MVar, ThreadId, forkIO, killThread, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Exception qualified as Exception
import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, isPrefixOf, stripPrefix)
import Data.Map.Strict qualified as Map
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.Environment qualified as Environment
import System.Exit (exitFailure)
import System.FilePath (normalise, splitSearchPath, takeDirectory)
import System.IO (hFlush, hIsTerminalDevice, isEOF, stdin, stdout)
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
    ["--check-editor-diagnostic-bundle-stdin"] -> checkDiagnosticBundleStdin
    ["--type-of-stdin", name] -> typeOfStdin name Nothing
    ["--type-of-stdin", name, path] -> typeOfStdin name (Just path)
    ["--type-of-bundle-stdin", name] -> typeOfBundleStdin name
    ["--editor-session"] -> editorSession
    ["--check", path] -> checkFile True path
    ["--check-module", path] -> checkFile False path
    ["--type-of", name, path] -> typeOfFile name path
    ["--format-stdin"] -> formatSource <$> getContents >>= putStr
    "--format" : paths | not (null paths) -> mapM_ formatFile paths
    ["--run-quiet", path] -> runFile True path []
    ["--repl"] -> repl
    ["--help"] -> putStr usage
    path : programArgs
      | not ("-" `isPrefixOf` path) -> runFile False path programArgs
    _ -> putStr usage >> exitFailure

formatFile :: FilePath -> IO ()
formatFile path = do
  source <- readFile path
  _ <- Exception.evaluate (length source)
  let formatted = formatSource source
  unless (formatted == source) (writeFile path formatted)

runFile :: Bool -> FilePath -> [String] -> IO ()
runFile quiet path programArgs = withProject path \Project{projectSource, projectImports} -> do
  result <- evaluateMainWithArgsAndImports programArgs projectSource projectImports
  if quiet
    then unless (result == "eval ok: null") (putStrLn result >> exitFailure)
    else putStrLn result >> unless ("eval ok:" `isPrefixOf` result) exitFailure

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

checkFileStdin :: FilePath -> IO ()
checkFileStdin path = do
  source <- getContents
  bookhoard <- readBookhoardImports
  loadConfiguredSource bookhoard (takeDirectory path) source >>= \case
    Left message -> putStrLn ("type error: " ++ message)
    Right Project{projectSource, projectImports} -> putStrLn (checkEditorWithImports projectSource projectImports)

typeOfStdin :: String -> Maybe FilePath -> IO ()
typeOfStdin name sourcePath = do
  source <- getContents
  bookhoard <- readBookhoardImports
  project <- case sourcePath of
    Nothing -> pure (Right (Project "<stdin>" source bookhoard Map.empty))
    Just path -> loadConfiguredSource bookhoard (takeDirectory path) source
  putStrLn $ case project of
    Left message -> "type error: " ++ message
    Right Project{projectSource, projectImports} -> case typeOfWithImports projectSource projectImports name of
      Right ty -> "type: " ++ ty
      Left message -> "type error: " ++ message

checkBundleStdin :: (String -> Map.Map String String -> String) -> IO ()
checkBundleStdin checkSource = do
  bundle <- getContents
  bookhoard <- readBookhoardImports
  putStrLn $ case readMaybe bundle of
    Nothing -> "parse error: invalid editor source bundle"
    Just (source, imports) -> checkSource source (Map.union (Map.fromList imports) bookhoard)

checkDiagnosticBundleStdin :: IO ()
checkDiagnosticBundleStdin = do
  bundle <- getContents
  bookhoard <- readBookhoardImports
  putStrLn $ case readMaybe bundle of
    Nothing -> renderDiagnostic (Diagnostic ParseDiagnostic Nothing (SourceSpan 0 0) "parse error: invalid editor source bundle")
    Just (source, imports) ->
      maybe "tung-ok" renderDiagnostic (checkEditorDiagnosticWithImports source (Map.union (Map.fromList imports) bookhoard))

typeOfBundleStdin :: String -> IO ()
typeOfBundleStdin name = do
  bundle <- getContents
  bookhoard <- readBookhoardImports
  putStrLn $ case readMaybe bundle of
    Nothing -> "type error: invalid editor source bundle"
    Just (source, imports) -> case typeOfWithImports source (Map.union (Map.fromList imports) bookhoard) name of
      Right ty -> "type: " ++ ty
      Left message -> "type error: " ++ message

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

data Repl = Repl
  { replBase :: FilePath
  , replSource :: String
  }

repl :: IO ()
repl = do
  bookhoard <- readBookhoardImports
  base <- getCurrentDirectory
  interactive <- hIsTerminalDevice stdin
  when interactive (putStrLn "tung repl; :help listeþ commands")
  replLoop interactive bookhoard (Repl base "")

replLoop :: Bool -> Map.Map String String -> Repl -> IO ()
replLoop interactive bookhoard session = do
  next <- readReplInput interactive
  case next of
    Nothing -> pure ()
    Just input ->
      runReplInput bookhoard session (trim input) >>= \case
        Nothing -> pure ()
        Just session2 -> replLoop interactive bookhoard session2

readReplInput :: Bool -> IO (Maybe String)
readReplInput interactive = do
  when interactive (putStr "tung> " >> hFlush stdout)
  eof <- isEOF
  if eof
    then pure Nothing
    else do
      line <- getLine
      if trim line == ":{"
        then Just <$> readReplBlock interactive []
        else pure (Just line)

readReplBlock :: Bool -> [String] -> IO String
readReplBlock interactive linesRead = do
  when interactive (putStr "....> " >> hFlush stdout)
  eof <- isEOF
  if eof
    then pure (unlines (reverse linesRead))
    else do
      line <- getLine
      if trim line == ":}"
        then pure (unlines (reverse linesRead))
        else readReplBlock interactive (line : linesRead)

runReplInput :: Map.Map String String -> Repl -> String -> IO (Maybe Repl)
runReplInput _ _ input | input `elem` [":quit", ":q"] = pure Nothing
runReplInput _ session ":clear" = putStrLn "cleared" >> pure (Just session{replSource = ""})
runReplInput _ session ":help" = putStr replUsage >> pure (Just session)
runReplInput bookhoard session ":check" = do
  withReplProject bookhoard session (replSource session) \project ->
    putStrLn (checkWithImports (projectSource project) (projectImports project))
  pure (Just session)
runReplInput bookhoard session ":run" = do
  withReplProject bookhoard session (replSource session) \project ->
    evaluateMainWithImports (projectSource project) (projectImports project) >>= putStrLn
  pure (Just session)
runReplInput bookhoard session input
  | Just name <- stripPrefix ":type " input = do
      withReplProject bookhoard session (replSource session) \project ->
        putStrLn $ case typeOfWithImports (projectSource project) (projectImports project) (trim name) of
          Right ty -> "type: " ++ ty
          Left message -> "type error: " ++ message
      pure (Just session)
  | Just path <- stripPrefix ":load " input = Just <$> loadReplFile bookhoard session (trim path)
  | ":" `isPrefixOf` input = putStrLn "unknown repl command; use :help" >> pure (Just session)
  | null input = pure (Just session)
  | otherwise = Just <$> runReplSource bookhoard session input

loadReplFile :: Map.Map String String -> Repl -> FilePath -> IO Repl
loadReplFile bookhoard session path = do
  absolute <- normalise <$> makeAbsolute path
  loadConfiguredFile bookhoard absolute >>= \case
    Left message -> putStrLn ("project error: " ++ message) >> pure session
    Right Project{projectSource} -> do
      putStrLn ("loaded " ++ absolute)
      pure (Repl (takeDirectory absolute) projectSource)

runReplSource :: Map.Map String String -> Repl -> String -> IO Repl
runReplSource bookhoard session input = case parse input of
  Left message -> putStrLn ("parse error: " ++ message) >> pure session
  Right (Program declarations)
    | not (null declarations) && all persistent declarations -> define
    | otherwise -> evaluateInput
 where
  candidate = combineSource (replSource session) input
  define = do
    accepted <- withReplProject bookhoard session candidate \project -> do
      let result = checkWithImports (projectSource project) (projectImports project)
      putStrLn result
      pure (result == "type ok")
    pure (if accepted then session{replSource = candidate} else session)
  evaluateInput = do
    withReplProject bookhoard session candidate \project ->
      evaluateWithImports (projectSource project) (projectImports project) >>= putStrLn
    pure session
  persistent = \case
    Let "_" _ _ -> False
    _ -> True

withReplProject :: Map.Map String String -> Repl -> String -> (Project -> IO a) -> IO a
withReplProject bookhoard Repl{replBase} source action =
  loadConfiguredSource bookhoard replBase source >>= \case
    Left message -> putStrLn ("project error: " ++ message) >> action (Project "<repl>" source bookhoard Map.empty)
    Right project -> action project

combineSource :: String -> String -> String
combineSource "" addition = addition
combineSource source addition = source ++ "\n" ++ addition

loadConfiguredFile :: Map.Map String String -> FilePath -> IO (Either String Project)
loadConfiguredFile bookhoard path = moduleRoots >>= \roots -> loadProjectFileWithRoots bookhoard roots path

loadConfiguredSource :: Map.Map String String -> FilePath -> String -> IO (Either String Project)
loadConfiguredSource bookhoard base source = moduleRoots >>= \roots -> loadProjectSourceWithRoots bookhoard roots base source

moduleRoots :: IO [FilePath]
moduleRoots = maybe [] splitSearchPath <$> Environment.lookupEnv "TUNG_PATH"

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

usage :: String
usage =
  unlines
    [ "usage: tung <file.tung> [arguments...]"
    , "       tung --check <file.tung>"
    , "       tung --check-module <file.tung>"
    , "       tung --type-of <name> <file.tung>"
    , "       tung --format <file.tung>..."
    , "       tung --format-stdin"
    , "       tung --repl"
    ]

replUsage :: String
replUsage =
  unlines
    [ ":type name  show an inferred term type"
    , ":load path  replace the session with a file"
    , ":check      check all session declarations"
    , ":run        run the session main"
    , ":clear      remove session declarations"
    , ":{ and :}   enter multiline source"
    , "yield expr   evaluate without keeping the result"
    , ":quit       leave the repl"
    ]
