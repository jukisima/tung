{- | filesystem project loading is kept outside parsing and import semantics.
the loader resolveþ relative brings into the flat source bundle consumed by the
checker, and rejecteþ path collisions instead of silently choosing one file.
-}
module Tung.Project (
  Project (..),
  loadProjectFile,
  loadProjectFileWithRoots,
  loadProjectSource,
  loadProjectSourceWithRoots,
) where

import Control.Monad (filterM, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, execStateT, gets, modify')
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (canonicalizePath, doesFileExist, makeAbsolute)
import System.FilePath (normalise, takeDirectory, (</>))
import System.IO.Error (tryIOError)
import Tung.Parse (parse)
import Tung.Syntax (Decl (..), Program (..))

data Project = Project
  { projectPath :: FilePath
  , projectSource :: String
  , projectImports :: Map.Map String String
  , projectImportPaths :: Map.Map String FilePath
  }
  deriving stock (Eq, Show)

data Loaded = Loaded
  { loadedFiles :: Map.Map String (FilePath, String)
  , loadedPaths :: Set.Set FilePath
  }

type Loader = StateT Loaded (ExceptT String IO)

loadProjectFile :: Map.Map String String -> FilePath -> IO (Either String Project)
loadProjectFile builtins = loadProjectFileWithRoots builtins []

loadProjectFileWithRoots :: Map.Map String String -> [FilePath] -> FilePath -> IO (Either String Project)
loadProjectFileWithRoots builtins roots path = runExceptT do
  absolute <- liftIOError "resolve" path (canonicalizePath =<< makeAbsolute path)
  absoluteRoots <- traverse absoluteRoot roots
  source <- readSource absolute
  loadProjectSourceM builtins absoluteRoots absolute (takeDirectory absolute) source

loadProjectSource :: Map.Map String String -> FilePath -> String -> IO (Either String Project)
loadProjectSource builtins = loadProjectSourceWithRoots builtins []

loadProjectSourceWithRoots :: Map.Map String String -> [FilePath] -> FilePath -> String -> IO (Either String Project)
loadProjectSourceWithRoots builtins roots base source = runExceptT do
  absolute <- liftIOError "resolve" base (normalise <$> makeAbsolute base)
  absoluteRoots <- traverse absoluteRoot roots
  loadProjectSourceM builtins absoluteRoots "<input>" absolute source

loadProjectSourceM :: Map.Map String String -> [FilePath] -> FilePath -> FilePath -> String -> ExceptT String IO Project
loadProjectSourceM builtins roots owner base source = do
  loaded <- execStateT (discover builtins roots owner base source) (Loaded Map.empty Set.empty)
  let locals = snd <$> loadedFiles loaded
      paths = fst <$> loadedFiles loaded
  pure (Project owner source (Map.union locals builtins) paths)

discover :: Map.Map String String -> [FilePath] -> FilePath -> FilePath -> String -> Loader ()
discover builtins roots owner base source = mapM_ (loadImport builtins roots owner base) (sourceImports source)

loadImport :: Map.Map String String -> [FilePath] -> FilePath -> FilePath -> FilePath -> Loader ()
loadImport builtins roots owner base importPath = do
  candidates <- importCandidates roots base importPath
  case candidates of
    []
      | Map.member importPath builtins -> pure ()
      | otherwise -> failLoader ("missing bring '" ++ importPath ++ "' from '" ++ owner ++ "'")
    [candidate] -> loadCandidate candidate
    paths ->
      failLoader
        ( "ambiguous bring '"
            ++ importPath
            ++ "' found at "
            ++ intercalate ", " (map (\path -> "'" ++ path ++ "'") paths)
        )
 where
  loadCandidate candidate = do
    absolute <- liftIOErrorL "resolve" candidate (canonicalizePath candidate)
    known <- gets (Map.lookup importPath . loadedFiles)
    case known of
      Just (other, _)
        | other /= absolute ->
            failLoader
              ( "ambiguous bring '"
                  ++ importPath
                  ++ "' resolveþ to both '"
                  ++ other
                  ++ "' and '"
                  ++ absolute
                  ++ "'"
              )
      _ -> pure ()
    visited <- gets (Set.member absolute . loadedPaths)
    unless visited do
      imported <- readSourceL absolute
      modify' \loaded ->
        loaded
          { loadedFiles = Map.insert importPath (absolute, imported) (loadedFiles loaded)
          , loadedPaths = Set.insert absolute (loadedPaths loaded)
          }
      discover builtins roots absolute (takeDirectory absolute) imported

importCandidates :: [FilePath] -> FilePath -> FilePath -> Loader [FilePath]
importCandidates roots base importPath = do
  let local = normalise (base </> importPath)
  localExists <- liftIO (doesFileExist local)
  if localExists
    then pure [local]
    else filterM (liftIO . doesFileExist) [normalise (root </> importPath) | root <- roots]

absoluteRoot :: FilePath -> ExceptT String IO FilePath
absoluteRoot root = liftIOError "resolve module root" root (normalise <$> makeAbsolute root)

sourceImports :: String -> [FilePath]
sourceImports source = case parse source of
  Left _ -> []
  Right (Program declarations) -> concatMap declarationImports declarations

declarationImports :: Decl -> [FilePath]
declarationImports = \case
  Import path _ -> [path]
  Export declaration -> declarationImports declaration
  _ -> []

readSource :: FilePath -> ExceptT String IO String
readSource path = liftIOError "read" path (Text.unpack <$> Text.readFile path)

readSourceL :: FilePath -> Loader String
readSourceL path = lift (readSource path)

liftIOError :: String -> FilePath -> IO a -> ExceptT String IO a
liftIOError action path operation = do
  result <- liftIO (tryIOError operation)
  either (throwE . renderIoError action path . show) pure result

liftIOErrorL :: String -> FilePath -> IO a -> Loader a
liftIOErrorL action path operation = lift (liftIOError action path operation)

failLoader :: String -> Loader a
failLoader = lift . throwE

renderIoError :: String -> FilePath -> String -> String
renderIoError action path message = "cannot " ++ action ++ " '" ++ path ++ "': " ++ message
