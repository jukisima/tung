-- git-pinned libraries supply ordinary module roots without compiler-owned names.
module Tung.Library (
  resolveLibraryRoots,
  readLibraryImportFiles,
  readLibraryImports,
) where

import Control.Exception (finally)
import Control.Monad (foldM)
import Data.Char (isHexDigit, toLower)
import Data.List (intercalate, isInfixOf, nub, sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, removeFile, removePathForcibly, renameDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, normalise, splitSearchPath, takeDirectory, takeExtension, (</>))
import System.IO (hClose, openTempFile)
import System.IO.Error (tryIOError)
import System.Process (readProcessWithExitCode)

data Library = Library
  { libraryName :: String
  , libraryRepository :: String
  , libraryCommit :: String
  }

data Pin = Pin
  { pinName :: String
  , pinRepository :: String
  , pinCommit :: String
  , pinManifest :: FilePath
  }

data Resolved = Resolved
  { pinsByName :: Map.Map String Pin
  , pinsByRepository :: Map.Map String Pin
  }

emptyResolved :: Resolved
emptyResolved = Resolved Map.empty Map.empty

manifestName :: FilePath
manifestName = "tung.libraries"

resolveLibraryRoots :: FilePath -> IO (Either String [FilePath])
resolveLibraryRoots owner = do
  manifest <- findManifest owner
  pinned <- case manifest of
    Nothing -> pure (Right [])
    Just path -> fmap (fmap snd) (resolveManifest (takeDirectory path </> ".tung" </> "libraries") emptyResolved path)
  case pinned of
    Left message -> pure (Left message)
    Right roots -> do
      configured <- maybe [] (filter (not . null) . splitSearchPath) <$> lookupEnv "TUNG_PATH"
      absolute <- traverse makeAbsolute configured
      pure (Right (nub (roots ++ absolute)))

readLibraryImportFiles :: FilePath -> IO (Either String [(FilePath, String)])
readLibraryImportFiles owner =
  resolveLibraryRoots owner >>= \case
    Left message -> pure (Left message)
    Right roots -> do
      discovered <- traverse (\root -> fmap (map (\name -> (root </> name, name))) <$> discover root "") roots
      pure (concat <$> sequence discovered)

readLibraryImports :: FilePath -> IO (Either String (Map.Map String String))
readLibraryImports owner =
  readLibraryImportFiles owner >>= \case
    Left message -> pure (Left message)
    Right files -> do
      unique <- foldM add (Right Map.empty) files
      case unique of
        Left message -> pure (Left message)
        Right paths -> do
          sources <- traverse readSource paths
          pure (sequence sources)
 where
  add (Left message) _ = pure (Left message)
  add (Right paths) (path, name) = pure $ case Map.lookup name paths of
    Nothing -> Right (Map.insert name path paths)
    Just other -> Left ("library module '" ++ name ++ "' occurs at both '" ++ other ++ "' and '" ++ path ++ "'")
  readSource path = do
    result <- tryIOError (Text.readFile path)
    pure (either (\failure -> Left ("cannot read library module '" ++ path ++ "': " ++ show failure)) (Right . Text.unpack) result)

findManifest :: FilePath -> IO (Maybe FilePath)
findManifest owner = do
  absolute <- makeAbsolute owner
  file <- doesFileExist absolute
  search (if file then takeDirectory absolute else absolute)
 where
  search directory = do
    let manifest = directory </> manifestName
    present <- doesFileExist manifest
    if present
      then Just <$> canonicalizePath manifest
      else
        let parent = takeDirectory directory
         in if parent == directory then pure Nothing else search parent

resolveManifest :: FilePath -> Resolved -> FilePath -> IO (Either String (Resolved, [FilePath]))
resolveManifest cache resolved manifest =
  readManifest manifest >>= \case
    Left message -> pure (Left message)
    Right libraries -> go resolved [] libraries
 where
  go known roots [] = pure (Right (known, roots))
  go known roots (library : rest) = do
    let repository = repositoryAt manifest (libraryRepository library)
        pin = Pin (libraryName library) repository (libraryCommit library) manifest
    case registerPin known pin of
      Left message -> pure (Left message)
      Right (alreadyResolved, next)
        | alreadyResolved -> go next roots rest
        | otherwise ->
            checkout cache library{libraryRepository = repository} >>= \case
              Left message -> pure (Left message)
              Right root -> do
                nested <- doesFileExist (root </> manifestName)
                dependencies <-
                  if nested
                    then resolveManifest cache next (root </> manifestName)
                    else pure (Right (next, []))
                case dependencies of
                  Left message -> pure (Left message)
                  Right (afterChildren, children) -> go afterChildren (roots ++ [root] ++ children) rest

registerPin :: Resolved -> Pin -> Either String (Bool, Resolved)
registerPin known pin = do
  case Map.lookup (pinName pin) (pinsByName known) of
    Just earlier
      | pinRepository earlier /= pinRepository pin || pinCommit earlier /= pinCommit pin ->
          Left (pinConflict ("library '" ++ pinName pin ++ "'") earlier pin)
    _ -> Right ()
  case Map.lookup (pinRepository pin) (pinsByRepository known) of
    Just earlier
      | pinName earlier /= pinName pin || pinCommit earlier /= pinCommit pin ->
          Left (pinConflict ("repository '" ++ pinRepository pin ++ "'") earlier pin)
    _ -> Right ()
  let alreadyResolved = Map.member (pinRepository pin) (pinsByRepository known)
      updated =
        Resolved
          { pinsByName = Map.insertWith (\_ earlier -> earlier) (pinName pin) pin (pinsByName known)
          , pinsByRepository = Map.insertWith (\_ earlier -> earlier) (pinRepository pin) pin (pinsByRepository known)
          }
  pure (alreadyResolved, updated)

pinConflict :: String -> Pin -> Pin -> String
pinConflict subject earlier later =
  "conflicting pins for "
    ++ subject
    ++ ": '"
    ++ pinManifest earlier
    ++ "' pins library '"
    ++ pinName earlier
    ++ "' from '"
    ++ pinRepository earlier
    ++ "' at "
    ++ pinCommit earlier
    ++ ", but '"
    ++ pinManifest later
    ++ "' pins library '"
    ++ pinName later
    ++ "' from '"
    ++ pinRepository later
    ++ "' at "
    ++ pinCommit later

readManifest :: FilePath -> IO (Either String [Library])
readManifest path = do
  content <- tryIOError (Text.readFile path)
  pure $ case content of
    Left errorMessage -> Left ("cannot read '" ++ path ++ "': " ++ show errorMessage)
    Right source -> do
      libraries <- traverse (parseLine path) (zip [1 :: Int ..] (Text.lines source))
      let entries = concat libraries
          names = map libraryName entries
      if length names == Set.size (Set.fromList names)
        then Right entries
        else Left ("duplicate library name in '" ++ path ++ "'")

parseLine :: FilePath -> (Int, Text.Text) -> Either String [Library]
parseLine path (number, raw)
  | Text.null line || Text.pack "#" `Text.isPrefixOf` line = Right []
  | otherwise = case map (Text.unpack . Text.strip) (Text.splitOn (Text.pack "\t") raw) of
      [name, repository, commit]
        | validName name && not (null repository) && validCommit commit -> Right [Library name repository (map toLower commit)]
      _ -> Left (path ++ ":" ++ show number ++ ": expected name, git repository, and full commit hash separated by tabs")
 where
  line = Text.strip raw
  validName name = not (null name) && all (\character -> character `elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_")) name
  validCommit commit = length commit `elem` [40, 64] && all isHexDigit commit

repositoryAt :: FilePath -> String -> String
repositoryAt manifest repository
  | isAbsolute repository = normalise repository
  | "://" `isInfixOf` repository || scpStyle repository = repository
  | otherwise = normalise (takeDirectory manifest </> repository)
 where
  scpStyle value = case break (== ':') value of
    (host, ':' : _) -> not (null host) && all (/= '/') host
    _ -> False

checkout :: FilePath -> Library -> IO (Either String FilePath)
checkout cache Library{libraryName, libraryRepository, libraryCommit} = do
  let parent = cache </> libraryName
      target = parent </> libraryCommit
  present <- doesDirectoryExist target
  if present
    then verify target libraryCommit
    else do
      createDirectoryIfMissing True parent
      (temporary, handle) <- openTempFile parent "checkout-"
      hClose handle
      removeFile temporary
      let cleanup = do
            remaining <- doesDirectoryExist temporary
            if remaining then removePathForcibly temporary else pure ()
      ( do
          cloned <- git Nothing ["clone", "--quiet", "--no-checkout", "--", libraryRepository, temporary]
          case cloned of
            Left message -> pure (Left message)
            Right _ -> do
              checked <- git (Just temporary) ["checkout", "--quiet", "--detach", libraryCommit]
              case checked of
                Left message -> pure (Left message)
                Right _ -> do
                  verified <- verify temporary libraryCommit
                  case verified of
                    Left message -> pure (Left message)
                    Right _ -> do
                      moved <- tryIOError (renameDirectory temporary target)
                      case moved of
                        Right () -> pure (Right target)
                        Left failure -> do
                          concurrent <- doesDirectoryExist target
                          if concurrent
                            then verify target libraryCommit
                            else pure (Left ("cannot cache library checkout '" ++ target ++ "': " ++ show failure))
        )
        `finally` cleanup

verify :: FilePath -> String -> IO (Either String FilePath)
verify root commit =
  git (Just root) ["rev-parse", "HEAD"] >>= \case
    Left message -> pure (Left message)
    Right actual
      | actual /= commit -> pure (Left ("library checkout '" ++ root ++ "' is at " ++ actual ++ ", expected " ++ commit))
      | otherwise ->
          git (Just root) ["status", "--porcelain"] >>= \case
            Left message -> pure (Left message)
            Right "" -> pure (Right root)
            Right _ -> pure (Left ("library checkout '" ++ root ++ "' has local changes"))

git :: Maybe FilePath -> [String] -> IO (Either String String)
git directory arguments = do
  let command = maybe arguments (\root -> ["-C", root] ++ arguments) directory
  result <- tryIOError (readProcessWithExitCode "git" command "")
  pure $ case result of
    Left failure -> Left ("cannot run git " ++ intercalate " " command ++ ": " ++ show failure)
    Right (ExitSuccess, output, _) -> Right (trim output)
    Right (ExitFailure _, _, errorMessage) -> Left ("git " ++ intercalate " " command ++ ": " ++ trim errorMessage)
 where
  trim = Text.unpack . Text.strip . Text.pack

discover :: FilePath -> FilePath -> IO (Either String [FilePath])
discover root relative = do
  let directory = root </> relative
  contents <- tryIOError (listDirectory directory)
  case contents of
    Left failure -> pure (Left ("cannot list library root '" ++ directory ++ "': " ++ show failure))
    Right entries -> do
      children <- traverse visit (sort entries)
      pure (concat <$> sequence children)
 where
  visit entry
    | entry == ".git" || entry == ".tung" = pure (Right [])
    | otherwise = do
        let name = relative </> entry
        directory <- doesDirectoryExist (root </> name)
        if directory
          then discover root name
          else pure (Right [name | takeExtension name == ".tung"])
