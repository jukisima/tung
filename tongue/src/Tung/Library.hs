{-# LANGUAGE OverloadedStrings #-}

-- manifest libraries supply ordinary module roots without compiler-owned names.
module Tung.Library
  ( resolveLibraryRoots,
    readLibraryImportFiles,
    readLibraryImports,
  )
where

import Control.Exception (finally)
import Control.Monad (foldM, forM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Char (isHexDigit, toLower)
import Data.List (intercalate, isInfixOf, isPrefixOf, nub, sort)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Yaml qualified as Yaml
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, removeFile, removePathForcibly, renameDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (addTrailingPathSeparator, isAbsolute, normalise, splitDirectories, splitSearchPath, takeDirectory, takeExtension, (</>))
import System.IO (hClose, openTempFile)
import System.IO.Error (tryIOError)
import System.Process (readProcessWithExitCode)

data Library = Library
  { libraryName :: String,
    librarySource :: LibrarySource
  }

data LibrarySource
  = GitSource String String FilePath
  | PathSource FilePath
  deriving stock (Eq)

instance FromJSON LibrarySource where
  parseJSON = withObject "library" \fields -> do
    rejectUnknown "library" ["repo", "hash", "subdir", "path"] fields
    repository <- fields .:? "repo"
    commit <- fields .:? "hash"
    subdirectory <- fields .:? "subdir"
    path <- fields .:? "path"
    case (repository, commit, path) of
      (Just source, Just revision, Nothing) -> pure (GitSource source revision (maybe "." id subdirectory))
      (Nothing, Nothing, Just directory) | subdirectory == Nothing -> pure (PathSource directory)
      _ -> fail "library needs either 'path' or 'repo' with 'hash' and optional 'subdir'"

newtype Manifest = Manifest [(String, LibrarySource)]

instance FromJSON Manifest where
  parseJSON = withObject "tung.yaml" \fields -> do
    rejectUnknown "manifest" ["dependencies"] fields
    dependencies <- fields .: "dependencies" :: Parser Aeson.Value
    Manifest <$> withObject "dependencies" (traverse parseEntry . KeyMap.toAscList) dependencies
    where
      parseEntry (name, source) = (Key.toString name,) <$> parseJSON source

rejectUnknown :: String -> [String] -> Aeson.Object -> Parser ()
rejectUnknown context allowed fields =
  case filter (`notElem` allowed) (map Key.toString (KeyMap.keys fields)) of
    [] -> pure ()
    unknown : _ -> fail ("unknown " ++ context ++ " field '" ++ unknown ++ "'")

data LibraryUse = LibraryUse
  { useName :: String,
    useSource :: LibrarySource,
    useManifest :: FilePath
  }

data SourceKey = RepositoryKey String FilePath | PathKey FilePath
  deriving stock (Eq, Ord)

data Resolved = Resolved
  { usesByName :: Map.Map String LibraryUse,
    usesBySource :: Map.Map SourceKey LibraryUse
  }

emptyResolved :: Resolved
emptyResolved = Resolved Map.empty Map.empty

type Resolver = StateT Resolved (ExceptT String IO)

resolveIO :: IO (Either String a) -> Resolver a
resolveIO = lift . ExceptT

manifestName :: FilePath
manifestName = "tung.yaml"

legacyManifestName :: FilePath
legacyManifestName = "tung.libraries"

resolveLibraryRoots :: FilePath -> IO (Either String [FilePath])
resolveLibraryRoots owner = do
  manifest <- findManifest owner
  pinned <- case manifest of
    Left message -> pure (Left message)
    Right Nothing -> pure (Right [])
    Right (Just path) -> runExceptT (evalStateT (resolveManifest (takeDirectory path </> ".tung" </> "libraries") path) emptyResolved)
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
    Right files -> case foldM add Map.empty files of
      Left message -> pure (Left message)
      Right paths -> fmap sequence (traverse readSource paths)
  where
    add paths (path, name) = case Map.lookup name paths of
      Nothing -> Right (Map.insert name path paths)
      Just other -> Left ("library module '" ++ name ++ "' occurs at both '" ++ other ++ "' and '" ++ path ++ "'")
    readSource path = do
      result <- tryIOError (Text.readFile path)
      pure (either (\failure -> Left ("cannot read library module '" ++ path ++ "': " ++ show failure)) (Right . Text.unpack) result)

findManifest :: FilePath -> IO (Either String (Maybe FilePath))
findManifest owner = do
  absolute <- makeAbsolute owner
  file <- doesFileExist absolute
  search (if file then takeDirectory absolute else absolute)
  where
    search directory = do
      manifest <- manifestAt directory
      case manifest of
        Right Nothing ->
          let parent = takeDirectory directory
           in if parent == directory then pure (Right Nothing) else search parent
        result -> pure result

manifestAt :: FilePath -> IO (Either String (Maybe FilePath))
manifestAt directory = do
  let path = directory </> manifestName
      legacy = directory </> legacyManifestName
  present <- doesFileExist path
  old <- doesFileExist legacy
  if old
    then pure (Left ("replace '" ++ legacy ++ "' with '" ++ path ++ "'"))
    else if present then Right . Just <$> canonicalizePath path else pure (Right Nothing)

resolveManifest :: FilePath -> FilePath -> Resolver [FilePath]
resolveManifest cache manifest = do
  libraries <- resolveIO (readManifest manifest)
  fmap concat $ forM libraries \Library {libraryName, librarySource} -> do
    source <- resolveIO (resolveSource manifest librarySource)
    known <- get
    (alreadyResolved, next) <- either (lift . throwE) pure (registerUse known (LibraryUse libraryName source manifest))
    put next
    if alreadyResolved
      then pure []
      else do
        root <- resolveIO (libraryRoot cache libraryName source)
        nested <- resolveIO (manifestAt root)
        children <- maybe (pure []) (resolveManifest cache) nested
        pure (root : children)

registerUse :: Resolved -> LibraryUse -> Either String (Bool, Resolved)
registerUse known use = do
  case Map.lookup (useName use) (usesByName known) of
    Just earlier
      | useSource earlier /= useSource use ->
          Left (sourceConflict ("library '" ++ useName use ++ "'") earlier use)
    _ -> Right ()
  let key = sourceKey (useSource use)
  case Map.lookup key (usesBySource known) of
    Just earlier
      | useName earlier /= useName use || useSource earlier /= useSource use ->
          Left (sourceConflict (sourceLabel key) earlier use)
    _ -> Right ()
  let alreadyResolved = Map.member key (usesBySource known)
      updated =
        Resolved
          { usesByName = Map.insertWith (\_ earlier -> earlier) (useName use) use (usesByName known),
            usesBySource = Map.insertWith (\_ earlier -> earlier) key use (usesBySource known)
          }
  pure (alreadyResolved, updated)

sourceKey :: LibrarySource -> SourceKey
sourceKey = \case
  GitSource repository _ subdirectory -> RepositoryKey repository subdirectory
  PathSource path -> PathKey path

sourceLabel :: SourceKey -> String
sourceLabel = \case
  RepositoryKey repository "." -> "repository '" ++ repository ++ "'"
  RepositoryKey repository subdirectory -> "repository '" ++ repository ++ "' subdirectory '" ++ subdirectory ++ "'"
  PathKey path -> "path '" ++ path ++ "'"

sourceConflict :: String -> LibraryUse -> LibraryUse -> String
sourceConflict subject earlier later =
  "conflicting sources for "
    ++ subject
    ++ ": '"
    ++ useManifest earlier
    ++ "' declares library '"
    ++ useName earlier
    ++ "' "
    ++ describeSource (useSource earlier)
    ++ ", but '"
    ++ useManifest later
    ++ "' declares library '"
    ++ useName later
    ++ "' "
    ++ describeSource (useSource later)

describeSource :: LibrarySource -> String
describeSource = \case
  GitSource repository commit "." -> "from '" ++ repository ++ "' at " ++ commit
  GitSource repository commit subdirectory -> "from '" ++ repository ++ "' at " ++ commit ++ " in '" ++ subdirectory ++ "'"
  PathSource path -> "from local path '" ++ path ++ "'"

readManifest :: FilePath -> IO (Either String [Library])
readManifest path = do
  decoded <- tryIOError (Yaml.decodeFileWithWarnings path)
  pure $ case decoded of
    Left failure -> Left ("cannot read '" ++ path ++ "': " ++ show failure)
    Right (Left failure) -> Left (path ++ ": " ++ Yaml.prettyPrintParseException failure)
    Right (Right (warnings, Manifest entries))
      | not (null warnings) -> Left (path ++ ": " ++ intercalate "; " (map show warnings))
      | otherwise -> traverse validateEntry entries
  where
    validateEntry (name, source)
      | not (validName name) = Left (path ++ ": invalid library name '" ++ name ++ "'")
      | otherwise = Library name <$> validateSource name source
    validateSource name = \case
      GitSource repository commit subdirectory
        | null repository -> Left (path ++ ": library '" ++ name ++ "' hath an empty repository")
        | not (validCommit commit) -> Left (path ++ ": library '" ++ name ++ "' needeth a full 40- or 64-digit hexadecimal commit hash")
        | isAbsolute subdirectory || ".." `elem` splitDirectories subdirectory -> Left (path ++ ": library '" ++ name ++ "' hath an invalid subdirectory")
        | otherwise -> Right (GitSource repository (map toLower commit) (normalise subdirectory))
      PathSource directory
        | null directory -> Left (path ++ ": library '" ++ name ++ "' hath an empty path")
        | otherwise -> Right (PathSource directory)
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

resolveSource :: FilePath -> LibrarySource -> IO (Either String LibrarySource)
resolveSource manifest = \case
  GitSource repository commit subdirectory -> pure (Right (GitSource (repositoryAt manifest repository) commit subdirectory))
  PathSource directory -> do
    let local = if isAbsolute directory then directory else takeDirectory manifest </> directory
    present <- doesDirectoryExist local
    if not present
      then pure (Left (manifest ++ ": library path '" ++ local ++ "' does not exist"))
      else do
        resolved <- tryIOError (canonicalizePath local)
        pure $ case resolved of
          Left failure -> Left (manifest ++ ": cannot resolve library path '" ++ local ++ "': " ++ show failure)
          Right root -> Right (PathSource root)

libraryRoot :: FilePath -> String -> LibrarySource -> IO (Either String FilePath)
libraryRoot cache name = \case
  GitSource repository commit subdirectory ->
    checkout cache name repository commit >>= \case
      Left message -> pure (Left message)
      Right root -> selectedSubdirectory root subdirectory
  PathSource path -> pure (Right path)

selectedSubdirectory :: FilePath -> FilePath -> IO (Either String FilePath)
selectedSubdirectory root "." = pure (Right root)
selectedSubdirectory root subdirectory = do
  let selected = root </> subdirectory
  present <- doesDirectoryExist selected
  if not present
    then pure (Left ("library subdirectory '" ++ subdirectory ++ "' does not exist in '" ++ root ++ "'"))
    else do
      resolved <- tryIOError ((,) <$> canonicalizePath root <*> canonicalizePath selected)
      pure $ case resolved of
        Left failure -> Left ("cannot resolve library subdirectory '" ++ selected ++ "': " ++ show failure)
        Right (checkoutRoot, libraryPath)
          | addTrailingPathSeparator checkoutRoot `isPrefixOf` addTrailingPathSeparator libraryPath -> Right libraryPath
          | otherwise -> Left ("library subdirectory '" ++ selected ++ "' escapeþ checkout '" ++ checkoutRoot ++ "'")

checkout :: FilePath -> String -> String -> String -> IO (Either String FilePath)
checkout cache name repository commit = do
  let parent = cache </> name
      target = parent </> commit
  present <- doesDirectoryExist target
  if present
    then verify target commit
    else do
      createDirectoryIfMissing True parent
      (temporary, handle) <- openTempFile parent "checkout-"
      hClose handle
      removeFile temporary
      let cleanup = do
            remaining <- doesDirectoryExist temporary
            if remaining then removePathForcibly temporary else pure ()
      ( do
          cloned <- git Nothing ["clone", "--quiet", "--no-checkout", "--", repository, temporary]
          case cloned of
            Left message -> pure (Left message)
            Right _ -> do
              checked <- git (Just temporary) ["checkout", "--quiet", "--detach", commit]
              case checked of
                Left message -> pure (Left message)
                Right _ -> do
                  verified <- verify temporary commit
                  case verified of
                    Left message -> pure (Left message)
                    Right _ -> do
                      moved <- tryIOError (renameDirectory temporary target)
                      case moved of
                        Right () -> pure (Right target)
                        Left failure -> do
                          concurrent <- doesDirectoryExist target
                          if concurrent
                            then verify target commit
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
