-- pinned git checkouts supply ordinary modules and remain fixed after source changes.
module Test.Library (group) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Char (toUpper)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly, renameDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (readProcessWithExitCode)
import Test.Harness (Group, Test, expect)
import Test.Harness qualified as Harness
import Tung (loadProjectFileWithRoots, projectImports, resolveLibraryRoots)

group :: IO Group
group = Harness.group "library" [pinnedCheckout, transitiveCheckout, sharedTransitivePin, conflictingTransitivePins, conflictingAliasedPins, conflictingRepositoryNames, conflictingLibrarySources, invalidManifest, duplicateManifestKey, unknownManifestField, malformedManifest, legacyManifest]

pinnedCheckout :: Test
pinnedCheckout = withDirectory \root -> do
  let repository = root </> "source"
      project = root </> "project"
      owner = project </> "main.tung"
      libraryFile = repository </> "value.tung"
      manifest = project </> "tung.yaml"
  initRepository repository
  createDirectory project
  writeFile libraryFile "show let value = 41"
  commitAll repository "first"
  commit <- runGit ["-C", repository, "rev-parse", "HEAD"]
  writeFile manifest (yamlManifest [("sample", "file://" ++ repository, map toUpper commit)])
  writeFile owner "use value.tung let answer = value + 1"
  first <- resolveLibraryRoots owner
  imported <- case first of
    Left message -> pure (Left message)
    Right roots -> do
      projectResult <- loadProjectFileWithRoots Map.empty roots owner
      pure (Map.lookup "value.tung" . projectImports <$> projectResult)
  writeFile libraryFile "show let value = 99"
  commitAll repository "second"
  renameDirectory repository (root </> "source-moved")
  offline <- resolveLibraryRoots owner
  pure $ case (first, imported, offline) of
    (Right [firstRoot], Right (Just "show let value = 41"), Right [offlineRoot]) ->
      if firstRoot == offlineRoot then Nothing else Just "git library cache changed after its source moved"
    other -> Just ("pinned git library failed: " ++ show other)

invalidManifest :: Test
invalidManifest = withDirectory \root -> do
  let owner = root </> "main.tung"
  writeFile owner "let answer = 1"
  writeFile (root </> "tung.yaml") (yamlManifest [("sample", "../source", "main")])
  result <- resolveLibraryRoots owner
  expect "rejecteþ a moving git revision" (either (const True) (const False) result)

duplicateManifestKey :: Test
duplicateManifestKey = withDirectory \root -> do
  let owner = root </> "main.tung"
  writeFile owner "let answer = 1"
  writeFile (root </> "tung.yaml") "dependencies:\n  sample:\n    repo: ../first\n    hash: abc\n  sample:\n    repo: ../second\n    hash: def\n"
  result <- resolveLibraryRoots owner
  expect "rejecteþ duplicate yaml dependency names" $
    either (isInfixOf "DuplicateKey") (const False) result

unknownManifestField :: Test
unknownManifestField = withDirectory \root -> do
  let owner = root </> "main.tung"
  writeFile owner "let answer = 1"
  writeFile (root </> "tung.yaml") "dependencies:\n  sample:\n    repository: ../source\n    hash: abc\n"
  result <- resolveLibraryRoots owner
  expect "rejecteþ misspelled yaml fields" $
    either (isInfixOf "unknown library field 'repository'") (const False) result

malformedManifest :: Test
malformedManifest = withDirectory \root -> do
  let owner = root </> "main.tung"
  writeFile owner "let answer = 1"
  writeFile (root </> "tung.yaml") "dependencies: [\n"
  result <- resolveLibraryRoots owner
  expect "rejecteþ malformed yaml" (either (const True) (const False) result)

legacyManifest :: Test
legacyManifest = withDirectory \root -> do
  let owner = root </> "main.tung"
  writeFile owner "let answer = 1"
  writeFile (root </> "tung.libraries") "sample\t../source\tmain\n"
  result <- resolveLibraryRoots owner
  expect "explaineþ þe legacy manifest migration" $
    either (\message -> all (`isInfixOf` message) ["tung.libraries", "tung.yaml"]) (const False) result

transitiveCheckout :: Test
transitiveCheckout = withDirectory \root -> do
  let dependency = root </> "dependency"
      parent = root </> "parent"
      project = root </> "project"
      owner = project </> "main.tung"
  initRepository dependency
  writeFile (dependency </> "dep.tung") "show let answer = 42"
  commitAll dependency "dependency"
  dependencyCommit <- runGit ["-C", dependency, "rev-parse", "HEAD"]
  initRepository parent
  writeFile (parent </> "parent.tung") "use dep.tung show let value = answer"
  writeFile (parent </> "tung.yaml") (yamlManifest [("dependency", dependency, dependencyCommit)])
  commitAll parent "parent"
  parentCommit <- runGit ["-C", parent, "rev-parse", "HEAD"]
  createDirectory project
  writeFile (project </> "tung.yaml") (yamlManifest [("parent", "../parent", parentCommit)])
  writeFile owner "use parent.tung let result = value"
  resolved <- resolveLibraryRoots owner
  imported <- case resolved of
    Left message -> pure (Left message)
    Right roots -> do
      projectResult <- loadProjectFileWithRoots Map.empty roots owner
      pure (Map.lookup "dep.tung" . projectImports <$> projectResult)
  pure $ case (resolved, imported) of
    (Right [_, _], Right (Just "show let answer = 42")) -> Nothing
    other -> Just ("transitive git library failed: " ++ show other)

sharedTransitivePin :: Test
sharedTransitivePin = withDirectory \root -> do
  (repository, first, _) <- dependencyVersions root
  owner <- createDiamond root repository first "shared" first
  result <- resolveLibraryRoots owner
  expect "shareþ one checkout for an agreed transitive pin" (either (const False) ((== 3) . length) result)

conflictingTransitivePins :: Test
conflictingTransitivePins = withDirectory \root -> do
  (repository, first, second) <- dependencyVersions root
  owner <- createDiamond root repository first "shared" second
  result <- resolveLibraryRoots owner
  expect "rejecteþ conflicting transitive pins before import" $
    either (\message -> all (`isInfixOf` message) ["library 'shared'", first, second]) (const False) result

conflictingAliasedPins :: Test
conflictingAliasedPins = withDirectory \root -> do
  (repository, first, second) <- dependencyVersions root
  owner <- createDiamond root repository first "renamed" second
  result <- resolveLibraryRoots owner
  expect "rejecteþ different aliases for two versions of one repository" $
    either (\message -> all (`isInfixOf` message) ["repository '" ++ repository ++ "'", first, second]) (const False) result

conflictingRepositoryNames :: Test
conflictingRepositoryNames = withDirectory \root -> do
  (repository, commit, _) <- dependencyVersions root
  owner <- createDiamond root repository commit "renamed" commit
  result <- resolveLibraryRoots owner
  expect "rejecteþ two names for one library repository" $
    either (\message -> all (`isInfixOf` message) ["repository '" ++ repository ++ "'", "library 'shared'", "library 'renamed'"]) (const False) result

conflictingLibrarySources :: Test
conflictingLibrarySources = withDirectory \root -> do
  (firstRepository, firstCommit, _) <- dependencyVersions root
  let other = root </> "other"
  createDirectory other
  (secondRepository, secondCommit, _) <- dependencyVersions other
  owner <- createDiamondWith root (firstRepository, "shared", firstCommit) (secondRepository, "shared", secondCommit)
  result <- resolveLibraryRoots owner
  expect "rejecteþ one library name from two repositories" $
    either (\message -> all (`isInfixOf` message) ["library 'shared'", firstRepository, secondRepository]) (const False) result

dependencyVersions :: FilePath -> IO (FilePath, String, String)
dependencyVersions root = do
  let repository = root </> "dependency"
      source = repository </> "dep.tung"
  initRepository repository
  writeFile source "show let answer = 1"
  commitAll repository "first"
  first <- runGit ["-C", repository, "rev-parse", "HEAD"]
  writeFile source "show let answer = 2"
  commitAll repository "second"
  second <- runGit ["-C", repository, "rev-parse", "HEAD"]
  pure (repository, first, second)

createDiamond :: FilePath -> FilePath -> String -> String -> String -> IO FilePath
createDiamond root dependency first secondName second =
  createDiamondWith root (dependency, "shared", first) (dependency, secondName, second)

createDiamondWith :: FilePath -> (FilePath, String, String) -> (FilePath, String, String) -> IO FilePath
createDiamondWith root left right = do
  parents <- traverse createParent [("left", left), ("right", right)]
  let project = root </> "project"
      owner = project </> "main.tung"
  createDirectory project
  writeFile (project </> "tung.yaml") (yamlManifest [(name, "../" ++ name, commit) | (name, commit) <- parents])
  writeFile owner "let answer = 1"
  pure owner
 where
  createParent (name, (dependency, dependencyName, commit)) = do
    let repository = root </> name
    initRepository repository
    writeFile (repository </> "tung.yaml") (yamlManifest [(dependencyName, dependency, commit)])
    commitAll repository name
    parentCommit <- runGit ["-C", repository, "rev-parse", "HEAD"]
    pure (name, parentCommit)

yamlManifest :: [(String, String, String)] -> String
yamlManifest entries =
  "dependencies:\n"
    ++ concat
      [ "  " ++ name ++ ":\n    repo: " ++ show repository ++ "\n    hash: " ++ show commit ++ "\n"
      | (name, repository, commit) <- entries
      ]

initRepository :: FilePath -> IO ()
initRepository repository = do
  createDirectory repository
  runGit ["init", "--quiet", repository]
  runGit ["-C", repository, "config", "user.name", "Tung Test"]
  void (runGit ["-C", repository, "config", "user.email", "tung-test@example.invalid"])

commitAll :: FilePath -> String -> IO ()
commitAll repository message = do
  runGit ["-C", repository, "add", "."]
  void (runGit ["-C", repository, "commit", "--quiet", "-m", message])

withDirectory :: (FilePath -> Test) -> Test
withDirectory action = bracket temporary removePathForcibly action
 where
  temporary = do
    directory <- getTemporaryDirectory
    stamp <- round . (* 1000000) <$> getPOSIXTime
    let path = directory </> ("tung-library-test-" ++ show (stamp :: Integer))
    createDirectoryIfMissing True (takeDirectory path)
    createDirectory path
    pure path

runGit :: [String] -> IO String
runGit arguments = do
  (status, output, errors) <- readProcessWithExitCode "git" arguments ""
  case status of
    ExitSuccess -> pure (filter (/= '\n') output)
    ExitFailure _ -> fail ("git " ++ unwords arguments ++ ": " ++ errors)
