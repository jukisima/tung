-- project loading tests filesystem discovery without involving cli presentation.
module Test.Project (group) where

import Control.Exception (bracket)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (
  canonicalizePath,
  createDirectory,
  createDirectoryIfMissing,
  createFileLink,
  getTemporaryDirectory,
  removePathForcibly,
 )
import System.FilePath (takeDirectory, (</>))
import Test.Harness (Group, Test, expect, expectEq)
import Test.Harness qualified as Harness
import Tung (Project (..), checkBundleDiagnostic, loadProjectFile, loadProjectFileWithRoots, projectImports, projectSource, typeOfBundle)

group :: IO Group
group =
  Harness.group
    "project"
    [ nestedImports
    , preparedProject
    , localImportPaths
    , moduleRootImport
    , ambiguousModuleRoots
    , embeddedFallback
    , localShadowsBuiltin
    , missingImport
    , ambiguousRelativeImport
    , canonicalAlias
    ]

nestedImports :: Test
nestedImports = withProject files "main.tung" \result ->
  expectEq "loadeþ nested relative imports" (Right expected) (projectContents <$> result)
 where
  files =
    [ ("main.tung", "use lib/a.tung let _ main = only")
    , ("lib/a.tung", "use b.tung show let a = b")
    , ("lib/b.tung", "show let b = 1")
    ]
  expected =
    ( "use lib/a.tung let _ main = only"
    , Map.fromList
        [ ("lib/a.tung", "use b.tung show let a = b")
        , ("b.tung", "show let b = 1")
        ]
    )

  projectContents project = (projectSource project, projectImports project)

preparedProject :: Test
preparedProject = withProject [("main.tung", "use dep.tung let answer = value + 1"), ("dep.tung", "show let value = 41")] "main.tung" \case
  Left message -> pure (Just message)
  Right Project{projectBundle} -> do
    checked <- expectEq "prepared project check" Nothing (checkBundleDiagnostic False projectBundle)
    inspected <- expectEq "prepared project type inspection" (Right "ℤ") (typeOfBundle projectBundle "answer")
    pure (checked <> inspected)

localImportPaths :: Test
localImportPaths = withDirectory \root -> do
  let mainPath = root </> "main.tung"
      depPath = root </> "dep.tung"
  writeProject root [("main.tung", "use dep.tung"), ("dep.tung", "show let value = 1")]
  canonicalMain <- canonicalizePath mainPath
  canonicalDep <- canonicalizePath depPath
  result <- loadProjectFile Map.empty mainPath
  expectEq
    "retaineþ owning filesystem paths"
    (Just (canonicalMain, canonicalDep))
    ( either
        (const Nothing)
        (\Project{projectPath, projectImportPaths} -> fmap (\dep -> (projectPath, dep)) (Map.lookup "dep.tung" projectImportPaths))
        result
    )

localShadowsBuiltin :: Test
localShadowsBuiltin = withProjectUsing (Map.singleton "value.tung" "show let value = 1") files "main.tung" \result ->
  expectEq
    "local use shadows an embedded module"
    (Just "show let value = 2")
    (either (const Nothing) (Map.lookup "value.tung" . projectImports) result)
 where
  files =
    [ ("main.tung", "use value.tung")
    , ("value.tung", "show let value = 2")
    ]

moduleRootImport :: Test
moduleRootImport = withDirectory \root -> do
  let project = root </> "project"
      modules = root </> "modules"
      source = "show let shared = 7"
  writeProject root [("project/main.tung", "use shared.tung"), ("modules/shared.tung", source)]
  result <- loadProjectFileWithRoots Map.empty [modules] (project </> "main.tung")
  expectEq "loadeþ an import from a module root" (Just source) (either (const Nothing) (Map.lookup "shared.tung" . projectImports) result)

ambiguousModuleRoots :: Test
ambiguousModuleRoots = withDirectory \root -> do
  let project = root </> "project"
      left = root </> "left"
      right = root </> "right"
  writeProject
    root
    [ ("project/main.tung", "use shared.tung")
    , ("left/shared.tung", "show let left = 1")
    , ("right/shared.tung", "show let right = 2")
    ]
  result <- loadProjectFileWithRoots Map.empty [left, right] (project </> "main.tung")
  expect "rejecteþ duplicate module-root matches" (either (isInfixOf "ambiguous use 'shared.tung'") (const False) result)

embeddedFallback :: Test
embeddedFallback = withProjectUsing builtins [("main.tung", "use ground.tung")] "main.tung" \result ->
  expectEq "useþ an embedded module when no local file exists" (Just source) (either (const Nothing) (Map.lookup "ground.tung" . projectImports) result)
 where
  source = "show let value = 1"
  builtins = Map.singleton "ground.tung" source

missingImport :: Test
missingImport = withProject [("main.tung", "use absent.tung")] "main.tung" \result ->
  expect "reporteþ the owner of a missing use" (either (isInfixOf "from '") (const False) result)

ambiguousRelativeImport :: Test
ambiguousRelativeImport = withProject files "main.tung" \result ->
  expect "rejecteþ one use spelling for two files" (either (isInfixOf "ambiguous use 'shared.tung'") (const False) result)
 where
  files =
    [ ("main.tung", "use left/a.tung use right/b.tung")
    , ("left/a.tung", "use shared.tung")
    , ("left/shared.tung", "show let left = 1")
    , ("right/b.tung", "use shared.tung")
    , ("right/shared.tung", "show let right = 2")
    ]

canonicalAlias :: Test
canonicalAlias = withDirectory \root -> do
  let mainPath = root </> "main.tung"
      sourcePath = root </> "source.tung"
      aliasPath = root </> "alias.tung"
  writeProject
    root
    [ ("main.tung", "use source.tung use alias.tung")
    , ("source.tung", "show let value = 1")
    ]
  createFileLink sourcePath aliasPath
  result <- loadProjectFile Map.empty mainPath
  expect
    "rejecteþ two import paths for one canonical file"
    (either (isInfixOf "resolveþ to the same file") (const False) result)

withProject :: [(FilePath, String)] -> FilePath -> (Either String Project -> Test) -> Test
withProject = withProjectUsing Map.empty

withProjectUsing :: Map.Map String String -> [(FilePath, String)] -> FilePath -> (Either String Project -> Test) -> Test
withProjectUsing builtins files entry assertion = do
  withDirectory \root -> writeProject root files >> loadProjectFile builtins (root </> entry) >>= assertion

withDirectory :: (FilePath -> Test) -> Test
withDirectory action = do
  directory <- temporaryDirectory
  bracket (pure directory) removePathForcibly action

writeProject :: FilePath -> [(FilePath, String)] -> IO ()
writeProject root = mapM_ \(path, source) -> do
  let target = root </> path
  createDirectoryIfMissing True (takeDirectory target)
  writeFile target source

temporaryDirectory :: IO FilePath
temporaryDirectory = do
  base <- getTemporaryDirectory
  stamp <- round . (* 1000000) <$> getPOSIXTime
  let path = base </> ("tung-project-test-" ++ show (stamp :: Integer))
  createDirectory path
  pure path
