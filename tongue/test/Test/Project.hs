-- project loading tests filesystem discovery without involving cli presentation.
module Test.Project (group) where

import Control.Exception (bracket)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (canonicalizePath, createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath (takeDirectory, (</>))
import Test.Harness (Group, Test, expect, expectEq)
import Test.Harness qualified as Harness
import Tung (Project (..), loadProjectFile, loadProjectFileWithRoots)

group :: IO Group
group = Harness.group "project" [nestedImports, localImportPaths, moduleRootImport, ambiguousModuleRoots, embeddedFallback, localShadowsBuiltin, missingImport, ambiguousRelativeImport]

nestedImports :: Test
nestedImports = withProject files "main.tung" \result ->
  expectEq "loadeth nested relative brings" (Right expected) (projectContents <$> result)
 where
  files =
    [ ("main.tung", "bring lib/a.tung; let _ main = null;")
    , ("lib/a.tung", "bring b.tung; show let a = b;")
    , ("lib/b.tung", "show let b = 1;")
    ]
  expected =
    ( "bring lib/a.tung; let _ main = null;"
    , Map.fromList
        [ ("lib/a.tung", "bring b.tung; show let a = b;")
        , ("b.tung", "show let b = 1;")
        ]
    )

  projectContents Project{projectSource, projectImports} = (projectSource, projectImports)

localImportPaths :: Test
localImportPaths = withDirectory \root -> do
  let mainPath = root </> "main.tung"
      depPath = root </> "dep.tung"
  writeProject root [("main.tung", "bring dep.tung;"), ("dep.tung", "show let value = 1;")]
  canonicalMain <- canonicalizePath mainPath
  canonicalDep <- canonicalizePath depPath
  result <- loadProjectFile Map.empty mainPath
  expectEq
    "retaineth owning filesystem paths"
    (Just (canonicalMain, canonicalDep))
    ( either
        (const Nothing)
        (\Project{projectPath, projectImportPaths} -> fmap (\dep -> (projectPath, dep)) (Map.lookup "dep.tung" projectImportPaths))
        result
    )

localShadowsBuiltin :: Test
localShadowsBuiltin = withProjectUsing (Map.singleton "value.tung" "show let value = 1;") files "main.tung" \result ->
  expectEq
    "local bring shadows an embedded module"
    (Just "show let value = 2;")
    (either (const Nothing) (Map.lookup "value.tung" . projectImports) result)
 where
  files =
    [ ("main.tung", "bring value.tung;")
    , ("value.tung", "show let value = 2;")
    ]

moduleRootImport :: Test
moduleRootImport = withDirectory \root -> do
  let project = root </> "project"
      modules = root </> "modules"
      source = "show let shared = 7;"
  writeProject root [("project/main.tung", "bring shared.tung;"), ("modules/shared.tung", source)]
  result <- loadProjectFileWithRoots Map.empty [modules] (project </> "main.tung")
  expectEq "loadeth a bring from a module root" (Just source) (either (const Nothing) (Map.lookup "shared.tung" . projectImports) result)

ambiguousModuleRoots :: Test
ambiguousModuleRoots = withDirectory \root -> do
  let project = root </> "project"
      left = root </> "left"
      right = root </> "right"
  writeProject
    root
    [ ("project/main.tung", "bring shared.tung;")
    , ("left/shared.tung", "show let left = 1;")
    , ("right/shared.tung", "show let right = 2;")
    ]
  result <- loadProjectFileWithRoots Map.empty [left, right] (project </> "main.tung")
  expect "rejecteth duplicate module-root matches" (either (isInfixOf "ambiguous bring 'shared.tung'") (const False) result)

embeddedFallback :: Test
embeddedFallback = withProjectUsing builtins [("main.tung", "bring ground.tung;")] "main.tung" \result ->
  expectEq "useth an embedded module when no local file exists" (Just source) (either (const Nothing) (Map.lookup "ground.tung" . projectImports) result)
 where
  source = "show let value = 1;"
  builtins = Map.singleton "ground.tung" source

missingImport :: Test
missingImport = withProject [("main.tung", "bring absent.tung;")] "main.tung" \result ->
  expect "reporteth the owner of a missing bring" (either (isInfixOf "from '") (const False) result)

ambiguousRelativeImport :: Test
ambiguousRelativeImport = withProject files "main.tung" \result ->
  expect "rejecteth one bring spelling for two files" (either (isInfixOf "ambiguous bring 'shared.tung'") (const False) result)
 where
  files =
    [ ("main.tung", "bring left/a.tung; bring right/b.tung;")
    , ("left/a.tung", "bring shared.tung;")
    , ("left/shared.tung", "show let left = 1;")
    , ("right/b.tung", "bring shared.tung;")
    , ("right/shared.tung", "show let right = 2;")
    ]

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
