-- bookhoard registry, dependency, ownership, export, and type-checking invariants.
-- the graph must be acyclic, dependencies may not import ground, and each source
-- owneþ at most one algebraic data declaration.
module Test.Bookhoard (group) where

import Data.Foldable (traverse_)
import Data.List (intercalate, isPrefixOf, nub, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Set qualified as Set
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import Test.Harness (Group, Test)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = do
  files <- readLibraryImportFiles "." >>= either fail pure
  imports <- readLibraryImports "." >>= either fail pure
  registry <- registryCase files
  loaded <- loadedSourcesCase files imports
  Harness.group "bookhoard" $
    registry
      : loaded
      : acyclicCase imports
      : noUmbrellaDependencyCase files
      : groundEffectExportsCase imports
      : systemExplicitCase imports
      : runnerEffectsCase imports
      : primitiveCatalogueCase imports
      : frameLayoutCase files
      : map (typeCase imports) files
      ++ map oneDataTypeCase (nub (map fst files))

loadedSourcesCase :: [(FilePath, String)] -> Map.Map String String -> IO Test
loadedSourcesCase files imports = do
  differences <- traverse differs files
  pure . pure $ listToMaybe (catMaybes differences)
 where
  differs (path, name) = do
    source <- readFile path
    pure $ case Map.lookup name imports of
      Just loaded | loaded == source -> Nothing
      _ -> Just ("loaded bookhoard source differs from " ++ path)

registryCase :: [(FilePath, String)] -> IO Test
registryCase registeredFiles = case lookup "ground.tung" [(name, path) | (path, name) <- registeredFiles] of
  Nothing -> pure (pure (Just "the declared bookhoard library lacks ground.tung"))
  Just groundPath -> do
    files <- discover (takeDirectory groundPath)
    let registered = sort (nub (map fst registeredFiles))
        actual = sort (filter ((== ".tung") . takeExtension) files)
    pure . pure $
      if registered == actual
        then Nothing
        else Just ("bookhoard registry differs from source tree: " ++ intercalate ", " (registered `symmetricDifference` actual))

discover :: FilePath -> IO [FilePath]
discover directory = do
  entries <- listDirectory directory
  concat <$> traverse visit entries
 where
  visit entry = do
    let path = directory </> entry
    isDirectory <- doesDirectoryExist path
    if isDirectory then discover path else pure [path]

symmetricDifference :: (Eq a) => [a] -> [a] -> [a]
symmetricDifference left right = filter (`notElem` right) left ++ filter (`notElem` left) right

typeCase :: Map.Map String String -> (FilePath, String) -> Test
typeCase imports (path, importName) = do
  source <- readFile path
  pure $ case checkWithImports source imports of
    "type ok" -> Nothing
    actual -> Just ("type " ++ importName ++ ": " ++ actual)

acyclicCase :: Map.Map String String -> Test
acyclicCase imports = pure $ case traverse_ (walk []) (Map.keys imports) of
  Left message -> Just message
  Right () -> Nothing
 where
  walk stack path = do
    nextStack <- enterImport stack path
    source <- maybe (Left ("missing use '" ++ path ++ "'")) Right (Map.lookup path imports)
    Program declarations <- either (Left . (("parse " ++ path ++ ": ") ++)) Right (parse source)
    traverse_ (walk nextStack) (concatMap importedPaths declarations)

noUmbrellaDependencyCase :: [(FilePath, String)] -> Test
noUmbrellaDependencyCase files = listToMaybe . catMaybes <$> traverse checkModule files
 where
  checkModule (_, "ground.tung") = pure Nothing
  checkModule (path, _) = do
    source <- readFile path
    pure $ case parse source of
      Left message -> Just ("parse " ++ path ++ ": " ++ message)
      Right (Program declarations)
        | "ground.tung" `elem` concatMap importedPaths declarations -> Just (path ++ " imports the ground umbrella")
        | otherwise -> Nothing

groundEffectExportsCase :: Map.Map String String -> Test
groundEffectExportsCase imports =
  pure $ case checkWithImports source imports of
    "type ok" -> Nothing
    actual -> Just ("ground effect exports: " ++ actual)
 where
  source =
    "use ground.tung "
      ++ "show-ilk ground~fail, ground~console, ground~random, ground~state, ground~async, ground~file"

systemExplicitCase :: Map.Map String String -> Test
systemExplicitCase imports =
  pure $ case checkWithImports "use ground.tung show-ilk ground~system" imports of
    actual | "type error:" `isPrefixOf` actual -> Nothing
    actual -> Just ("ground unexpectedly exports system: " ++ actual)

runnerEffectsCase :: Map.Map String String -> Test
runnerEffectsCase imports =
  pure $ case checkRunnableWithImports source imports of
    "type ok" -> Nothing
    actual -> Just ("runner effects: " ++ actual)
 where
  source =
    "use ground.tung use deed/clock.tung use deed/process.tung use deed/system.tung use web/server.tung use ilk/list.tung use ilk/option.tung "
      ++ "let (_: request) route: response = 'ok' ok "
      ++ "let (_: 𝟙) main: 𝟙 ! system, clock, process, web = ("
      ++ "let args = only arguments "
      ++ "let setting = 'TUNG_SETTING' environment "
      ++ "let stamp = only unix-time "
      ++ "yield try 8080 serve route { _ fail @ only })"

primitiveCatalogueCase :: Map.Map String String -> Test
primitiveCatalogueCase imports = pure $ case traverse parse (Map.elems imports) of
  Left message -> Just ("primitive catalogue parse: " ++ message)
  Right programs ->
    let declarations = concat [items | Program items <- programs]
        declaredForeigns = Set.fromList (concatMap foreignNames declarations)
        declaredEffects = Map.fromList (concatMap effectArities declarations)
        catalogueForeigns = Set.fromList [hostName | HostBinding{hostName, hostRole = ForeignBinding} <- hostBindings]
        catalogueEffects =
          Map.fromList
            [ ((owner, hostName), length hostArguments)
            | HostBinding{hostName, hostRole, hostSignature = HostSignature{hostArguments}} <- hostBindings
            , owner <- effectOwners hostRole
            ]
     in if catalogueForeigns /= declaredForeigns
          then Just "host catalogue and fremmed declarations differ"
          else
            if not (Map.isSubmapOfBy (==) catalogueEffects declaredEffects)
              then Just "host catalogue and dispatched effect declarations differ"
              else
                if any (null . hostArguments . hostSignature) hostBindings
                  then Just "host catalogue containeþ an empty function"
                  else Nothing
 where
  foreignNames = \case
    Export declaration -> foreignNames declaration
    Let _ _ (EForeign hostKey) -> [hostKey]
    _ -> []
  effectArities = \case
    Export declaration -> effectArities declaration
    EffectDecl _ owner operations -> [((owner, name), effectArity signature) | EffectOp name signature <- operations]
    _ -> []
  effectArity (TypeArrow arguments _ _) = length arguments
  effectArity _ = 0
  effectOwners (BaseEffect owner) = [owner]
  effectOwners (SourceEffect owner) = [owner]
  effectOwners _ = []

oneDataTypeCase :: FilePath -> Test
oneDataTypeCase path = do
  source <- readFile path
  pure $ case parse source of
    Left message -> Just ("parse " ++ path ++ ": " ++ message)
    Right (Program declarations) ->
      let count = length [() | declaration <- declarations, isDataDecl declaration]
       in if count <= 1 then Nothing else Just (path ++ " owneþ " ++ show count ++ " data types")

importedPaths :: Decl -> [String]
importedPaths (Import path _) = [path]
importedPaths (Export declaration) = importedPaths declaration
importedPaths _ = []

isDataDecl :: Decl -> Bool
isDataDecl DataDecl{} = True
isDataDecl (Export declaration) = isDataDecl declaration
isDataDecl _ = False

frameLayoutCase :: [(FilePath, String)] -> Test
frameLayoutCase files = listToMaybe . catMaybes <$> traverse checkModule files
 where
  checkModule (path, name) = do
    source <- readFile path
    pure $ case parse source of
      Left message -> Just ("parse " ++ path ++ ": " ++ message)
      Right (Program declarations) -> checkFrames path name (concatMap frameNames declarations)
  checkFrames path name names
    | framePrefix `isPrefixOf` name = case names of
        [name] | name == takeBaseName path -> Nothing
        [name] -> Just (path ++ " defineþ frame '" ++ name ++ "'")
        _ -> Just (path ++ " owneþ " ++ show (length names) ++ " frames")
    | null names = Nothing
    | otherwise = Just (path ++ " defineþ a frame outside the frame directory")
  framePrefix = "frame/"

frameNames :: Decl -> [String]
frameNames (ShapeDecl _ name _ _) = [name]
frameNames (Export declaration) = frameNames declaration
frameNames _ = []
