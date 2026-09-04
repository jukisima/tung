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
import System.FilePath (takeExtension, (</>))
import Test.Harness (Group, Test)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = do
  imports <- readBookhoardImports
  registry <- registryCase
  embedded <- embeddedSourcesCase imports
  Harness.group "bookhoard" $
    registry
      : embedded
      : acyclicCase imports
      : noUmbrellaDependencyCase
      : groundEffectExportsCase imports
      : systemExplicitCase imports
      : runnerEffectsCase imports
      : primitiveCatalogueCase imports
      : map (typeCase imports) bookhoardImportFiles
      ++ map oneDataTypeCase (nub (map fst bookhoardImportFiles))

embeddedSourcesCase :: Map.Map String String -> IO Test
embeddedSourcesCase imports = do
  differences <- traverse differs bookhoardImportFiles
  pure . pure $ listToMaybe (catMaybes differences)
 where
  differs (path, name) = do
    source <- readFile path
    pure $ case Map.lookup name imports of
      Just embedded | embedded == source -> Nothing
      _ -> Just ("embedded bookhoard source differs from " ++ path)

registryCase :: IO Test
registryCase = do
  files <- discover (".." </> "bookhoard")
  let registered = sort (nub (map fst bookhoardImportFiles))
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

noUmbrellaDependencyCase :: Test
noUmbrellaDependencyCase = listToMaybe . catMaybes <$> traverse checkModule (nub (map fst bookhoardImportFiles))
 where
  checkModule "../bookhoard/ground.tung" = pure Nothing
  checkModule path = do
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
    "use ground.tung use clock.tung use process.tung use system.tung use web/server.tung use ilk/list.tung use ilk/option.tung "
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
