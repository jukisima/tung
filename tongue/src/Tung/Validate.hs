{-# LANGUAGE ViewPatterns #-}

-- | type-independent tree validation, such as repeated fields, binders,
-- operations, and members. name and type questions belong to later stages.
module Tung.Validate (validateProgram) where

import Control.Monad (foldM_, void)
import Data.Foldable (traverse_)
import Data.List (group, isSuffixOf, sort)
import Data.Map.Strict qualified as Map
import Tung.Name (checkImportAlias, importAlias, isQualifiedName)
import Tung.Syntax

validateProgram :: Program -> Either String ()
validateProgram (Program declarations) = do
  let names = Map.fromListWith (++) (concatMap declarationNames declarations)
  traverse_ (\(namespace, entries) -> distinct (namespace ++ " name") entries) (Map.toList names)
  validateUseNamespaces declarations
  traverse_ validateDecl declarations
  where
    declarationNames (Export declaration) = declarationNames declaration
    declarationNames (TypeAlias _ name _) = [("type", [name])]
    declarationNames (DataDecl _ name _) = [("type", [name])]
    declarationNames (EffectDecl _ name _) = [("type", [name])]
    declarationNames (ClassDecl _ name _) = [("flock", [name])]
    declarationNames _ = []

validateUseNamespaces :: [Decl] -> Either String ()
validateUseNamespaces declarations = do
  traverse_ validateImport imports
  foldM_ register Map.empty [(importAlias path alias, path) | (path, alias) <- imports]
  where
    imports = [(path, alias) | Import path alias <- declarations]
    validateImport (path, alias) = validateImportPath path >> traverse_ checkImportAlias alias
    register owners (namespace, path) = case Map.lookup namespace owners of
      Just other | other /= path -> Left ("use namespace '" ++ namespace ++ "' referreþ to both '" ++ other ++ "' and '" ++ path ++ "'")
      _ -> pure (Map.insert namespace path owners)

validateImportPath :: String -> Either String ()
validateImportPath path
  | ".tung" `isSuffixOf` path = pure ()
  | otherwise = Left ("use path '" ++ path ++ "' must end in '.tung'")

validateDecl :: Decl -> Either String ()
validateDecl = \case
  Import path alias -> validateImportPath path >> traverse_ checkImportAlias alias
  Export Import {} -> Left "use cannot be shown"
  Export InstanceDecl {} -> Left "bizen evidence cannot be shown"
  Export declaration -> validateDecl declaration
  ReExport _ -> pure ()
  ReExportType _ -> pure ()
  Let _ Nothing EForeign {} -> Left "fremmed let requireþ a type annotation"
  Let _ (Just annotation) EForeign {} -> validateTypeAnn annotation
  Let _ annotation body -> traverse_ validateTypeAnn annotation >> validateExpr body
  TypeAlias (parameterNames -> params) name target -> do
    validateUnqualified "type alias" name
    distinct ("type alias '" ++ name ++ "' parameter") params
    validateType target
  DataDecl (parameterNames -> params) name constructors -> do
    validateUnqualified "data" name
    distinct ("data '" ++ name ++ "' parameter") params
    distinct ("data '" ++ name ++ "' constructor") [constructorName ctor | ctor <- constructors]
    traverse_ validateCtor constructors
  EffectDecl (parameterNames -> params) name operations -> do
    validateUnqualified "effect" name
    distinct ("effect '" ++ name ++ "' parameter") params
    distinct ("effect '" ++ name ++ "' operation") [operationName operation | operation <- operations]
    traverse_ validateEffectOp operations
  ElaboratedEffect _ operations -> traverse_ (validateEffectOp . snd) operations
  ClassDecl (typeHeaderParts -> (params, constraints)) name members -> do
    validateUnqualified "flock" name
    distinct ("flock '" ++ name ++ "' parameter") params
    distinct ("flock '" ++ name ++ "' member") (classMemberNames members)
    traverse_ validateConstraint constraints
    traverse_ validateClassMember members
  ElaboratedClass _ _ members -> traverse_ (validateClassMember . snd) members
  InstanceDecl (typeHeaderParts -> (names, constraints)) types name members -> do
    distinct "bizen type parameter" names
    distinct ("bizen '" ++ name ++ "' member") [memberName | Let memberName _ _ <- members]
    traverse_ validateType types
    traverse_ validateConstraint constraints
    traverse_ validateDecl members
  ElaboratedInstance _ _ types name constraints members -> validateDecl (InstanceDecl (map Requirement constraints) types name members)

validateCtor :: Ctor -> Either String ()
validateCtor (Ctor name (parameterNames -> names) fields result) = do
  distinct "constructor type parameter" names
  validateUnqualified "constructor" name
  traverse_ validateType fields
  traverse_ validateType result

validateEffectOp :: EffectOp -> Either String ()
validateEffectOp (EffectOp name operationType) = validateUnqualified "effect operation" name >> validateType operationType

validateClassMember :: ClassMember -> Either String ()
validateClassMember = \case
  ClassSignature name annotation -> validateUnqualified "flock member" name >> validateTypeAnn annotation
  ClassLaw (parameterNames -> names) parameters left right -> do
    distinct "law type parameter" names
    distinct "flock law parameter" (concatMap (patternNames . fst) parameters)
    traverse_ (validateType . snd) parameters
    validateExpr left
    validateExpr right

patternNames :: Pattern -> [String]
patternNames = \case
  PVar "_" -> []
  PVar name -> [name]
  PInteger _ -> []
  PText _ -> []
  -- the first name may be a binder or a constructor in bracketed patterns;
  -- the type checker resolveþ it before checking linearity.
  PCon _ (_ : arguments) -> concatMap patternNames arguments
  PCon _ [] -> []
  PConstructor _ arguments -> concatMap patternNames arguments

validateTypeAnn :: TypeAnn -> Either String ()
validateTypeAnn (TypeAnn value entries) = validateType value >> distinct "type parameter" (parameterNames (headerParameters entries)) >> traverse_ validateConstraint (headerRequirements entries)

validateConstraint :: ClassConstraint -> Either String ()
validateConstraint (ClassConstraint arguments _) = traverse_ validateType arguments

validateType :: TypeExpr -> Either String ()
validateType = \case
  TypeHole _ -> pure ()
  TypeName _ -> pure ()
  TypeApply _ arguments -> traverse_ validateType arguments
  TypeRecord fields -> distinct "record type field" (map fst fields) >> traverse_ (validateType . snd) fields
  TypeArrow arguments effects result -> traverse_ validateType arguments >> traverse_ validateType effects >> validateType result
  TypeForall (parameterNames -> names) body -> distinct "type parameter" names >> validateType body

validateExpr :: Expr -> Either String ()
validateExpr expression = do
  case expression of
    EForeign {} -> Left "fremmed is only allowed as the direct body of an annotated let"
    ERecord fields -> distinct "record field" (map fst fields)
    ETry _ _ cases -> distinct "handler case" [name | HandlerCase name _ _ <- cases]
    _ -> pure ()
  void (traverseExprChildren (checked validateDecl) (checked validateType) (checked validateExpr) expression)
  where
    checked validate value = value <$ validate value

distinct :: String -> [String] -> Either String ()
distinct owner names = case repeated of
  duplicate : _ -> Left (owner ++ " repeateþ '" ++ duplicate ++ "'")
  [] -> pure ()
  where
    repeated = [name | name : _ : _ <- group (sort names)]

validateUnqualified :: String -> String -> Either String ()
validateUnqualified owner name
  | isQualifiedName name = Left (owner ++ " name '" ++ name ++ "' must be unqualified")
  | otherwise = Right ()

constructorName :: Ctor -> String
constructorName (Ctor name _ _ _) = name

operationName :: EffectOp -> String
operationName (EffectOp name _) = name
