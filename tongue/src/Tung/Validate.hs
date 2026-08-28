{- | type-independent tree validation, such as repeated fields, binders,
operations, and members. name and type questions belong to later stages.
-}
module Tung.Validate (validateProgram) where

import Control.Monad (foldM_)
import Data.Foldable (traverse_)
import Data.List (group, isSuffixOf, sort)
import Data.Map.Strict qualified as Map
import Tung.Name (checkImportAlias, importAlias, isQualifiedName)
import Tung.Syntax

validateProgram :: Program -> Either String ()
validateProgram (Program declarations) = validateBringNamespaces declarations >> traverse_ validateDecl declarations

validateBringNamespaces :: [Decl] -> Either String ()
validateBringNamespaces declarations = do
  traverse_ validateImport imports
  foldM_ register Map.empty [(importAlias path alias, path) | (path, alias) <- imports]
 where
  imports = [(path, alias) | Import path alias <- declarations]
  validateImport (path, alias) = validateImportPath path >> traverse_ checkImportAlias alias
  register owners (namespace, path) = case Map.lookup namespace owners of
    Just other | other /= path -> Left ("bring namespace '" ++ namespace ++ "' referreþ to both '" ++ other ++ "' and '" ++ path ++ "'")
    _ -> pure (Map.insert namespace path owners)

validateImportPath :: String -> Either String ()
validateImportPath path
  | ".tung" `isSuffixOf` path = pure ()
  | otherwise = Left ("bring path '" ++ path ++ "' must end in '.tung'")

validateDecl :: Decl -> Either String ()
validateDecl = \case
  Import path alias -> validateImportPath path >> traverse_ checkImportAlias alias
  Export Import{} -> Left "bring cannot be shown"
  Export FillDecl{} -> Left "fill evidence cannot be shown"
  Export declaration -> validateDecl declaration
  ReExport _ -> pure ()
  ReExportType _ -> pure ()
  Let _ Nothing EForeign{} -> Left "fremmed let requireþ a type annotation"
  Let _ (Just annotation) EForeign{} -> validateTypeAnn annotation
  Let _ annotation body -> traverse_ validateTypeAnn annotation >> validateExpr body
  TypeAlias params name target -> do
    validateUnqualified "type alias" name
    distinct ("type alias '" ++ name ++ "' parameter") params
    validateType target
  DataDecl params name constructors -> do
    validateUnqualified "data" name
    distinct ("data '" ++ name ++ "' parameter") params
    distinct ("data '" ++ name ++ "' constructor") [constructorName ctor | ctor <- constructors]
    traverse_ validateCtor constructors
  EffectDecl params name operations -> do
    validateUnqualified "effect" name
    distinct ("effect '" ++ name ++ "' parameter") params
    distinct ("effect '" ++ name ++ "' operation") [operationName operation | operation <- operations]
    traverse_ validateEffectOp operations
  ElaboratedEffect _ operations -> traverse_ (validateEffectOp . snd) operations
  ShapeDecl params name needs members -> do
    validateUnqualified "shape" name
    distinct ("shape '" ++ name ++ "' parameter") params
    distinct ("shape '" ++ name ++ "' member") (shapeMemberNames members)
    traverse_ validateNeed needs
    traverse_ validateShapeMember members
  ElaboratedShape _ _ members -> traverse_ (validateShapeMember . snd) members
  FillDecl types name needs members -> do
    distinct ("fill '" ++ name ++ "' member") [memberName | Let memberName _ _ <- members]
    traverse_ validateType types
    traverse_ validateNeed needs
    traverse_ validateDecl members
  ElaboratedFill _ _ types name needs members -> validateDecl (FillDecl types name needs members)

validateCtor :: Ctor -> Either String ()
validateCtor (Ctor name fields) = validateUnqualified "constructor" name >> traverse_ validateType fields

validateEffectOp :: EffectOp -> Either String ()
validateEffectOp (EffectOp name operationType) = validateUnqualified "effect operation" name >> validateType operationType

validateShapeMember :: ShapeMember -> Either String ()
validateShapeMember = \case
  ShapeSpec name annotation -> validateUnqualified "shape member" name >> validateTypeAnn annotation
  ShapeLaw parameters left right -> do
    distinct "shape law parameter" (map fst parameters)
    traverse_ (validateType . snd) parameters
    validateExpr left
    validateExpr right

validateTypeAnn :: TypeAnn -> Either String ()
validateTypeAnn (TypeAnn value needs) = validateType value >> traverse_ validateNeed needs

validateNeed :: ShapeNeed -> Either String ()
validateNeed (ShapeNeed arguments _) = traverse_ validateType arguments

validateType :: TypeExpr -> Either String ()
validateType = \case
  TypeName _ -> pure ()
  TypeApply _ arguments -> traverse_ validateType arguments
  TypeRecord fields -> distinct "record type field" (map fst fields) >> traverse_ (validateType . snd) fields
  TypeArrow arguments effects result -> traverse_ validateType arguments >> traverse_ validateType effects >> validateType result

validateExpr :: Expr -> Either String ()
validateExpr = \case
  ELocated _ expression -> validateExpr expression
  EInteger _ -> pure ()
  EFloat _ -> pure ()
  EUnicode _ -> pure ()
  EText _ -> pure ()
  EForeign{} -> Left "fremmed is only allowed as the direct body of an annotated let"
  EVar _ -> pure ()
  EGlobal _ -> pure ()
  EEvidence _ -> pure ()
  EEvidenceLambda _ body -> validateExpr body
  EAscribe expression annotation -> validateExpr expression >> validateType annotation
  EApply function arguments -> validateExpr function >> traverse_ validateExpr arguments
  ERecord fields -> distinct "record field" (map fst fields) >> traverse_ (validateExpr . snd) fields
  EField base _ -> validateExpr base
  EUpdate base updates -> validateExpr base >> traverse_ validateUpdate updates
  ETry body returned cases -> do
    distinct "handler case" [name | HandlerCase name _ _ <- cases]
    validateExpr body
    traverse_ validateReturn returned
    traverse_ validateHandler cases
  EMatch scrutinees cases -> traverse_ validateExpr scrutinees >> traverse_ validateMatch cases
  EBlock declarations body -> traverse_ validateDecl declarations >> validateExpr body
  EWithEvidence function _ -> validateExpr function
  EDictionary _ _ required parents -> traverse_ validateExpr required >> traverse_ validateExpr parents

validateUpdate :: RecordUpdate -> Either String ()
validateUpdate = \case
  RecordSet _ value -> validateExpr value
  RecordRemove _ -> pure ()

validateReturn :: ReturnCase -> Either String ()
validateReturn (ReturnCase _ body) = validateExpr body

validateHandler :: HandlerCase -> Either String ()
validateHandler (HandlerCase _ _ body) = validateExpr body
validateHandler (ResolvedHandlerCase _ _ body) = validateExpr body

validateMatch :: MatchCase -> Either String ()
validateMatch (MatchCase _ body) = validateExpr body

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
constructorName (Ctor name _) = name

operationName :: EffectOp -> String
operationName (EffectOp name _) = name
