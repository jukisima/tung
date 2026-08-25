{- | type-independent tree validation, such as repeated fields, binders,
operations, and members. name and type questions belong to later stages.
-}
module Tung.Validate (validateProgram) where

import Control.Monad (foldM_)
import Data.Foldable (traverse_)
import Data.List (group, sort)
import Data.Map.Strict qualified as Map
import Tung.Name (checkImportAlias, importAlias)
import Tung.Syntax

validateProgram :: Program -> Either String ()
validateProgram (Program declarations) = validateBringNamespaces declarations >> traverse_ validateDecl declarations

validateBringNamespaces :: [Decl] -> Either String ()
validateBringNamespaces declarations = do
  traverse_ checkImportAlias [alias | Import _ (Just alias) <- declarations]
  foldM_ register Map.empty [(importAlias path alias, path) | Import path alias <- declarations]
 where
  register owners (namespace, path) = case Map.lookup namespace owners of
    Just other | other /= path -> Left ("bring namespace '" ++ namespace ++ "' referreþ to both '" ++ other ++ "' and '" ++ path ++ "'")
    _ -> pure (Map.insert namespace path owners)

validateDecl :: Decl -> Either String ()
validateDecl = \case
  Import _ _ -> pure ()
  Export Import{} -> Left "bring cannot be shown"
  Export FillDecl{} -> Left "fill evidence cannot be shown"
  Export declaration -> validateDecl declaration
  ReExport _ -> pure ()
  ReExportType _ -> pure ()
  Let _ Nothing EForeign{} -> Left "fremmed let requireþ a type annotation"
  Let _ (Just annotation) EForeign{} -> validateTypeAnn annotation
  Let _ annotation body -> traverse_ validateTypeAnn annotation >> validateExpr body
  TypeAlias params name target -> distinct ("type alias '" ++ name ++ "' parameter") params >> validateType target
  DataDecl params name constructors -> do
    distinct ("data '" ++ name ++ "' parameter") params
    distinct ("data '" ++ name ++ "' constructor") [constructorName ctor | ctor <- constructors]
    traverse_ validateCtor constructors
  EffectDecl params name operations -> do
    distinct ("effect '" ++ name ++ "' parameter") params
    distinct ("effect '" ++ name ++ "' operation") [operationName operation | operation <- operations]
    traverse_ validateEffectOp operations
  ShapeDecl params name needs members -> do
    distinct ("shape '" ++ name ++ "' parameter") params
    distinct ("shape '" ++ name ++ "' member") (shapeMemberNames members)
    traverse_ validateNeed needs
    traverse_ validateShapeMember members
  FillDecl types name needs members -> do
    distinct ("fill '" ++ name ++ "' member") [memberName | Let memberName _ _ <- members]
    traverse_ validateType types
    traverse_ validateNeed needs
    traverse_ validateDecl members
  ElaboratedFill _ types name needs members -> validateDecl (FillDecl types name needs members)

validateCtor :: Ctor -> Either String ()
validateCtor (Ctor _ fields) = traverse_ validateType fields

validateEffectOp :: EffectOp -> Either String ()
validateEffectOp (EffectOp _ operationType) = validateType operationType

validateShapeMember :: ShapeMember -> Either String ()
validateShapeMember = \case
  ShapeSpec _ annotation -> validateTypeAnn annotation
  ShapeDefault _ annotation body -> traverse_ validateTypeAnn annotation >> validateExpr body
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

validateUpdate :: RecordUpdate -> Either String ()
validateUpdate = \case
  RecordSet _ value -> validateExpr value
  RecordRemove _ -> pure ()

validateReturn :: ReturnCase -> Either String ()
validateReturn (ReturnCase _ body) = validateExpr body

validateHandler :: HandlerCase -> Either String ()
validateHandler (HandlerCase _ _ body) = validateExpr body

validateMatch :: MatchCase -> Either String ()
validateMatch (MatchCase _ body) = validateExpr body

distinct :: String -> [String] -> Either String ()
distinct owner names = case repeated of
  duplicate : _ -> Left (owner ++ " repeateþ '" ++ duplicate ++ "'")
  [] -> pure ()
 where
  repeated = [name | name : _ : _ <- group (sort names)]

constructorName :: Ctor -> String
constructorName (Ctor name _) = name

operationName :: EffectOp -> String
operationName (EffectOp name _) = name
