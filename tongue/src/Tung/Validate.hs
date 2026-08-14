module Tung.Validate (validateProgram) where

import Data.Foldable (traverse_)
import Data.List (group, sort)
import Tung.Syntax

validateProgram :: Program -> Either String ()
validateProgram (Program declarations) = traverse_ validateDecl declarations

validateDecl :: Decl -> Either String ()
validateDecl = \case
  Import _ -> pure ()
  Export FillDecl{} -> Left "fill evidence cannot be shown"
  Export declaration -> validateDecl declaration
  ReExport _ -> pure ()
  ReExportType _ -> pure ()
  Let _ annotation body -> traverse_ validateTypeAnn annotation >> validateExpr body
  TypeAlias _ target -> validateType target
  DataDecl params name constructors -> do
    distinct ("data '" ++ name ++ "' parameter") params
    distinct ("data '" ++ name ++ "' constructor") [constructorName ctor | ctor <- constructors]
    traverse_ validateCtor constructors
  EffectDecl params name operations -> do
    distinct ("effect '" ++ name ++ "' parameter") params
    distinct ("effect '" ++ name ++ "' operation") [operationName operation | operation <- operations]
    traverse_ validateEffectOp operations
  ForeignDecl members -> do
    distinct "foreign member" [name | ForeignMember name _ <- members]
    traverse_ (\(ForeignMember _ annotation) -> validateTypeAnn annotation) members
  ShapeDecl params name needs members -> do
    distinct ("shape '" ++ name ++ "' parameter") params
    distinct ("shape '" ++ name ++ "' member") (map shapeMemberName members)
    traverse_ validateNeed needs
    traverse_ validateShapeMember members
  FillDecl types name needs members -> do
    distinct ("fill '" ++ name ++ "' member") [memberName | Let memberName _ _ <- members]
    traverse_ validateType types
    traverse_ validateNeed needs
    traverse_ validateDecl members

validateCtor :: Ctor -> Either String ()
validateCtor (Ctor _ fields) = traverse_ validateType fields

validateEffectOp :: EffectOp -> Either String ()
validateEffectOp (EffectOp _ operationType) = validateType operationType

validateShapeMember :: ShapeMember -> Either String ()
validateShapeMember = \case
  ShapeSpec _ annotation -> validateTypeAnn annotation
  ShapeDefault _ annotation body -> traverse_ validateTypeAnn annotation >> validateExpr body

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
  EInteger _ -> pure ()
  EFloat _ -> pure ()
  EChar _ -> pure ()
  EString _ -> pure ()
  EVar _ -> pure ()
  EApply function arguments -> validateExpr function >> traverse_ validateExpr arguments
  ERecord fields -> distinct "record field" (map fst fields) >> traverse_ (validateExpr . snd) fields
  EUpdate base updates -> validateExpr base >> traverse_ validateUpdate updates
  ETry body returned cases -> do
    distinct "handler case" [name | HandlerCase name _ _ <- cases]
    validateExpr body
    traverse_ validateReturn returned
    traverse_ validateHandler cases
  EMatch scrutinees cases -> traverse_ validateExpr scrutinees >> traverse_ validateMatch cases
  EBlock declarations body -> traverse_ validateDecl declarations >> validateExpr body

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
  duplicate : _ -> Left (owner ++ " repeats '" ++ duplicate ++ "'")
  [] -> pure ()
 where
  repeated = [name | name : _ : _ <- group (sort names)]

constructorName :: Ctor -> String
constructorName (Ctor name _) = name

operationName :: EffectOp -> String
operationName (EffectOp name _) = name

shapeMemberName :: ShapeMember -> String
shapeMemberName = \case
  ShapeSpec name _ -> name
  ShapeDefault name _ _ -> name
