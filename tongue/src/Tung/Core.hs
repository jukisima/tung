-- | validated boundary between type-directed elaboration and evaluation.
module Tung.Core (
  CoreProgram,
  makeCoreProgram,
  makeCoreProgramWithImports,
  coreDeclarations,
  coreImportDeclarations,
)
where

import Data.Map.Strict qualified as Map
import Tung.Primitive (findForeignBinding)
import Tung.Syntax

-- a CoreProgram hath passed type checking and containeþ only resolved evidence.
-- keeping its constructor private preventeþ evaluation of unchecked surface syntax.
data CoreProgram = CoreProgram Program (Map.Map FilePath Program) deriving (Eq, Show)

makeCoreProgram :: Program -> Either String CoreProgram
makeCoreProgram program = makeCoreProgramWithImports program Map.empty

makeCoreProgramWithImports :: Program -> Map.Map FilePath CoreProgram -> Either String CoreProgram
makeCoreProgramWithImports program imports = do
  validateCoreProgram program
  pure (CoreProgram program (rootProgram <$> imports))
 where
  rootProgram (CoreProgram imported _) = imported

coreDeclarations :: CoreProgram -> [Decl]
coreDeclarations (CoreProgram (Program declarations) _) = declarations

coreImportDeclarations :: FilePath -> CoreProgram -> Maybe [Decl]
coreImportDeclarations path (CoreProgram _ imports) =
  declarations <$> Map.lookup path imports
 where
  declarations (Program imported) = imported

validateCoreProgram :: Program -> Either String ()
validateCoreProgram (Program declarations) = mapM_ validateDecl declarations
 where
  validateDecl = \case
    Import{} -> pure ()
    Export declaration -> validateDecl declaration
    ReExport{} -> pure ()
    ReExportType{} -> pure ()
    Let _ (Just _) (EForeign hostKey) ->
      maybe (Left ("internal unknown fremmed host binding '" ++ hostKey ++ "'")) (const (pure ())) (findForeignBinding hostKey)
    Let _ _ body -> validateExpr body
    TypeAlias{} -> pure ()
    DataDecl{} -> pure ()
    EffectDecl{} -> pure ()
    ShapeDecl _ _ _ members -> mapM_ validateShapeMember members
    FillDecl{} -> Left "internal unelaborated fill"
    ElaboratedFill _ _ _ _ members -> mapM_ validateDecl members

  validateShapeMember = \case
    ShapeSpec{} -> pure ()
    ShapeDefault _ _ body -> validateExpr body
    ShapeLaw _ left right -> validateExpr left >> validateExpr right

  validateExpr = \case
    ELocated _ expression -> validateExpr expression
    EInteger{} -> pure ()
    EFloat{} -> pure ()
    EUnicode{} -> pure ()
    EText{} -> pure ()
    EForeign{} -> Left "internal misplaced fremmed marker"
    EVar{} -> pure ()
    EAscribe expression annotation -> validateExpr expression >> validateType annotation
    EApply function arguments -> validateExpr function >> mapM_ validateExpr arguments
    ERecord fields -> mapM_ (validateExpr . snd) fields
    EField base _ -> validateExpr base
    EUpdate base updates -> validateExpr base >> mapM_ validateUpdate updates
    ETry body returned cases -> do
      validateExpr body
      mapM_ (\(ReturnCase _ result) -> validateExpr result) returned
      mapM_ (\(HandlerCase _ _ result) -> validateExpr result) cases
    EMatch scrutinees cases -> do
      mapM_ validateExpr scrutinees
      mapM_ (\(MatchCase _ result) -> validateExpr result) cases
    EBlock nested body -> mapM_ validateDecl nested >> validateExpr body
    EWithEvidence function evidence -> validateExpr function >> mapM_ validateEvidence evidence

  validateUpdate = \case
    RecordSet _ value -> validateExpr value
    RecordRemove{} -> pure ()

  validateEvidence = \case
    EvidenceHole{} -> Left "internal unresolved type-class evidence"
    EvidenceLocal{} -> pure ()
    EvidenceFill _ required parents -> mapM_ validateEvidence required >> mapM_ validateEvidence parents

  validateType = \case
    TypeName{} -> pure ()
    TypeApply _ arguments -> mapM_ validateType arguments
    TypeRecord fields -> mapM_ (validateType . snd) fields
    TypeArrow arguments effects result -> mapM_ validateType arguments >> mapM_ validateType effects >> validateType result
