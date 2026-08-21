{- | public compiler, evaluator, and bookhoard api.
internal modules remain hidden by the cabal package.
-}
module Tung (
  Program (..),
  Decl (..),
  TypeExpr (..),
  Evidence (..),
  Expr (..),
  Pattern (..),
  MatchCase (..),
  HandlerCase (..),
  RecordUpdate (..),
  EffectOp (..),
  Ctor (..),
  ShapeMember (..),
  ShapeNeed (..),
  Token (..),
  SourceSpan (..),
  LocatedToken (..),
  ParsedSource (..),
  Diagnostic (..),
  DiagnosticKind (..),
  HostBinding (..),
  HostRole (..),
  HostSignature (..),
  HostType (..),
  Project (..),
  Ty (..),
  Scheme (..),
  EnvLookup (..),
  TcContext (..),
  TcState (..),
  TcResult (..),
  TypeFailure (..),
  CoreProgram,
  parse,
  parseLocated,
  lexTokens,
  lexLocatedTokens,
  keywordNames,
  languageKeywordNames,
  specialNameChars,
  enterImport,
  validateProgram,
  check,
  checkWithImports,
  checkEditorWithImports,
  checkDiagnosticWithImports,
  checkEditorProgramWithImportsDetailed,
  checkEditorDiagnosticWithImports,
  renderFileDiagnostic,
  renderDiagnostic,
  checkRunnableWithImports,
  elaborateProgramWithImports,
  elaborateInteractiveProgramWithImports,
  typeOfWithImports,
  baseContext,
  inferProgramContext,
  lookupEnv,
  canonicalTypeName,
  primitiveTypeNames,
  showTy,
  evaluate,
  evaluateWithImports,
  evaluateWithArgsAndImports,
  evaluateCoreProgram,
  evaluateMainCoreProgram,
  evaluateMainCoreProgramWithArgs,
  evaluateMainWithImports,
  evaluateMainWithArgsAndImports,
  bookhoardImportFiles,
  readBookhoardImports,
  loadProjectFile,
  loadProjectFileWithRoots,
  loadProjectSource,
  loadProjectSourceWithRoots,
  hostBindings,
) where

import Tung.Bookhoard (bookhoardImportFiles, readBookhoardImports)
import Tung.Core (CoreProgram)
import Tung.Diagnostic
import Tung.Evaluate (evaluate, evaluateCoreProgram, evaluateMainCoreProgram, evaluateMainCoreProgramWithArgs, evaluateMainWithArgsAndImports, evaluateMainWithImports, evaluateWithArgsAndImports, evaluateWithImports)
import Tung.Import (enterImport)
import Tung.Parse (ParsedSource (..), parse, parseLocated)
import Tung.Primitive
import Tung.Project
import Tung.Syntax
import Tung.Token (LocatedToken (..), SourceSpan (..), Token (..), keywordNames, languageKeywordNames, lexLocatedTokens, lexTokens, specialNameChars)
import Tung.Type
import Tung.Validate (validateProgram)
