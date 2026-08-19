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
  enterImport,
  validateProgram,
  check,
  checkWithImports,
  checkEditorWithImports,
  checkEditorProgramWithImportsDetailed,
  checkEditorDiagnosticWithImports,
  renderDiagnostic,
  checkRunnableWithImports,
  elaborateProgramWithImports,
  typeOfWithImports,
  baseContext,
  inferProgramContext,
  lookupEnv,
  canonicalTypeName,
  showTy,
  evaluate,
  evaluateWithImports,
  evaluateWithArgsAndImports,
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
import Tung.Evaluate (evaluate, evaluateMainWithArgsAndImports, evaluateMainWithImports, evaluateWithArgsAndImports, evaluateWithImports)
import Tung.Import (enterImport)
import Tung.Parse (ParsedSource (..), parse, parseLocated)
import Tung.Primitive
import Tung.Project
import Tung.Syntax
import Tung.Token (LocatedToken (..), SourceSpan (..), Token (..), keywordNames, lexLocatedTokens, lexTokens)
import Tung.Type
import Tung.Validate (validateProgram)
