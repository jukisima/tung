{- | public compiler, evaluator, and bookhoard api.
internal modules remain hidden by the cabal package.
-}
module Tung (
  Program (..),
  Decl (..),
  TypeExpr (..),
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
  Diagnostic (..),
  DiagnosticKind (..),
  HostBinding (..),
  HostRole (..),
  HostSignature (..),
  ModuleId (..),
  SymbolId (..),
  TypeRef (..),
  HandlerTarget (..),
  FillType (..),
  FillId (..),
  Project (..),
  projectSource,
  projectImports,
  ParsedBundle,
  prepareBundle,
  Scheme (..),
  EnvLookup (..),
  TcContext,
  TcResult (..),
  CoreProgram,
  ModuleInterface (..),
  TermExport (..),
  TermKind (..),
  coreInterface,
  coreImportInterface,
  formatSource,
  parse,
  lexTokens,
  lexLocatedTokens,
  keywordNames,
  languageMetadata,
  enterImport,
  validateProgram,
  check,
  checkWithImports,
  checkDiagnosticWithImports,
  checkBundleDiagnostic,
  checkEditorDiagnosticWithImports,
  renderFileDiagnostic,
  renderDiagnostic,
  checkRunnableWithImports,
  elaborateProgramWithImports,
  elaborateInteractiveProgramWithImports,
  typeOfWithImports,
  typeOfBundle,
  baseContext,
  inferProgramContext,
  lookupEnv,
  canonicalTypeName,
  showTy,
  evaluate,
  evaluateWithImports,
  evaluateWithArgsAndImports,
  evaluateCoreProgram,
  evaluateMainCoreProgram,
  evaluateMainWithImports,
  evaluateMainWithArgsAndImports,
  evaluateMainBundleWithArgs,
  bookhoardImportFiles,
  readBookhoardImports,
  loadProjectFile,
  loadProjectFileWithRoots,
  hostBindings,
) where

import Tung.Bookhoard (bookhoardImportFiles, readBookhoardImports)
import Tung.Core (CoreProgram, ModuleInterface (..), coreImportInterface, coreInterface)
import Tung.Diagnostic
import Tung.Evaluate (evaluate, evaluateCoreProgram, evaluateMainBundleWithArgs, evaluateMainCoreProgram, evaluateMainWithArgsAndImports, evaluateMainWithImports, evaluateWithArgsAndImports, evaluateWithImports)
import Tung.Format (formatSource)
import Tung.Identity
import Tung.Import (enterImport)
import Tung.Metadata (languageMetadata)
import Tung.Parse (ParsedBundle, parse, prepareBundle)
import Tung.Primitive
import Tung.Project
import Tung.Syntax
import Tung.Token (LocatedToken (..), SourceSpan (..), Token (..), keywordNames, lexLocatedTokens, lexTokens)
import Tung.Type
import Tung.Validate (validateProgram)
