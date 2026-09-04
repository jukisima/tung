module Test.Diagnostic (group) where

import Data.Map.Strict qualified as Map
import Test.Harness (Group, Test, expectEq)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = Harness.group "diagnostic" [locatedTokens, locatedForeign, namedTypeError, repeatedName, precedingTypeAlias, precedingData, finalExpression, unicodeOffset, parsePosition, adjacentDeclarationEvidence, adjacentImportedEvidence, importedTypeError, nestedImportedTypeError, renderedProtocol, renderedFile]

locatedTokens :: Test
locatedTokens = case lexLocatedTokens "  answer + 1" of
  Left message -> pure (Just ("located tokens: " ++ message))
  Right tokens -> expectEq "located tokens" [SourceSpan 2 8, SourceSpan 9 10, SourceSpan 11 12] (map locatedSpan tokens)

locatedForeign :: Test
locatedForeign = case checkEditorDiagnosticWithImports source Map.empty of
  Nothing -> pure Nothing
  Just diagnostic -> pure (Just ("located fremmed declaration: " ++ show diagnostic))
 where
  source = "let plus: ℤ → ℤ → ℤ = 'add-integer' fremmed"

namedTypeError :: Test
namedTypeError =
  expectDiagnostic "named type error" "let value: text = 1" TypeDiagnostic (SourceSpan 18 19)

repeatedName :: Test
repeatedName =
  expectDiagnostic "later repeated name" "let value = 1 let other: text = value" TypeDiagnostic (SourceSpan 32 37)

precedingTypeAlias :: Test
precedingTypeAlias =
  expectDiagnostic "term after type alias" "let-ilk count = ℤ let value: text = ℤ" TypeDiagnostic (SourceSpan 36 37)

precedingData :: Test
precedingData =
  expectDiagnostic "term after data declaration" "ilk item { value } let other: text = value" TypeDiagnostic (SourceSpan 37 42)

finalExpression :: Test
finalExpression =
  expectDiagnostic "final expression after type alias" "let-ilk count = ℤ yield ℤ" TypeDiagnostic (SourceSpan 24 25)

unicodeOffset :: Test
unicodeOffset =
  expectDiagnostic "unicode source offset" "let 😀 = 1 let value: text = 😀" TypeDiagnostic (SourceSpan 29 31)

parsePosition :: Test
parsePosition =
  expectDiagnostic "parse position" "\n  'unterminated" ParseDiagnostic (SourceSpan 16 16)

adjacentDeclarationEvidence :: Test
adjacentDeclarationEvidence =
  case checkEditorDiagnosticWithImports source Map.empty of
    Nothing -> pure Nothing
    Just diagnostic -> pure (Just ("adjacent declaration evidence: " ++ show diagnostic))
 where
  source =
    unlines
      [ "ilk truth { yea, nay }"
      , "frame a less { let a < a: truth }"
      , "fill ℤ less { let x < y = yea }"
      , "let (score: ℤ) checked: ℤ = match score < 0 { yea @ score, nay @ score }"
      ]

adjacentImportedEvidence :: Test
adjacentImportedEvidence =
  case checkEditorDiagnosticWithImports source (Map.singleton "dep.tung" dependency) of
    Nothing -> pure Nothing
    Just diagnostic -> pure (Just ("adjacent imported evidence: " ++ show diagnostic))
 where
  source = "use dep.tung\nlet checked: truth = 1 < 0"
  dependency =
    unlines
      [ "show ilk truth { yea, nay }"
      , "show frame a less { let a ≤ a: truth }"
      , "graiþ a less show let (a: a, b: a) <: truth = a ≤ b"
      , "fill ℤ less { let x ≤ y = yea }"
      ]

renderedProtocol :: Test
renderedProtocol =
  expectEq
    "rendered diagnostic protocol"
    "tung-diagnostic\ttype\tdep.tung\t4\t9\ntype error: in 'value'"
    (renderDiagnostic (Diagnostic TypeDiagnostic (Just "dep.tung") (SourceSpan 4 9) "type error: in 'value'"))

renderedFile :: Test
renderedFile =
  expectEq
    "rendered file diagnostic"
    "/tmp/dep.tung:2:3:2:5: type error: bad value"
    (renderFileDiagnostic "/tmp/dep.tung" "😀\n  no" (Diagnostic TypeDiagnostic (Just "dep.tung") (SourceSpan 5 7) "type error: bad value"))

importedTypeError :: Test
importedTypeError =
  expectImportedDiagnostic
    "imported type error"
    "use dep.tung"
    (Map.singleton "dep.tung" "let value: text = 1")
    "dep.tung"
    (SourceSpan 18 19)

nestedImportedTypeError :: Test
nestedImportedTypeError =
  expectImportedDiagnostic
    "nested imported type error"
    "use dep.tung"
    ( Map.fromList
        [ ("dep.tung", "use nested.tung")
        , ("nested.tung", "let value: text = 1")
        ]
    )
    "nested.tung"
    (SourceSpan 18 19)

expectImportedDiagnostic :: String -> String -> Map.Map String String -> FilePath -> SourceSpan -> Test
expectImportedDiagnostic name source imports path span = case checkEditorDiagnosticWithImports source imports of
  Nothing -> pure (Just (name ++ ": expected a diagnostic"))
  Just diagnostic -> do
    pathFailure <- expectEq (name ++ " path") (Just path) (diagnosticPath diagnostic)
    spanFailure <- expectEq (name ++ " span") span (diagnosticSpan diagnostic)
    pure (pathFailure <> spanFailure)

expectDiagnostic :: String -> String -> DiagnosticKind -> SourceSpan -> Test
expectDiagnostic name source kind span = case checkEditorDiagnosticWithImports source Map.empty of
  Nothing -> pure (Just (name ++ ": expected a diagnostic"))
  Just diagnostic -> do
    kindFailure <- expectEq (name ++ " kind") kind (diagnosticKind diagnostic)
    spanFailure <- expectEq (name ++ " span") span (diagnosticSpan diagnostic)
    pure (kindFailure <> spanFailure)
