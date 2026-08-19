module Test.Diagnostic (group) where

import Data.Map.Strict qualified as Map
import Test.Harness (Group, Test, expectEq)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = Harness.group "diagnostic" [locatedTokens, namedTypeError, repeatedName, precedingTypeAlias, precedingData, finalExpression, unicodeOffset, parsePosition, renderedProtocol]

locatedTokens :: Test
locatedTokens = case lexLocatedTokens "  answer + 1" of
  Left message -> pure (Just ("located tokens: " ++ message))
  Right tokens -> expectEq "located tokens" [SourceSpan 2 8, SourceSpan 9 10, SourceSpan 11 12] (map locatedSpan tokens)

namedTypeError :: Test
namedTypeError =
  expectDiagnostic "named type error" "let value: text = 1;" TypeDiagnostic (SourceSpan 18 19)

repeatedName :: Test
repeatedName =
  expectDiagnostic "later repeated name" "let value = 1; let other: text = value;" TypeDiagnostic (SourceSpan 33 38)

precedingTypeAlias :: Test
precedingTypeAlias =
  expectDiagnostic "term after type alias" "let-ilk count = integer; let value: text = integer;" TypeDiagnostic (SourceSpan 43 50)

precedingData :: Test
precedingData =
  expectDiagnostic "term after data declaration" "kin item { value }; let other: text = value;" TypeDiagnostic (SourceSpan 38 43)

finalExpression :: Test
finalExpression =
  expectDiagnostic "final expression after type alias" "let-ilk count = integer; integer" TypeDiagnostic (SourceSpan 25 32)

unicodeOffset :: Test
unicodeOffset =
  expectDiagnostic "unicode source offset" "let 😀 = 1; let value: text = 😀;" TypeDiagnostic (SourceSpan 30 32)

parsePosition :: Test
parsePosition =
  expectDiagnostic "parse position" "\n  'unterminated" ParseDiagnostic (SourceSpan 16 16)

renderedProtocol :: Test
renderedProtocol =
  expectEq
    "rendered diagnostic protocol"
    "tung-diagnostic\ttype\t4\t9\ntype error: in 'value'"
    (renderDiagnostic (Diagnostic TypeDiagnostic (SourceSpan 4 9) "type error: in 'value'"))

expectDiagnostic :: String -> String -> DiagnosticKind -> SourceSpan -> Test
expectDiagnostic name source kind span = case checkEditorDiagnosticWithImports source Map.empty of
  Nothing -> pure (Just (name ++ ": expected a diagnostic"))
  Just diagnostic -> do
    kindFailure <- expectEq (name ++ " kind") kind (diagnosticKind diagnostic)
    spanFailure <- expectEq (name ++ " span") span (diagnosticSpan diagnostic)
    pure (kindFailure <> spanFailure)
