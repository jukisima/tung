module Test.Diagnostic (group) where

import Data.Map.Strict qualified as Map
import Test.Harness (Group, Test, expectEq)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = Harness.group "diagnostic" (tokenCases ++ diagnosticCases ++ importedCases ++ acceptedCases ++ [renderedProtocol, renderedFile])

tokenCases :: [Test]
tokenCases =
  [ expectEq ("located tokens " ++ show source) (Right spans) (map locatedSpan <$> lexLocatedTokens source)
  | (source, spans) <-
      [ ("", [])
      , (" # comment\n /* comment */ ", [])
      , ("  answer + 1", [SourceSpan 2 8, SourceSpan 9 10, SourceSpan 11 12])
      , ("😀 `😀 '\\128512;'", [SourceSpan 0 2, SourceSpan 3 6, SourceSpan 7 17])
      ]
  ]

diagnosticCases :: [Test]
diagnosticCases =
  [ expectEq name (Just (kind, Nothing, span)) (summary <$> checkEditorDiagnosticWithImports source Map.empty)
  | (name, source, kind, span) <-
      [ ("named type error", "let value: text = 1", TypeDiagnostic, SourceSpan 18 19)
      , ("later repeated name", "let value = 1 let other: text = value", TypeDiagnostic, SourceSpan 32 37)
      , ("term after type alias", "let-ilk count = ℤ let value: text = ℤ", TypeDiagnostic, SourceSpan 36 37)
      , ("term after data declaration", "ilk item { value } let other: text = value", TypeDiagnostic, SourceSpan 37 42)
      , ("final expression after type alias", "let-ilk count = ℤ yield ℤ", TypeDiagnostic, SourceSpan 24 25)
      , ("unicode source offset", "let 😀 = 1 let value: text = 😀", TypeDiagnostic, SourceSpan 29 31)
      , ("lexer eof position", "\n  'unterminated", ParseDiagnostic, SourceSpan 16 16)
      , ("lexer unicode eof position", "'😀", ParseDiagnostic, SourceSpan 3 3)
      , ("parser retaineþ the later failure position", "let value = 1 let other = )", ParseDiagnostic, SourceSpan 26 27)
      , ("last expression span excludeþ trailing whitespace", "let 😀 = 1 let value: text = 😀   \n", TypeDiagnostic, SourceSpan 29 31)
      , ("last expression span excludeþ trailing comments", "let 😀 = 1 let value: text = 😀 /* tail */", TypeDiagnostic, SourceSpan 29 31)
      ]
  ]
 where
  summary diagnostic = (diagnosticKind diagnostic, diagnosticPath diagnostic, diagnosticSpan diagnostic)

importedCases :: [Test]
importedCases =
  [ expectEq ("imported diagnostic " ++ path ++ ": " ++ dependency) (Just (Just path, span)) (summary <$> checkEditorDiagnosticWithImports "use dep.tung" imports)
  | (dependency, span) <- [("let value = 1 let other = )", SourceSpan 26 27), ("let value: text = 1", SourceSpan 18 19), ("'😀", SourceSpan 3 3)]
  , (path, imports) <-
      [ ("dep.tung", Map.singleton "dep.tung" dependency)
      , ("nested.tung", Map.fromList [("dep.tung", "use nested.tung"), ("nested.tung", dependency)])
      ]
  ]
 where
  summary diagnostic = (diagnosticPath diagnostic, diagnosticSpan diagnostic)

acceptedCases :: [Test]
acceptedCases =
  [ expectEq name Nothing (checkEditorDiagnosticWithImports source imports)
  | (name, source, imports) <-
      [ ("located fremmed declaration", "let plus: ℤ → ℤ → ℤ = 'add-integer' fremmed", Map.empty)
      ,
        ( "adjacent declaration evidence"
        , "ilk truth { yea, nay } frame a less { let a < a: truth } fill ℤ less { let x < y = yea } let (score: ℤ) checked: ℤ = match score < 0 { yea @ score, nay @ score }"
        , Map.empty
        )
      ,
        ( "adjacent imported evidence"
        , "use dep.tung\nlet checked: truth = 1 < 0"
        , Map.singleton "dep.tung" "show ilk truth { yea, nay } show frame a less { let a ≤ a: truth } graiþ a less show let (a: a, b: a) <: truth = a ≤ b fill ℤ less { let x ≤ y = yea }"
        )
      ]
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
