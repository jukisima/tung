-- | structured source diagnostics for command-line and editor clients.
module Tung.Diagnostic (
  Diagnostic (..),
  DiagnosticKind (..),
  checkEditorDiagnosticWithImports,
  diagnoseResult,
  renderDiagnostic,
) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, isSuffixOf, tails)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Tung.Parse (ParsedSource (..), parseLocated)
import Tung.Token
import Tung.Type (TypeFailure (..), checkEditorProgramWithImportsDetailed)

data DiagnosticKind = ParseDiagnostic | TypeDiagnostic
  deriving (Eq, Show)

data Diagnostic = Diagnostic
  { diagnosticKind :: !DiagnosticKind
  , diagnosticSpan :: !SourceSpan
  , diagnosticMessage :: String
  }
  deriving (Eq, Show)

checkEditorDiagnosticWithImports :: String -> Map.Map String String -> Maybe Diagnostic
checkEditorDiagnosticWithImports source imports = case parseLocated source of
  Left message -> Just (diagnoseResult source ("parse error: " ++ message))
  Right ParsedSource{parsedProgram} -> case checkEditorProgramWithImportsDetailed parsedProgram imports of
    Nothing -> Nothing
    Just TypeFailure{typeFailureMessage, typeFailureSpan} ->
      Just
        Diagnostic
          { diagnosticKind = TypeDiagnostic
          , diagnosticSpan = fromMaybe (findSourceSpan source typeFailureMessage) typeFailureSpan
          , diagnosticMessage = "type error: " ++ typeFailureMessage
          }

diagnoseResult :: String -> String -> Diagnostic
diagnoseResult source message =
  Diagnostic
    { diagnosticKind = if "parse error:" `isPrefixOf` message then ParseDiagnostic else TypeDiagnostic
    , diagnosticSpan = findSourceSpan source message
    , diagnosticMessage = message
    }

renderDiagnostic :: Diagnostic -> String
renderDiagnostic Diagnostic{diagnosticKind, diagnosticSpan = SourceSpan start end, diagnosticMessage} =
  intercalate "\t" ["tung-diagnostic", renderKind diagnosticKind, show start, show end] ++ "\n" ++ diagnosticMessage

renderKind :: DiagnosticKind -> String
renderKind ParseDiagnostic = "parse"
renderKind TypeDiagnostic = "type"

findSourceSpan :: String -> String -> SourceSpan
findSourceSpan source message = case lexLocatedTokens source of
  Right tokens -> maybe fallback locatedSpan (findDiagnosticToken message tokens)
  Left _ -> maybe fallback (pointSpan source . uncurry (sourceOffset source)) (parseErrorPosition message)
 where
  fallback = pointSpan source (firstCodeOffset source)

findDiagnosticToken :: String -> [LocatedToken] -> Maybe LocatedToken
findDiagnosticToken message tokens =
  (diagnosticNames message >>= \names -> find (matchesAny names . locatedToken) tokens)
    <|> keywordToken
    <|> listToMaybe tokens
 where
  keywordToken
    | "bring" `isInfixOf` message = find ((== TBring) . locatedToken) tokens
    | otherwise = Nothing

diagnosticNames :: String -> Maybe [String]
diagnosticNames message = case mapMaybe (`quotedAfter` message) prefixes of
  [] -> Nothing
  names -> Just names
 where
  prefixes = ["type error: in '", "while checking let '", "unknown name '", "ambiguous name '", "unknown type '", "ambiguous type '", "handler case '"]

matchesAny :: [String] -> Token -> Bool
matchesAny names (TIdent name) = name `elem` names || any ((`isSuffixOf` name) . ('@' :)) names
matchesAny _ _ = False

quotedAfter :: String -> String -> Maybe String
quotedAfter prefix text = do
  suffix <- find (prefix `isPrefixOf`) (tails text)
  let quoted = takeWhile (/= '\'') (drop (length prefix) suffix)
  if null quoted then Nothing else Just quoted

parseErrorPosition :: String -> Maybe (Int, Int)
parseErrorPosition message =
  listToMaybe
    [ (line, column)
    | suffix <- tails message
    , "source:" `isPrefixOf` suffix
    , let rest = drop 7 suffix
    , [(line, ':' : rest2)] <- [reads rest]
    , [(column, ':' : _)] <- [reads rest2]
    ]

sourceOffset :: String -> Int -> Int -> Int
sourceOffset source line column =
  utf16Length (concat (take (max 0 (line - 1)) (linesWithEnds source)) ++ take (max 0 (column - 1)) currentLine)
 where
  currentLine = fromMaybe "" (listToMaybe (drop (max 0 (line - 1)) (lines source)))

linesWithEnds :: String -> [String]
linesWithEnds [] = []
linesWithEnds text = case break (== '\n') text of
  (line, []) -> [line]
  (line, _ : rest) -> (line ++ "\n") : linesWithEnds rest

firstCodeOffset :: String -> Int
firstCodeOffset source = maybe 0 (utf16Length . (`take` source) . fst) (find (not . isSpace . snd) (zip [0 ..] source))

utf16Length :: String -> Int
utf16Length = sum . map (\character -> if fromEnum character > 0xffff then 2 else 1)

pointSpan :: String -> Int -> SourceSpan
pointSpan source offset = SourceSpan offset (min (utf16Length source) (offset + 1))
