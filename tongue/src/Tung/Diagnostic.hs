-- | structured source diagnostics for command-line and editor clients.
module Tung.Diagnostic (
  Diagnostic (..),
  DiagnosticKind (..),
  checkDiagnosticWithImports,
  checkEditorDiagnosticWithImports,
  diagnoseResult,
  renderFileDiagnostic,
  renderDiagnostic,
) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, isSuffixOf, tails)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Tung.Parse (ParsedSource (..), parseLocated)
import Tung.Token
import Tung.Type (TypeFailure (..), checkEditorProgramWithImportsDetailed, checkProgramWithImportsDetailed)

data DiagnosticKind = ParseDiagnostic | TypeDiagnostic
  deriving (Eq, Show)

data Diagnostic = Diagnostic
  { diagnosticKind :: !DiagnosticKind
  , diagnosticPath :: Maybe FilePath
  , diagnosticSpan :: !SourceSpan
  , diagnosticMessage :: String
  }
  deriving (Eq, Show)

checkEditorDiagnosticWithImports :: String -> Map.Map String String -> Maybe Diagnostic
checkEditorDiagnosticWithImports source imports = case parseLocated source of
  Left message -> Just (diagnoseResult source ("parse error: " ++ message))
  Right ParsedSource{parsedProgram} -> diagnosticFromFailure source imports (checkEditorProgramWithImportsDetailed parsedProgram imports)

checkDiagnosticWithImports :: Bool -> String -> Map.Map String String -> Maybe Diagnostic
checkDiagnosticWithImports runnable source imports = case parseLocated source of
  Left message -> Just (diagnoseResult source ("parse error: " ++ message))
  Right ParsedSource{parsedProgram} -> diagnosticFromFailure source imports (checkProgramWithImportsDetailed parsedProgram imports runnable)

diagnosticFromFailure :: String -> Map.Map String String -> Maybe TypeFailure -> Maybe Diagnostic
diagnosticFromFailure source imports = fmap \TypeFailure{typeFailureMessage, typeFailureSpan, typeFailurePath} ->
  let ownerSource = maybe source (\path -> Map.findWithDefault source path imports) typeFailurePath
   in Diagnostic
        { diagnosticKind = TypeDiagnostic
        , diagnosticPath = typeFailurePath
        , diagnosticSpan = fromMaybe (findSourceSpan ownerSource typeFailureMessage) typeFailureSpan
        , diagnosticMessage = "type error: " ++ typeFailureMessage
        }

diagnoseResult :: String -> String -> Diagnostic
diagnoseResult source message =
  Diagnostic
    { diagnosticKind = if "parse error:" `isPrefixOf` message then ParseDiagnostic else TypeDiagnostic
    , diagnosticPath = Nothing
    , diagnosticSpan = findSourceSpan source message
    , diagnosticMessage = message
    }

renderDiagnostic :: Diagnostic -> String
renderDiagnostic Diagnostic{diagnosticKind, diagnosticPath, diagnosticSpan = SourceSpan start end, diagnosticMessage} =
  intercalate "\t" ["tung-diagnostic", renderKind diagnosticKind, fromMaybe "" diagnosticPath, show start, show end] ++ "\n" ++ diagnosticMessage

renderFileDiagnostic :: FilePath -> String -> Diagnostic -> String
renderFileDiagnostic path source Diagnostic{diagnosticSpan = SourceSpan start end, diagnosticMessage} =
  intercalate ":" [path, show startLine, show startColumn, show endLine, show endColumn] ++ ": " ++ diagnosticMessage
 where
  (startLine, startColumn) = sourcePosition source start
  (endLine, endColumn) = sourcePosition source end

sourcePosition :: String -> Int -> (Int, Int)
sourcePosition source wanted = go 0 1 1 source
 where
  go _ line column [] = (line, column)
  go offset line column _ | offset >= wanted = (line, column)
  go offset line _ ('\n' : rest) = go (offset + 1) (line + 1) 1 rest
  go offset line column (character : rest) =
    let width = if fromEnum character > 0xffff then 2 else 1
     in go (offset + width) line (column + width) rest

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
    | "use" `isInfixOf` message = find ((== TUse) . locatedToken) tokens
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
