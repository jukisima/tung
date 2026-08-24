{- | authoritative two-space formatting for the cli and editor. the formatter
is structural so comments and incomplete source survive without parsing or
type checking; formatting is idempotent.
-}
module Tung.Format (formatSource) where

import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd, intercalate, isPrefixOf, isSuffixOf, mapAccumL, stripPrefix)

data FormatState = FormatState
  { formatDepth :: !Int
  , formatLexicalState :: !LexicalState
  , formatContinuation :: !Int
  , formatContinuationBlocks :: ![(Int, Int)]
  }

data LexicalState = Code | Text | BlockComment

formatSource :: String -> String
formatSource source =
  let (_, formatted) = mapAccumL formatLine initialState (sourceLines source)
      result = intercalate "\n" formatted
   in if "\n" `isSuffixOf` source
        then dropWhileEnd (== '\n') result ++ "\n"
        else result

initialState :: FormatState
initialState = FormatState 0 Code 0 []

formatLine :: FormatState -> String -> (FormatState, String)
formatLine state@FormatState{formatLexicalState = BlockComment} line =
  let (_, lexicalState, _) = scanLine BlockComment line
   in (state{formatLexicalState = lexicalState}, line)
formatLine state line
  | null content = (state{formatContinuation = 0}, "")
  | otherwise =
      let closes = leadingClosures content
          startsDeclaration = declarationHead content
          startsComment = commentHead content
          lineContinuation = if closes > 0 || startsDeclaration || startsComment then 0 else formatContinuation state
          lineDepth = max 0 (formatDepth state - closes + lineContinuation)
          (delta, lexicalState, code) = scanLine (formatLexicalState state) content
          oldDepth = formatDepth state
          balancedDepth = max 0 (oldDepth + delta + if lineContinuation > 0 && delta > 0 then lineContinuation else 0)
          blocks =
            if lineContinuation > 0 && delta > 0
              then (oldDepth + lineContinuation, lineContinuation) : formatContinuationBlocks state
              else formatContinuationBlocks state
          (depth, remainingBlocks) = if delta < 0 then releaseBlocks balancedDepth blocks else (balancedDepth, blocks)
          continuation
            | startsComment = formatContinuation state
            | closes > 0 = 0
            | delta == 0 && lineContinues code (lineContinuation > 0) = 1
            | keepsContinuation (lineContinuation > 0) delta = max 2 lineContinuation
            | otherwise = 0
          next = FormatState depth lexicalState continuation remainingBlocks
       in (next, replicate (lineDepth * 2) ' ' ++ content)
 where
  content = trim line

releaseBlocks :: Int -> [(Int, Int)] -> (Int, [(Int, Int)])
releaseBlocks depth blocks@((closingDepth, extraDepth) : rest)
  | closingDepth == depth = releaseBlocks (max 0 (depth - extraDepth)) rest
  | otherwise = (depth, blocks)
releaseBlocks depth [] = (depth, [])

sourceLines :: String -> [String]
sourceLines = map (dropWhileEnd (== '\r')) . lines

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

leadingClosures :: String -> Int
leadingClosures = length . takeWhile (`elem` ("})" :: String))

declarationHead :: String -> Bool
declarationHead line = any (`startsWord` line) ["bring", "graiþ", "yield", "show", "show-ilk", "let", "let-ilk", "kin", "deed", "shape", "fill", "law"]

commentHead :: String -> Bool
commentHead line = "#" `isPrefixOf` line || "/*" `isPrefixOf` line

startsWord :: String -> String -> Bool
startsWord word line = case stripPrefix word line of
  Just [] -> True
  Just (next : _) -> isSpace next
  Nothing -> False

lineContinues :: String -> Bool -> Bool
lineContinues code continued =
  "=" `isSuffixOf` code
    || (startsWord "law" code && ":" `isSuffixOf` code)
    || (continued && ":" `isPrefixOf` code)
    || ("let" `elem` words code && '=' `notElem` code)

keepsContinuation :: Bool -> Int -> Bool
keepsContinuation continued delimiterDelta = continued && delimiterDelta == 0

scanLine :: LexicalState -> String -> (Int, LexicalState, String)
scanLine started input =
  let (delta, lexicalState, reversedCode) = go 0 started True [] input
   in (delta, lexicalState, dropWhileEnd isSpace (reverse reversedCode))
 where
  go delta lexicalState _ code [] = (delta, lexicalState, code)
  go delta BlockComment collect code ('*' : '/' : rest) = go delta Code collect code rest
  go delta BlockComment collect code (_ : rest) = go delta BlockComment collect code rest
  go delta Text collect code ('\\' : _ : rest) = go delta Text collect code rest
  go delta Text collect code ('\'' : rest) = go delta Code collect code rest
  go delta Text collect code (_ : rest) = go delta Text collect code rest
  go delta Code _ code ('#' : _) = (delta, Code, code)
  go delta Code _ code ('/' : '*' : rest) = go delta BlockComment False code rest
  go delta Code collect code ('\'' : rest) = go delta Text collect code rest
  go delta Code collect code ('`' : rest) =
    go delta Code collect code (characterLiteralTail rest)
  go delta Code collect code (character : rest)
    | character `elem` ("{(" :: String) = go (delta + 1) Code collect next rest
    | character `elem` ("})" :: String) = go (delta - 1) Code collect next rest
    | otherwise = go delta Code collect next rest
   where
    next = keep character collect code

  keep character collect code = if collect then character : code else code

characterLiteralTail :: String -> String
characterLiteralTail [] = []
characterLiteralTail ('\\' : rest) = case rest of
  [] -> []
  first : remaining
    | isDigit first ->
        let (_, afterDigits) = span isDigit rest
         in case afterDigits of
              ';' : after -> after
              _ -> afterDigits
    | otherwise -> remaining
characterLiteralTail (_ : rest) = rest
