module Jbini.Token (
  Token (..),
  lexTokens,
  keywordNames,
  specialNameChars,
  isNameChar,
)
where

import Control.Applicative (empty, many, some)
import Data.Char (chr, isSpace)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

data Token
  = TIdent String
  | TInteger Int
  | TFloat String
  | TChar String
  | TString String
  | TLet
  | TGraith
  | TShow
  | TShowIlk
  | TBring
  | TLetIlk
  | TChoose
  | TDeed
  | TShape
  | TFill
  | TMatch
  | TTry
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TLBracket
  | TRBracket
  | TColon
  | TComma
  | TMapsTo
  | TArrow
  | TBang
  | TEquals
  | TSemicolon
  | TDot
  | TDollar
  deriving (Eq, Show)

type Lexer = Parsec Void String

lexTokens :: String -> Either String [Token]
lexTokens source =
  case M.parse (spaceConsumer *> many tokenParser <* M.eof) "source" source of
    Left err -> Left (M.errorBundlePretty err)
    Right toks -> Right toks

tokenParser :: Lexer Token
tokenParser =
  lexeme $
    M.choice
      [ TInteger <$> M.try integerParser
      , TFloat <$> M.try floatParser
      , charParser
      , stringParser
      , nameParser
      , singleTokenParser
      ]

singleTokenParser :: Lexer Token
singleTokenParser =
  M.choice
    [ TLParen <$ C.char '('
    , TRParen <$ C.char ')'
    , TLBrace <$ C.char '{'
    , TRBrace <$ C.char '}'
    , TLBracket <$ C.char '['
    , TRBracket <$ C.char ']'
    , TColon <$ C.char ':'
    , TComma <$ C.char ','
    , TMapsTo <$ C.char '|'
    , TBang <$ C.char '!'
    , TEquals <$ C.char '='
    , TSemicolon <$ C.char ';'
    , TDot <$ C.char '.'
    , TDollar <$ C.char '$'
    , TArrow <$ C.char '→'
    ]

integerParser :: Lexer Int
integerParser = do
  sign <- M.optional (C.char '-')
  value <- L.decimal
  M.notFollowedBy (C.char '.' *> C.digitChar)
  pure (maybe value (const (-value)) sign)

floatParser :: Lexer String
floatParser = do
  sign <- M.optional (C.char '-')
  whole <- some C.digitChar
  _ <- C.char '.'
  frac <- some C.digitChar
  pure (maybe "" (: []) sign ++ whole ++ "." ++ frac)

charParser :: Lexer Token
charParser = do
  _ <- C.char '`'
  TChar <$> M.choice [(: []) <$> escapedCharParser, unicodeCharParser, (: []) <$> M.anySingle]

unicodeCharParser :: Lexer String
unicodeCharParser = do
  _ <- C.char '{'
  digits <- some C.digitChar
  _ <- C.char '}'
  decimalUnicode digits

stringParser :: Lexer Token
stringParser = TString <$> (C.char '\'' *> many stringCharParser <* C.char '\'')

stringCharParser :: Lexer Char
stringCharParser =
  M.choice
    [ escapedCharParser
    , M.anySingleBut '\''
    ]

escapedCharParser :: Lexer Char
escapedCharParser = do
  _ <- C.char '\\'
  M.choice
    [ '\n' <$ C.char 'n'
    , '\r' <$ C.char 'r'
    , '\t' <$ C.char 't'
    , '\'' <$ C.char '\''
    , '\\' <$ C.char '\\'
    , unicodeEscapeCharParser
    ]

unicodeEscapeCharParser :: Lexer Char
unicodeEscapeCharParser = do
  _ <- C.char '{'
  digits <- some C.digitChar
  _ <- C.char '}'
  decoded <- decimalUnicode digits
  case decoded of
    [c] -> pure c
    _ -> fail "decimal unicode escape must name one character"

decimalUnicode :: String -> Lexer String
decimalUnicode digits =
  case read digits :: Int of
    n | n >= 0 && n <= 0x10ffff -> pure [chr n]
    _ -> fail "decimal unicode escape out of range"

nameParser :: Lexer Token
nameParser = keywordOrIdent . concat <$> some nameChunk

nameChunk :: Lexer String
nameChunk = M.try (C.string ".*") M.<|> ((: []) <$> M.satisfy isNameChar)

spaceConsumer :: Lexer ()
spaceConsumer = L.space C.space1 (L.skipLineComment "#") empty

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme spaceConsumer

keywordNames :: [String]
keywordNames = map fst keywordTokens

keywordOrIdent :: String -> Token
keywordOrIdent name = fromMaybe (TIdent name) (lookup name keywordTokens)

keywordTokens :: [(String, Token)]
keywordTokens =
  [ ("let", TLet)
  , ("graith", TGraith)
  , ("show", TShow)
  , ("show-ilk", TShowIlk)
  , ("bring", TBring)
  , ("let-ilk", TLetIlk)
  , ("choose", TChoose)
  , ("deed", TDeed)
  , ("shape", TShape)
  , ("fill", TFill)
  , ("match", TMatch)
  , ("try", TTry)
  ]

isNameChar :: Char -> Bool
isNameChar c = not (isSpace c) && c `notElem` specialNameChars

specialNameChars :: [Char]
specialNameChars = "#(){}[]:,|!=;.$→`'"
