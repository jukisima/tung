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
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

data Token
  = TIdent String
  | TInteger Int
  | TFloat String
  | TChar String
  | TString String
  | TLet
  | TGiven
  | TBring
  | TType
  | TData
  | TEffect
  | TBone
  | TFlesh
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
      , TIdent <$> M.try prependNameParser
      , charParser
      , stringParser
      , singleTokenParser
      , nameParser
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
  sign <- optionalChar '-'
  digits <- some C.digitChar
  M.notFollowedBy (C.char '.' *> C.digitChar)
  pure (signed sign (read digits))

floatParser :: Lexer String
floatParser = do
  sign <- optionalChar '-'
  whole <- some C.digitChar
  _ <- C.char '.'
  frac <- some C.digitChar
  pure (maybe "" (: []) sign ++ whole ++ "." ++ frac)

prependNameParser :: Lexer String
prependNameParser = do
  _ <- C.string ".*"
  rest <- many (M.satisfy isNameChar)
  pure (".*" ++ rest)

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
nameParser = keywordOrIdent <$> some (M.satisfy isNameChar)

spaceConsumer :: Lexer ()
spaceConsumer = L.space C.space1 (L.skipLineComment "#") empty

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme spaceConsumer

optionalChar :: Char -> Lexer (Maybe Char)
optionalChar c = M.optional (C.char c)

signed :: Maybe Char -> Int -> Int
signed (Just '-') n = -n
signed _ n = n

keywordNames :: [String]
keywordNames =
  [ "let"
  , "given"
  , "bring"
  , "type"
  , "data"
  , "effect"
  , "bone"
  , "flesh"
  , "match"
  , "try"
  ]

keywordOrIdent :: String -> Token
keywordOrIdent = \case
  "let" -> TLet
  "given" -> TGiven
  "bring" -> TBring
  "type" -> TType
  "data" -> TData
  "effect" -> TEffect
  "bone" -> TBone
  "flesh" -> TFlesh
  "match" -> TMatch
  "try" -> TTry
  s -> TIdent s

isNameChar :: Char -> Bool
isNameChar c = not (isSpace c) && not (c `elem` specialNameChars)

specialNameChars :: [Char]
specialNameChars = "#(){}[]:,|!=;.$→`'"
