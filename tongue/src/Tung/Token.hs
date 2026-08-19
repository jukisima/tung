{- | source lexer. a name containeth any non-space character not reserved by
'specialNameChars'; literals and comments are consumed before name parsing.
-}
module Tung.Token (
  Token (..),
  SourceSpan (..),
  LocatedToken (..),
  lexTokens,
  lexLocatedTokens,
  keywordNames,
  specialNameChars,
  isNameChar,
  unicodeScalar,
)
where

import Control.Applicative (many, some)
import Data.Char (chr, isSpace, ord)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe, isJust)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

data Token
  = TIdent String
  | TInteger Integer
  | TFloat Double
  | TUnicode Char
  | TText String
  | TLet
  | TGraith
  | TShow
  | TShowIlk
  | TBring
  | TLetIlk
  | TKin
  | TDeed
  | TForeign
  | TShape
  | TFill
  | TLaw
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

data SourceSpan = SourceSpan
  { spanStart :: !Int
  , spanEnd :: !Int
  }
  deriving (Eq, Show)

data LocatedToken = LocatedToken
  { locatedSpan :: !SourceSpan
  , locatedToken :: !Token
  }
  deriving (Eq, Show)

type Lexer = Parsec Void String

lexTokens :: String -> Either String [Token]
lexTokens = fmap (map locatedToken) . lexLocatedTokens

lexLocatedTokens :: String -> Either String [LocatedToken]
lexLocatedTokens source =
  case M.parse (spaceConsumer *> many locatedTokenParser <* M.eof) "source" source of
    Left err -> Left (M.errorBundlePretty err)
    Right toks -> Right (map convertSpan toks)
 where
  offsets = IntMap.fromList (zip [0 ..] (scanl (+) 0 (map utf16Width source)))
  convertSpan token@LocatedToken{locatedSpan = SourceSpan start end} =
    token{locatedSpan = SourceSpan (offsets IntMap.! start) (offsets IntMap.! end)}

utf16Width :: Char -> Int
utf16Width character = if ord character > 0xffff then 2 else 1

locatedTokenParser :: Lexer LocatedToken
locatedTokenParser = lexeme do
  start <- M.getOffset
  token <- rawTokenParser
  end <- M.getOffset
  pure (LocatedToken (SourceSpan start end) token)

rawTokenParser :: Lexer Token
rawTokenParser =
  M.choice
    [ TInteger <$> M.try integerParser
    , TFloat <$> M.try floatParser
    , unicodeLiteralParser
    , textParser
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

integerParser :: Lexer Integer
integerParser = do
  sign <- M.optional (C.char '-')
  value <- L.decimal
  M.notFollowedBy (C.char '.' *> C.digitChar)
  pure (maybe value (const (-value)) sign)

floatParser :: Lexer Double
floatParser = do
  sign <- M.optional (C.char '-')
  whole <- some C.digitChar
  _ <- C.char '.'
  frac <- some C.digitChar
  pure (read (maybe "" (: []) sign ++ whole ++ "." ++ frac))

unicodeLiteralParser :: Lexer Token
unicodeLiteralParser = do
  _ <- C.char '`'
  TUnicode <$> M.choice [escapedCodePointParser, bracedCodePointParser, scalarCodePointParser]

bracedCodePointParser :: Lexer Char
bracedCodePointParser = do
  _ <- C.char '{'
  digits <- some C.digitChar
  _ <- C.char '}'
  decimalCodePoint digits

textParser :: Lexer Token
textParser = TText <$> (C.char '\'' *> many textCharParser <* C.char '\'')

textCharParser :: Lexer Char
textCharParser =
  M.choice
    [ escapedCodePointParser
    , M.satisfy (\codePoint -> codePoint /= '\'' && isUnicodeScalar codePoint)
    ]

escapedCodePointParser :: Lexer Char
escapedCodePointParser = do
  _ <- C.char '\\'
  M.choice
    [ '\n' <$ C.char 'n'
    , '\r' <$ C.char 'r'
    , '\t' <$ C.char 't'
    , '\'' <$ C.char '\''
    , '\\' <$ C.char '\\'
    , bracedCodePointParser
    ]

decimalCodePoint :: String -> Lexer Char
decimalCodePoint digits = maybe (fail "decimal unicode escape must name a unicode scalar value") pure (unicodeScalar (read digits))

scalarCodePointParser :: Lexer Char
scalarCodePointParser = M.satisfy isUnicodeScalar

isUnicodeScalar :: Char -> Bool
isUnicodeScalar = isJust . unicodeScalar . toInteger . ord

-- literals and native integer conversion share this scalar-value boundary.
unicodeScalar :: Integer -> Maybe Char
unicodeScalar value
  | value >= 0 && value <= 0x10ffff && (value < 0xd800 || 0xdfff < value) = Just (chr (fromInteger value))
  | otherwise = Nothing

nameParser :: Lexer Token
nameParser = keywordOrIdent . concat <$> some nameChunk

nameChunk :: Lexer String
nameChunk = M.try (C.string ".*") M.<|> ((: []) <$> M.satisfy isNameChar)

spaceConsumer :: Lexer ()
spaceConsumer = L.space C.space1 (L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

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
  , ("kin", TKin)
  , ("deed", TDeed)
  , ("foreign", TForeign)
  , ("shape", TShape)
  , ("fill", TFill)
  , ("law", TLaw)
  , ("match", TMatch)
  , ("try", TTry)
  ]

isNameChar :: Char -> Bool
isNameChar c = not (isSpace c) && c `notElem` specialNameChars

specialNameChars :: [Char]
specialNameChars = "#(){}[]:,|!=;.$→`'"
