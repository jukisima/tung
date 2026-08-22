{-# LANGUAGE PatternSynonyms #-}

{- | source lexer. a name containeth any non-space character not reserved by
'specialNameChars'; literals and comments are consumed before name parsing.
-}
module Tung.Token (
  Token (TIdent, TInteger, TFloat, TUnicode, TText, TLet, TGraith, TShow, TShowIlk, TBring, TLetIlk, TKin, TDeed, TYield, TForeign, TShape, TFill, TLaw, TMatch, TTry, TLParen, TRParen, TLBrace, TRBrace, TLBracket, TRBracket, TColon, TComma, TMapsTo, TArrow, TBang, TEquals, TSemicolon, TDot, TDollar),
  SourceSpan (..),
  LocatedToken (..),
  tokenSpan,
  lexTokens,
  lexLocatedTokens,
  keywordNames,
  languageKeywordNames,
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

data SourceSpan = SourceSpan
  { spanStart :: !Int
  , spanEnd :: !Int
  }
  deriving (Eq, Show)

data Token = Token !(Maybe SourceSpan) !TokenKind

data TokenKind
  = KTIdent String
  | KTInteger Integer
  | KTFloat Double
  | KTUnicode Char
  | KTText String
  | KTLet
  | KTGraith
  | KTShow
  | KTShowIlk
  | KTBring
  | KTLetIlk
  | KTKin
  | KTDeed
  | KTYield
  | KTForeign
  | KTShape
  | KTFill
  | KTLaw
  | KTMatch
  | KTTry
  | KTLParen
  | KTRParen
  | KTLBrace
  | KTRBrace
  | KTLBracket
  | KTRBracket
  | KTColon
  | KTComma
  | KTMapsTo
  | KTArrow
  | KTBang
  | KTEquals
  | KTSemicolon
  | KTDot
  | KTDollar
  deriving (Eq, Show)

instance Eq Token where
  Token _ left == Token _ right = left == right

instance Show Token where
  show (Token _ kind) = drop 1 (show kind)

pattern TIdent :: String -> Token
pattern TIdent name <- Token _ (KTIdent name)
 where
  TIdent name = Token Nothing (KTIdent name)

pattern TInteger :: Integer -> Token
pattern TInteger value <- Token _ (KTInteger value)
 where
  TInteger value = Token Nothing (KTInteger value)

pattern TFloat :: Double -> Token
pattern TFloat value <- Token _ (KTFloat value)
 where
  TFloat value = Token Nothing (KTFloat value)

pattern TUnicode :: Char -> Token
pattern TUnicode value <- Token _ (KTUnicode value)
 where
  TUnicode value = Token Nothing (KTUnicode value)

pattern TText :: String -> Token
pattern TText value <- Token _ (KTText value)
 where
  TText value = Token Nothing (KTText value)

pattern TLet, TGraith, TShow, TShowIlk, TBring, TLetIlk, TKin, TDeed, TYield, TForeign, TShape, TFill, TLaw, TMatch, TTry, TLParen, TRParen, TLBrace, TRBrace, TLBracket, TRBracket, TColon, TComma, TMapsTo, TArrow, TBang, TEquals, TSemicolon, TDot, TDollar :: Token
pattern TLet <- Token _ KTLet where TLet = Token Nothing KTLet
pattern TGraith <- Token _ KTGraith where TGraith = Token Nothing KTGraith
pattern TShow <- Token _ KTShow where TShow = Token Nothing KTShow
pattern TShowIlk <- Token _ KTShowIlk where TShowIlk = Token Nothing KTShowIlk
pattern TBring <- Token _ KTBring where TBring = Token Nothing KTBring
pattern TLetIlk <- Token _ KTLetIlk where TLetIlk = Token Nothing KTLetIlk
pattern TKin <- Token _ KTKin where TKin = Token Nothing KTKin
pattern TDeed <- Token _ KTDeed where TDeed = Token Nothing KTDeed
pattern TYield <- Token _ KTYield where TYield = Token Nothing KTYield
pattern TForeign <- Token _ KTForeign where TForeign = Token Nothing KTForeign
pattern TShape <- Token _ KTShape where TShape = Token Nothing KTShape
pattern TFill <- Token _ KTFill where TFill = Token Nothing KTFill
pattern TLaw <- Token _ KTLaw where TLaw = Token Nothing KTLaw
pattern TMatch <- Token _ KTMatch where TMatch = Token Nothing KTMatch
pattern TTry <- Token _ KTTry where TTry = Token Nothing KTTry
pattern TLParen <- Token _ KTLParen where TLParen = Token Nothing KTLParen
pattern TRParen <- Token _ KTRParen where TRParen = Token Nothing KTRParen
pattern TLBrace <- Token _ KTLBrace where TLBrace = Token Nothing KTLBrace
pattern TRBrace <- Token _ KTRBrace where TRBrace = Token Nothing KTRBrace
pattern TLBracket <- Token _ KTLBracket where TLBracket = Token Nothing KTLBracket
pattern TRBracket <- Token _ KTRBracket where TRBracket = Token Nothing KTRBracket
pattern TColon <- Token _ KTColon where TColon = Token Nothing KTColon
pattern TComma <- Token _ KTComma where TComma = Token Nothing KTComma
pattern TMapsTo <- Token _ KTMapsTo where TMapsTo = Token Nothing KTMapsTo
pattern TArrow <- Token _ KTArrow where TArrow = Token Nothing KTArrow
pattern TBang <- Token _ KTBang where TBang = Token Nothing KTBang
pattern TEquals <- Token _ KTEquals where TEquals = Token Nothing KTEquals
pattern TSemicolon <- Token _ KTSemicolon where TSemicolon = Token Nothing KTSemicolon
pattern TDot <- Token _ KTDot where TDot = Token Nothing KTDot
pattern TDollar <- Token _ KTDollar where TDollar = Token Nothing KTDollar

{-# COMPLETE TIdent, TInteger, TFloat, TUnicode, TText, TLet, TGraith, TShow, TShowIlk, TBring, TLetIlk, TKin, TDeed, TYield, TForeign, TShape, TFill, TLaw, TMatch, TTry, TLParen, TRParen, TLBrace, TRBrace, TLBracket, TRBracket, TColon, TComma, TMapsTo, TArrow, TBang, TEquals, TSemicolon, TDot, TDollar #-}

tokenSpan :: Token -> Maybe SourceSpan
tokenSpan (Token span _) = span

withTokenSpan :: SourceSpan -> Token -> Token
withTokenSpan span (Token _ kind) = Token (Just span) kind

withoutTokenSpan :: Token -> Token
withoutTokenSpan (Token _ kind) = Token Nothing kind

data LocatedToken = LocatedToken
  { locatedSpan :: !SourceSpan
  , locatedToken :: !Token
  }
  deriving (Eq, Show)

type Lexer = Parsec Void String

lexTokens :: String -> Either String [Token]
lexTokens = fmap (map (withoutTokenSpan . locatedToken)) . lexLocatedTokens

lexLocatedTokens :: String -> Either String [LocatedToken]
lexLocatedTokens source =
  case M.parse (spaceConsumer *> many locatedTokenParser <* M.eof) "source" source of
    Left err -> Left (M.errorBundlePretty err)
    Right toks -> Right (map convertSpan toks)
 where
  offsets = IntMap.fromList (zip [0 ..] (scanl (+) 0 (map utf16Width source)))
  convertSpan LocatedToken{locatedSpan = SourceSpan start end, locatedToken} =
    let span = SourceSpan (offsets IntMap.! start) (offsets IntMap.! end)
     in LocatedToken span (withTokenSpan span locatedToken)

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
  TUnicode <$> M.choice [escapedCodePointParser, scalarCodePointParser]

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
    , decimalEscapeParser
    ]

decimalEscapeParser :: Lexer Char
decimalEscapeParser = do
  digits <- some C.digitChar
  _ <- C.char ';'
  decimalCodePoint digits

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

-- handler words are contextual rather than lexer tokens, but editor metadata
-- still presenteth them as language keywords.
languageKeywordNames :: [String]
languageKeywordNames = keywordNames ++ ["return", "resume"]

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
  , ("yield", TYield)
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
