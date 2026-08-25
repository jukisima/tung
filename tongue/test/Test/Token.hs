-- literal, escape, comment, keyword, broad-name, and lexical failure coverage.
module Test.Token (group) where

import Data.List (intercalate)
import System.FilePath ((</>))
import Test.Harness (Group, Test, expectEq, expectPrefix)
import Test.Harness qualified as Harness
import Tung

group :: IO Group
group = do
  metadata <- languageMetadataCase
  Harness.group "token" (metadata : map lexCase lexCases ++ map lexError lexErrors)

languageMetadataCase :: IO Test
languageMetadataCase = do
  actual <- readFile (".." </> "vscode" </> "generated" </> "language-names.json")
  pure $ expectEq "self-hosted language metadata agreeþ with the compiler" expected actual
 where
  expected =
    "{\"keywords\":"
      ++ jsonArray languageKeywordNames
      ++ ",\"primitiveTypes\":"
      ++ jsonArray primitiveTypeNames
      ++ ",\"specialNameChars\":"
      ++ jsonString specialNameChars
      ++ "}\n"

jsonArray :: [String] -> String
jsonArray values = "[" ++ intercalate "," (map jsonString values) ++ "]"

jsonString :: String -> String
jsonString value = "\"" ++ concatMap escape value ++ "\""
 where
  escape '"' = "\\\""
  escape '\\' = "\\\\"
  escape character = [character]

lexCases :: [(String, String, [Token])]
lexCases =
  [ ("field access stayeþ one name", "person@age", [TIdent "person@age"])
  , ("qualified operator stayeþ one name", "list@.*", [TIdent "list@.*"])
  , ("unicode operator is a name", "≡", [TIdent "≡"])
  , ("minus signs an integer", "-1", [TInteger (-1)])
  , ("integer literals are arbitrary precision", "123456789012345678901234567890", [TInteger 123456789012345678901234567890])
  , ("minus signs a float", "-1.25", [TFloat (-1.25)])
  , ("plus remaineþ a name", "+1", [TIdent "+1"])
  , ("double minus remaineþ a name", "--1", [TIdent "--1"])
  , ("dot prepend is one name", ".*", [TIdent ".*"])
  , ("standalone dot is syntax", ".", [TDot])
  , ("backslash is an ordinary name", "\\\\", [TIdent "\\\\"])
  , ("record opener stayeþ one token", "r(", [TParenKeyword "r"])
  , ("left association opener stayeþ one token", "<(", [TParenKeyword "<"])
  , ("right association opener stayeþ one token", ">(", [TParenKeyword ">"])
  , ("separated special openers remain names", "r ( < ( > (", [TIdent "r", TLParen, TIdent "<", TLParen, TIdent ">", TLParen])
  , ("freed characters are ordinary names", "[ ] ;", [TIdent "[", TIdent "]", TIdent ";"])
  , ("comment endeþ at newline", "1 # hidden\n2", [TInteger 1, TInteger 2])
  , ("block comment is skipped", "1 /* hidden { = */ 2", [TInteger 1, TInteger 2])
  , ("unicode escapes", "`\\n `\\r `\\t `\\' `\\\\", map TUnicode ['\n', '\r', '\t', '\'', '\\'])
  , ("unicode decimal code points", "`\\65; `\\23383; `\\128512;", [TUnicode 'A', TUnicode '字', TUnicode '😀'])
  , ("raw astral unicode code point", "`😀", [TUnicode '😀'])
  , ("text escapes", "'\\n\\r\\t\\'\\\\'", [TText "\n\r\t'\\"])
  , ("text decimal code points", "'\\65;\\23383;'", [TText "A字"])
  , ("unicode text", "'λ字'", [TText "λ字"])
  , ("all keywords", unwords keywordNames, map keywordToken keywordNames)
  ]

lexErrors :: [(String, String)]
lexErrors =
  [ ("path-qualified namespace", "data/list@empty")
  , ("unterminated text", "'no")
  , ("unknown text escape", "'\\q'")
  , ("unicode escape above range", "`\\1114112;")
  , ("unicode surrogate escape", "`\\55296;")
  , ("empty unicode escape", "`\\;")
  , ("unterminated decimal unicode escape", "`\\65")
  , ("braced unicode escape", "`\\{65}")
  , ("bare unicode marker", "`")
  , ("unterminated block comment", "1 /* no")
  ]

lexCase :: (String, String, [Token]) -> Test
lexCase (name, source, expected) = case lexTokens source of
  Left message -> expectPrefix name "tokenisation should succeed" message
  Right actual -> expectEq name expected actual

lexError :: (String, String) -> Test
lexError (name, source) = case lexTokens source of
  Left _ -> pure Nothing
  Right actual -> pure (Just (name ++ ": unexpectedly tokenised as " ++ show actual))

keywordToken :: String -> Token
keywordToken = \case
  "let" -> TLet
  "graiþ" -> TGraith
  "show" -> TShow
  "show-ilk" -> TShowIlk
  "bring" -> TBring
  "let-ilk" -> TLetIlk
  "kin" -> TKin
  "deed" -> TDeed
  "yield" -> TYield
  "fremmed" -> TForeign
  "shape" -> TShape
  "fill" -> TFill
  "law" -> TLaw
  "match" -> TMatch
  "try" -> TTry
  name -> error ("missing keyword token in test: " ++ name)
