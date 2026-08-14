module Test.Token (group) where

import Jbini
import Test.Harness (Group, Test, expectEq, expectPrefix)
import Test.Harness qualified as Harness

group :: IO Group
group = Harness.group "token" (map lexCase lexCases ++ map lexError lexErrors)

lexCases :: [(String, String, [Token])]
lexCases =
  [ ("field access stays one name", "person@age", [TIdent "person@age"])
  , ("qualified operator stays one name", "list@.*", [TIdent "list@.*"])
  , ("unicode operator is a name", "≡", [TIdent "≡"])
  , ("minus signs an integer", "-1", [TInteger (-1)])
  , ("minus signs a float", "-1.25", [TFloat "-1.25"])
  , ("plus remains a name", "+1", [TIdent "+1"])
  , ("double minus remains a name", "--1", [TIdent "--1"])
  , ("dot prepend is one name", ".*", [TIdent ".*"])
  , ("standalone dot is syntax", ".", [TDot])
  , ("backslash is an ordinary name", "\\\\", [TIdent "\\\\"])
  , ("comment ends at newline", "1 # hidden\n2", [TInteger 1, TInteger 2])
  , ("character escapes", "`\\n `\\r `\\t `\\' `\\\\", map TChar ["\n", "\r", "\t", "'", "\\"])
  , ("character decimal unicode", "`\\{65} `{23383}", [TChar "A", TChar "字"])
  , ("string escapes", "'\\n\\r\\t\\'\\\\\\{65}'", [TString "\n\r\t'\\A"])
  , ("all keywords", unwords keywordNames, map keywordToken keywordNames)
  ]

lexErrors :: [(String, String)]
lexErrors =
  [ ("unterminated string", "'no")
  , ("unknown string escape", "'\\q'")
  , ("unicode escape above range", "`\\{1114112}")
  , ("empty unicode escape", "`\\{}")
  , ("bare character marker", "`")
  ]

lexCase :: (String, String, [Token]) -> Test
lexCase (name, source, expected) = case lexTokens source of
  Left message -> expectPrefix name "tokenization should succeed" message
  Right actual -> expectEq name expected actual

lexError :: (String, String) -> Test
lexError (name, source) = case lexTokens source of
  Left _ -> pure Nothing
  Right actual -> pure (Just (name ++ ": unexpectedly tokenized as " ++ show actual))

keywordToken :: String -> Token
keywordToken = \case
  "let" -> TLet
  "graith" -> TGraith
  "show" -> TShow
  "show-ilk" -> TShowIlk
  "bring" -> TBring
  "let-ilk" -> TLetIlk
  "choose" -> TChoose
  "deed" -> TDeed
  "shape" -> TShape
  "fill" -> TFill
  "match" -> TMatch
  "try" -> TTry
  name -> error ("missing keyword token in test: " ++ name)
