-- | compiler-owned language metadata consumed by editor and documentation tools.
module Tung.Metadata (languageMetadata) where

import Data.List (intercalate)
import Tung.Primitive (primitiveTypeNames)
import Tung.Token (languageKeywordNames, specialNameChars)

languageMetadata :: String
languageMetadata =
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
