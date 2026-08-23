-- | pure helpers for import namespaces, qualification, and record-field fallback.
module Tung.Name (
  checkImportAlias,
  constructorNamesMatch,
  hasPathNamespace,
  importAlias,
  importNamespace,
  isQualifiedName,
  lastQualifiedSegment,
  replaceNamespace,
  splitFieldAccessName,
)
where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

checkImportAlias :: String -> Either String ()
checkImportAlias alias
  | isQualifiedName alias || '/' `elem` alias = Left "bring alias must be slash-free and unqualified"
  | otherwise = pure ()

constructorNamesMatch :: String -> String -> Bool
constructorNamesMatch patternName constructorName
  | isQualifiedName patternName = patternName == constructorName || patternName == importedName constructorName
  | otherwise = patternName == lastQualifiedSegment constructorName

importedName :: String -> String
importedName name = takeWhile (/= '@') name ++ "@" ++ lastQualifiedSegment name

importNamespace :: String -> String
importNamespace = takeWhile (/= '.')

importAlias :: String -> Maybe String -> String
importAlias path = fromMaybe (reverse . takeWhile (/= '/') . reverse $ importNamespace path)

hasPathNamespace :: String -> Bool
hasPathNamespace name = isQualifiedName name && '/' `elem` takeWhile (/= '@') name

isQualifiedName :: String -> Bool
isQualifiedName = elem '@'

lastQualifiedSegment :: String -> String
lastQualifiedSegment = reverse . takeWhile (/= '@') . reverse

replaceNamespace :: String -> String -> String -> String
replaceNamespace old new name = maybe name (\rest -> new ++ "@" ++ rest) (stripPrefix (old ++ "@") name)

splitFieldAccessName :: String -> Maybe (String, String)
splitFieldAccessName name = case break (== '@') (reverse name) of
  (field, '@' : base) | not (null field) && not (null base) -> Just (reverse base, reverse field)
  _ -> Nothing
