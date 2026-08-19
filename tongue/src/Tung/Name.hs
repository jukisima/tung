-- | pure helpers for import namespaces, qualification, and record-field fallback.
module Tung.Name (
  constructorNamesMatch,
  importNamespace,
  isQualifiedName,
  lastQualifiedSegment,
  splitFieldAccessName,
)
where

constructorNamesMatch :: String -> String -> Bool
constructorNamesMatch patternName constructorName
  | isQualifiedName patternName = patternName == constructorName || patternName == importedName constructorName
  | otherwise = patternName == lastQualifiedSegment constructorName

importedName :: String -> String
importedName name = takeWhile (/= '@') name ++ "@" ++ lastQualifiedSegment name

importNamespace :: String -> String
importNamespace = takeWhile (/= '.')

isQualifiedName :: String -> Bool
isQualifiedName = elem '@'

lastQualifiedSegment :: String -> String
lastQualifiedSegment = reverse . takeWhile (/= '@') . reverse

splitFieldAccessName :: String -> Maybe (String, String)
splitFieldAccessName name = case break (== '@') (reverse name) of
  (field, '@' : base) | not (null field) && not (null base) -> Just (reverse base, reverse field)
  _ -> Nothing
