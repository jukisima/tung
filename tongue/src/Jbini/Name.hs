module Jbini.Name (
  importNamespace,
  isQualifiedName,
  lastQualifiedSegment,
  splitFieldAccessName,
)
where

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
