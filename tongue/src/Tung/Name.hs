-- | pure helpers for import namespaces, qualification, and record-field fallback.
module Tung.Name (
  checkImportAlias,
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
import System.FilePath (stripExtension, takeFileName)

checkImportAlias :: String -> Either String ()
checkImportAlias alias
  | isQualifiedName alias || '/' `elem` alias = Left "use alias must be slash-free and unqualified"
  | otherwise = pure ()

importNamespace :: String -> String
importNamespace path = fromMaybe path (stripExtension ".tung" path)

importAlias :: String -> Maybe String -> String
importAlias path = fromMaybe (takeWhile (/= '.') (takeFileName path))

hasPathNamespace :: String -> Bool
hasPathNamespace name = isQualifiedName name && '/' `elem` name

isQualifiedName :: String -> Bool
isQualifiedName name = case break (== '.') name of
  (namespace, '.' : member) -> not (null namespace) && not (null member)
  _ -> False

lastQualifiedSegment :: String -> String
lastQualifiedSegment name = case break (== '@') (reverse name) of
  (member, '@' : _) | not (null member) -> reverse member
  _ -> case break (== '.') name of
    (namespace, '.' : member) | not (null namespace) && not (null member) -> member
    _ -> name

replaceNamespace :: String -> String -> String -> String
replaceNamespace old new name = maybe name (\rest -> new ++ "." ++ rest) (stripPrefix (old ++ ".") name)

splitFieldAccessName :: String -> Maybe (String, String)
splitFieldAccessName name = case break (== '@') (reverse name) of
  (field, '@' : base) | not (null field) && not (null base) -> Just (reverse base, reverse field)
  _ -> Nothing
