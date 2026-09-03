-- | pure helpers for import namespaces, qualification, and record-field fallback.
module Tung.Name (
  checkImportAlias,
  importAlias,
  importNamespace,
  isQualifiedName,
  lastQualifiedSegment,
  splitQualifiedName,
  replaceNamespace,
  splitFieldAccessName,
)
where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, isJust)
import System.FilePath (stripExtension, takeFileName)

checkImportAlias :: String -> Either String ()
checkImportAlias alias
  | isQualifiedName alias || '/' `elem` alias = Left "use alias must be slash-free and unqualified"
  | otherwise = pure ()

importNamespace :: String -> String
importNamespace path = fromMaybe path (stripExtension ".tung" path)

importAlias :: String -> Maybe String -> String
importAlias path = fromMaybe (takeWhile (/= '.') (takeFileName path))

isQualifiedName :: String -> Bool
isQualifiedName = isJust . splitQualifiedName

splitQualifiedName :: String -> Maybe (String, String)
splitQualifiedName name = case break (== '.') (reverse name) of
  (member, '.' : namespace)
    | not (null namespace) && not (null member) ->
        Just (reverse namespace, reverse member)
  _ -> Nothing

lastQualifiedSegment :: String -> String
lastQualifiedSegment name = case break (== '@') (reverse name) of
  (member, '@' : _) | not (null member) -> reverse member
  _ -> maybe name snd (splitQualifiedName name)

replaceNamespace :: String -> String -> String -> String
replaceNamespace old new name = maybe name (\rest -> new ++ "." ++ rest) (stripPrefix (old ++ ".") name)

splitFieldAccessName :: String -> Maybe (String, String)
splitFieldAccessName name = case break (== '@') (reverse name) of
  (field, '@' : base) | not (null field) && not (null base) -> Just (reverse base, reverse field)
  _ -> Nothing
