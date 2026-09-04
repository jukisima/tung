-- | pure helpers for import namespaces and qualification.
module Tung.Name (
  checkImportAlias,
  importAlias,
  importNamespace,
  isQualifiedName,
  lastQualifiedSegment,
  splitQualifiedName,
  replaceNamespace,
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
splitQualifiedName name = case break (== '~') (reverse name) of
  (member, '~' : namespace)
    | not (null namespace) && not (null member) ->
        Just (reverse namespace, reverse member)
  _ -> Nothing

lastQualifiedSegment :: String -> String
lastQualifiedSegment name = case break (== '@') (reverse name) of
  (member, '@' : _) | not (null member) -> reverse member
  _ -> maybe name snd (splitQualifiedName name)

replaceNamespace :: String -> String -> String -> String
replaceNamespace old new name = maybe name (\rest -> new ++ "~" ++ rest) (stripPrefix (old ++ "~") name)
