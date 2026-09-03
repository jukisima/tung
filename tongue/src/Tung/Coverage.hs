-- | constructor-matrix coverage for match expressions.
module Tung.Coverage (
  uncoveredPatterns,
  firstRedundantRow,
  showPatternList,
)
where

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Tung.Identity (SymbolId (symbolName))
import Tung.Name (lastQualifiedSegment)
import Tung.Syntax (Pattern (..))

-- the caller supplieþ constructors for each scrutinee type, keeping this
-- algorithm independent of inference state and type representation.
uncoveredPatterns :: (ty -> Maybe [(SymbolId, [ty])]) -> [ty] -> [[Pattern]] -> Maybe [Pattern]
uncoveredPatterns constructors = go
 where
  go [] rows = if any null rows then Nothing else Just []
  go (ty : restTys) rows = case constructors ty of
    Nothing -> (PVar "_" :) <$> go restTys (defaultRows rows)
    Just [] -> Nothing
    Just ctors ->
      case go restTys (defaultRows rows) of
        Nothing -> Nothing
        Just _ -> firstUncovered ctors rows restTys

  firstUncovered ctors rows restTys = foldr inspect Nothing ctors
   where
    inspect (target, fieldTys) fallback =
      case go (fieldTys ++ restTys) (specialiseRows target (length fieldTys) rows) of
        Nothing -> fallback
        Just witness ->
          let (args, rest) = splitAt (length fieldTys) witness
           in Just (PCon (lastQualifiedSegment (symbolName target)) args : rest)

-- a row is useful when it matcheþ at least one value not matched by an earlier
-- row. row numbers are one-based for source diagnostics.
firstRedundantRow :: (ty -> Maybe [(SymbolId, [ty])]) -> [ty] -> [[Pattern]] -> Maybe Int
firstRedundantRow constructors tys = go 1 []
 where
  go _ _ [] = Nothing
  go index previous (row : rows)
    | usefulPatterns constructors tys previous row = go (index + 1) (row : previous) rows
    | otherwise = Just index

usefulPatterns :: (ty -> Maybe [(SymbolId, [ty])]) -> [ty] -> [[Pattern]] -> [Pattern] -> Bool
usefulPatterns constructors = go
 where
  go [] rows [] = not (any null rows)
  go (ty : restTys) rows (pattern : restPatterns) = case pattern of
    PInteger value -> go restTys (specialiseIntegerRows value rows) restPatterns
    PText value -> go restTys (specialiseTextRows value rows) restPatterns
    PCon{} -> False
    PConstructor target arguments -> usefulResolvedConstructor ty target arguments restTys rows restPatterns
    PVar _ -> case constructors ty of
      Just ctors -> any (usefulWildcardConstructor rows restTys restPatterns) ctors
      Nothing -> go restTys (defaultRows rows) restPatterns
  go _ _ _ = False

  usefulResolvedConstructor ty target arguments restTys rows restPatterns = case constructors ty >>= lookup target of
    Just fieldTys ->
      go (fieldTys ++ restTys) (specialiseRows target (length fieldTys) rows) (arguments ++ restPatterns)
    Nothing -> False

  usefulWildcardConstructor rows restTys restPatterns (target, fieldTys) =
    go (fieldTys ++ restTys) (specialiseRows target (length fieldTys) rows) (replicate (length fieldTys) (PVar "_") ++ restPatterns)

specialiseIntegerRows :: Integer -> [[Pattern]] -> [[Pattern]]
specialiseIntegerRows value = mapMaybe $ \case
  PInteger other : rest | value == other -> Just rest
  PVar _ : rest -> Just rest
  _ -> Nothing

specialiseTextRows :: String -> [[Pattern]] -> [[Pattern]]
specialiseTextRows value = mapMaybe $ \case
  PText other : rest | value == other -> Just rest
  PVar _ : rest -> Just rest
  _ -> Nothing

specialiseRows :: SymbolId -> Int -> [[Pattern]] -> [[Pattern]]
specialiseRows constructor arity = mapMaybe (specialiseRow constructor arity)

specialiseRow :: SymbolId -> Int -> [Pattern] -> Maybe [Pattern]
specialiseRow _ _ [] = Nothing
specialiseRow _ arity (PVar _ : rest) = Just (replicate arity (PVar "_") ++ rest)
specialiseRow constructor _ (PConstructor target args : rest)
  | target == constructor = Just (args ++ rest)
specialiseRow _ _ _ = Nothing

defaultRows :: [[Pattern]] -> [[Pattern]]
defaultRows = mapMaybe $ \case
  PVar _ : rest -> Just rest
  _ -> Nothing

showPatternList :: [Pattern] -> String
showPatternList [] = "()"
showPatternList patterns = intercalate ", " (map showPattern patterns)

showPattern :: Pattern -> String
showPattern = \case
  PVar name -> name
  PInteger value -> show value
  PText value -> "'" ++ concatMap escapeTextCharacter value ++ "'"
  PCon name [] -> name
  PCon name arguments -> "$" ++ name ++ " " ++ showPatternList arguments
  PConstructor target [] -> lastQualifiedSegment (symbolName target)
  PConstructor target arguments -> "$" ++ lastQualifiedSegment (symbolName target) ++ " " ++ showPatternList arguments

escapeTextCharacter :: Char -> String
escapeTextCharacter = \case
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  '\'' -> "\\'"
  '\\' -> "\\\\"
  character -> [character]
