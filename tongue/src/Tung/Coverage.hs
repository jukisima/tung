-- | constructor-matrix coverage for match expressions.
module Tung.Coverage (
  uncoveredPatterns,
  firstRedundantRow,
  showPatternList,
)
where

import Data.List (find, intercalate)
import Data.Maybe (mapMaybe)
import Tung.Name (constructorNamesMatch, lastQualifiedSegment)
import Tung.Syntax (Pattern (..))

-- the caller supplieþ constructors for each scrutinee type, keeping this
-- algorithm independent of inference state and type representation.
uncoveredPatterns :: (ty -> Maybe [(String, [ty])]) -> [ty] -> [[Pattern]] -> Maybe [Pattern]
uncoveredPatterns constructors = go
 where
  go [] rows = if any null rows then Nothing else Just []
  go (ty : restTys) rows = case constructors ty of
    Nothing -> (PVar "_" :) <$> go restTys (defaultRows rows)
    Just [] -> Nothing
    Just ctors ->
      case go restTys (defaultRowsForConstructors (map fst ctors) rows) of
        Nothing -> Nothing
        Just _ -> firstUncovered ctors rows restTys

  firstUncovered ctors rows restTys = foldr inspect Nothing ctors
   where
    names = map fst ctors
    inspect (name, fieldTys) fallback =
      case go (fieldTys ++ restTys) (specialiseRows name (length fieldTys) names rows) of
        Nothing -> fallback
        Just witness ->
          let (args, rest) = splitAt (length fieldTys) witness
           in Just (PCon (lastQualifiedSegment name) args : rest)

-- a row is useful when it matcheþ at least one value not matched by an earlier
-- row. row numbers are one-based for source diagnostics.
firstRedundantRow :: (ty -> Maybe [(String, [ty])]) -> [ty] -> [[Pattern]] -> Maybe Int
firstRedundantRow constructors tys = go 1 []
 where
  go _ _ [] = Nothing
  go index previous (row : rows)
    | usefulPatterns constructors tys previous row = go (index + 1) (row : previous) rows
    | otherwise = Just index

usefulPatterns :: (ty -> Maybe [(String, [ty])]) -> [ty] -> [[Pattern]] -> [Pattern] -> Bool
usefulPatterns constructors = go
 where
  go [] rows [] = not (any null rows)
  go (ty : restTys) rows (pattern : restPatterns) = case pattern of
    PInteger value -> go restTys (specialiseIntegerRows value rows) restPatterns
    PCon name arguments -> usefulConstructor ty name arguments restTys rows restPatterns
    PVar name -> case constructors ty of
      Just ctors
        | Just (_, fieldTys) <- findConstructor name ctors ->
            go (fieldTys ++ restTys) (specialiseRows name (length fieldTys) (map fst ctors) rows) (replicate (length fieldTys) (PVar "_") ++ restPatterns)
        | otherwise -> any (usefulWildcardConstructor rows restTys restPatterns (map fst ctors)) ctors
      Nothing -> go restTys (defaultRows rows) restPatterns
  go _ _ _ = False

  usefulConstructor ty name arguments restTys rows restPatterns = case constructors ty >>= findConstructor name of
    Just (_, fieldTys) ->
      go (fieldTys ++ restTys) (specialiseRows name (length fieldTys) (constructorNames ty) rows) (arguments ++ restPatterns)
    Nothing -> False

  usefulWildcardConstructor rows restTys restPatterns names (name, fieldTys) =
    go (fieldTys ++ restTys) (specialiseRows name (length fieldTys) names rows) (replicate (length fieldTys) (PVar "_") ++ restPatterns)

  constructorNames ty = maybe [] (map fst) (constructors ty)

findConstructor :: String -> [(String, fields)] -> Maybe (String, fields)
findConstructor name = find (constructorNamesMatch name . fst)

specialiseIntegerRows :: Integer -> [[Pattern]] -> [[Pattern]]
specialiseIntegerRows value = mapMaybe $ \case
  PInteger other : rest | value == other -> Just rest
  PVar _ : rest -> Just rest
  _ -> Nothing

specialiseRows :: String -> Int -> [String] -> [[Pattern]] -> [[Pattern]]
specialiseRows constructor arity constructors = mapMaybe (specialiseRow constructor arity constructors)

specialiseRow :: String -> Int -> [String] -> [Pattern] -> Maybe [Pattern]
specialiseRow _ _ _ [] = Nothing
specialiseRow constructor arity constructors (PVar name : rest)
  | arity == 0 && isConstructorName name [constructor] = Just rest
  | isConstructorName name constructors = Nothing
  | otherwise = Just (replicate arity (PVar "_") ++ rest)
specialiseRow constructor _ _ (PCon name args : rest)
  | constructorNamesMatch name constructor = Just (args ++ rest)
specialiseRow _ _ _ _ = Nothing

defaultRows :: [[Pattern]] -> [[Pattern]]
defaultRows = mapMaybe $ \case
  PVar _ : rest -> Just rest
  _ -> Nothing

defaultRowsForConstructors :: [String] -> [[Pattern]] -> [[Pattern]]
defaultRowsForConstructors constructors = mapMaybe $ \case
  PVar name : _ | isConstructorName name constructors -> Nothing
  PVar _ : rest -> Just rest
  _ -> Nothing

isConstructorName :: String -> [String] -> Bool
isConstructorName name = any (constructorNamesMatch name)

showPatternList :: [Pattern] -> String
showPatternList [] = "()"
showPatternList patterns = intercalate ", " (map showPattern patterns)

showPattern :: Pattern -> String
showPattern = \case
  PVar name -> name
  PInteger value -> show value
  PCon name [] -> name
  PCon name arguments -> "$" ++ name ++ " " ++ showPatternList arguments
