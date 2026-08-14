module Jbini.Import (
  ImportStack,
  enterImport,
)
where

import Data.List (intercalate)

type ImportStack = [String]

enterImport :: ImportStack -> String -> Either String ImportStack
enterImport stack path
  | path `elem` stack = Left ("bring cycle: " ++ intercalate " -> " (reverse stack ++ [path]))
  | otherwise = Right (path : stack)
