-- | one data-only catalogue for the checker/runtime host boundary.
module Tung.Primitive (
  HostBinding (..),
  HostRole (..),
  HostSignature (..),
  HostType (..),
  hostBindings,
  hostArity,
  foreignBindings,
  baseNativeBindings,
  baseEffectBindings,
  sourceEffectBindings,
) where

data HostType
  = HostVar String
  | HostCon String
  | HostApp String [HostType]
  deriving (Eq, Show)

data HostSignature = HostSignature
  { hostVariables :: [String]
  , hostArguments :: [HostType]
  , hostEffects :: [HostType]
  , hostResult :: HostType
  }
  deriving (Eq, Show)

data HostRole
  = ForeignBinding
  | BaseNative
  | BaseEffect String
  | SourceEffect String
  deriving (Eq, Show)

data HostBinding = HostBinding
  { hostName :: String
  , hostRole :: HostRole
  , hostSignature :: HostSignature
  }
  deriving (Eq, Show)

hostArity :: HostBinding -> Int
hostArity = length . hostArguments . hostSignature

foreignBindings, baseNativeBindings, baseEffectBindings, sourceEffectBindings :: [HostBinding]
foreignBindings = filter ((== ForeignBinding) . hostRole) hostBindings
baseNativeBindings = filter ((== BaseNative) . hostRole) hostBindings
baseEffectBindings = filter isBaseEffect hostBindings
sourceEffectBindings = filter isSourceEffect hostBindings

isBaseEffect, isSourceEffect :: HostBinding -> Bool
isBaseEffect HostBinding{hostRole = BaseEffect _} = True
isBaseEffect _ = False
isSourceEffect HostBinding{hostRole = SourceEffect _} = True
isSourceEffect _ = False

hostBindings :: [HostBinding]
hostBindings =
  [ foreignBinding "add-integer" (binary integer integer [] integer)
  , foreignBinding "subtract-integer" (binary integer integer [] integer)
  , foreignBinding "multiply-integer" (binary integer integer [] integer)
  , foreignBinding "divide-remainder-integer" (binary integer integer [failText] (product integer integer))
  , foreignBinding "add-float" (binary float float [] float)
  , foreignBinding "subtract-float" (binary float float [] float)
  , foreignBinding "multiply-float" (binary float float [] float)
  , foreignBinding "divide-float" (binary float float [] float)
  , foreignBinding "sine-float" (unary float [] float)
  , foreignBinding "arctan-float" (binary float float [] float)
  , foreignBinding "exponent-float" (unary float [] float)
  , foreignBinding "logarithm-float" (unary float [] float)
  , foreignBinding "⌊" (unary float [failText] integer)
  , foreignBinding "equal-integer" (binary integer integer [] two)
  , foreignBinding "integer-to-text" (unary integer [] text)
  , foreignBinding "float-to-text" (unary float [] text)
  , foreignBinding "unicode-to-integer" (unary unicode [] integer)
  , foreignBinding "integer-to-unicode" (unary integer [failText] unicode)
  , foreignBinding "behead-text" (unary text [] (option (product unicode text)))
  , foreignBinding "text-to-list" (unary text [] (list unicode))
  , foreignBinding "list-to-text" (unary (list unicode) [] text)
  , foreignBinding "fold-join-text" (unary (list text) [] text)
  , foreignBinding "join-text" (binary text text [] text)
  , foreignBinding "equal-text" (binary text text [] two)
  , foreignBinding "equal-unicode" (binary unicode unicode [] two)
  , foreignBinding "less-equal-text" (binary text text [] two)
  , foreignBinding "less-equal-unicode" (binary unicode unicode [] two)
  , native "to-text" (unary integer [] text)
  , native "to-text" (unary float [] text)
  , native "from-text" (unary text [failText] float)
  , native "⌊" (unary float [failText] integer)
  , native "≡" (binary integer integer [] twoBare)
  , native "≡" (binary float float [] twoBare)
  , native "≤" (binary integer integer [] twoBare)
  , native "≤" (binary float float [] twoBare)
  , native "+" (binary integer integer [] integer)
  , native "+" (binary float float [] float)
  , native "×" (binary integer integer [] integer)
  , native "×" (binary float float [] float)
  , native "-" (binary integer integer [] integer)
  , native "-" (binary float float [] float)
  , native "∕" (binary float float [] float)
  , native "÷" (binary integer integer [failText] (product integer integer))
  , native "exponent" (unary float [] float)
  , native "logarithm" (unary float [] float)
  , native "sine" (unary float [] float)
  , baseEffect "console" "write" (unary text [effect "console"] oneBare)
  , baseEffect "console" "read" (unary oneBare [effect "console"] text)
  , baseEffect "async" "sleep" (unary integer [effect "async"] oneBare)
  , baseEffect "random" "random" (unary oneBare [effect "random"] float)
  , baseEffect "file" "read-file" (unary text [effect "file", failText] text)
  , baseEffect "file" "write-file" (binary text text [effect "file", failText] oneBare)
  , baseEffect "file" "append-file" (binary text text [effect "file", failText] oneBare)
  , baseEffect "system" "arguments" (unary oneBare [effect "system"] (HostApp "list" [text]))
  , baseEffect "system" "environment" (unary text [effect "system"] (HostApp "option" [text]))
  , HostBinding "exit" (BaseEffect "system") (HostSignature ["a"] [integer] [effect "system"] (HostVar "a"))
  , baseEffect "clock" "unix-time" (unary oneBare [effect "clock"] integer)
  , sourceEffect "async" "fork" 1
  , sourceEffect "async" "wait" 1
  , sourceEffect "async" "fordo" 1
  , sourceEffect "async" "wait-for" 2
  , sourceEffect "process" "run-process" 2
  ]
 where
  integer = HostCon "integer"
  float = HostCon "float"
  unicode = HostCon "unicode"
  text = HostCon "text"
  oneBare = HostCon "𝟙"
  twoBare = HostCon "𝟚"
  two = HostCon "data/two@𝟚"
  failText = HostApp "fail" [text]
  list value = HostApp "data/list@list" [value]
  option value = HostApp "data/option@option" [value]
  product left right = HostApp "data/product@∏" [left, right]
  effect name = HostApp name []

foreignBinding, native :: String -> HostSignature -> HostBinding
foreignBinding name = HostBinding name ForeignBinding
native name = HostBinding name BaseNative

baseEffect :: String -> String -> HostSignature -> HostBinding
baseEffect owner name = HostBinding name (BaseEffect owner)

sourceEffect :: String -> String -> Int -> HostBinding
sourceEffect owner name arity = HostBinding name (SourceEffect owner) (HostSignature [] (replicate arity (HostVar "_")) [] (HostVar "_"))

unary :: HostType -> [HostType] -> HostType -> HostSignature
unary argument = HostSignature [] [argument]

binary :: HostType -> HostType -> [HostType] -> HostType -> HostSignature
binary left right = HostSignature [] [left, right]
