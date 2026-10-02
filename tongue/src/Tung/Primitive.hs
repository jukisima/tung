{-# LANGUAGE ViewPatterns #-}

-- | shared host ABI catalogues for the checker/runtime boundary.
module Tung.Primitive
  ( HostBinding (..),
    HostRole (..),
    HostSignature (..),
    HostType (..),
    PrimitiveInstanceSpec (..),
    hostBindings,
    hostArity,
    hostEffectNames,
    runnerEffectNames,
    primitiveTypeNames,
    effectId,
    effectOperationId,
    renderEffectId,
    primitiveClassId,
    canonicalHostTypeName,
    hostTypeId,
    dataDeclarationRef,
    primitiveInstanceSpecs,
    onlyConstructorId,
    yeaConstructorId,
    nayConstructorId,
    emptyConstructorId,
    consConstructorId,
    noneConstructorId,
    someConstructorId,
    productConstructorId,
    processResultConstructorId,
    requestConstructorId,
    responseConstructorId,
    tableConstructorId,
    findForeignBinding,
    baseNativeBindings,
    baseEffectBindings,
  )
where

import Data.Char (isAscii, isDigit, isLower)
import Data.List (find, nub, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Tung.Identity (ModuleId (..), SymbolId (..), TypeRef (..))
import Tung.Name (importNamespace)
import Tung.Syntax (ClassMember (..), Ctor (..), EffectOp (..), TypeAnn (..), TypeExpr (..), parameterNames, unknownParameters)

data HostType
  = HostVar String
  | HostCon String
  | HostApp String [HostType]
  deriving (Eq, Show)

data HostSignature = HostSignature
  { hostVariables :: [String],
    hostArguments :: [HostType],
    hostEffects :: [HostType],
    hostResult :: HostType
  }
  deriving (Eq, Show)

data HostRole
  = ForeignBinding
  | BaseNative
  | BaseEffect String
  | SourceEffect String
  deriving (Eq, Show)

data HostBinding = HostBinding
  { hostName :: String,
    hostRole :: HostRole,
    hostSignature :: HostSignature
  }
  deriving (Eq, Show)

data HostData = HostData
  { hostDataSource :: FilePath,
    hostDataName :: String,
    hostDataParameters :: [String],
    hostDataConstructors :: [Ctor]
  }

data HostEffect = HostEffect
  { hostEffectName :: String,
    hostEffectParameters :: [String],
    hostEffectOperations :: [EffectOp]
  }

data PrimitiveInstanceSpec = PrimitiveInstanceSpec
  { primitiveInstanceClass :: String,
    primitiveInstanceClassTarget :: SymbolId,
    primitiveInstanceType :: String,
    primitiveInstanceMember :: String
  }
  deriving (Eq, Show)

data PrimitiveClass = PrimitiveClass
  { primitiveClassSource :: FilePath,
    primitiveClassName :: String,
    primitiveClassParameters :: [String],
    primitiveClassRequired :: (String, TypeAnn)
  }

data SchemaOrdering = SourceOrdering | StructuralOrdering

oneData, twoData, listData, optionData, productData, tableData :: HostData
oneData = HostData "ilk/one.tung" "𝟙" [] [Ctor "only" [] [] Nothing]
twoData = HostData "ilk/two.tung" "𝟚" [] [Ctor "yea" [] [] Nothing, Ctor "nay" [] [] Nothing]
listData = HostData "ilk/list.tung" "list" ["a"] [Ctor "empty" [] [] Nothing, Ctor "_*" [] [TypeName "a", TypeApply "list" [TypeName "a"]] Nothing]
optionData = HostData "ilk/option.tung" "option" ["a"] [Ctor "none" [] [] Nothing, Ctor "some" [] [TypeName "a"] Nothing]
productData = HostData "ilk/product.tung" "∏" ["a", "b"] [Ctor "∏" [] [TypeName "a", TypeName "b"] Nothing]
tableData = HostData "ilk/table.tung" "table" ["k", "v"] [Ctor "from-list" [] [TypeApply "list" [TypeApply "∏" [TypeName "k", TypeName "v"]]] Nothing]

processResultData, requestData, responseData :: HostData
processResultData = HostData "process/result.tung" "process-result" [] [Ctor "process-result" [] [TypeName "ℤ", TypeName "text", TypeName "text"] Nothing]
requestData = HostData "web/request.tung" "request" [] [Ctor "request" [] [TypeName "text", TypeName "text", textTable, TypeName "text"] Nothing]
responseData = HostData "web/response.tung" "response" [] [Ctor "response" [] [TypeName "ℤ", textTable, TypeName "text"] Nothing]

textTable :: TypeExpr
textTable = TypeApply "table" [TypeName "text", TypeName "text"]

hostData :: [HostData]
hostData =
  [ oneData,
    twoData,
    listData,
    optionData,
    productData,
    tableData,
    processResultData,
    requestData,
    responseData
  ]

primitiveInstanceSpecs :: [PrimitiveInstanceSpec]
primitiveInstanceSpecs =
  [ instance_ typeName class_
  | (typeName, supported) <-
      [ ("ℤ", ["equal", "less-equal", "add", "zero", "subtract", "multiply", "one", "divide-remainder", "to-text"]),
        ("float", ["equal", "less-equal", "add", "zero", "subtract", "multiply", "one", "divide", "from-text", "to-text"])
      ],
    class_@PrimitiveClass {primitiveClassName} <- primitiveClasses,
    primitiveClassName `elem` supported
  ]
  where
    instance_ typeName class_@PrimitiveClass {primitiveClassName, primitiveClassRequired = (member, _)} =
      PrimitiveInstanceSpec primitiveClassName (primitiveClassTarget class_) typeName member

-- | assign þe reserved primitive identity only to a compatible class schema.
primitiveClassId :: Bool -> ModuleId -> [String] -> String -> [ClassMember] -> SymbolId
primitiveClassId schemaResolves owner parameters name members = case find compatible primitiveClasses of
  Just class_ | schemaResolves -> primitiveClassTarget class_
  _
    | any reservedOwner primitiveClasses -> SymbolId owner ("$nominal@" ++ name)
    | otherwise -> SymbolId owner name
  where
    compatible PrimitiveClass {..} =
      name == primitiveClassName
        && owner `elem` [RootModule, SourceModule primitiveClassSource]
        && length parameters == length primitiveClassParameters
        && normaliseClassSchema parameters members == normaliseExpectedClass primitiveClassParameters [primitiveClassRequired]
    reservedOwner PrimitiveClass {primitiveClassSource, primitiveClassName} = owner == SourceModule primitiveClassSource && name == primitiveClassName

primitiveClassTarget :: PrimitiveClass -> SymbolId
primitiveClassTarget PrimitiveClass {primitiveClassSource, primitiveClassName} = SymbolId (SourceModule primitiveClassSource) primitiveClassName

primitiveClasses :: [PrimitiveClass]
primitiveClasses =
  [ class_ "flock/equal.tung" "equal" "≡" (arrow [variable "a", variable "a"] [] two),
    class_ "flock/order/less-equal.tung" "less-equal" "≤" (arrow [variable "a", variable "a"] [] two),
    class_ "flock/algebra/arithmetic/add.tung" "add" "+" (arrow [variable "a", variable "a"] [] (variable "a")),
    class_ "flock/algebra/arithmetic/zero.tung" "zero" "zero" (variable "a"),
    class_ "flock/algebra/arithmetic/subtract.tung" "subtract" "-" (arrow [variable "a", variable "a"] [] (variable "a")),
    class_ "flock/algebra/arithmetic/multiply.tung" "multiply" "×" (arrow [variable "a", variable "a"] [] (variable "a")),
    class_ "flock/algebra/arithmetic/one.tung" "one" "one" (variable "a"),
    class_ "flock/algebra/arithmetic/divide-remainder.tung" "divide-remainder" "%" (arrow [variable "a", variable "a"] [failText] (TypeApply "∏" [variable "a", variable "a"])),
    class_ "flock/algebra/arithmetic/divide.tung" "divide" "÷" (arrow [variable "a", variable "a"] [] (variable "a")),
    class_ "flock/from-text.tung" "from-text" "from-text" (arrow [text] [failText] (variable "a")),
    class_ "flock/to-text.tung" "to-text" "to-text" (arrow [variable "a"] [] text)
  ]
  where
    class_ source name member signature = PrimitiveClass source name ["a"] (member, TypeAnn signature [])
    arrow (argument : arguments) effects result = TypeArrow (argument :| arguments) effects result
    arrow [] _ _ = error "internal primitive flock member without an argument"
    variable = TypeName
    text = TypeName "text"
    two = TypeName "𝟚"
    failText = TypeApply "fail" [text]

normaliseClassSchema :: [String] -> [ClassMember] -> [(String, TypeAnn)]
normaliseClassSchema parameters = sortOn fst . map normaliseMember . foldr required []
  where
    required (ClassSignature member annotation) rest = (member, annotation) : rest
    required _ rest = rest
    normaliseMember (member, TypeAnn signature constraints) = (member, TypeAnn (normaliseClassType parameters signature) constraints)

normaliseExpectedClass :: [String] -> [(String, TypeAnn)] -> [(String, TypeAnn)]
normaliseExpectedClass parameters = sortOn fst . map (fmap normaliseAnnotation)
  where
    normaliseAnnotation (TypeAnn signature constraints) = TypeAnn (normaliseClassType parameters signature) constraints

normaliseClassType :: [String] -> TypeExpr -> TypeExpr
normaliseClassType parameters ty = normaliseSchemaType StructuralOrdering (numberedVariables "$" parameters) (unquantified ty)
  where
    unquantified (TypeForall _ body) = body
    unquantified body = body

normaliseSchemaType :: SchemaOrdering -> [(String, String)] -> TypeExpr -> TypeExpr
normaliseSchemaType ordering variables = go
  where
    normaliseName name = fromMaybe name (lookup name variables)
    go = \case
      TypeHole ident -> TypeHole ident
      TypeName name -> TypeName (normaliseName name)
      TypeApply name arguments -> TypeApply (normaliseName name) (map go arguments)
      TypeRecord fields -> TypeRecord (orderFields (map (fmap go) fields))
      TypeArrow arguments effects result -> TypeArrow (fmap go arguments) (orderEffects (map go effects)) (go result)
      TypeForall (parameterNames -> names) body ->
        let local = numberedVariables ("$bound" ++ show (length variables) ++ "_") names
         in TypeForall (unknownParameters (map snd local)) (normaliseSchemaType ordering (local ++ filter ((`notElem` names) . fst) variables) body)
    orderFields = case ordering of
      SourceOrdering -> id
      StructuralOrdering -> sortOn fst
    orderEffects = case ordering of
      SourceOrdering -> id
      StructuralOrdering -> sortOn show

numberedVariables :: String -> [String] -> [(String, String)]
numberedVariables prefix variables = zip variables [prefix ++ show index | index <- [(0 :: Int) ..]]

hostEffectNames :: [String]
hostEffectNames = map hostEffectName hostEffectSchemas

-- only effects whose operations enter the runner's host dispatch appear here.
runnerEffectNames :: [String]
runnerEffectNames = nub (concatMap executedEffect hostBindings)
  where
    executedEffect HostBinding {hostRole = BaseEffect owner} = [owner]
    executedEffect HostBinding {hostRole = SourceEffect owner} = [owner]
    executedEffect _ = []

effectId :: (String -> Bool) -> ModuleId -> [String] -> String -> [EffectOp] -> SymbolId
effectId isConcrete owner parameters name operations
  | isHostEffectDeclaration isConcrete parameters name operations = SymbolId RuntimeModule name
  | otherwise = SymbolId owner name

effectOperationId :: SymbolId -> String -> SymbolId
effectOperationId (SymbolId RuntimeModule _) operationName = SymbolId RuntimeModule operationName
effectOperationId (SymbolId owner effectName) operationName = SymbolId owner (effectName ++ "@" ++ operationName)

renderEffectId :: SymbolId -> String
renderEffectId SymbolId {symbolModule = RuntimeModule, symbolName} = symbolName
renderEffectId SymbolId {symbolModule = RootModule, symbolName}
  | symbolName `elem` hostEffectNames = "$root@" ++ symbolName
  | otherwise = symbolName
renderEffectId SymbolId {symbolModule = SourceModule path, symbolName} = importNamespace path ++ "~" ++ symbolName

findHostEffect :: String -> Maybe HostEffect
findHostEffect name = find ((== name) . hostEffectName) hostEffectSchemas

findHostEffectOperation :: String -> String -> Maybe (HostEffect, EffectOp)
findHostEffectOperation effectName operationName = do
  effect@HostEffect {hostEffectOperations} <- findHostEffect effectName
  operation <- find ((== operationName) . effectOperationName) hostEffectOperations
  pure (effect, operation)

effectOperationName :: EffectOp -> String
effectOperationName (EffectOp name _) = name

isHostEffectDeclaration :: (String -> Bool) -> [String] -> String -> [EffectOp] -> Bool
isHostEffectDeclaration isConcrete parameters name operations = maybe False matches (findHostEffect name)
  where
    matches HostEffect {hostEffectName, hostEffectParameters, hostEffectOperations} =
      name == hostEffectName
        && length parameters == length hostEffectParameters
        && normaliseEffectSchema isConcrete parameters operations == normaliseEffectSchema (`elem` hostConcreteTypeNames) hostEffectParameters hostEffectOperations

normaliseEffectSchema :: (String -> Bool) -> [String] -> [EffectOp] -> [EffectOp]
normaliseEffectSchema isConcrete parameters operations = sortOn effectOperationName (map normaliseOperation operations)
  where
    shared = numberedVariables "$parameter" parameters
    normaliseOperation operation@(EffectOp name signature) =
      let (names, body) = case signature of TypeForall (parameterNames -> declared) inner -> (declared, inner); _ -> (operationVariables isConcrete operation, signature)
          local = filter (`notElem` parameters) names
          variables = shared ++ numberedVariables "$local" local
       in EffectOp name (normaliseSchemaType StructuralOrdering variables body)

operationVariables :: (String -> Bool) -> EffectOp -> [String]
operationVariables isConcrete (EffectOp _ signature) = nub (typeVariables signature)
  where
    typeVariables = \case
      TypeHole _ -> []
      TypeName name -> [name | isHostTypeVariable name]
      TypeApply name arguments -> [name | isHostTypeVariable name] ++ concatMap typeVariables arguments
      TypeRecord fields -> concatMap (typeVariables . snd) fields
      TypeArrow arguments effects result -> concatMap typeVariables (foldr (:) [result] arguments) ++ concatMap effectVariables effects
      TypeForall (parameterNames -> names) body -> filter (`notElem` names) (typeVariables body)
    effectVariables = \case
      TypeName name -> [name | isEffectVariable name]
      TypeApply _ arguments -> concatMap typeVariables arguments
      other -> typeVariables other
    isHostTypeVariable name =
      not (isConcrete name) && case name of
        first : rest -> isAscii first && isLower first && all (\character -> isAscii character && (isLower character || isDigit character)) rest
        [] -> False
    isEffectVariable ('e' : rest) = all (\character -> isDigit character || character `elem` ("₀₁₂₃₄₅₆₇₈₉" :: String)) rest
    isEffectVariable _ = False

hostConcreteTypeNames :: [String]
hostConcreteTypeNames = ["ℤ", "float", "unicode", "text", "𝟙", "list", "option", "task", "process-result", "request", "response", "fail"]

-- literal-backed types are available before library loading.
primitiveTypeNames :: [String]
primitiveTypeNames = ["ℤ", "float", "unicode", "text", "→"]

hostEffectSchemas :: [HostEffect]
hostEffectSchemas =
  [ HostEffect "fail" ["e"] [operation "fail" [variable "e"] [] (variable "a")],
    HostEffect "console" [] [operation "write" [text] [] one, operation "read" [one] [] text],
    HostEffect "random" [] [operation "random" [one] [] float],
    HostEffect "state" ["a"] [operation "get" [one] [] (variable "a"), operation "set" [variable "a"] [] one],
    HostEffect
      "async"
      []
      [ operation "fork" [arrow [one] [variable "e"] (variable "a")] [variable "e"] (task (variable "a")),
        operation "wait" [task (variable "a")] [failText] (variable "a"),
        operation "fordo" [task (variable "a")] [] one,
        operation "wait-for" [task (variable "a"), integer] [failText] (option (variable "a")),
        operation "sleep" [integer] [] one
      ],
    HostEffect
      "file"
      []
      [ operation "read-file" [text] [failText] text,
        operation "write-file" [text, text] [failText] one,
        operation "append-file" [text, text] [failText] one
      ],
    HostEffect
      "system"
      []
      [ operation "arguments" [one] [] (list text),
        operation "environment" [text] [] (option text),
        operation "exit" [integer] [] (variable "a")
      ],
    HostEffect "clock" [] [operation "unix-time" [one] [] integer],
    HostEffect "process" [] [operation "run-process" [text, list text] [failText] (TypeName "process-result")],
    HostEffect "web" [] [operation "serve" [integer, arrow [TypeName "request"] [variable "e"] (TypeName "response")] [variable "e", failText] one]
  ]
  where
    operation name arguments effects result = EffectOp name (arrow arguments effects result)
    arrow (argument : arguments) effects result = TypeArrow (argument :| arguments) effects result
    arrow [] _ _ = error "internal host effect operation without an argument"
    variable = TypeName
    integer = TypeName "ℤ"
    float = TypeName "float"
    text = TypeName "text"
    one = TypeName "𝟙"
    list value = TypeApply "list" [value]
    option value = TypeApply "option" [value]
    task value = TypeApply "task" [value]
    failText = TypeApply "fail" [text]

-- | find the canonical imported name of a host-provided data type.
canonicalHostTypeName :: String -> Maybe String
canonicalHostTypeName name = canonicalDataTypeName <$> find ((== name) . hostDataName) hostData

hostTypeId :: String -> Maybe SymbolId
hostTypeId name = dataTypeId <$> find matches hostData
  where
    matches dataType = name == hostDataName dataType || name == canonicalDataTypeName dataType

-- | assign þe host identity only to a complete ABI declaration. a rejected
-- declaration at its reserved source retaineþ an explicit nominal identity.
dataDeclarationRef :: Bool -> ModuleId -> [String] -> String -> [Ctor] -> TypeRef
dataDeclarationRef schemaResolves owner parameters name constructors = case find ((== name) . hostDataName) hostData of
  Just dataType@HostData {hostDataSource}
    | schemaResolves && sameHostDataSchema parameters constructors dataType ->
        TypeRef (dataTypeId dataType) (canonicalDataTypeName dataType)
    | owner == SourceModule hostDataSource -> nominal ("$nominal@" ++ name)
  _ -> nominal name
  where
    nominal displayName = TypeRef (SymbolId owner displayName) displayName

sameHostDataSchema :: [String] -> [Ctor] -> HostData -> Bool
sameHostDataSchema parameters constructors HostData {hostDataName, hostDataParameters, hostDataConstructors} =
  length parameters == length hostDataParameters
    && normalise parameters constructors == normalise hostDataParameters hostDataConstructors
  where
    normalise vars = sortOn constructorName . map (normaliseConstructor hostDataName vars)
    constructorName (Ctor name _ _ _) = name

normaliseConstructor :: String -> [String] -> Ctor -> Ctor
normaliseConstructor dataName parameters (Ctor name names fields result) =
  Ctor name names (map normaliseType fields) normalizedResult
  where
    normaliseType = normaliseSchemaType SourceOrdering (numberedVariables "$" parameters)
    resultType = case parameters of
      [] -> TypeName dataName
      _ -> TypeApply dataName (map TypeName parameters)
    normalizedResult = case normaliseType <$> result of
      Just ty | ty == normaliseType resultType -> Nothing
      other -> other

onlyConstructorId, yeaConstructorId, nayConstructorId :: SymbolId
onlyConstructorId = constructorId oneData "only"
yeaConstructorId = constructorId twoData "yea"
nayConstructorId = constructorId twoData "nay"

emptyConstructorId, consConstructorId :: SymbolId
emptyConstructorId = constructorId listData "empty"
consConstructorId = constructorId listData "_*"

noneConstructorId, someConstructorId :: SymbolId
noneConstructorId = constructorId optionData "none"
someConstructorId = constructorId optionData "some"

productConstructorId :: SymbolId
productConstructorId = constructorId productData "∏"

processResultConstructorId, requestConstructorId, responseConstructorId, tableConstructorId :: SymbolId
processResultConstructorId = constructorId processResultData "process-result"
requestConstructorId = constructorId requestData "request"
responseConstructorId = constructorId responseData "response"
tableConstructorId = constructorId tableData "from-list"

canonicalDataTypeName :: HostData -> String
canonicalDataTypeName HostData {hostDataSource, hostDataName} = importNamespace hostDataSource ++ "~" ++ hostDataName

constructorId :: HostData -> String -> SymbolId
constructorId HostData {hostDataSource, hostDataName} name = SymbolId (SourceModule hostDataSource) (hostDataName ++ "@" ++ name)

dataTypeId :: HostData -> SymbolId
dataTypeId HostData {hostDataSource, hostDataName} = SymbolId (SourceModule hostDataSource) hostDataName

hostArity :: HostBinding -> Int
hostArity = length . hostArguments . hostSignature

baseNativeBindings, baseEffectBindings :: [HostBinding]
baseNativeBindings = filter ((== BaseNative) . hostRole) hostBindings
baseEffectBindings = filter isBaseEffect hostBindings

findForeignBinding :: String -> Maybe HostBinding
findForeignBinding key = find (\binding -> hostRole binding == ForeignBinding && hostName binding == key) hostBindings

isBaseEffect :: HostBinding -> Bool
isBaseEffect HostBinding {hostRole = BaseEffect _} = True
isBaseEffect _ = False

hostBindings :: [HostBinding]
hostBindings =
  [ foreignBinding "add-integer" (binary integer integer [] integer),
    foreignBinding "subtract-integer" (binary integer integer [] integer),
    foreignBinding "multiply-integer" (binary integer integer [] integer),
    foreignBinding "divide-remainder-integer" (binary integer integer [failText] (product integer integer)),
    foreignBinding "add-float" (binary float float [] float),
    foreignBinding "subtract-float" (binary float float [] float),
    foreignBinding "multiply-float" (binary float float [] float),
    foreignBinding "divide-float" (binary float float [] float),
    foreignBinding "sine-float" (unary float [] float),
    foreignBinding "arctan-float" (binary float float [] float),
    foreignBinding "exponent-float" (unary float [] float),
    foreignBinding "logariþm-float" (unary float [] float),
    foreignBinding "⌊" (unary float [failText] integer),
    foreignBinding "equal-integer" (binary integer integer [] two),
    foreignBinding "integer-to-text" (unary integer [] text),
    foreignBinding "float-to-text" (unary float [] text),
    foreignBinding "unicode-to-integer" (unary unicode [] integer),
    foreignBinding "integer-to-unicode" (unary integer [failText] unicode),
    foreignBinding "behead-text" (unary text [] (option (product unicode text))),
    foreignBinding "text-to-list" (unary text [] (list unicode)),
    foreignBinding "list-to-text" (unary (list unicode) [] text),
    foreignBinding "join-text" (binary text text [] text),
    foreignBinding "equal-text" (binary text text [] two),
    foreignBinding "equal-unicode" (binary unicode unicode [] two),
    foreignBinding "less-equal-text" (binary text text [] two),
    foreignBinding "less-equal-unicode" (binary unicode unicode [] two),
    native "to-text" (unary integer [] text),
    native "to-text" (unary float [] text),
    native "from-text" (unary text [failText] float),
    native "⌊" (unary float [failText] integer),
    native "≡" (binary integer integer [] two),
    native "≡" (binary float float [] two),
    native "≤" (binary integer integer [] two),
    native "≤" (binary float float [] two),
    native "+" (binary integer integer [] integer),
    native "+" (binary float float [] float),
    native "×" (binary integer integer [] integer),
    native "×" (binary float float [] float),
    native "-" (binary integer integer [] integer),
    native "-" (binary float float [] float),
    native "÷" (binary float float [] float),
    native "%" (binary integer integer [failText] (product integer integer)),
    native "exponent" (unary float [] float),
    native "logariþm" (unary float [] float),
    native "sine" (unary float [] float),
    baseEffect "console" "write",
    baseEffect "console" "read",
    baseEffect "async" "sleep",
    baseEffect "random" "random",
    baseEffect "file" "read-file",
    baseEffect "file" "write-file",
    baseEffect "file" "append-file",
    baseEffect "system" "arguments",
    baseEffect "system" "environment",
    baseEffect "system" "exit",
    baseEffect "clock" "unix-time",
    sourceEffect "async" "fork",
    sourceEffect "async" "wait",
    sourceEffect "async" "fordo",
    sourceEffect "async" "wait-for",
    sourceEffect "process" "run-process",
    sourceEffect "web" "serve"
  ]
  where
    integer = HostCon "ℤ"
    float = HostCon "float"
    unicode = HostCon "unicode"
    text = HostCon "text"
    two = HostCon (canonicalDataTypeName twoData)
    failText = HostApp "fail" [text]
    list value = HostApp (canonicalDataTypeName listData) [value]
    option value = HostApp (canonicalDataTypeName optionData) [value]
    product left right = HostApp (canonicalDataTypeName productData) [left, right]

foreignBinding, native :: String -> HostSignature -> HostBinding
foreignBinding name = HostBinding name ForeignBinding
native name = HostBinding name BaseNative

baseEffect :: String -> String -> HostBinding
baseEffect owner name = HostBinding name (BaseEffect owner) (baseEffectSignature owner name)

-- base operations enter þe static environment and therefore retain þeir full
-- first-order schema. source-dispatched operations constraint only runtime arity.
baseEffectSignature :: String -> String -> HostSignature
baseEffectSignature owner name = case findHostEffectOperation owner name of
  Just (HostEffect {hostEffectName, hostEffectParameters}, operation@(EffectOp _ (TypeArrow (argument :| arguments) effects result))) ->
    let variables = nub (hostEffectParameters ++ operationVariables (`elem` hostConcreteTypeNames) operation)
        convert = firstOrderHostType variables
     in HostSignature
          variables
          (map convert (argument : arguments))
          (HostApp hostEffectName (map HostVar hostEffectParameters) : map convert effects)
          (convert result)
  _ -> error ("internal unknown base effect operation '" ++ owner ++ "@" ++ name ++ "'")

firstOrderHostType :: [String] -> TypeExpr -> HostType
firstOrderHostType variables = \case
  TypeName name
    | name `elem` variables -> HostVar name
    | otherwise -> HostCon (canonical name)
  TypeApply name arguments -> HostApp (canonical name) (map (firstOrderHostType variables) arguments)
  unsupported -> error ("internal non-first-order base effect type " ++ show unsupported)
  where
    canonical name = fromMaybe name (canonicalHostTypeName name)

sourceEffect :: String -> String -> HostBinding
sourceEffect owner name =
  HostBinding name (SourceEffect owner) (HostSignature [] (replicate (hostEffectArity owner name) (HostVar "_")) [] (HostVar "_"))

hostEffectArity :: String -> String -> Int
hostEffectArity owner name = case findHostEffectOperation owner name of
  Just (_, EffectOp _ (TypeArrow arguments _ _)) -> length arguments
  _ -> error ("internal unknown source effect operation '" ++ owner ++ "@" ++ name ++ "'")

unary :: HostType -> [HostType] -> HostType -> HostSignature
unary argument = HostSignature [] [argument]

binary :: HostType -> HostType -> [HostType] -> HostType -> HostSignature
binary left right = HostSignature [] [left, right]
