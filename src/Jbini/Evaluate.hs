{-# LANGUAGE LambdaCase #-}

module Jbini.Evaluate
  ( evaluate, evaluateWithImports
  ) where

import Control.Concurrent (threadDelay)
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Jbini.Parse (parse)
import Jbini.Syntax
import Jbini.TypeCheck

data RuntimeValue
  = VUnit
  | VInteger Int
  | VFloat String
  | VCharacter String
  | VText String
  | VRecord (Map.Map String RuntimeValue)
  | VMatcher (NonEmpty MatchCase) RuntimeEnv [RuntimeValue]
  | VRecursiveMatcher String (NonEmpty MatchCase) RuntimeEnv [RuntimeValue]
  | VConstructor String String Int [RuntimeValue]
  | VData String String [RuntimeValue]
  | VNative String Int [RuntimeValue]
  | VEffectOp String String Int [RuntimeValue]
  | VContinuation (RuntimeValue -> IO (RuntimeResult RuntimeValue))

instance Eq RuntimeValue where
  VUnit == VUnit = True
  VInteger a == VInteger b = a == b
  VFloat a == VFloat b = a == b
  VCharacter a == VCharacter b = a == b
  VText a == VText b = a == b
  VRecord xs == VRecord ys = xs == ys
  VData xName _ xArgs == VData yName _ yArgs = xName == yName && xArgs == yArgs
  _ == _ = False

data RuntimeEnv = RuntimeEnv [(String, RuntimeValue)] (Map.Map String BoneInfo)
data RuntimeError = RuntimeMessage String | RuntimeEffect String
data RuntimeResult a = RuntimeOk a | RuntimeErr RuntimeError | RuntimeOp String String [RuntimeValue] (RuntimeValue -> IO (RuntimeResult a))
data PatternBind = PatternMatched RuntimeEnv | PatternNoMatch

bindRuntimeResult :: IO (RuntimeResult a) -> (a -> IO (RuntimeResult b)) -> IO (RuntimeResult b)
bindRuntimeResult result f = result >>= \case
  RuntimeOk value -> f value
  RuntimeErr err -> pure (RuntimeErr err)
  RuntimeOp effectName opName args resume ->
    pure (RuntimeOp effectName opName args (\value -> bindRuntimeResult (resume value) f))

mapRuntimeResult :: IO (RuntimeResult a) -> (a -> b) -> IO (RuntimeResult b)
mapRuntimeResult result f = bindRuntimeResult result (pure . RuntimeOk . f)

foldRuntimeResult :: (b -> a -> IO (RuntimeResult b)) -> b -> [a] -> IO (RuntimeResult b)
foldRuntimeResult step =
  foldl' (\result item -> bindRuntimeResult result (`step` item)) . pure . RuntimeOk

performRuntime :: String -> String -> [RuntimeValue] -> IO (RuntimeResult RuntimeValue)
performRuntime effectName opName args = pure (RuntimeOp effectName opName args (pure . RuntimeOk))

evaluate :: String -> IO String
evaluate source = evaluateWithImports source Map.empty

evaluateWithImports :: String -> Map.Map String String -> IO String
evaluateWithImports source imports = case parse source of
  Left msg -> pure ("parse error: " ++ msg)
  Right program -> do
    result <- evalProgramWithImports program imports
    pure $ case result of
      RuntimeOk value -> "eval ok: " ++ showRuntimeValue value
      RuntimeErr err -> "eval error: " ++ showRuntimeError err
      RuntimeOp effectName opName _ _ -> "eval error: " ++ showUnhandledOperation effectName opName

evalProgramWithImports :: Program -> Map.Map String String -> IO (RuntimeResult RuntimeValue)
evalProgramWithImports program imports = mapRuntimeResult (evalProgramEnvWithImports program imports) snd

evalImportedProgram :: Program -> Map.Map String String -> IO (RuntimeResult RuntimeEnv)
evalImportedProgram program imports = mapRuntimeResult (evalProgramEnvWithImports program imports) fst

evalProgramEnvWithImports :: Program -> Map.Map String String -> IO (RuntimeResult (RuntimeEnv, RuntimeValue))
evalProgramEnvWithImports (Program ds) imports = evalDeclsWithImports ds imports baseRuntimeEnv VUnit

evalDeclsWithImports :: [Decl] -> Map.Map String String -> RuntimeEnv -> RuntimeValue -> IO (RuntimeResult (RuntimeEnv, RuntimeValue))
evalDeclsWithImports ds imports env lastValue = foldRuntimeResult step (env, lastValue) ds
  where
    step (currentEnv, currentLast) = \case
      Import path -> bindRuntimeResult (evalImport path imports) $ \importedEnv -> do
        let env2 = mergeRuntimeEnv (namespaceRuntimeImportEnv (importNamespace path) importedEnv) currentEnv
        pure (RuntimeOk (env2, currentLast))
      d -> bindRuntimeResult (evalDecl d currentEnv) $ \(env2, value) ->
        pure (RuntimeOk (env2, fromMaybe currentLast value))

evalImport :: String -> Map.Map String String -> IO (RuntimeResult RuntimeEnv)
evalImport path imports = case Map.lookup path imports of
  Nothing -> pure (RuntimeErr (RuntimeMessage ("missing bring '" ++ path ++ "'")))
  Just source -> case parse source of
    Left msg -> pure (RuntimeErr (RuntimeMessage ("in bring '" ++ path ++ "': " ++ msg)))
    Right program -> evalImportedProgram program imports

namespaceRuntimeImportEnv :: String -> RuntimeEnv -> RuntimeEnv
namespaceRuntimeImportEnv ns (RuntimeEnv values bones) = RuntimeEnv (namespaceRuntimeImportValues ns values) (namespaceImportBones ns bones)

namespaceRuntimeImportValues :: String -> [(String, RuntimeValue)] -> [(String, RuntimeValue)]
namespaceRuntimeImportValues ns = concatMap $ \(name, value) ->
  if isBaseRuntimeImportName name
    then []
    else let value2 = namespaceRuntimeValue ns value in [(lastQualifiedSegment name, value2), (ns ++ "@" ++ name, value2)]

namespaceRuntimeValue :: String -> RuntimeValue -> RuntimeValue
namespaceRuntimeValue ns = \case
  VData qualifiedName displayName args -> VData (ns ++ "@" ++ qualifiedName) displayName args
  VConstructor qualifiedName displayName arity args -> VConstructor (ns ++ "@" ++ qualifiedName) displayName arity args
  value@(VEffectOp effectName _ _ _) | isBaseRuntimeEffectName effectName -> value
  VEffectOp effectName opName arity args -> VEffectOp (ns ++ "@" ++ effectName) opName arity args
  value -> value

mergeRuntimeEnv :: RuntimeEnv -> RuntimeEnv -> RuntimeEnv
mergeRuntimeEnv (RuntimeEnv leftValues leftBones) (RuntimeEnv rightValues rightBones) =
  RuntimeEnv (leftValues ++ rightValues) (Map.union leftBones rightBones)

evalDecl :: Decl -> RuntimeEnv -> IO (RuntimeResult (RuntimeEnv, Maybe RuntimeValue))
evalDecl decl env = case decl of
  Import path -> pure (RuntimeErr (RuntimeMessage ("evaluation does not support bring '" ++ path ++ "'")))
  TypeAlias _ _ -> pure (RuntimeOk (env, Nothing))
  DataDecl _ name ctors -> pure (RuntimeOk (addConstructors name ctors env, Nothing))
  EffectDecl _ name ops -> pure (RuntimeOk (addEffectOps name ops env, Nothing))
  BoneDecl params boneName needs members -> evalBoneDefaultMembers boneName members (addRuntimeBoneInfo boneName params needs members env)
  FleshDecl _ boneName _ members -> evalFleshMembers boneName members env
  Let name _ expr -> bindRuntimeResult (evalBoundValue name expr env) $ \value -> do
    let lastValue = if name == "_" then Just value else Nothing
    pure (RuntimeOk (addValue name value env, lastValue))

evalBoundValue :: String -> Expr -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
evalBoundValue name expr env = case expr of
  EMatch [] cases -> pure (RuntimeOk (VRecursiveMatcher name cases env []))
  _ -> evalExpr expr env

evalBoneDefaultMembers :: String -> [BoneMember] -> RuntimeEnv -> IO (RuntimeResult (RuntimeEnv, Maybe RuntimeValue))
evalBoneDefaultMembers boneName members env = foldRuntimeResult step (env, Nothing) members
  where
    step (currentEnv, lastValue) = \case
      BoneSpec _ _ -> pure (RuntimeOk (currentEnv, lastValue))
      BoneDefault name _ expr -> bindRuntimeResult (evalBoundValue name expr currentEnv) $ \value -> do
        let env2 = addValue (boneName ++ "@" ++ name) value (addValue name value currentEnv)
        pure (RuntimeOk (env2, keepFirstRuntimeValue lastValue value))

evalFleshMembers :: String -> [Decl] -> RuntimeEnv -> IO (RuntimeResult (RuntimeEnv, Maybe RuntimeValue))
evalFleshMembers boneName members env = foldRuntimeResult step (env, Nothing) members
  where
    step (currentEnv, lastValue) (Let name _ expr) =
      bindRuntimeResult (evalBoundValue name expr currentEnv) $ \value ->
        pure (RuntimeOk (addFleshValue boneName name value currentEnv, keepFirstRuntimeValue lastValue value))
    step _ _ = pure (RuntimeErr (RuntimeMessage "flesh member must be a let"))

keepFirstRuntimeValue :: Maybe RuntimeValue -> RuntimeValue -> Maybe RuntimeValue
keepFirstRuntimeValue Nothing value = Just value
keepFirstRuntimeValue old@(Just _) _ = old

evalExpr :: Expr -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
evalExpr expr env = case expr of
  EInteger i -> pure (RuntimeOk (VInteger i))
  EFloat s -> pure (RuntimeOk (VFloat s))
  EChar c -> pure (RuntimeOk (VCharacter c))
  EString s -> pure (RuntimeOk (VText s))
  EVar name -> evalVar name env
  EApply f args -> evalApply f (NE.toList args) env
  ERecord fields -> evalRecord fields env Map.empty
  EUpdate base updates -> evalRecordUpdate base updates env
  ETry body cases -> evalTry body cases env
  EMatch scrutinees cases -> evalMatchExpr scrutinees cases env
  EBlock ds body -> evalBlock ds body env

evalVar :: String -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
evalVar name env = pure $ maybe (RuntimeErr (RuntimeMessage ("unknown name '" ++ name ++ "'"))) RuntimeOk (lookupValue name env)

evalApply :: Expr -> [Expr] -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
evalApply f args env = bindRuntimeResult (evalExpr f env) (\fn -> evalApplyArgs fn args env)

evalApplyArgs :: RuntimeValue -> [Expr] -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
evalApplyArgs fn args env = foldRuntimeResult step fn args
  where
    step currentFn arg =
      bindRuntimeResult (evalExpr arg env) (applyOne currentFn)

applyOne :: RuntimeValue -> RuntimeValue -> IO (RuntimeResult RuntimeValue)
applyOne fn arg = case fn of
  VMatcher cases matchEnv supplied -> applyMatcher Nothing cases matchEnv supplied arg
  VRecursiveMatcher name cases matchEnv supplied -> applyMatcher (Just name) cases matchEnv supplied arg
  VConstructor qualifiedName displayName arity supplied ->
    applyArity arity supplied arg (VConstructor qualifiedName displayName arity) (pure . RuntimeOk . VData qualifiedName displayName)
  VNative name arity supplied ->
    applyArity arity supplied arg (VNative name arity) (evalNative name)
  VEffectOp effectName opName arity supplied ->
    applyArity arity supplied arg (VEffectOp effectName opName arity) (evalEffectOp effectName opName)
  VContinuation resume -> resume arg
  _ -> pure (RuntimeErr (RuntimeMessage ("cannot apply " ++ showRuntimeValue fn)))

applyArity :: Int -> [RuntimeValue] -> RuntimeValue -> ([RuntimeValue] -> RuntimeValue) -> ([RuntimeValue] -> IO (RuntimeResult RuntimeValue)) -> IO (RuntimeResult RuntimeValue)
applyArity arity supplied arg partial full =
  let supplied2 = supplied ++ [arg]
  in if length supplied2 < arity
      then pure (RuntimeOk (partial supplied2))
      else if length supplied2 == arity
        then full supplied2
        else pure (RuntimeErr (RuntimeMessage "too many arguments"))

applyMatcher :: Maybe String -> NonEmpty MatchCase -> RuntimeEnv -> [RuntimeValue] -> RuntimeValue -> IO (RuntimeResult RuntimeValue)
applyMatcher maybeName cases matchEnv supplied arg =
  let supplied2 = supplied ++ [arg]
      selfEnv = case maybeName of
        Nothing -> matchEnv
        Just name -> addValue name (VRecursiveMatcher name cases matchEnv []) matchEnv
      nextMatcher = case maybeName of
        Nothing -> VMatcher cases matchEnv supplied2
        Just name -> VRecursiveMatcher name cases matchEnv supplied2
      arity = matchCaseArity cases
  in if length supplied2 < arity
      then pure (RuntimeOk nextMatcher)
      else if length supplied2 == arity
        then evalMatchCases (NE.toList cases) supplied2 selfEnv
        else pure (RuntimeErr (RuntimeMessage "too many arguments for match"))

evalRecord :: [(String, Expr)] -> RuntimeEnv -> Map.Map String RuntimeValue -> IO (RuntimeResult RuntimeValue)
evalRecord fields env acc = mapRuntimeResult (foldRuntimeResult step acc fields) VRecord
  where
    step record (name, expr) =
      bindRuntimeResult (evalExpr expr env) $ \value ->
        pure (RuntimeOk (Map.insert name value record))

evalRecordUpdate :: Expr -> [RecordUpdate] -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
evalRecordUpdate base updates env =
  bindRuntimeResult (evalExpr base env) $ \case
    VRecord fields -> evalRecordUpdates updates fields env
    value -> pure (RuntimeErr (RuntimeMessage ("record update expected record, found " ++ showRuntimeValue value)))

evalRecordUpdates :: [RecordUpdate] -> Map.Map String RuntimeValue -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
evalRecordUpdates updates fields env = mapRuntimeResult (foldRuntimeResult step fields updates) VRecord
  where
    step record = \case
      RecordRemove name ->
        if Map.member name record
          then pure (RuntimeOk (Map.delete name record))
          else pure (RuntimeErr (RuntimeMessage ("unknown record field '" ++ name ++ "'")))
      RecordSet name expr ->
        bindRuntimeResult (evalExpr expr env) $ \value ->
          pure (RuntimeOk (Map.insert name value record))

evalTry :: Expr -> [HandlerCase] -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
evalTry body cases env = evalExpr body env >>= \result -> handleRuntimeResult result cases env

handleRuntimeResult :: RuntimeResult RuntimeValue -> [HandlerCase] -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
handleRuntimeResult result cases env = case result of
  RuntimeOk value -> pure (RuntimeOk value)
  RuntimeErr err -> pure (RuntimeErr err)
  RuntimeOp effectName opName args resume -> case findHandler effectName opName cases of
    Nothing -> pure (RuntimeOp effectName opName args (\value -> resume value >>= \r -> handleRuntimeResult r cases env))
    Just handlerCase -> evalHandlerCase handlerCase args (\value -> resume value >>= \r -> handleRuntimeResult r cases env) env

findHandler :: String -> String -> [HandlerCase] -> Maybe HandlerCase
findHandler effectName opName = find (\(HandlerCase name _ _) -> handlerMatches name effectName opName)

handlerMatches :: String -> String -> String -> Bool
handlerMatches name effectName opName =
  name `elem` [opName, effectName, lastQualifiedSegment effectName, effectName ++ "@" ++ opName]

evalHandlerCase :: HandlerCase -> [RuntimeValue] -> (RuntimeValue -> IO (RuntimeResult RuntimeValue)) -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
evalHandlerCase (HandlerCase _ patterns body) args resume env =
  case bindHandlerRuntimePatterns patterns args env of
    PatternNoMatch -> pure (RuntimeErr (RuntimeMessage "handler operation arguments did not match pattern"))
    PatternMatched handlerEnv -> evalExpr body (addValue "resume" (VContinuation resume) handlerEnv)

bindHandlerRuntimePatterns :: [Pattern] -> [RuntimeValue] -> RuntimeEnv -> PatternBind
bindHandlerRuntimePatterns [] _ env = PatternMatched env
bindHandlerRuntimePatterns patterns values env = bindRuntimePatterns patterns values env

evalMatchExpr :: [Expr] -> NonEmpty MatchCase -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
evalMatchExpr [] cases env = pure (RuntimeOk (VMatcher cases env []))
evalMatchExpr scrutinees cases env =
  bindRuntimeResult (evalExprList scrutinees env) $ \values -> evalMatchCases (NE.toList cases) values env

evalExprList :: [Expr] -> RuntimeEnv -> IO (RuntimeResult [RuntimeValue])
evalExprList exprs env = mapRuntimeResult (foldRuntimeResult step [] exprs) reverse
  where
    step values expr = mapRuntimeResult (evalExpr expr env) (: values)

evalMatchCases :: [MatchCase] -> [RuntimeValue] -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
evalMatchCases cases values env = foldr step noMatch cases
  where
    noMatch = pure (RuntimeErr (RuntimeMessage "non-exhaustive match"))
    step (MatchCase patterns body) fallback =
      case bindRuntimePatterns (NE.toList patterns) values env of
        PatternNoMatch -> fallback
        PatternMatched boundEnv -> evalExpr body boundEnv

evalBlock :: [Decl] -> Expr -> RuntimeEnv -> IO (RuntimeResult RuntimeValue)
evalBlock ds body env = bindRuntimeResult (evalLocalDecls ds env) (\env2 -> evalExpr body env2)

evalLocalDecls :: [Decl] -> RuntimeEnv -> IO (RuntimeResult RuntimeEnv)
evalLocalDecls ds env = foldRuntimeResult step env ds
  where
    step currentEnv (Let name _ expr) =
      bindRuntimeResult (evalExpr expr currentEnv) $ \value ->
        pure (RuntimeOk (addValue name value currentEnv))
    step currentEnv _ = pure (RuntimeOk currentEnv)

bindRuntimePatterns :: [Pattern] -> [RuntimeValue] -> RuntimeEnv -> PatternBind
bindRuntimePatterns patterns values env
  | length patterns /= length values = PatternNoMatch
  | otherwise = foldl' step (PatternMatched env) (zip patterns values)
  where
    step PatternNoMatch _ = PatternNoMatch
    step (PatternMatched currentEnv) (pattern, value) = bindRuntimePattern pattern value currentEnv

bindRuntimePattern :: Pattern -> RuntimeValue -> RuntimeEnv -> PatternBind
bindRuntimePattern (PVar "_") _ env = PatternMatched env
bindRuntimePattern (PVar name) value env =
  if isNullaryConstructorPattern name env
    then if runtimeConstructorMatches name value then PatternMatched env else PatternNoMatch
    else PatternMatched (addValue name value env)
bindRuntimePattern (PCon name args) value env = case value of
  VData _ _ fields | runtimeConstructorMatches name value -> bindRuntimePatterns args fields env
  _ -> PatternNoMatch

isNullaryConstructorPattern :: String -> RuntimeEnv -> Bool
isNullaryConstructorPattern name env = case lookupValue name env of
  Just (VData _ _ []) -> True
  _ -> False

runtimeConstructorMatches :: String -> RuntimeValue -> Bool
runtimeConstructorMatches patternName = \case
  VData qualifiedName displayName _ ->
    if isRuntimeQualifiedName patternName then patternName == qualifiedName else patternName == displayName
  _ -> False

evalNative :: String -> [RuntimeValue] -> IO (RuntimeResult RuntimeValue)
evalNative name args = case (name, args) of
  ("+", [VInteger a, VInteger b]) -> pure (RuntimeOk (VInteger (a + b)))
  ("+", [VFloat a, VFloat b]) -> evalFloatBinary (+) a b
  ("-", [VInteger a, VInteger b]) -> pure (RuntimeOk (VInteger (a - b)))
  ("-", [VFloat a]) -> evalFloatNegate a
  ("-", [VFloat a, VFloat b]) -> evalFloatBinary (-) a b
  ("×", [VInteger a, VInteger b]) -> pure (RuntimeOk (VInteger (a * b)))
  ("×", [VFloat a, VFloat b]) -> evalFloatBinary (*) a b
  ("¯", [VInteger a]) -> pure (RuntimeOk (VInteger (-a)))
  ("¯", [VFloat a]) -> evalFloatNegate a
  ("÷", [VInteger a, VInteger b]) -> evalIntegerDivide a b
  ("divide", [VInteger a, VInteger b]) -> evalIntegerDivide a b
  ("≡", [a, b]) -> pure (RuntimeOk (runtimeBool (a == b)))
  ("≤", [VInteger a, VInteger b]) -> pure (RuntimeOk (runtimeBool (a <= b)))
  ("≤", [VFloat a, VFloat b]) -> evalFloatLessEqual a b
  ("/", [VFloat a]) -> evalFloatReciprocal a
  ("⌊", [VFloat a]) -> evalFloatFloor a
  ("exp", [VFloat a]) -> evalFloatUnary a (show . exp)
  ("log", [VFloat a]) -> evalFloatUnary a (show . log)
  ("sin", [VFloat a]) -> evalFloatUnary a (show . sin)
  ("cos", [VFloat a]) -> evalFloatUnary a (show . cos)
  ("from-string", [VText text]) -> floatFromString text
  ("to-string", [value]) -> pure (RuntimeOk (VText (showRuntimeValue value)))
  _ -> pure (RuntimeErr (RuntimeMessage ("native '" ++ name ++ "' does not support these arguments")))

evalEffectOp :: String -> String -> [RuntimeValue] -> IO (RuntimeResult RuntimeValue)
evalEffectOp effectName opName args = case (effectName, opName, args) of
  ("console", "write-line", [VText text]) -> writeConsoleLine text
  ("console", "read-line", [VData _ "null" []]) -> RuntimeOk . VText <$> readConsoleLine
  ("random", "random-float", [VData _ "null" []]) -> RuntimeOk . VFloat . show <$> nextRandomFloat
  ("random", "random-integer", [VData _ "null" []]) -> RuntimeOk . VInteger <$> nextRandomInteger
  ("async", "sleep", [VInteger ms]) -> sleepMilliseconds ms
  ("async", "fork", [work]) -> forkSync work
  ("async", "wait", [VData _ "done" [value]]) -> pure (RuntimeOk value)
  _ -> performRuntime effectName opName args

failRuntime :: String -> IO (RuntimeResult RuntimeValue)
failRuntime message = performRuntime "fail" "fail" [VText message]

writeConsoleLine :: String -> IO (RuntimeResult RuntimeValue)
writeConsoleLine text = putStrLn text >> pure (RuntimeOk runtimeNull)

readConsoleLine :: IO String
readConsoleLine = hFlush stdout >> getLine

sleepMilliseconds :: Int -> IO (RuntimeResult RuntimeValue)
sleepMilliseconds ms = threadDelay (max 0 ms * 1000) >> pure (RuntimeOk runtimeNull)

forkSync :: RuntimeValue -> IO (RuntimeResult RuntimeValue)
forkSync work = bindRuntimeResult (applyOne work runtimeNull) (pure . RuntimeOk . runtimeDone)

runtimeNull :: RuntimeValue
runtimeNull = VData "𝟙@null" "null" []

runtimeDone :: RuntimeValue -> RuntimeValue
runtimeDone value = VData "task@done" "done" [value]

nextRandomFloat :: IO Double
nextRandomFloat = do
  t <- getPOSIXTime
  pure (realToFrac (t - fromIntegral (floor t :: Integer)))

nextRandomInteger :: IO Int
nextRandomInteger = do
  t <- getPOSIXTime
  pure (floor (t * 1000000) :: Int)

evalFloatNegate :: String -> IO (RuntimeResult RuntimeValue)
evalFloatNegate text = evalFloatUnary text (show . negate)

evalFloatUnary :: String -> (Double -> String) -> IO (RuntimeResult RuntimeValue)
evalFloatUnary text f = case readMaybe text of
  Nothing -> pure (RuntimeErr (RuntimeMessage ("invalid float '" ++ text ++ "'")))
  Just x -> pure (RuntimeOk (VFloat (f x)))

evalFloatBinary :: (Double -> Double -> Double) -> String -> String -> IO (RuntimeResult RuntimeValue)
evalFloatBinary f left right = case (readMaybe left, readMaybe right) of
  (Just x, Just y) -> pure (RuntimeOk (VFloat (show (f x y))))
  _ -> pure (RuntimeErr (RuntimeMessage "invalid float"))

evalIntegerDivide :: Int -> Int -> IO (RuntimeResult RuntimeValue)
evalIntegerDivide _ 0 = failRuntime "division by zero"
evalIntegerDivide a b = pure (RuntimeOk (VInteger (a `div` b)))

evalFloatLessEqual :: String -> String -> IO (RuntimeResult RuntimeValue)
evalFloatLessEqual left right = case (readMaybe left :: Maybe Double, readMaybe right :: Maybe Double) of
  (Just x, Just y) -> pure (RuntimeOk (runtimeBool (x <= y)))
  _ -> pure (RuntimeErr (RuntimeMessage "invalid float"))

evalFloatReciprocal :: String -> IO (RuntimeResult RuntimeValue)
evalFloatReciprocal text = case readMaybe text of
  Just 0.0 -> failRuntime "division by zero"
  Just x -> pure (RuntimeOk (VFloat (show (1.0 / (x :: Double)))))
  Nothing -> pure (RuntimeErr (RuntimeMessage ("invalid float '" ++ text ++ "'")))

evalFloatFloor :: String -> IO (RuntimeResult RuntimeValue)
evalFloatFloor text = case readMaybe text of
  Just x -> pure (RuntimeOk (VInteger (floor (x :: Double))))
  Nothing -> pure (RuntimeErr (RuntimeMessage ("invalid float '" ++ text ++ "'")))

floatFromString :: String -> IO (RuntimeResult RuntimeValue)
floatFromString text = case (readMaybe text :: Maybe Double) of
  Just x -> pure (RuntimeOk (VFloat (show x)))
  Nothing -> failRuntime "could not parse float"

runtimeBool :: Bool -> RuntimeValue
runtimeBool True = VData "𝟚@yea" "yea" []
runtimeBool False = VData "𝟚@nay" "nay" []

addConstructors :: String -> [Ctor] -> RuntimeEnv -> RuntimeEnv
addConstructors dataName ctors env = foldl' step env ctors
  where
    step current (Ctor name fields) =
      let arity = length fields
          qualifiedName = dataName ++ "@" ++ name
          value = if arity == 0 then VData qualifiedName name [] else VConstructor qualifiedName name arity []
      in addValue name value (addValue qualifiedName value current)

addEffectOps :: String -> [EffectOp] -> RuntimeEnv -> RuntimeEnv
addEffectOps effectName ops env = foldl' step env ops
  where
    step current (EffectOp name t) =
      let value = VEffectOp effectName name (typeArity t) []
      in addValue name value (addValue (effectName ++ "@" ++ name) value current)

typeArity :: TypeExpr -> Int
typeArity (TypeArrow args _ _) = NE.length args
typeArity _ = 0

baseRuntimeEnv :: RuntimeEnv
baseRuntimeEnv = addBaseEffectOps (addBaseNatives (RuntimeEnv [] Map.empty))

isBaseRuntimeName :: String -> Bool
isBaseRuntimeName name = name `elem` baseRuntimeNames

isBaseRuntimeImportName :: String -> Bool
isBaseRuntimeImportName name = isBaseRuntimeName name || isBaseRuntimeName (lastQualifiedSegment name)

isBaseRuntimeEffectName :: String -> Bool
isBaseRuntimeEffectName name = name `elem` baseEffectNames

addBaseNatives :: RuntimeEnv -> RuntimeEnv
addBaseNatives env = foldl' (\current (name, arity) -> addNative name arity current) env runtimeNativeSpecs

addBaseEffectOps :: RuntimeEnv -> RuntimeEnv
addBaseEffectOps env = foldl' (\current (effectName, opName, arity) -> addBaseEffectOp effectName opName arity current) env runtimeEffectOpSpecs

addNative :: String -> Int -> RuntimeEnv -> RuntimeEnv
addNative name arity = addValue name (VNative name arity [])

addBaseEffectOp :: String -> String -> Int -> RuntimeEnv -> RuntimeEnv
addBaseEffectOp effectName opName arity = addValue opName (VEffectOp effectName opName arity [])

addValue :: String -> RuntimeValue -> RuntimeEnv -> RuntimeEnv
addValue name value (RuntimeEnv values bones) = RuntimeEnv ((name, value) : values) bones

lookupValue :: String -> RuntimeEnv -> Maybe RuntimeValue
lookupValue name (RuntimeEnv values _) = lookup name values

addRuntimeBoneInfo :: String -> [String] -> [BoneNeed] -> [BoneMember] -> RuntimeEnv -> RuntimeEnv
addRuntimeBoneInfo boneName params needs members (RuntimeEnv values bones) =
  RuntimeEnv values (Map.insert boneName (BoneInfo params needs members) bones)

addFleshValue :: String -> String -> RuntimeValue -> RuntimeEnv -> RuntimeEnv
addFleshValue boneName memberName value env =
  addFleshOwnerValues memberName (runtimeMemberOwnersOrDirect boneName memberName env) value (addValue memberName value env)

addFleshOwnerValues :: String -> [String] -> RuntimeValue -> RuntimeEnv -> RuntimeEnv
addFleshOwnerValues memberName owners value env =
  foldl' (\current owner -> addValue (owner ++ "@" ++ memberName) value current) env owners

runtimeMemberOwnersOrDirect :: String -> String -> RuntimeEnv -> [String]
runtimeMemberOwnersOrDirect boneName memberName env = case runtimeMemberOwners boneName memberName env of
  [] -> [boneName]
  owners -> owners

runtimeMemberOwners :: String -> String -> RuntimeEnv -> [String]
runtimeMemberOwners boneName memberName (RuntimeEnv _ bones) = runtimeMemberOwnersIn boneName memberName bones []

runtimeMemberOwnersIn :: String -> String -> Map.Map String BoneInfo -> [String] -> [String]
runtimeMemberOwnersIn boneName memberName bones seen
  | boneName `elem` seen = []
  | otherwise = case Map.lookup boneName bones of
      Nothing -> []
      Just (BoneInfo _ needs members) ->
        let own = maybe [] (const [boneName]) (findBoneMember memberName members)
        in own ++ runtimeMemberOwnersInNeeds needs memberName bones (boneName : seen)

runtimeMemberOwnersInNeeds :: [BoneNeed] -> String -> Map.Map String BoneInfo -> [String] -> [String]
runtimeMemberOwnersInNeeds needs memberName bones seen =
  concatMap (\(BoneNeed _ neededName) -> runtimeMemberOwnersIn neededName memberName bones seen) needs

isRuntimeQualifiedName :: String -> Bool
isRuntimeQualifiedName = elem '@'

showRuntimeError :: RuntimeError -> String
showRuntimeError = \case
  RuntimeMessage msg -> msg
  RuntimeEffect effectName -> "unhandled effect '" ++ effectName ++ "'"

showUnhandledOperation :: String -> String -> String
showUnhandledOperation effectName opName = "unhandled effect '" ++ effectName ++ "' operation '" ++ opName ++ "'"

showRuntimeValue :: RuntimeValue -> String
showRuntimeValue = \case
  VUnit -> "()"
  VInteger i -> show i
  VFloat s -> s
  VCharacter c -> "`" ++ c
  VText s -> "'" ++ s ++ "'"
  VRecord fields -> "[" ++ showRuntimeFields fields ++ "]"
  VData _ displayName [] -> displayName
  VData _ displayName args -> "(" ++ showRuntimeValues args ++ " " ++ displayName ++ ")"
  VMatcher {} -> "<function>"
  VRecursiveMatcher {} -> "<function>"
  VConstructor _ displayName _ _ -> "<constructor " ++ displayName ++ ">"
  VNative name _ _ -> "<native " ++ name ++ ">"
  VEffectOp _ opName _ _ -> "<effect " ++ opName ++ ">"
  VContinuation _ -> "<continuation>"

showRuntimeValues :: [RuntimeValue] -> String
showRuntimeValues = unwords . map showRuntimeValue

showRuntimeFields :: Map.Map String RuntimeValue -> String
showRuntimeFields fields = intercalate ", " [name ++ " = " ++ showRuntimeValue value | (name, value) <- Map.toList fields]
