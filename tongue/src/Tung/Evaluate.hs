{- | strict call-by-value evaluation of checked 'CoreProgram' values.
applications evaluate function then arguments left-to-right; handlers are deep
and their captured continuations are reusable.
-}
module Tung.Evaluate (
  evaluate,
  evaluateWithImports,
  evaluateWithArgsAndImports,
  evaluateMainWithImports,
  evaluateMainWithArgsAndImports,
)
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (ap, foldM, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Class qualified as MonadIO
import Control.Monad.Trans.State.Strict qualified as State
import Data.List (find, intercalate, isPrefixOf, stripPrefix)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment qualified as Environment
import System.Exit (ExitCode (..), exitWith)
import System.IO (hFlush, stdout)
import System.IO qualified as IO
import Text.Read (readMaybe)
import Tung.Core (CoreProgram, coreDeclarations)
import Tung.Import (ImportStack, enterImport)
import Tung.Name (importNamespace, isQualifiedName, lastQualifiedSegment, splitFieldAccessName)
import Tung.Parse (parse)
import Tung.Syntax
import Tung.Type

-- callable values retain supplied arguments, making partial application a pure
-- value. natives and effect operations run only when 'applyArity' is saturated.
data RuntimeValue
  = VUnit
  | VInteger Int
  | VFloat String
  | VCharacter String
  | VText String
  | VRecord (Map.Map String RuntimeValue)
  | VMatcher [MatchCase] RuntimeEnv [RuntimeValue]
  | VRecursiveMatcher String [MatchCase] RuntimeEnv [RuntimeValue]
  | VConstructor String String Int [RuntimeValue]
  | VData String String [RuntimeValue]
  | VNative String Int [RuntimeValue]
  | VEffectOp String String Int [RuntimeValue]
  | VContinuation (RuntimeValue -> Eval RuntimeValue)
  | VShapeMember String
  | VDictionary RuntimeDictionary

data RuntimeDictionary
  = FillDictionary RuntimeFill [RuntimeValue] [RuntimeDictionary]
  | PrimitiveDictionary String String

data RuntimeFill = RuntimeFill
  { runtimeFillShape :: String
  , runtimeFillNeeds :: [ShapeNeed]
  , runtimeFillMembers :: Map.Map String Expr
  , runtimeFillEnv :: RuntimeEnv
  }

instance Eq RuntimeValue where
  VUnit == VUnit = True
  VInteger a == VInteger b = a == b
  VFloat a == VFloat b = a == b
  VCharacter a == VCharacter b = a == b
  VText a == VText b = a == b
  VRecord xs == VRecord ys = xs == ys
  VData xName _ xArgs == VData yName _ yArgs = xName == yName && xArgs == yArgs
  _ == _ = False

type RuntimeBinding = (String, RuntimeValue, Int, String)

data RuntimeEnv = RuntimeEnv
  { runtimeValues :: [RuntimeBinding]
  , runtimeLookup :: Map.Map String RuntimeValue
  , runtimeShapes :: Map.Map String ShapeInfo
  , runtimeFills :: Map.Map String RuntimeFill
  }

newtype RuntimeError = RuntimeError String

-- an operation carries its explicit continuation. the 'Eval' monad composes
-- through that continuation, which gives handlers deep, reusable resumption.
data RuntimeResult a = RuntimeOk a | RuntimeErr RuntimeError | RuntimeOp String String [RuntimeValue] (RuntimeValue -> Eval a)

data PatternBind = PatternMatched RuntimeEnv | PatternNoMatch

newtype Eval a = Eval {runEval :: RuntimeHost -> State.StateT RuntimeCache IO (RuntimeResult a)}

newtype RuntimeHost = RuntimeHost {hostArguments :: [String]}

type RuntimeCache = Map.Map String RuntimeEnv

instance Functor Eval where
  fmap f (Eval action) =
    Eval $ \host ->
      action host >>= \case
        RuntimeOk value -> pure (RuntimeOk (f value))
        RuntimeErr err -> pure (RuntimeErr err)
        RuntimeOp effectName opName args resume -> pure (RuntimeOp effectName opName args (resume >=> pure . f))

instance Applicative Eval where
  pure = Eval . const . pure . RuntimeOk
  (<*>) = ap

instance Monad Eval where
  Eval action >>= next =
    Eval $ \host ->
      action host >>= \case
        RuntimeOk value -> runEval (next value) host
        RuntimeErr err -> pure (RuntimeErr err)
        RuntimeOp effectName opName args resume ->
          pure (RuntimeOp effectName opName args (resume >=> next))

instance MonadIO Eval where
  liftIO action = Eval (const (RuntimeOk <$> MonadIO.liftIO action))

raiseRuntime :: RuntimeError -> Eval a
raiseRuntime = Eval . const . pure . RuntimeErr

runtimeError :: String -> Eval a
runtimeError = raiseRuntime . RuntimeError

performRuntime :: String -> String -> [RuntimeValue] -> Eval RuntimeValue
performRuntime effectName opName args = Eval (const (pure (RuntimeOp effectName opName args pure)))

evaluate :: String -> IO String
evaluate source = evaluateWithImports source Map.empty

evaluateWithImports :: String -> Map.Map String String -> IO String
evaluateWithImports = evaluateWithArgsAndImports []

evaluateWithArgsAndImports :: [String] -> String -> Map.Map String String -> IO String
evaluateWithArgsAndImports args source imports = evaluateParsedWith False (RuntimeHost args) source imports evalProgramWithImports

evaluateMainWithImports :: String -> Map.Map String String -> IO String
evaluateMainWithImports = evaluateMainWithArgsAndImports []

evaluateMainWithArgsAndImports :: [String] -> String -> Map.Map String String -> IO String
evaluateMainWithArgsAndImports args source imports = evaluateParsedWith True (RuntimeHost args) source imports runMainWithImports

-- evaluation never accepts surface syntax directly: this function first asks
-- the type checker for a mode-appropriate 'CoreProgram'.
evaluateParsedWith :: Bool -> RuntimeHost -> String -> Map.Map String String -> (CoreProgram -> Map.Map String String -> Eval RuntimeValue) -> IO String
evaluateParsedWith runnable host source imports action = case parse source of
  Left msg -> pure ("parse error: " ++ msg)
  Right parsed -> case elaborate parsed of
    Left msg -> pure ((if runnable then "type error: " else "eval error: type error: ") ++ msg)
    Right program -> runProgram program
 where
  -- interactive evaluation allows top-level computation, but still checks and
  -- elaborates it. runnable evaluation additionally checks the main boundary.
  elaborate program
    | runnable = elaborateProgramWithImports program imports True
    | otherwise = elaborateInteractiveProgramWithImports program imports
  runProgram program = do
    result <- State.evalStateT (runEval (action program imports) host) Map.empty
    pure $ case result of
      RuntimeOk value -> "eval ok: " ++ showRuntimeValue value
      RuntimeErr err -> "eval error: " ++ showRuntimeError err
      RuntimeOp effectName opName _ _ -> "eval error: " ++ showUnhandledOperation effectName opName

runMainWithImports :: CoreProgram -> Map.Map String String -> Eval RuntimeValue
runMainWithImports program imports = do
  (env, _) <- evalProgramEnvWithImports [] program imports
  mainValue <- maybe (runtimeError "runnable file must declare main") pure (lookupValue "main" env)
  applyOne env mainValue runtimeNull

evalProgramWithImports :: CoreProgram -> Map.Map String String -> Eval RuntimeValue
evalProgramWithImports program imports = snd <$> evalProgramEnvWithImports [] program imports

evalImportedProgram :: ImportStack -> CoreProgram -> Map.Map String String -> Eval RuntimeEnv
evalImportedProgram importStack program imports = fst <$> evalProgramEnvWithImports importStack program imports

evalProgramEnvWithImports :: ImportStack -> CoreProgram -> Map.Map String String -> Eval (RuntimeEnv, RuntimeValue)
evalProgramEnvWithImports importStack program imports = evalDeclsWithImports importStack (coreDeclarations program) imports baseRuntimeEnv VUnit

evalDeclsWithImports :: ImportStack -> [Decl] -> Map.Map String String -> RuntimeEnv -> RuntimeValue -> Eval (RuntimeEnv, RuntimeValue)
evalDeclsWithImports importStack ds imports env lastValue = foldM step (env, lastValue) ds
 where
  step (currentEnv, currentLast) = \case
    Import path -> do
      importedEnv <- evalImport importStack path imports
      let env2 = mergeRuntimeEnv (namespaceRuntimeImportEnv (importNamespace path) importedEnv) currentEnv
      pure (env2, currentLast)
    d -> do
      (env2, value) <- evalDecl d currentEnv
      pure (env2, fromMaybe currentLast value)

-- runtime import caching mirrors static imports, but stores evaluated exported
-- environments so repeated brings do not repeat module initialisation.
evalImport :: ImportStack -> String -> Map.Map String String -> Eval RuntimeEnv
evalImport importStack path imports = case enterImport importStack path of
  Left message -> runtimeError message
  Right nextStack ->
    lookupRuntimeImport path >>= \case
      Just env -> pure env
      Nothing -> case Map.lookup path imports of
        Nothing -> runtimeError ("missing bring '" ++ path ++ "'")
        Just source -> case parse source of
          Left msg -> runtimeError ("in bring '" ++ path ++ "': " ++ msg)
          Right parsed -> case elaborateProgramWithImports parsed imports False of
            Left msg -> runtimeError ("in bring '" ++ path ++ "': type error: " ++ msg)
            Right program -> do
              env <- evalImportedProgram nextStack program imports
              cacheRuntimeImport path env
              pure env

lookupRuntimeImport :: String -> Eval (Maybe RuntimeEnv)
lookupRuntimeImport path = Eval $ const $ RuntimeOk . Map.lookup path <$> State.get

cacheRuntimeImport :: String -> RuntimeEnv -> Eval ()
cacheRuntimeImport path env = Eval $ const $ State.modify' (Map.insert path env) >> pure (RuntimeOk ())

namespaceRuntimeImportEnv :: String -> RuntimeEnv -> RuntimeEnv
namespaceRuntimeImportEnv ns RuntimeEnv{..} =
  let values = namespaceRuntimeImportValues ns runtimeValues
   in RuntimeEnv
        { runtimeValues = values
        , runtimeLookup = indexRuntimeValues values
        , runtimeShapes = namespaceImportShapes ns runtimeShapes
        , runtimeFills = Map.mapKeys (\key -> ns ++ "@" ++ key) runtimeFills
        }

namespaceRuntimeImportValues :: String -> [RuntimeBinding] -> [RuntimeBinding]
namespaceRuntimeImportValues ns values = concatMap one values
 where
  one (name, value, rank, origin)
    | isBaseRuntimeImportBinding name origin = []
    | rank == exportRuntimeRank && isShownRuntimeOrigin origin =
        let value2 = namespaceRuntimeValue ns value
            origin2 = ns ++ "@" ++ origin
            exact = (ns ++ "@" ++ name, value2, importRuntimeRank, origin2)
         in (lastQualifiedSegment name, value2, aliasRuntimeRank, origin2) : [exact]
    | otherwise = []

isShownRuntimeOrigin :: String -> Bool
isShownRuntimeOrigin origin = "show@" `isPrefixOf` origin

namespaceRuntimeValue :: String -> RuntimeValue -> RuntimeValue
namespaceRuntimeValue ns = \case
  VData qualifiedName displayName args -> VData (ns ++ "@" ++ qualifiedName) displayName args
  VConstructor qualifiedName displayName arity args -> VConstructor (ns ++ "@" ++ qualifiedName) displayName arity args
  value@(VEffectOp effectName _ _ _) | isBaseRuntimeEffectName effectName -> value
  VEffectOp effectName opName arity args -> VEffectOp (ns ++ "@" ++ effectName) opName arity args
  value -> value

mergeRuntimeEnv :: RuntimeEnv -> RuntimeEnv -> RuntimeEnv
mergeRuntimeEnv left right =
  RuntimeEnv
    { runtimeValues = runtimeValues left ++ runtimeValues right
    , runtimeLookup = Map.union (runtimeLookup left) (runtimeLookup right)
    , runtimeShapes = Map.union (runtimeShapes left) (runtimeShapes right)
    , runtimeFills = Map.union (runtimeFills left) (runtimeFills right)
    }

localRuntimeRank, aliasRuntimeRank, importRuntimeRank, baseRuntimeRank, exportRuntimeRank :: Int
localRuntimeRank = 0
aliasRuntimeRank = 1
importRuntimeRank = 1
baseRuntimeRank = 2
exportRuntimeRank = 3

-- declarations extend the environment strictly in source order. recursive
-- anonymous matches capture their own binding without evaluating a body.
evalDecl :: Decl -> RuntimeEnv -> Eval (RuntimeEnv, Maybe RuntimeValue)
evalDecl decl env = case decl of
  Import path -> runtimeError ("evaluation does not support bring '" ++ path ++ "'")
  Export declaration -> do
    (env2, value) <- evalDecl declaration env
    pure (markRuntimeDeclExport declaration env2, value)
  ReExport name -> pure (addShownValue name env, Nothing)
  ReExportType _ -> pure (env, Nothing)
  TypeAlias _ _ -> pure (env, Nothing)
  DataDecl _ name ctors -> pure (addConstructors name ctors env, Nothing)
  EffectDecl _ name ops -> pure (addEffectOps name ops env, Nothing)
  ForeignDecl members -> pure (addForeignMembers members env, Nothing)
  ShapeDecl params shapeName needs members -> pure (addShapeSelectors shapeName members (addRuntimeShapeInfo shapeName params needs members env), Nothing)
  FillDecl{} -> runtimeError "internal unelaborated fill"
  ElaboratedFill key _ shapeName needs members -> pure (addSelectedRuntimeFill key shapeName needs members env, Nothing)
  Let name _ expr -> do
    value <- evalBoundValue name expr env
    let lastValue = if name == "_" then Just value else Nothing
    pure (addValue name value env, lastValue)

evalBoundValue :: String -> Expr -> RuntimeEnv -> Eval RuntimeValue
evalBoundValue name expr env = case expr of
  EMatch [] cases -> pure (VRecursiveMatcher name cases env [])
  _ -> evalExpr expr env

addShapeSelectors :: String -> [ShapeMember] -> RuntimeEnv -> RuntimeEnv
addShapeSelectors shapeName members env = foldl' add env (shapeMemberNames members)
 where
  add current name = addValue name selector (addValue (shapeName ++ "@" ++ name) selector current)
   where
    selector = VShapeMember name

addSelectedRuntimeFill :: String -> String -> [ShapeNeed] -> [Decl] -> RuntimeEnv -> RuntimeEnv
addSelectedRuntimeFill key shapeName needs members env@RuntimeEnv{runtimeFills} =
  env{runtimeFills = Map.insert key template runtimeFills}
 where
  template = RuntimeFill shapeName needs (Map.fromList [(name, expr) | Let name _ expr <- members]) env

-- core expressions contain selected evidence and field accesses, but no raw
-- fills or unresolved evidence holes.
evalExpr :: Expr -> RuntimeEnv -> Eval RuntimeValue
evalExpr expr env = case expr of
  EInteger i -> pure (VInteger i)
  EFloat s -> pure (VFloat s)
  EChar c -> pure (VCharacter c)
  EString s -> pure (VText s)
  EVar name -> evalVar name env
  EApply f args -> evalApply f (NE.toList args) env
  ERecord fields -> evalRecord fields env Map.empty
  EField base field -> evalExpr base env >>= evalRecordField field
  EUpdate base updates -> evalRecordUpdate base updates env
  ETry body returnCase cases -> evalTry body returnCase cases env
  EMatch scrutinees cases -> evalMatchExpr scrutinees cases env
  EBlock ds body -> evalBlock ds body env
  EWithEvidence function evidence -> do
    value <- evalExpr function env
    dictionaries <- traverse (evalEvidence env) evidence
    foldM (applyOne env) value dictionaries

evalRecordField :: String -> RuntimeValue -> Eval RuntimeValue
evalRecordField field = \case
  VRecord fields -> maybe (runtimeError ("unknown record field '" ++ field ++ "'")) pure (Map.lookup field fields)
  value -> runtimeError ("record field access expected record, found " ++ showRuntimeValue value)

evalEvidence :: RuntimeEnv -> Evidence -> Eval RuntimeValue
evalEvidence env = \case
  EvidenceHole _ -> runtimeError "unresolved type-class evidence"
  EvidenceLocal name -> maybe (runtimeError ("missing type-class evidence '" ++ name ++ "'")) pure (lookupValue name env)
  EvidenceFill key nested parents -> do
    arguments <- traverse (evalEvidence env) nested
    parentValues <- traverse (evalEvidence env) parents
    parentDictionaries <- traverse expectDictionary parentValues
    case Map.lookup key (runtimeFills env) of
      Just fill
        | length arguments == length (runtimeFillNeeds fill) -> pure (VDictionary (FillDictionary fill arguments parentDictionaries))
        | otherwise -> runtimeError "type-class evidence arity mismatch"
      Nothing -> case primitiveDictionary key of
        Just dictionary | null arguments -> pure (VDictionary dictionary)
        _ -> runtimeError ("missing selected fill '" ++ key ++ "'")
 where
  expectDictionary (VDictionary dictionary) = pure dictionary
  expectDictionary _ = runtimeError "parent fill evidence is not a dictionary"

evalVar :: String -> RuntimeEnv -> Eval RuntimeValue
evalVar name env = case lookupValue name env of
  Just value -> pure value
  Nothing -> evalFieldAccessName name env

evalFieldAccessName :: String -> RuntimeEnv -> Eval RuntimeValue
evalFieldAccessName name env = case splitFieldAccessName name of
  Just (baseName, fieldName) ->
    evalVar baseName env >>= \case
      VRecord fields -> maybe (runtimeError ("unknown record field '" ++ fieldName ++ "'")) pure (Map.lookup fieldName fields)
      value -> runtimeError ("record field access expected record, found " ++ showRuntimeValue value)
  Nothing -> runtimeError ("unknown name '" ++ name ++ "'")

-- function and arguments are evaluated left to right. 'applyOne' is the sole
-- saturation gate for matchers, constructors, natives, deeds, and continuations.
evalApply :: Expr -> [Expr] -> RuntimeEnv -> Eval RuntimeValue
evalApply f args env = evalExpr f env >>= \fn -> evalApplyArgs fn args env

evalApplyArgs :: RuntimeValue -> [Expr] -> RuntimeEnv -> Eval RuntimeValue
evalApplyArgs fn args env = foldM step fn args
 where
  step currentFn arg = evalExpr arg env >>= applyOne env currentFn

applyOne :: RuntimeEnv -> RuntimeValue -> RuntimeValue -> Eval RuntimeValue
applyOne callEnv fn arg = case fn of
  VMatcher cases matchEnv supplied -> applyMatcher callEnv Nothing cases matchEnv supplied arg
  VRecursiveMatcher name cases matchEnv supplied -> applyMatcher callEnv (Just name) cases matchEnv supplied arg
  VConstructor qualifiedName displayName arity supplied ->
    applyArity arity supplied arg (VConstructor qualifiedName displayName arity) (pure . VData qualifiedName displayName)
  VNative name arity supplied ->
    applyArity arity supplied arg (VNative name arity) (evalNative name)
  VEffectOp effectName opName arity supplied ->
    applyArity arity supplied arg (VEffectOp effectName opName arity) (evalEffectOp effectName opName)
  VContinuation resume -> resume arg
  VShapeMember member -> case arg of
    VDictionary dictionary -> evalDictionaryMember member dictionary
    _ -> runtimeError ("shape member '" ++ member ++ "' received non-dictionary evidence")
  _ -> runtimeError ("cannot apply " ++ showRuntimeValue fn)

evalDictionaryMember :: String -> RuntimeDictionary -> Eval RuntimeValue
evalDictionaryMember member = \case
  PrimitiveDictionary shapeName typeName -> primitiveDictionaryMember shapeName typeName member
  dictionary@(FillDictionary fill requirements parents) -> case fillMemberExpr member fill of
    Just expr -> do
      let evidence = VDictionary dictionary : requirements
          memberEnv = foldl' addEvidence (runtimeFillEnv fill) (zip [(0 :: Int) ..] evidence)
      evalBoundValue ("$fill-member@" ++ member) expr memberEnv
    Nothing -> case find (dictionaryHasMember member) parents of
      Just parent -> evalDictionaryMember member parent
      Nothing -> runtimeError ("selected fill has no member '" ++ member ++ "'")
 where
  addEvidence env (index, value) = addValue ("$evidence" ++ show index) value env

dictionaryHasMember :: String -> RuntimeDictionary -> Bool
dictionaryHasMember member = \case
  PrimitiveDictionary shapeName typeName -> primitiveDictionarySupports shapeName typeName member
  FillDictionary fill _ parents -> isJust (fillMemberExpr member fill) || any (dictionaryHasMember member) parents

fillMemberExpr :: String -> RuntimeFill -> Maybe Expr
fillMemberExpr member RuntimeFill{runtimeFillShape, runtimeFillMembers, runtimeFillEnv} =
  Map.lookup member runtimeFillMembers <|> findDefault runtimeFillShape []
 where
  findDefault shapeName seen
    | shapeName `elem` seen = Nothing
    | otherwise = case findRuntimeShapeEntry shapeName runtimeFillEnv of
        Nothing -> Nothing
        Just (_, ShapeInfo{shapeNeeds, shapeMembers}) ->
          listToMaybe [expr | ShapeDefault name _ expr <- shapeMembers, name == member]
            <|> foldr (<|>) Nothing [findDefault neededName (shapeName : seen) | ShapeNeed _ neededName <- shapeNeeds]

primitiveDictionary :: String -> Maybe RuntimeDictionary
primitiveDictionary key = case stripPrefix "$primitive@" key of
  Nothing -> Nothing
  Just rest -> case break (== '@') rest of
    (shapeName, '@' : typeName) -> Just (PrimitiveDictionary shapeName typeName)
    _ -> Nothing

primitiveDictionaryMember :: String -> String -> String -> Eval RuntimeValue
primitiveDictionaryMember shapeName typeName member = case (shapeName, typeName, member) of
  ("zero", "integer", "zero") -> pure (VInteger 0)
  ("zero", "float", "zero") -> pure (VFloat "0.0")
  ("one", "integer", "one") -> pure (VInteger 1)
  ("one", "float", "one") -> pure (VFloat "1.0")
  _ | member `elem` map fst runtimeNativeSpecs -> pure (VNative member (fromMaybe 0 (lookup member runtimeNativeSpecs)) [])
  _ -> runtimeError ("primitive fill '" ++ shapeName ++ " " ++ typeName ++ "' has no member '" ++ member ++ "'")

primitiveDictionarySupports :: String -> String -> String -> Bool
primitiveDictionarySupports shapeName typeName member = case (shapeName, typeName, member) of
  ("zero", "integer", "zero") -> True
  ("zero", "float", "zero") -> True
  ("one", "integer", "one") -> True
  ("one", "float", "one") -> True
  _ -> member `elem` map fst runtimeNativeSpecs

applyArity :: Int -> [RuntimeValue] -> RuntimeValue -> ([RuntimeValue] -> RuntimeValue) -> ([RuntimeValue] -> Eval RuntimeValue) -> Eval RuntimeValue
applyArity arity supplied arg partial full =
  let supplied2 = supplied ++ [arg]
   in case compare (length supplied2) arity of
        LT -> pure (partial supplied2)
        EQ -> full supplied2
        GT -> runtimeError "too many arguments"

applyMatcher :: RuntimeEnv -> Maybe String -> [MatchCase] -> RuntimeEnv -> [RuntimeValue] -> RuntimeValue -> Eval RuntimeValue
applyMatcher callEnv maybeName cases matchEnv supplied arg =
  let supplied2 = supplied ++ [arg]
      visibleEnv = mergeRuntimeEnv matchEnv callEnv
      selfEnv = case maybeName of
        Nothing -> visibleEnv
        Just name -> addValue name (VRecursiveMatcher name cases matchEnv []) visibleEnv
      nextMatcher = case maybeName of
        Nothing -> VMatcher cases matchEnv supplied2
        Just name -> VRecursiveMatcher name cases matchEnv supplied2
      arity = length patterns
   in case compare (length supplied2) arity of
        LT -> pure nextMatcher
        EQ -> evalMatchCases cases supplied2 selfEnv
        GT -> runtimeError "too many arguments for match"
 where
  patterns = case cases of
    MatchCase ps _ : _ -> NE.toList ps
    [] -> []

evalRecord :: [(String, Expr)] -> RuntimeEnv -> Map.Map String RuntimeValue -> Eval RuntimeValue
evalRecord fields env acc = VRecord <$> foldM step acc fields
 where
  step record (name, expr) = Map.insert name <$> evalExpr expr env <*> pure record

evalRecordUpdate :: Expr -> [RecordUpdate] -> RuntimeEnv -> Eval RuntimeValue
evalRecordUpdate base updates env =
  evalExpr base env >>= \case
    VRecord fields -> evalRecordUpdates updates fields env
    value -> runtimeError ("record update expected record, found " ++ showRuntimeValue value)

evalRecordUpdates :: [RecordUpdate] -> Map.Map String RuntimeValue -> RuntimeEnv -> Eval RuntimeValue
evalRecordUpdates updates fields env = VRecord <$> foldM step fields updates
 where
  step record = \case
    RecordRemove name ->
      if Map.member name record
        then pure (Map.delete name record)
        else runtimeError ("unknown record field '" ++ name ++ "'")
    RecordSet name expr -> Map.insert name <$> evalExpr expr env <*> pure record

-- handlers are deep because an escaping operation is rewrapped with 'handleEval'.
-- the captured continuation is an ordinary reusable runtime value.
evalTry :: Expr -> Maybe ReturnCase -> [HandlerCase] -> RuntimeEnv -> Eval RuntimeValue
evalTry body returnCase cases env = handleEval (evalExpr body env)
 where
  handleEval action = Eval $ \host -> runEval action host >>= \result -> runEval (handleRuntimeResult result) host
  handleRuntimeResult result = case result of
    RuntimeOk value -> evalReturnCase returnCase value env
    RuntimeErr err -> raiseRuntime err
    RuntimeOp effectName opName args resume -> case findHandler effectName opName cases of
      Nothing -> Eval (const (pure (RuntimeOp effectName opName args (handleEval . resume))))
      Just handlerCase -> evalHandlerCase handlerCase args (handleEval . resume) env

evalReturnCase :: Maybe ReturnCase -> RuntimeValue -> RuntimeEnv -> Eval RuntimeValue
evalReturnCase Nothing value _ = pure value
evalReturnCase (Just (ReturnCase pat body)) value env =
  case bindRuntimePattern pat value env of
    PatternNoMatch -> runtimeError "handler return value did not match pattern"
    PatternMatched handlerEnv -> evalExpr body handlerEnv

findHandler :: String -> String -> [HandlerCase] -> Maybe HandlerCase
findHandler effectName opName = find (\(HandlerCase name _ _) -> handlerMatches name effectName opName)

handlerMatches :: String -> String -> String -> Bool
handlerMatches name effectName opName =
  name `elem` [opName, effectName, lastQualifiedSegment effectName, effectName ++ "@" ++ opName]

evalHandlerCase :: HandlerCase -> [RuntimeValue] -> (RuntimeValue -> Eval RuntimeValue) -> RuntimeEnv -> Eval RuntimeValue
evalHandlerCase (HandlerCase _ patterns body) args resume env =
  case bindHandlerRuntimePatterns patterns args env of
    PatternNoMatch -> runtimeError "handler operation arguments did not match pattern"
    PatternMatched handlerEnv -> evalExpr body (addValue "resume" (VContinuation resume) handlerEnv)

bindHandlerRuntimePatterns :: [Pattern] -> [RuntimeValue] -> RuntimeEnv -> PatternBind
bindHandlerRuntimePatterns [] _ env = PatternMatched env
bindHandlerRuntimePatterns patterns values env = bindRuntimePatterns patterns values env

evalMatchExpr :: [Expr] -> [MatchCase] -> RuntimeEnv -> Eval RuntimeValue
evalMatchExpr [] cases env = pure (VMatcher cases env [])
evalMatchExpr scrutinees cases env = evalExprList scrutinees env >>= \values -> evalMatchCases cases values env

evalExprList :: [Expr] -> RuntimeEnv -> Eval [RuntimeValue]
evalExprList exprs env = traverse (`evalExpr` env) exprs

evalMatchCases :: [MatchCase] -> [RuntimeValue] -> RuntimeEnv -> Eval RuntimeValue
evalMatchCases cases values env = foldr step noMatch cases
 where
  -- coverage checking should make this defensive branch unreachable for typed data.
  noMatch = runtimeError ("non-exhaustive match " ++ show (map (NE.toList . matchPatterns) cases) ++ " on " ++ showRuntimeValues values)
  matchPatterns (MatchCase patterns _) = patterns
  step (MatchCase patterns body) fallback =
    case bindRuntimePatterns (NE.toList patterns) values env of
      PatternNoMatch -> fallback
      PatternMatched boundEnv -> evalExpr body boundEnv

evalBlock :: [Decl] -> Expr -> RuntimeEnv -> Eval RuntimeValue
evalBlock ds body env = evalLocalDecls ds env >>= \env2 -> evalExpr body env2

evalLocalDecls :: [Decl] -> RuntimeEnv -> Eval RuntimeEnv
evalLocalDecls ds env = foldM step env ds
 where
  step currentEnv (Let name _ expr) = addValue name <$> evalExpr expr currentEnv <*> pure currentEnv
  step currentEnv _ = pure currentEnv

bindRuntimePatterns :: [Pattern] -> [RuntimeValue] -> RuntimeEnv -> PatternBind
bindRuntimePatterns patterns values env
  | length patterns /= length values = PatternNoMatch
  | otherwise = foldl' step (PatternMatched env) (zip patterns values)
 where
  step PatternNoMatch _ = PatternNoMatch
  step (PatternMatched currentEnv) (pat, value) = bindRuntimePattern pat value currentEnv

bindRuntimePattern :: Pattern -> RuntimeValue -> RuntimeEnv -> PatternBind
bindRuntimePattern (PVar "_") _ env = PatternMatched env
bindRuntimePattern (PVar name) value env =
  if isNullaryConstructorPattern name env
    then if runtimeConstructorMatches env name value then PatternMatched env else PatternNoMatch
    else PatternMatched (addValue name value env)
bindRuntimePattern (PCon name args) value env = case value of
  VData _ _ fields | runtimeConstructorMatches env name value -> bindRuntimePatterns args fields env
  _ -> PatternNoMatch

isNullaryConstructorPattern :: String -> RuntimeEnv -> Bool
isNullaryConstructorPattern name env = case lookupValue name env of
  Just value@(VData _ _ []) -> runtimeConstructorMatches env name value
  _ -> False

runtimeConstructorMatches :: RuntimeEnv -> String -> RuntimeValue -> Bool
runtimeConstructorMatches env patternName value = case value of
  VData qualifiedName displayName _ ->
    if isQualifiedName patternName
      then maybe (patternName == qualifiedName) (== qualifiedName) (runtimeConstructorName =<< lookupValue patternName env)
      else patternName == displayName
  _ -> False

runtimeConstructorName :: RuntimeValue -> Maybe String
runtimeConstructorName = \case
  VData qualifiedName _ _ -> Just qualifiedName
  VConstructor qualifiedName _ _ _ -> Just qualifiedName
  _ -> Nothing

-- these two dispatch tables are the host boundary. their names and arities must
-- stay aligned with 'runtimeNativeSpecs' and 'runtimeEffectOpSpecs'.
evalNative :: String -> [RuntimeValue] -> Eval RuntimeValue
evalNative name args = case (name, args) of
  ("add-integer", [VInteger a, VInteger b]) -> pure (VInteger (a + b))
  ("subtract-integer", [VInteger a, VInteger b]) -> pure (VInteger (a - b))
  ("multiply-integer", [VInteger a, VInteger b]) -> pure (VInteger (a * b))
  ("divide-integer", [VInteger a, VInteger b]) -> evalIntegerDivide a b
  ("add-float", [VFloat a, VFloat b]) -> evalFloatBinary (+) a b
  ("subtract-float", [VFloat a, VFloat b]) -> evalFloatBinary (-) a b
  ("multiply-float", [VFloat a, VFloat b]) -> evalFloatBinary (*) a b
  ("divide-float", [VFloat a, VFloat b]) -> evalFloatDivide a b
  ("exponent-float", [VFloat a]) -> evalFloatUnary a (show . exp)
  ("sine-float", [VFloat a]) -> evalFloatUnary a (show . sin)
  ("arctan-float", [VFloat a, VFloat b]) -> evalFloatBinary arctanFloat a b
  ("logarithm-float", [VFloat a]) -> evalFloatUnary a (show . log)
  ("exponent", [VFloat a]) -> evalFloatUnary a (show . exp)
  ("sine", [VFloat a]) -> evalFloatUnary a (show . sin)
  ("cosine", [VFloat a]) -> evalFloatUnary a (show . cos)
  ("logarithm", [VFloat a]) -> evalFloatUnary a (show . log)
  ("⌊", [VFloat a]) -> evalFloatFloor a
  ("integer-to-string", [VInteger a]) -> pure (VText (show a))
  ("float-to-string", [VFloat a]) -> pure (VText a)
  ("equal-integer", [VInteger a, VInteger b]) -> pure (runtimeBool (a == b))
  ("+", [VInteger a, VInteger b]) -> pure (VInteger (a + b))
  ("+", [VFloat a, VFloat b]) -> evalFloatBinary (+) a b
  ("-", [VInteger a, VInteger b]) -> pure (VInteger (a - b))
  ("-", [VFloat a, VFloat b]) -> evalFloatBinary (-) a b
  ("×", [VInteger a, VInteger b]) -> pure (VInteger (a * b))
  ("×", [VFloat a, VFloat b]) -> evalFloatBinary (*) a b
  ("÷", [VInteger a, VInteger b]) -> evalIntegerDivide a b
  ("÷", [VFloat a, VFloat b]) -> evalFloatDivide a b
  ("≡", [a, b]) -> pure (runtimeBool (a == b))
  ("≤", [VInteger a, VInteger b]) -> pure (runtimeBool (a <= b))
  ("≤", [VFloat a, VFloat b]) -> evalFloatLessEqual a b
  ("from-string", [VText text]) -> floatFromString text
  ("to-string", [value]) -> pure (VText (showRuntimeValue value))
  _ -> runtimeError ("native '" ++ name ++ "' does not support these arguments")

evalEffectOp :: String -> String -> [RuntimeValue] -> Eval RuntimeValue
evalEffectOp effectName opName args = case (effectName, opName, args) of
  ("console", "write", [VText text]) -> writeConsole text
  ("console", "read", [VData _ "null" []]) -> VText <$> liftIO readConsoleLine
  ("random", "random", [VData _ "null" []]) -> VFloat . show <$> liftIO nextRandomFloat
  ("async", "sleep", [VInteger ms]) -> sleepMilliseconds ms
  ("async", "fork", [work]) -> forkSync work
  ("async", "wait", [VData _ "done" [value]]) -> pure value
  ("file", "read-file", [VText path]) -> fileRuntime (VText <$> IO.readFile path)
  ("file", "write-file", [VText path, VText text]) -> fileRuntime (IO.writeFile path text >> pure runtimeNull)
  ("file", "append-file", [VText path, VText text]) -> fileRuntime (IO.appendFile path text >> pure runtimeNull)
  ("system", "arguments", [VData _ "null" []]) -> runtimeList . map VText <$> readHostArguments
  ("system", "environment", [VText name]) -> runtimeOption . fmap VText <$> liftIO (Environment.lookupEnv name)
  ("system", "exit", [VInteger status]) -> liftIO (exitWith (if status == 0 then ExitSuccess else ExitFailure status))
  ("clock", "unix-time", [VData _ "null" []]) -> VInteger . floor <$> liftIO getPOSIXTime
  _ -> performRuntime effectName opName args

readHostArguments :: Eval [String]
readHostArguments = Eval (pure . RuntimeOk . hostArguments)

failRuntime :: String -> Eval RuntimeValue
failRuntime message = performRuntime "fail" "fail" [VText message]

fileRuntime :: IO RuntimeValue -> Eval RuntimeValue
fileRuntime action = do
  result <- liftIO (try action)
  case (result :: Either IOException RuntimeValue) of
    Left err -> failRuntime (show err)
    Right value -> pure value

writeConsole :: String -> Eval RuntimeValue
writeConsole text = liftIO (putStr text >> hFlush stdout) >> pure runtimeNull

readConsoleLine :: IO String
readConsoleLine = hFlush stdout >> getLine

sleepMilliseconds :: Int -> Eval RuntimeValue
sleepMilliseconds ms = liftIO (threadDelay (max 0 ms * 1000)) >> pure runtimeNull

forkSync :: RuntimeValue -> Eval RuntimeValue
forkSync work = runtimeDone <$> applyOne baseRuntimeEnv work runtimeNull

runtimeNull :: RuntimeValue
runtimeNull = VData "𝟙@null" "null" []

runtimeDone :: RuntimeValue -> RuntimeValue
runtimeDone value = VData "task@done" "done" [value]

runtimeList :: [RuntimeValue] -> RuntimeValue
runtimeList = foldr (\value rest -> VData "data/list@list@.*" ".*" [value, rest]) (VData "data/list@list@empty" "empty" [])

runtimeOption :: Maybe RuntimeValue -> RuntimeValue
runtimeOption = maybe (VData "data/option@option@none" "none" []) (\value -> VData "data/option@option@some" "some" [value])

nextRandomFloat :: IO Double
nextRandomFloat = do
  t <- getPOSIXTime
  pure (realToFrac (t - fromIntegral (floor t :: Integer)))

evalFloatUnary :: String -> (Double -> String) -> Eval RuntimeValue
evalFloatUnary text f = case readMaybe text of
  Nothing -> runtimeError ("invalid float '" ++ text ++ "'")
  Just x -> pure (VFloat (f x))

evalFloatBinary :: (Double -> Double -> Double) -> String -> String -> Eval RuntimeValue
evalFloatBinary f left right = case (readMaybe left, readMaybe right) of
  (Just x, Just y) -> pure (VFloat (show (f x y)))
  _ -> runtimeError "invalid float"

arctanFloat :: Double -> Double -> Double
arctanFloat real imaginary =
  let angle = atan2 imaginary real
      tau = 2 * pi
   in if angle < 0 then angle + tau else angle

evalIntegerDivide :: Int -> Int -> Eval RuntimeValue
evalIntegerDivide _ 0 = failRuntime "division by zero"
evalIntegerDivide a b = pure (VInteger (a `div` b))

evalFloatLessEqual :: String -> String -> Eval RuntimeValue
evalFloatLessEqual left right = case (readMaybe left :: Maybe Double, readMaybe right :: Maybe Double) of
  (Just x, Just y) -> pure (runtimeBool (x <= y))
  _ -> runtimeError "invalid float"

evalFloatDivide :: String -> String -> Eval RuntimeValue
evalFloatDivide left right = case (readMaybe left, readMaybe right) of
  (_, Just 0.0) -> failRuntime "division by zero"
  (Just x, Just y) -> pure (VFloat (show (x / (y :: Double))))
  _ -> runtimeError "invalid float"

evalFloatFloor :: String -> Eval RuntimeValue
evalFloatFloor text = case readMaybe text of
  Just x -> pure (VInteger (floor (x :: Double)))
  Nothing -> runtimeError ("invalid float '" ++ text ++ "'")

floatFromString :: String -> Eval RuntimeValue
floatFromString text = case (readMaybe text :: Maybe Double) of
  Just x -> pure (VFloat (show x))
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
        qualifiedName = effectName ++ "@" ++ name
     in addValue name value (addValue qualifiedName value current)

addForeignMembers :: [ForeignMember] -> RuntimeEnv -> RuntimeEnv
addForeignMembers members env = foldl' step env members
 where
  step current (ForeignMember name (TypeAnn t _)) = addValue name (foreignRuntimeValue name (typeArity t)) current

foreignRuntimeValue :: String -> Int -> RuntimeValue
foreignRuntimeValue name arity = VNative name arity []

typeArity :: TypeExpr -> Int
typeArity (TypeArrow args _ _) = NE.length args
typeArity _ = 0

-- base bindings are implementation capabilities, not implicit source imports;
-- ordinary bookhoard files still decide which names are shown to users.
baseRuntimeEnv :: RuntimeEnv
baseRuntimeEnv = addBaseEffectOps (addBaseNatives RuntimeEnv{runtimeValues = [], runtimeLookup = Map.empty, runtimeShapes = Map.empty, runtimeFills = Map.empty})

isBaseRuntimeName :: String -> Bool
isBaseRuntimeName name = name `elem` baseRuntimeNames

isBaseRuntimeImportName :: String -> Bool
isBaseRuntimeImportName name = isBaseRuntimeName name || isBaseRuntimeName (lastQualifiedSegment name)

isBaseRuntimeImportBinding :: String -> String -> Bool
isBaseRuntimeImportBinding name origin = isBaseRuntimeImportName name && origin == "base@" ++ name

isBaseRuntimeEffectName :: String -> Bool
isBaseRuntimeEffectName name = name `elem` baseEffectNames

addBaseNatives :: RuntimeEnv -> RuntimeEnv
addBaseNatives env = foldl' (\current (name, arity) -> addNative name arity current) env runtimeNativeSpecs

addBaseEffectOps :: RuntimeEnv -> RuntimeEnv
addBaseEffectOps env = foldl' (\current (effectName, opName, arity) -> addBaseEffectOp effectName opName arity current) env runtimeEffectOpSpecs

addNative :: String -> Int -> RuntimeEnv -> RuntimeEnv
addNative name arity = addValueWith name (VNative name arity []) baseRuntimeRank ("base@" ++ name)

addBaseEffectOp :: String -> String -> Int -> RuntimeEnv -> RuntimeEnv
addBaseEffectOp effectName opName arity = addValueWith opName (VEffectOp effectName opName arity []) baseRuntimeRank ("base@" ++ opName)

addValue :: String -> RuntimeValue -> RuntimeEnv -> RuntimeEnv
addValue name value = addValueWith name value localRuntimeRank name

addValueWith :: String -> RuntimeValue -> Int -> String -> RuntimeEnv -> RuntimeEnv
addValueWith name value rank origin env@RuntimeEnv{runtimeValues, runtimeLookup} =
  env
    { runtimeValues = (name, value, rank, origin) : runtimeValues
    , runtimeLookup = Map.insert name value runtimeLookup
    }

addShownValue :: String -> RuntimeEnv -> RuntimeEnv
addShownValue name env =
  let exportName = lastQualifiedSegment name
   in maybe env (\value -> addValueWith exportName value exportRuntimeRank ("show@" ++ exportName) env) (lookupValue name env)

lookupValue :: String -> RuntimeEnv -> Maybe RuntimeValue
lookupValue name = Map.lookup name . runtimeLookup

indexRuntimeValues :: [RuntimeBinding] -> Map.Map String RuntimeValue
indexRuntimeValues = foldr (\(name, value, _, _) -> Map.insert name value) Map.empty

markRuntimeDeclExport :: Decl -> RuntimeEnv -> RuntimeEnv
markRuntimeDeclExport declaration env = case declaration of
  Import _ -> env
  Export nested -> markRuntimeDeclExport nested env
  ReExport name -> addShownValue name env
  ReExportType _ -> env
  Let name _ _ -> addShownValue name env
  TypeAlias _ _ -> env
  DataDecl _ _ constructors -> foldl' (flip addShownValue) env [name | Ctor name _ <- constructors]
  EffectDecl _ _ operations -> foldl' (flip addShownValue) env [name | EffectOp name _ <- operations]
  ForeignDecl members -> foldl' (flip addShownValue) env [name | ForeignMember name _ <- members]
  ShapeDecl _ shapeName _ members ->
    foldl' (flip addShownValue) (markRuntimeShapeExported shapeName env) (shapeMemberNames members)
  FillDecl{} -> env
  ElaboratedFill{} -> env

findRuntimeShapeEntry :: String -> RuntimeEnv -> Maybe (String, ShapeInfo)
findRuntimeShapeEntry name RuntimeEnv{runtimeShapes} = case Map.lookup name runtimeShapes of
  Just info -> Just (name, info)
  Nothing -> listToMaybe [(key, info) | (key, info) <- Map.toList runtimeShapes, lastQualifiedSegment key == lastQualifiedSegment name]

markRuntimeShapeExported :: String -> RuntimeEnv -> RuntimeEnv
markRuntimeShapeExported name env@RuntimeEnv{runtimeShapes} = case findRuntimeShapeEntry name env of
  Nothing -> env
  Just (key, info) ->
    let marked = env{runtimeShapes = Map.insert (lastQualifiedSegment name) info{shapeExported = True} runtimeShapes}
     in foldl' (\current member -> addShownValue (runtimeOwnedBinding key member) current) marked (shapeMemberNames (shapeMembers info))

runtimeOwnedBinding :: String -> String -> String
runtimeOwnedBinding owner member = case splitFieldAccessName owner of
  Just (qualifier, _) -> qualifier ++ "@" ++ member
  Nothing -> owner ++ "@" ++ member

addRuntimeShapeInfo :: String -> [String] -> [ShapeNeed] -> [ShapeMember] -> RuntimeEnv -> RuntimeEnv
addRuntimeShapeInfo shapeName params needs members env@RuntimeEnv{runtimeShapes} =
  env{runtimeShapes = Map.insert shapeName (ShapeInfo params needs members False) runtimeShapes}

showRuntimeError :: RuntimeError -> String
showRuntimeError (RuntimeError msg) = msg

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
  VMatcher{} -> "<function>"
  VRecursiveMatcher{} -> "<function>"
  VConstructor _ displayName _ _ -> "<constructor " ++ displayName ++ ">"
  VNative name _ _ -> "<native " ++ name ++ ">"
  VEffectOp _ opName _ _ -> "<effect " ++ opName ++ ">"
  VContinuation _ -> "<continuation>"
  VShapeMember name -> "<shape member " ++ name ++ ">"
  VDictionary{} -> "<dictionary>"

showRuntimeValues :: [RuntimeValue] -> String
showRuntimeValues = unwords . map showRuntimeValue

showRuntimeFields :: Map.Map String RuntimeValue -> String
showRuntimeFields fields = intercalate ", " [name ++ " = " ++ showRuntimeValue value | (name, value) <- Map.toList fields]
