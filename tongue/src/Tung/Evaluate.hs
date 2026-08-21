{- | strict call-by-value evaluation of checked 'CoreProgram' values.
applications evaluate function then arguments left-to-right; handlers are deep
and their captured continuations are reusable.
-}
module Tung.Evaluate (
  evaluate,
  evaluateWithImports,
  evaluateWithArgsAndImports,
  evaluateCoreProgram,
  evaluateMainCoreProgram,
  evaluateMainCoreProgramWithArgs,
  evaluateMainWithImports,
  evaluateMainWithArgsAndImports,
)
where

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, ThreadId, forkIO, getNumCapabilities, killThread, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (AsyncException (ThreadKilled), IOException, SomeException, displayException, finally, fromException, mask, try)
import Control.Monad (ap, foldM, guard, unless, when, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isControl, ord)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (find, intercalate, isPrefixOf, stripPrefix)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment qualified as Environment
import System.Exit (ExitCode (..), exitWith)
import System.IO (hFlush, stdout)
import System.Process qualified as Process
import System.Timeout qualified as Timeout
import Text.Read (readMaybe)
import Tung.Core (CoreProgram, coreDeclarations, coreImportDeclarations)
import Tung.Import (ImportStack, enterImport)
import Tung.Name (importNamespace, isQualifiedName, lastQualifiedSegment, splitFieldAccessName)
import Tung.Parse (parse)
import Tung.Syntax
import Tung.Token (unicodeScalar)
import Tung.Type

-- callable values retain supplied arguments, making partial application a pure
-- value. natives and effect operations run only when 'applyArity' is saturated.
data RuntimeValue
  = VUnit
  | VInteger Integer
  | VFloat Double
  | VUnicode Char
  | VText Text.Text
  | VRecord (Map.Map String RuntimeValue)
  | VMatcher Int [RuntimeMatchCase] RuntimeEnv [RuntimeValue]
  | VConstructor String String Int [RuntimeValue]
  | VData String String [RuntimeValue]
  | VNative String Int [RuntimeValue]
  | VEffectOp String String Int [RuntimeValue]
  | VTask RuntimeTask
  | VContinuation (RuntimeValue -> Eval RuntimeValue)
  | VShapeMember String
  | VDictionary RuntimeDictionary

type RuntimeExpr = RuntimeEnv -> Eval RuntimeValue

data RuntimeMatchCase = RuntimeMatchCase (NE.NonEmpty Pattern) RuntimeExpr

-- primitive dictionaries retain the declaration environment so native fills
-- can use ordinary bookhoard defaults and inherited members.
data RuntimeDictionary
  = FillDictionary RuntimeFill [RuntimeValue] [RuntimeDictionary]
  | PrimitiveDictionary String String RuntimeEnv [RuntimeDictionary]

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
  VUnicode a == VUnicode b = a == b
  VText a == VText b = a == b
  VRecord xs == VRecord ys = xs == ys
  VData xName _ xArgs == VData yName _ yArgs = xName == yName && xArgs == yArgs
  _ == _ = False

type RuntimeBinding = (String, RuntimeValue, Int, String)

data RuntimeEnv = RuntimeEnv
  { runtimeValues :: [RuntimeBinding]
  , runtimeLocals :: [(String, RuntimeValue)]
  , runtimeLookup :: Map.Map String RuntimeValue
  , runtimeShapes :: Map.Map String ShapeInfo
  , runtimeFills :: Map.Map String RuntimeFill
  }

newtype RuntimeError = RuntimeError String

-- an operation carrieth its explicit continuation. the 'Eval' monad composeth
-- through that continuation, which giveth handlers deep, reusable resumption.
data RuntimeResult a = RuntimeOk a | RuntimeErr RuntimeError | RuntimeOp String String [RuntimeValue] (RuntimeValue -> Eval a)

data PatternBind = PatternMatched RuntimeEnv | PatternNoMatch

data Eval a
  = EvalPure (RuntimeResult a)
  | Eval (RuntimeHost -> IO (RuntimeResult a))

runEval :: Eval a -> RuntimeHost -> IO (RuntimeResult a)
runEval (EvalPure result) _ = pure result
runEval (Eval action) host = action host

data RuntimeTask = RuntimeTask
  { runtimeTaskId :: !Int
  , runtimeTaskThread :: MVar (Maybe ThreadId)
  , runtimeTaskResult :: MVar TaskResult
  }

data TaskResult
  = TaskFinished (RuntimeResult RuntimeValue)
  | TaskCancelled
  | TaskCrashed String

data RuntimeHost = RuntimeHost
  { hostArguments :: [String]
  , hostImportCache :: IORef RuntimeCache
  , hostTaskSlots :: TVar Int
  , hostNextTask :: TVar Int
  , hostActiveTasks :: TVar (Map.Map Int RuntimeTask)
  }

type RuntimeCache = Map.Map String RuntimeEnv

instance Functor Eval where
  fmap f action = action >>= pure . f

instance Applicative Eval where
  pure = EvalPure . RuntimeOk
  (<*>) = ap

instance Monad Eval where
  EvalPure result >>= next = bindRuntimeResult result next
  Eval action >>= next =
    Eval $ \host ->
      action host >>= \case
        RuntimeOk value -> runEval (next value) host
        RuntimeErr err -> pure (RuntimeErr err)
        RuntimeOp effectName opName args resume ->
          pure (RuntimeOp effectName opName args (resume >=> next))

bindRuntimeResult :: RuntimeResult a -> (a -> Eval b) -> Eval b
bindRuntimeResult result next = case result of
  RuntimeOk value -> next value
  RuntimeErr err -> EvalPure (RuntimeErr err)
  RuntimeOp effectName opName args resume -> EvalPure (RuntimeOp effectName opName args (resume >=> next))

instance MonadIO Eval where
  liftIO action = Eval (const (RuntimeOk <$> action))

raiseRuntime :: RuntimeError -> Eval a
raiseRuntime = EvalPure . RuntimeErr

runtimeError :: String -> Eval a
runtimeError = raiseRuntime . RuntimeError

performRuntime :: String -> String -> [RuntimeValue] -> Eval RuntimeValue
performRuntime effectName opName args = EvalPure (RuntimeOp effectName opName args pure)

evaluate :: String -> IO String
evaluate source = evaluateWithImports source Map.empty

evaluateWithImports :: String -> Map.Map String String -> IO String
evaluateWithImports = evaluateWithArgsAndImports []

evaluateWithArgsAndImports :: [String] -> String -> Map.Map String String -> IO String
evaluateWithArgsAndImports args source imports = evaluateParsedWith False args source imports evalProgram

evaluateCoreProgram :: CoreProgram -> IO String
evaluateCoreProgram program = runCoreProgram [] program evalProgram

evaluateMainCoreProgram :: CoreProgram -> IO String
evaluateMainCoreProgram = evaluateMainCoreProgramWithArgs []

evaluateMainCoreProgramWithArgs :: [String] -> CoreProgram -> IO String
evaluateMainCoreProgramWithArgs arguments program = runCoreProgram arguments program runMain

evaluateMainWithImports :: String -> Map.Map String String -> IO String
evaluateMainWithImports = evaluateMainWithArgsAndImports []

evaluateMainWithArgsAndImports :: [String] -> String -> Map.Map String String -> IO String
evaluateMainWithArgsAndImports args source imports = evaluateParsedWith True args source imports runMain

-- evaluation never accepteth surface syntax directly: this function first asketh
-- the type checker for a mode-appropriate 'CoreProgram'.
evaluateParsedWith :: Bool -> [String] -> String -> Map.Map String String -> (CoreProgram -> Eval RuntimeValue) -> IO String
evaluateParsedWith runnable arguments source imports action = case parse source of
  Left msg -> pure ("parse error: " ++ msg)
  Right parsed -> case elaborate parsed of
    Left msg -> pure ((if runnable then "type error: " else "eval error: type error: ") ++ msg)
    Right program -> runCoreProgram arguments program action
 where
  -- interactive evaluation alloweth top-level computation, but still checketh and
  -- elaborateth it. runnable evaluation additionally checketh the main boundary.
  elaborate program
    | runnable = elaborateProgramWithImports program imports True
    | otherwise = elaborateInteractiveProgramWithImports program imports

runCoreProgram :: [String] -> CoreProgram -> (CoreProgram -> Eval RuntimeValue) -> IO String
runCoreProgram arguments program action = do
  host <- newRuntimeHost arguments
  result <- runEval (action program) host `finally` cancelActiveTasks host
  pure $ case result of
    RuntimeOk value -> "eval ok: " ++ showRuntimeValue value
    RuntimeErr err -> "eval error: " ++ showRuntimeError err
    RuntimeOp effectName opName _ _ -> "eval error: " ++ showUnhandledOperation effectName opName

newRuntimeHost :: [String] -> IO RuntimeHost
newRuntimeHost arguments = do
  capabilities <- getNumCapabilities
  hostImportCache <- newIORef Map.empty
  hostTaskSlots <- newTVarIO (max 2 capabilities)
  hostNextTask <- newTVarIO 0
  hostActiveTasks <- newTVarIO Map.empty
  pure RuntimeHost{hostArguments = arguments, ..}

cancelActiveTasks :: RuntimeHost -> IO ()
cancelActiveTasks host@RuntimeHost{hostActiveTasks} = do
  tasks <- Map.elems <$> readTVarIO hostActiveTasks
  unless (null tasks) do
    mapM_ cancelTaskIO tasks
    mapM_ (readMVar . runtimeTaskResult) tasks
    cancelActiveTasks host

runMain :: CoreProgram -> Eval RuntimeValue
runMain program = do
  (env, _) <- evalProgramEnv [] program (coreDeclarations program)
  mainValue <- maybe (runtimeError "runnable file must declare main") pure (lookupValue "main" env)
  applyOne mainValue runtimeNull

evalProgram :: CoreProgram -> Eval RuntimeValue
evalProgram program = snd <$> evalProgramEnv [] program (coreDeclarations program)

evalImportedProgram :: ImportStack -> CoreProgram -> [Decl] -> Eval RuntimeEnv
evalImportedProgram importStack program declarations = fst <$> evalProgramEnv importStack program declarations

evalProgramEnv :: ImportStack -> CoreProgram -> [Decl] -> Eval (RuntimeEnv, RuntimeValue)
evalProgramEnv importStack program declarations = evalDeclsWithImports importStack program declarations baseRuntimeEnv VUnit

evalDeclsWithImports :: ImportStack -> CoreProgram -> [Decl] -> RuntimeEnv -> RuntimeValue -> Eval (RuntimeEnv, RuntimeValue)
evalDeclsWithImports importStack program ds env lastValue = foldM step (env, lastValue) ds
 where
  step (currentEnv, currentLast) = \case
    Import path -> do
      importedEnv <- evalImport importStack path program
      let env2 = mergeRuntimeEnv (namespaceRuntimeImportEnv (importNamespace path) importedEnv) currentEnv
      pure (env2, currentLast)
    d -> do
      (env2, value) <- evalDecl d currentEnv
      pure (env2, fromMaybe currentLast value)

-- runtime import caching mirrors static imports, but stores evaluated exported
-- environments so repeated brings do not repeat module initialisation.
evalImport :: ImportStack -> String -> CoreProgram -> Eval RuntimeEnv
evalImport importStack path program = case enterImport importStack path of
  Left message -> runtimeError message
  Right nextStack ->
    lookupRuntimeImport path >>= \case
      Just env -> pure env
      Nothing -> case coreImportDeclarations path program of
        Nothing -> runtimeError ("internal missing checked bring '" ++ path ++ "'")
        Just declarations -> do
          env <- evalImportedProgram nextStack program declarations
          cacheRuntimeImport path env
          pure env

lookupRuntimeImport :: String -> Eval (Maybe RuntimeEnv)
lookupRuntimeImport path = Eval $ \RuntimeHost{hostImportCache} -> RuntimeOk . Map.lookup path <$> readIORef hostImportCache

cacheRuntimeImport :: String -> RuntimeEnv -> Eval ()
cacheRuntimeImport path env = Eval $ \RuntimeHost{hostImportCache} -> do
  atomicModifyIORef' hostImportCache (\cache -> (Map.insert path env cache, ()))
  pure (RuntimeOk ())

namespaceRuntimeImportEnv :: String -> RuntimeEnv -> RuntimeEnv
namespaceRuntimeImportEnv ns RuntimeEnv{..} =
  let values = namespaceRuntimeImportValues ns runtimeValues
   in RuntimeEnv
        { runtimeValues = values
        , runtimeLocals = []
        , runtimeLookup = indexRuntimeValues values
        , runtimeShapes = namespaceImportShapes ns runtimeShapes
        , runtimeFills = Map.mapKeys (\key -> ns ++ "@" ++ key) runtimeFills
        }

namespaceRuntimeImportValues :: String -> [RuntimeBinding] -> [RuntimeBinding]
namespaceRuntimeImportValues ns = concatMap one
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
    , runtimeLocals = runtimeLocals left ++ runtimeLocals right
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
  Import path -> runtimeError ("evaluation doth not support bring '" ++ path ++ "'")
  Export declaration -> do
    (env2, value) <- evalDecl declaration env
    pure (markRuntimeDeclExport declaration env2, value)
  ReExport name -> pure (addShownValue name env, Nothing)
  ReExportType _ -> pure (env, Nothing)
  TypeAlias _ _ _ -> pure (env, Nothing)
  DataDecl _ name ctors -> pure (addConstructors name ctors env, Nothing)
  EffectDecl _ name ops -> pure (addEffectOps name ops env, Nothing)
  ShapeDecl params shapeName needs members -> pure (addShapeSelectors shapeName members (addRuntimeShapeInfo shapeName params needs members env), Nothing)
  FillDecl{} -> runtimeError "internal unelaborated fill"
  ElaboratedFill key _ shapeName needs members -> pure (addSelectedRuntimeFill key shapeName needs members env, Nothing)
  Let name (Just (TypeAnn functionType _)) EForeign ->
    pure (addValue name (VNative name (typeArity functionType) []) env, Nothing)
  Let name _ expr -> do
    value <- evalBoundValue name expr env
    let lastValue = if name == "_" then Just value else Nothing
    pure (addValue name value env, lastValue)

evalBoundValue :: String -> Expr -> RuntimeEnv -> Eval RuntimeValue
evalBoundValue name expr env = case unlocatedMatch expr of
  Just cases ->
    let value = newMatcher (compileMatchCases (Just env) cases) (addLocalValue name value env)
     in pure value
  Nothing -> compileExpr expr env

addShapeSelectors :: String -> [ShapeMember] -> RuntimeEnv -> RuntimeEnv
addShapeSelectors shapeName members env = foldl' add env (shapeMemberNames members)
 where
  add current name = addValue name selector (addValue (shapeName ++ "@" ++ name) selector current)
   where
    selector = VShapeMember name

addSelectedRuntimeFill :: String -> String -> [ShapeNeed] -> [Decl] -> RuntimeEnv -> RuntimeEnv
addSelectedRuntimeFill key shapeName needs members env@RuntimeEnv{runtimeFills} =
  fillEnv
 where
  -- a member may select this dictionary while calling another member.
  fillEnv = env{runtimeFills = Map.insert key template runtimeFills}
  template = RuntimeFill shapeName needs (Map.fromList [(name, expr) | Let name _ expr <- members]) fillEnv

-- core expressions contain selected evidence and field accesses, but no raw
-- fills or unresolved evidence holes.
evalExpr :: Expr -> RuntimeEnv -> Eval RuntimeValue
evalExpr = compileExpr

compileExpr :: Expr -> RuntimeExpr
compileExpr = compileExprWith Nothing

-- recurring match bodies are compiled once. closed dictionary selection is
-- specialised at closure creation, but local evidence remaineth dynamic.
compileExprWith :: Maybe RuntimeEnv -> Expr -> RuntimeExpr
compileExprWith staticEnv = \case
  ELocated _ inner -> compileExprWith staticEnv inner
  EInteger i -> const (pure (VInteger i))
  EFloat s -> const (pure (VFloat s))
  EUnicode codePoint -> const (pure (VUnicode codePoint))
  EText s -> const (pure (VText (Text.pack s)))
  EForeign -> const (runtimeError "internal misplaced foreign marker")
  EVar name -> evalVar name
  EAscribe expression _ -> compileExprWith staticEnv expression
  EApply function arguments ->
    let compiledArguments = map (compileExprWith staticEnv) (NE.toList arguments)
     in case staticEnv >>= \env -> resolveClosedShapeMemberCall env function (length compiledArguments) of
          Just native -> \env -> traverse ($ env) compiledArguments >>= native
          Nothing ->
            let compiledFunction = compileExprWith staticEnv function
             in \env -> compiledFunction env >>= \value -> evalCompiledArguments value compiledArguments env
  ERecord fields ->
    let compiledFields = map (fmap (compileExprWith staticEnv)) fields
     in \env -> evalCompiledRecord compiledFields env Map.empty
  EField base field ->
    let compiledBase = compileExprWith staticEnv base
     in \env -> compiledBase env >>= evalRecordField field
  EUpdate base updates ->
    let compiledBase = compileExprWith staticEnv base
        compiledUpdates = map (compileRecordUpdate staticEnv) updates
     in \env -> compiledBase env >>= \value -> evalCompiledRecordUpdate value compiledUpdates env
  ETry body returnCase cases -> evalTry body returnCase cases
  EMatch scrutinees cases ->
    let compiledScrutinees = map (compileExprWith staticEnv) scrutinees
        compiledCases = compileMatchCases staticEnv cases
     in \env ->
          if null compiledScrutinees
            then pure (newMatcher compiledCases env)
            else traverse ($ env) compiledScrutinees >>= \values -> evalMatchCases compiledCases values env
  EBlock declarations body ->
    let compiledDeclarations = map (compileLocalDecl staticEnv) declarations
        compiledBody = compileExprWith staticEnv body
     in \env -> evalCompiledLocalDecls compiledDeclarations env >>= compiledBody
  EWithEvidence function evidence -> case staticEnv >>= \env -> resolveClosedShapeMember env function evidence of
    Just value -> const (pure value)
    Nothing ->
      let compiledFunction = compileExprWith staticEnv function
       in \env -> compiledFunction env >>= \value -> applyEvidence value evidence env

resolveClosedShapeMember :: RuntimeEnv -> Expr -> [Evidence] -> Maybe RuntimeValue
resolveClosedShapeMember env function evidence = do
  name <- variableName function
  guard (all evidenceClosed evidence)
  value@(VShapeMember _) <- lookupValue name env
  pureEval (applyEvidence value evidence env)
 where
  variableName = \case
    ELocated _ inner -> variableName inner
    EAscribe inner _ -> variableName inner
    EVar name -> Just name
    _ -> Nothing
  pureEval = \case
    EvalPure (RuntimeOk value) -> Just value
    _ -> Nothing

resolveClosedShapeMemberCall :: RuntimeEnv -> Expr -> Int -> Maybe ([RuntimeValue] -> Eval RuntimeValue)
resolveClosedShapeMemberCall env expression argumentCount = do
  (function, evidence) <- evidenceApplication expression
  VNative name remaining [] <- resolveClosedShapeMember env function evidence
  guard (remaining == argumentCount)
  pure (evalNative name)
 where
  evidenceApplication = \case
    ELocated _ inner -> evidenceApplication inner
    EAscribe inner _ -> evidenceApplication inner
    EWithEvidence function evidence -> Just (function, evidence)
    _ -> Nothing

evidenceClosed :: Evidence -> Bool
evidenceClosed = \case
  EvidenceFill _ nested parents -> all evidenceClosed nested && all evidenceClosed parents
  EvidenceHole{} -> False
  EvidenceLocal{} -> False

evalCompiledArguments :: RuntimeValue -> [RuntimeExpr] -> RuntimeEnv -> Eval RuntimeValue
evalCompiledArguments value arguments env = foldM step value arguments
 where
  step function argument = argument env >>= applyOne function

type RuntimeRecordUpdate = Either String (String, RuntimeExpr)

compileRecordUpdate :: Maybe RuntimeEnv -> RecordUpdate -> RuntimeRecordUpdate
compileRecordUpdate staticEnv = \case
  RecordRemove name -> Left name
  RecordSet name expr -> Right (name, compileExprWith staticEnv expr)

evalCompiledRecord :: [(String, RuntimeExpr)] -> RuntimeEnv -> Map.Map String RuntimeValue -> Eval RuntimeValue
evalCompiledRecord fields env record = VRecord <$> foldM step record fields
 where
  step record (name, expr) = Map.insert name <$> expr env <*> pure record

evalCompiledRecordUpdate :: RuntimeValue -> [RuntimeRecordUpdate] -> RuntimeEnv -> Eval RuntimeValue
evalCompiledRecordUpdate value updates env = case value of
  VRecord fields -> VRecord <$> foldM step fields updates
  other -> runtimeError ("record update expected record, found " ++ showRuntimeValue other)
 where
  step record = \case
    Left name ->
      if Map.member name record
        then pure (Map.delete name record)
        else runtimeError ("unknown record field '" ++ name ++ "'")
    Right (name, expr) -> Map.insert name <$> expr env <*> pure record

data RuntimeLocalDecl
  = RuntimeLocalLet String RuntimeExpr
  | RuntimeLocalMatcher String [RuntimeMatchCase]
  | RuntimeLocalOther

compileLocalDecl :: Maybe RuntimeEnv -> Decl -> RuntimeLocalDecl
compileLocalDecl staticEnv (Let name _ expr) = case unlocatedMatch expr of
  Just cases -> RuntimeLocalMatcher name (compileMatchCases staticEnv cases)
  Nothing -> RuntimeLocalLet name (compileExprWith staticEnv expr)
compileLocalDecl _ _ = RuntimeLocalOther

unlocatedMatch :: Expr -> Maybe [MatchCase]
unlocatedMatch = \case
  ELocated _ inner -> unlocatedMatch inner
  EAscribe inner _ -> unlocatedMatch inner
  EMatch [] cases -> Just cases
  _ -> Nothing

evalCompiledLocalDecls :: [RuntimeLocalDecl] -> RuntimeEnv -> Eval RuntimeEnv
evalCompiledLocalDecls declarations env = foldM step env declarations
 where
  step env (RuntimeLocalLet name expr) = addLocalValue name <$> expr env <*> pure env
  step env (RuntimeLocalMatcher name cases) =
    let value = newMatcher cases (addLocalValue name value env)
     in pure (addLocalValue name value env)
  step env RuntimeLocalOther = pure env

compileMatchCases :: Maybe RuntimeEnv -> [MatchCase] -> [RuntimeMatchCase]
compileMatchCases staticEnv = map (\(MatchCase patterns body) -> RuntimeMatchCase patterns (compileExprWith staticEnv body))

newMatcher :: [RuntimeMatchCase] -> RuntimeEnv -> RuntimeValue
newMatcher cases env = VMatcher arity cases env []
 where
  arity = case cases of
    RuntimeMatchCase patterns _ : _ -> NE.length patterns
    [] -> 0

applyEvidence :: RuntimeValue -> [Evidence] -> RuntimeEnv -> Eval RuntimeValue
applyEvidence value evidence env = traverse (evalEvidence env) evidence >>= foldM applyOne value

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
      Nothing -> case primitiveDictionary key env parentDictionaries of
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
applyOne :: RuntimeValue -> RuntimeValue -> Eval RuntimeValue
applyOne fn arg = case fn of
  VMatcher remaining cases matchEnv supplied -> applyMatcher remaining cases matchEnv supplied arg
  VConstructor qualifiedName displayName remaining supplied ->
    applyArity remaining supplied arg (VConstructor qualifiedName displayName) (pure . VData qualifiedName displayName)
  VNative name remaining supplied ->
    applyArity remaining supplied arg (VNative name) (evalNative name)
  VEffectOp effectName opName remaining supplied ->
    applyArity remaining supplied arg (VEffectOp effectName opName) (evalEffectOp effectName opName)
  VContinuation resume -> resume arg
  VShapeMember member -> case arg of
    VDictionary dictionary -> evalDictionaryMember member dictionary
    _ -> runtimeError ("shape member '" ++ member ++ "' received non-dictionary evidence")
  _ -> runtimeError ("cannot apply " ++ showRuntimeValue fn)

evalDictionaryMember :: String -> RuntimeDictionary -> Eval RuntimeValue
evalDictionaryMember member = \case
  dictionary@(PrimitiveDictionary shapeName typeName env parents)
    | primitiveDictionarySupports shapeName typeName member -> primitiveDictionaryMember shapeName typeName member
    | Just expr <- shapeDefaultExpr member shapeName env ->
        evalBoundValue ("$primitive-member@" ++ member) expr (addLocalValue "$evidence0" (VDictionary dictionary) env)
    | Just parent <- find (dictionaryHasMember member) parents -> evalDictionaryMember member parent
    | otherwise -> runtimeError ("primitive fill '" ++ shapeName ++ " " ++ typeName ++ "' hath no member '" ++ member ++ "'")
  dictionary@(FillDictionary fill requirements parents) -> case fillMemberExpr member fill of
    Just expr -> do
      let evidence = VDictionary dictionary : requirements
          memberEnv = foldl' addEvidence (runtimeFillEnv fill) (zip [(0 :: Int) ..] evidence)
      evalBoundValue ("$fill-member@" ++ member) expr memberEnv
    Nothing -> case find (dictionaryHasMember member) parents of
      Just parent -> evalDictionaryMember member parent
      Nothing -> runtimeError ("selected fill hath no member '" ++ member ++ "'")
 where
  addEvidence env (index, value) = addLocalValue ("$evidence" ++ show index) value env

dictionaryHasMember :: String -> RuntimeDictionary -> Bool
dictionaryHasMember member = \case
  PrimitiveDictionary shapeName typeName env parents ->
    primitiveDictionarySupports shapeName typeName member
      || isJust (shapeDefaultExpr member shapeName env)
      || any (dictionaryHasMember member) parents
  FillDictionary fill _ parents -> isJust (fillMemberExpr member fill) || any (dictionaryHasMember member) parents

fillMemberExpr :: String -> RuntimeFill -> Maybe Expr
fillMemberExpr member RuntimeFill{runtimeFillShape, runtimeFillMembers, runtimeFillEnv} =
  Map.lookup member runtimeFillMembers <|> shapeDefaultExpr member runtimeFillShape runtimeFillEnv

shapeDefaultExpr :: String -> String -> RuntimeEnv -> Maybe Expr
shapeDefaultExpr member rootShape env = findDefault rootShape []
 where
  findDefault shapeName seen
    | shapeName `elem` seen = Nothing
    | otherwise = case findRuntimeShapeEntry shapeName env of
        Nothing -> Nothing
        Just (_, ShapeInfo{shapeNeeds, shapeMembers}) ->
          listToMaybe [expr | ShapeDefault name _ expr <- shapeMembers, name == member]
            <|> foldr (<|>) Nothing [findDefault neededName (shapeName : seen) | ShapeNeed _ neededName <- shapeNeeds]

primitiveDictionary :: String -> RuntimeEnv -> [RuntimeDictionary] -> Maybe RuntimeDictionary
primitiveDictionary key env parents = case stripPrefix "$primitive@" key of
  Nothing -> Nothing
  Just rest -> case break (== '@') rest of
    (shapeName, '@' : typeName) -> Just (PrimitiveDictionary shapeName typeName env parents)
    _ -> Nothing

primitiveDictionaryMember :: String -> String -> String -> Eval RuntimeValue
primitiveDictionaryMember shapeName typeName member = case (shapeName, typeName, member) of
  ("zero", "integer", "zero") -> pure (VInteger 0)
  ("zero", "float", "zero") -> pure (VFloat 0)
  ("one", "integer", "one") -> pure (VInteger 1)
  ("one", "float", "one") -> pure (VFloat 1)
  _ | Just arity <- lookup member runtimeNativeSpecs -> pure (VNative member arity [])
  _ -> runtimeError ("primitive fill '" ++ shapeName ++ " " ++ typeName ++ "' hath no member '" ++ member ++ "'")

primitiveDictionarySupports :: String -> String -> String -> Bool
primitiveDictionarySupports shapeName typeName member = case (shapeName, typeName, member) of
  ("zero", "integer", "zero") -> True
  ("zero", "float", "zero") -> True
  ("one", "integer", "one") -> True
  ("one", "float", "one") -> True
  _ -> isJust (lookup member runtimeNativeSpecs)

applyArity :: Int -> [RuntimeValue] -> RuntimeValue -> (Int -> [RuntimeValue] -> RuntimeValue) -> ([RuntimeValue] -> Eval RuntimeValue) -> Eval RuntimeValue
applyArity remaining supplied arg partial full
  | remaining > 1 = pure (partial (remaining - 1) (arg : supplied))
  | remaining == 1 = full (reverse (arg : supplied))
  | otherwise = runtimeError "too many arguments"

applyMatcher :: Int -> [RuntimeMatchCase] -> RuntimeEnv -> [RuntimeValue] -> RuntimeValue -> Eval RuntimeValue
applyMatcher remaining cases matchEnv supplied arg
  | remaining > 1 = pure (VMatcher (remaining - 1) cases matchEnv (arg : supplied))
  | remaining == 1 = evalMatchCases cases (reverse (arg : supplied)) matchEnv
  | otherwise = runtimeError "too many arguments for match"

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
    PatternMatched handlerEnv -> evalExpr body (addLocalValue "resume" (VContinuation resume) handlerEnv)

bindHandlerRuntimePatterns :: [Pattern] -> [RuntimeValue] -> RuntimeEnv -> PatternBind
bindHandlerRuntimePatterns [] _ env = PatternMatched env
bindHandlerRuntimePatterns patterns values env = bindRuntimePatterns patterns values env

evalMatchCases :: [RuntimeMatchCase] -> [RuntimeValue] -> RuntimeEnv -> Eval RuntimeValue
evalMatchCases cases values env = foldr step noMatch cases
 where
  -- coverage checking should make this defensive branch unreachable for typed data.
  noMatch = runtimeError ("non-exhaustive match " ++ show (map (NE.toList . runtimeMatchPatterns) cases) ++ " on " ++ showRuntimeValues values)
  runtimeMatchPatterns (RuntimeMatchCase patterns _) = patterns
  step (RuntimeMatchCase patterns body) fallback =
    case bindRuntimePatterns (NE.toList patterns) values env of
      PatternNoMatch -> fallback
      PatternMatched boundEnv -> body boundEnv

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
    else PatternMatched (addLocalValue name value env)
bindRuntimePattern (PInteger expected) value env = case value of
  VInteger actual | actual == expected -> PatternMatched env
  _ -> PatternNoMatch
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

-- foreign lets reach this boundary only after their names and full signatures
-- pass the type registry; base natives and deeds use their runtime spec tables.
evalNative :: String -> [RuntimeValue] -> Eval RuntimeValue
evalNative name args = case (name, args) of
  ("add-integer", [VInteger a, VInteger b]) -> pure (VInteger (a + b))
  ("subtract-integer", [VInteger a, VInteger b]) -> pure (VInteger (a - b))
  ("multiply-integer", [VInteger a, VInteger b]) -> pure (VInteger (a * b))
  ("divide-remainder-integer", [VInteger a, VInteger b]) -> evalIntegerDivideRemainder a b
  ("add-float", [VFloat a, VFloat b]) -> evalFloatBinary (+) a b
  ("subtract-float", [VFloat a, VFloat b]) -> evalFloatBinary (-) a b
  ("multiply-float", [VFloat a, VFloat b]) -> evalFloatBinary (*) a b
  ("divide-float", [VFloat a, VFloat b]) -> evalFloatDivide a b
  ("exponent-float", [VFloat a]) -> evalFloatUnary exp a
  ("sine-float", [VFloat a]) -> evalFloatUnary sin a
  ("arctan-float", [VFloat a, VFloat b]) -> evalFloatBinary arctanFloat a b
  ("logarithm-float", [VFloat a]) -> evalFloatUnary log a
  ("exponent", [VFloat a]) -> evalFloatUnary exp a
  ("sine", [VFloat a]) -> evalFloatUnary sin a
  ("logarithm", [VFloat a]) -> evalFloatUnary log a
  ("⌊", [VFloat a]) -> evalFloatFloor a
  ("integer-to-text", [VInteger a]) -> pure (VText (Text.pack (show a)))
  ("float-to-text", [VFloat a]) -> pure (VText (Text.pack (show a)))
  ("unicode-to-integer", [VUnicode codePoint]) -> pure (VInteger (toInteger (ord codePoint)))
  ("integer-to-unicode", [VInteger value]) -> integerToUnicode value
  ("equal-integer", [VInteger a, VInteger b]) -> pure (runtimeBool (a == b))
  ("behead-text", [VText value]) -> pure (runtimeTextBehead value)
  ("text-to-list", [VText value]) -> pure (runtimeList (map VUnicode (Text.unpack value)))
  ("list-to-text", [value]) -> runtimeListToText value
  ("fold-join-text", [value]) -> runtimeFoldJoinText value
  ("join-text", [VText left, VText right]) -> pure (VText (Text.append left right))
  ("equal-text", [VText left, VText right]) -> pure (runtimeBool (left == right))
  ("equal-unicode", [VUnicode left, VUnicode right]) -> pure (runtimeBool (left == right))
  ("less-equal-text", [VText left, VText right]) -> pure (runtimeBool (left <= right))
  ("less-equal-unicode", [VUnicode left, VUnicode right]) -> pure (runtimeBool (left <= right))
  ("+", [VInteger a, VInteger b]) -> pure (VInteger (a + b))
  ("+", [VFloat a, VFloat b]) -> evalFloatBinary (+) a b
  ("-", [VInteger a, VInteger b]) -> pure (VInteger (a - b))
  ("-", [VFloat a, VFloat b]) -> evalFloatBinary (-) a b
  ("×", [VInteger a, VInteger b]) -> pure (VInteger (a * b))
  ("×", [VFloat a, VFloat b]) -> evalFloatBinary (*) a b
  ("∕", [VFloat a, VFloat b]) -> evalFloatDivide a b
  ("÷", [VInteger a, VInteger b]) -> evalIntegerDivideRemainder a b
  ("≡", [a, b]) -> pure (runtimeBool (a == b))
  ("≤", [VInteger a, VInteger b]) -> pure (runtimeBool (a <= b))
  ("≤", [VFloat a, VFloat b]) -> evalFloatLessEqual a b
  ("from-text", [VText text]) -> floatFromText text
  ("to-text", [value]) -> pure (VText (Text.pack (showRuntimeValue value)))
  _ -> runtimeError ("native '" ++ name ++ "' doth not support these arguments")

evalEffectOp :: String -> String -> [RuntimeValue] -> Eval RuntimeValue
evalEffectOp effectName opName args = case (effectName, opName, args) of
  ("console", "write", [VText text]) -> writeConsole text
  ("console", "read", [VData _ "null" []]) -> VText <$> liftIO readConsoleLine
  ("random", "random", [VData _ "null" []]) -> VFloat <$> liftIO nextRandomFloat
  ("async", "sleep", [VInteger ms]) -> sleepMilliseconds ms
  ("async", "fork", [work]) -> forkConcurrent work
  ("async", "wait", [VTask task]) -> waitTask task
  ("async", "fordo", [VTask task]) -> cancelTask task
  ("async", "wait-for", [VTask task, VInteger milliseconds]) -> waitForTask task milliseconds
  ("file", "read-file", [VText path]) -> ioRuntime (VText <$> TextIO.readFile (Text.unpack path))
  ("file", "write-file", [VText path, VText text]) -> ioRuntime (TextIO.writeFile (Text.unpack path) text >> pure runtimeNull)
  ("file", "append-file", [VText path, VText text]) -> ioRuntime (TextIO.appendFile (Text.unpack path) text >> pure runtimeNull)
  ("system", "arguments", [VData _ "null" []]) -> runtimeList . map (VText . Text.pack) <$> readHostArguments
  ("system", "environment", [VText name]) -> runtimeOption . fmap (VText . Text.pack) <$> liftIO (Environment.lookupEnv (Text.unpack name))
  ("system", "exit", [VInteger status]) -> liftIO (exitWith (if status == 0 then ExitSuccess else ExitFailure (boundedInt status)))
  ("clock", "unix-time", [VData _ "null" []]) -> VInteger . floor <$> liftIO getPOSIXTime
  ("process", "run-process", [VText command, arguments]) -> runProcessRuntime command arguments
  _ -> performRuntime effectName opName args

readHostArguments :: Eval [String]
readHostArguments = Eval (pure . RuntimeOk . hostArguments)

failRuntime :: String -> Eval RuntimeValue
failRuntime message = performRuntime "fail" "fail" [VText (Text.pack message)]

integerToUnicode :: Integer -> Eval RuntimeValue
integerToUnicode value = maybe (failRuntime "invalid unicode scalar value") (pure . VUnicode) (unicodeScalar value)

ioRuntime :: IO RuntimeValue -> Eval RuntimeValue
ioRuntime action = do
  result <- liftIO (try action)
  case (result :: Either IOException RuntimeValue) of
    Left err -> failRuntime (show err)
    Right value -> pure value

runProcessRuntime :: Text.Text -> RuntimeValue -> Eval RuntimeValue
runProcessRuntime command arguments = case runtimeListValues arguments >>= traverse runtimeText of
  Nothing -> runtimeError "native 'run-process' received an invalid text list"
  Just texts -> ioRuntime do
    (status, output, errors) <- Process.readProcessWithExitCode (Text.unpack command) (map Text.unpack texts) ""
    pure
      ( VData
          "process/result@process-result@process-result"
          "process-result"
          [VInteger (exitStatus status), VText (Text.pack output), VText (Text.pack errors)]
      )

exitStatus :: ExitCode -> Integer
exitStatus ExitSuccess = 0
exitStatus (ExitFailure status) = toInteger status

writeConsole :: Text.Text -> Eval RuntimeValue
writeConsole text = liftIO (TextIO.putStr text >> hFlush stdout) >> pure runtimeNull

readConsoleLine :: IO Text.Text
readConsoleLine = hFlush stdout >> TextIO.getLine

sleepMilliseconds :: Integer -> Eval RuntimeValue
sleepMilliseconds ms = liftIO (sleepMicroseconds (max 0 ms * 1000)) >> pure runtimeNull

sleepMicroseconds :: Integer -> IO ()
sleepMicroseconds remaining
  | remaining <= 0 = pure ()
  | otherwise = do
      let chunk = min remaining (toInteger (maxBound :: Int))
      threadDelay (fromInteger chunk)
      sleepMicroseconds (remaining - chunk)

boundedInt :: Integer -> Int
boundedInt value = fromInteger (max (toInteger (minBound :: Int)) (min (toInteger (maxBound :: Int)) value))

forkConcurrent :: RuntimeValue -> Eval RuntimeValue
forkConcurrent work = Eval $ \host@RuntimeHost{hostTaskSlots, hostNextTask, hostActiveTasks} -> do
  task <- do
    runtimeTaskId <- atomically do
      ident <- readTVar hostNextTask
      writeTVar hostNextTask (ident + 1)
      pure ident
    runtimeTaskThread <- newEmptyMVar
    runtimeTaskResult <- newEmptyMVar
    let task = RuntimeTask{..}
    atomically (modifyTVar' hostActiveTasks (Map.insert runtimeTaskId task))
    background <- atomically do
      available <- readTVar hostTaskSlots
      if available <= 0
        then pure False
        else writeTVar hostTaskSlots (available - 1) >> pure True
    if background
      then do
        started <- newEmptyMVar
        thread <- forkIO $ mask $ \restore -> do
          putMVar started ()
          finishTask host True task =<< restore (runTask host work)
        readMVar started
        putMVar runtimeTaskThread (Just thread)
      else do
        putMVar runtimeTaskThread Nothing
        finishTask host False task =<< runTask host work
    pure task
  pure (RuntimeOk (VTask task))

runTask :: RuntimeHost -> RuntimeValue -> IO TaskResult
runTask host work = do
  outcome <- try (runEval (applyOne work runtimeNull) host)
  pure $ case outcome of
    Left exception
      | Just ThreadKilled <- fromException exception -> TaskCancelled
      | otherwise -> TaskCrashed (displayException (exception :: SomeException))
    Right result -> TaskFinished result

finishTask :: RuntimeHost -> Bool -> RuntimeTask -> TaskResult -> IO ()
finishTask RuntimeHost{hostTaskSlots, hostActiveTasks} releaseSlot task result = do
  putMVar (runtimeTaskResult task) result
  atomically do
    modifyTVar' hostActiveTasks (Map.delete (runtimeTaskId task))
    when releaseSlot (modifyTVar' hostTaskSlots (+ 1))

waitTask :: RuntimeTask -> Eval RuntimeValue
waitTask task = liftIO (readMVar (runtimeTaskResult task)) >>= taskResult

waitForTask :: RuntimeTask -> Integer -> Eval RuntimeValue
waitForTask task milliseconds = do
  result <- liftIO (Timeout.timeout (boundedMicroseconds milliseconds) (readMVar (runtimeTaskResult task)))
  maybe (pure (runtimeOption Nothing)) (fmap (runtimeOption . Just) . taskResult) result

cancelTask :: RuntimeTask -> Eval RuntimeValue
cancelTask task = liftIO (cancelTaskIO task) >> pure runtimeNull

cancelTaskIO :: RuntimeTask -> IO ()
cancelTaskIO task = readMVar (runtimeTaskThread task) >>= mapM_ killThread

taskResult :: TaskResult -> Eval RuntimeValue
taskResult = \case
  TaskFinished result -> Eval (const (pure result))
  TaskCancelled -> failRuntime "task cancelled"
  TaskCrashed message -> failRuntime ("task failed: " ++ message)

boundedMicroseconds :: Integer -> Int
boundedMicroseconds milliseconds = fromInteger (min (toInteger (maxBound :: Int)) (max 0 milliseconds * 1000))

runtimeNull :: RuntimeValue
runtimeNull = VData "𝟙@null" "null" []

runtimeList :: [RuntimeValue] -> RuntimeValue
runtimeList = foldr (\value rest -> VData "data/list@list@.*" ".*" [value, rest]) (VData "data/list@list@empty" "empty" [])

runtimeOption :: Maybe RuntimeValue -> RuntimeValue
runtimeOption = maybe (VData "data/option@option@none" "none" []) (\value -> VData "data/option@option@some" "some" [value])

runtimeProduct :: RuntimeValue -> RuntimeValue -> RuntimeValue
runtimeProduct left right = VData "data/product@∏@∏" "∏" [left, right]

runtimeTextBehead :: Text.Text -> RuntimeValue
runtimeTextBehead value = runtimeOption do
  (codePoint, rest) <- Text.uncons value
  pure (runtimeProduct (VUnicode codePoint) (VText rest))

runtimeListToText :: RuntimeValue -> Eval RuntimeValue
runtimeListToText value = case runtimeListValues value >>= traverse runtimeUnicode of
  Just codePoints -> pure (VText (Text.pack codePoints))
  Nothing -> runtimeError "native 'list-to-text' received an invalid unicode list"

runtimeFoldJoinText :: RuntimeValue -> Eval RuntimeValue
runtimeFoldJoinText value = case runtimeListValues value >>= traverse runtimeText of
  Just values -> pure (VText (Text.concat values))
  Nothing -> runtimeError "native 'fold-join-text' received an invalid text list"

runtimeListValues :: RuntimeValue -> Maybe [RuntimeValue]
runtimeListValues = \case
  VData _ "empty" [] -> Just []
  VData _ ".*" [value, rest] -> (value :) <$> runtimeListValues rest
  _ -> Nothing

runtimeUnicode :: RuntimeValue -> Maybe Char
runtimeUnicode = \case
  VUnicode codePoint -> Just codePoint
  _ -> Nothing

runtimeText :: RuntimeValue -> Maybe Text.Text
runtimeText = \case
  VText value -> Just value
  _ -> Nothing

nextRandomFloat :: IO Double
nextRandomFloat = do
  t <- getPOSIXTime
  pure (realToFrac (t - fromIntegral (floor t :: Integer)))

evalFloatUnary :: (Double -> Double) -> Double -> Eval RuntimeValue
evalFloatUnary f = pure . VFloat . f

evalFloatBinary :: (Double -> Double -> Double) -> Double -> Double -> Eval RuntimeValue
evalFloatBinary f left right = pure (VFloat (f left right))

arctanFloat :: Double -> Double -> Double
arctanFloat real imaginary =
  let angle = atan2 imaginary real
      tau = 2 * pi
   in if angle < 0 then angle + tau else angle

evalIntegerDivideRemainder :: Integer -> Integer -> Eval RuntimeValue
evalIntegerDivideRemainder _ 0 = failRuntime "division by zero"
evalIntegerDivideRemainder a b =
  let (quotient, remainder) = euclideanQuotRem a b
   in pure (runtimeProduct (VInteger quotient) (VInteger remainder))

euclideanQuotRem :: Integer -> Integer -> (Integer, Integer)
euclideanQuotRem dividend divisor =
  let (quotient, remainder) = dividend `divMod` divisor
   in if remainder < 0
        then (quotient + 1, remainder - divisor)
        else (quotient, remainder)

evalFloatLessEqual :: Double -> Double -> Eval RuntimeValue
evalFloatLessEqual left right = pure (runtimeBool (left <= right))

evalFloatDivide :: Double -> Double -> Eval RuntimeValue
evalFloatDivide left right = pure (VFloat (left / right))

evalFloatFloor :: Double -> Eval RuntimeValue
evalFloatFloor value
  | isNaN value || isInfinite value = failRuntime "cannot floor a non-finite float"
  | otherwise = pure (VInteger (floor value))

floatFromText :: Text.Text -> Eval RuntimeValue
floatFromText text = case (readMaybe (Text.unpack text) :: Maybe Double) of
  Just x -> pure (VFloat x)
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

typeArity :: TypeExpr -> Int
typeArity (TypeArrow args _ _) = NE.length args
typeArity _ = 0

-- base bindings are implementation capabilities, not implicit source imports;
-- ordinary bookhoard files still decide which names are shown to users.
baseRuntimeEnv :: RuntimeEnv
baseRuntimeEnv = addBaseEffectOps (addBaseNatives RuntimeEnv{runtimeValues = [], runtimeLocals = [], runtimeLookup = Map.empty, runtimeShapes = Map.empty, runtimeFills = Map.empty})

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

addLocalValue :: String -> RuntimeValue -> RuntimeEnv -> RuntimeEnv
addLocalValue name value env@RuntimeEnv{runtimeLocals} = env{runtimeLocals = (name, value) : runtimeLocals}

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
lookupValue name RuntimeEnv{runtimeLocals, runtimeLookup} = lookup name runtimeLocals <|> Map.lookup name runtimeLookup

indexRuntimeValues :: [RuntimeBinding] -> Map.Map String RuntimeValue
indexRuntimeValues = foldr (\(name, value, _, _) -> Map.insert name value) Map.empty

markRuntimeDeclExport :: Decl -> RuntimeEnv -> RuntimeEnv
markRuntimeDeclExport declaration env = case declaration of
  Import _ -> env
  Export nested -> markRuntimeDeclExport nested env
  ReExport name -> addShownValue name env
  ReExportType _ -> env
  Let name _ _ -> addShownValue name env
  TypeAlias _ _ _ -> env
  DataDecl _ _ constructors -> foldl' (flip addShownValue) env [name | Ctor name _ <- constructors]
  EffectDecl _ _ operations -> foldl' (flip addShownValue) env [name | EffectOp name _ <- operations]
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
  VFloat value -> show value
  VUnicode codePoint -> "`" ++ escapeUnicodeCodePoint codePoint
  VText s -> "'" ++ concatMap escapeTextCharacter (Text.unpack s) ++ "'"
  VRecord fields -> "[" ++ showRuntimeFields fields ++ "]"
  VData _ displayName [] -> displayName
  VData _ displayName args -> "(" ++ showRuntimeValues args ++ " " ++ displayName ++ ")"
  VMatcher{} -> "<function>"
  VConstructor _ displayName _ _ -> "<constructor " ++ displayName ++ ">"
  VNative name _ _ -> "<native " ++ name ++ ">"
  VEffectOp _ opName _ _ -> "<effect " ++ opName ++ ">"
  VTask{} -> "<task>"
  VContinuation _ -> "<continuation>"
  VShapeMember name -> "<shape member " ++ name ++ ">"
  VDictionary{} -> "<dictionary>"

showRuntimeValues :: [RuntimeValue] -> String
showRuntimeValues = unwords . map showRuntimeValue

escapeTextCharacter :: Char -> String
escapeTextCharacter character = fromMaybe [character] (escapedSourceCharacter character)

escapeUnicodeCodePoint :: Char -> String
escapeUnicodeCodePoint codePoint = fromMaybe fallback (escapedSourceCharacter codePoint)
 where
  fallback
    | isControl codePoint = "\\" ++ show (ord codePoint) ++ ";"
    | otherwise = [codePoint]

escapedSourceCharacter :: Char -> Maybe String
escapedSourceCharacter character = lookup character [('\n', "\\n"), ('\r', "\\r"), ('\t', "\\t"), ('\'', "\\'"), ('\\', "\\\\")]

showRuntimeFields :: Map.Map String RuntimeValue -> String
showRuntimeFields fields = intercalate ", " [name ++ " = " ++ showRuntimeValue value | (name, value) <- Map.toList fields]
