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
import Control.Monad (ap, foldM, unless, when, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isControl, ord)
import Data.Foldable (asum)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (find, intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment qualified as Environment
import System.Exit (ExitCode (..), exitWith)
import System.IO (hFlush, stdout)
import System.Process qualified as Process
import System.Timeout qualified as Timeout
import Text.Read (readMaybe)
import Tung.Core (
  CoreDecl (..),
  CoreExpr (..),
  CoreHandlerCase (..),
  CoreLocalDecl (..),
  CoreMatchCase (..),
  CoreName (..),
  CorePattern (..),
  CoreProgram,
  CoreRecordUpdate (..),
  CoreReturnCase (..),
  LocalId (..),
  ModuleInterface (..),
  coreDeclarations,
  coreImportDeclarations,
  coreImportInterface,
  coreInterface,
  localIdText,
 )
import Tung.Identity (FillId (..), HandlerTarget (..), ModuleId (RuntimeModule), SymbolId (..), TermExport (..), TermKind (..), renderFillId)
import Tung.Import (ImportStack, enterImport)
import Tung.Name (lastQualifiedSegment)
import Tung.Parse (parse)
import Tung.Primitive (
  HostBinding (..),
  HostRole (..),
  PrimitiveFillSpec (..),
  baseNativeBindings,
  consConstructorId,
  emptyConstructorId,
  hostArity,
  hostBindings,
  nayConstructorId,
  noneConstructorId,
  onlyConstructorId,
  primitiveFillSpecs,
  processResultConstructorId,
  productConstructorId,
  renderEffectId,
  requestConstructorId,
  responseConstructorId,
  someConstructorId,
  tableConstructorId,
  yeaConstructorId,
 )
import Tung.Token (unicodeScalar)
import Tung.Type (elaborateInteractiveProgramWithImports, elaborateProgramWithImports)
import Tung.Web qualified as Web

-- callable values retain supplied arguments, making partial application a pure
-- value. natives and effect operations run only when a callable is saturated.
data RuntimeValue
  = VUnit
  | VInteger Integer
  | VFloat Double
  | VUnicode Char
  | VText Text.Text
  | VRecord (Map.Map String RuntimeValue)
  | VData SymbolId [RuntimeValue]
  | VCallable String ([RuntimeValue] -> Eval RuntimeValue) Int [RuntimeValue]
  | VTask RuntimeTask
  | VDictionary RuntimeDictionary

type RuntimeExpr = RuntimeEnv -> Eval RuntimeValue

data RuntimeMatchCase = RuntimeMatchCase (NE.NonEmpty CorePattern) RuntimeExpr

-- primitive dictionaries retain inherited dictionaries after native lookup.
data RuntimeDictionary
  = FillDictionary RuntimeFill [RuntimeValue] [RuntimeDictionary]
  | PrimitiveDictionary SymbolId String [RuntimeDictionary]

data RuntimeFill = RuntimeFill
  { runtimeFillMembers :: Map.Map String CoreExpr
  , runtimeFillEnv :: RuntimeEnv
  }

instance Eq RuntimeValue where
  VUnit == VUnit = True
  VInteger a == VInteger b = a == b
  VFloat a == VFloat b = a == b
  VUnicode a == VUnicode b = a == b
  VText a == VText b = a == b
  VRecord xs == VRecord ys = xs == ys
  VData xName xArgs == VData yName yArgs = xName == yName && xArgs == yArgs
  _ == _ = False

data RuntimeEnv = RuntimeEnv
  { runtimeGlobals :: Map.Map TermExport RuntimeValue
  , runtimeLocals :: Map.Map LocalId RuntimeValue
  , runtimeFills :: Map.Map FillId RuntimeFill
  }

newtype RuntimeError = RuntimeError String

-- an operation carrieþ its explicit continuation. the 'Eval' monad composeþ
-- through that continuation, which giveþ handlers deep, reusable resumption.
data RuntimeResult a = RuntimeOk a | RuntimeErr RuntimeError | RuntimeOp SymbolId SymbolId [RuntimeValue] (RuntimeValue -> Eval a)

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

performRuntime :: SymbolId -> SymbolId -> [RuntimeValue] -> Eval RuntimeValue
performRuntime effect operation args = EvalPure (RuntimeOp effect operation args pure)

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

-- evaluation never accepteþ surface syntax directly: this function first askeþ
-- the type checker for a mode-appropriate 'CoreProgram'.
evaluateParsedWith :: Bool -> [String] -> String -> Map.Map String String -> (CoreProgram -> Eval RuntimeValue) -> IO String
evaluateParsedWith runnable arguments source imports action = case parse source of
  Left msg -> pure ("parse error: " ++ msg)
  Right parsed -> case elaborate parsed of
    Left msg -> pure ((if runnable then "type error: " else "eval error: type error: ") ++ msg)
    Right program -> runCoreProgram arguments program action
 where
  -- interactive evaluation alloweþ top-level computation, but still checkeþ and
  -- elaborateþ it. runnable evaluation additionally checkeþ the main boundary.
  elaborate program
    | runnable = elaborateProgramWithImports program imports True
    | otherwise = elaborateInteractiveProgramWithImports program imports

runCoreProgram :: [String] -> CoreProgram -> (CoreProgram -> Eval RuntimeValue) -> IO String
runCoreProgram arguments program action = do
  host <- newRuntimeHost arguments
  result <- runEval (action program) host `finally` cancelActiveTasks host
  pure $ case result of
    RuntimeOk value -> "eval ok: " ++ showRuntimeValue value
    RuntimeErr (RuntimeError message) -> "eval error: " ++ message
    RuntimeOp effect operation _ _ -> "eval error: " ++ showUnhandledOperation effect operation

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
  let mainSymbol = SymbolId (interfaceModule (coreInterface program)) "main"
      mainTerm = TermExport mainSymbol OrdinaryTerm
  mainValue <- maybe (runtimeError "runnable file must declare main") pure (lookupGlobalValue mainTerm env)
  applyOne mainValue runtimeOnly

evalProgram :: CoreProgram -> Eval RuntimeValue
evalProgram program = snd <$> evalProgramEnv [] program (coreDeclarations program)

evalProgramEnv :: ImportStack -> CoreProgram -> [CoreDecl] -> Eval (RuntimeEnv, RuntimeValue)
evalProgramEnv importStack program declarations = evalDeclsWithImports importStack program declarations baseRuntimeEnv VUnit

evalDeclsWithImports :: ImportStack -> CoreProgram -> [CoreDecl] -> RuntimeEnv -> RuntimeValue -> Eval (RuntimeEnv, RuntimeValue)
evalDeclsWithImports importStack program declarations env lastValue = do
  importedEnv <- foldM use env declarations
  let declarationEnv = closeRuntimeFills declarations (foldl' hoistRuntimeDecl importedEnv declarations)
  foldM execute (declarationEnv, lastValue) declarations
 where
  use currentEnv = \case
    CoreImport path -> do
      importedEnv <- evalImport importStack path program
      pure (mergeRuntimeEnv importedEnv currentEnv)
    _ -> pure currentEnv

  -- executable declarations remain strict and left-to-right. reclosing local
  -- fills after each binding preserveþ their access to preceding private values.
  execute (currentEnv, currentLast) = \case
    CoreLet term expr -> do
      value <- evalGlobalBoundValue term expr currentEnv
      let nextEnv = closeRuntimeFills declarations (addGlobalValue term value currentEnv)
          nextLast = if symbolName (termExportTarget term) == "_" then value else currentLast
      pure (nextEnv, nextLast)
    _ -> pure (currentEnv, currentLast)

-- runtime import caching mirrors static imports, but stores evaluated exported
-- environments so repeated imports do not repeat module initialisation.
evalImport :: ImportStack -> String -> CoreProgram -> Eval RuntimeEnv
evalImport importStack path program = case enterImport importStack path of
  Left message -> runtimeError message
  Right nextStack ->
    lookupRuntimeImport path >>= \case
      Just env -> pure env
      Nothing -> case (coreImportInterface path program, coreImportDeclarations path program) of
        (Just interface, Just declarations) -> do
          env <- projectRuntimeInterface interface . fst <$> evalProgramEnv nextStack program declarations
          cacheRuntimeImport path env
          pure env
        _ -> runtimeError ("internal missing checked use '" ++ path ++ "'")

lookupRuntimeImport :: String -> Eval (Maybe RuntimeEnv)
lookupRuntimeImport path = Eval $ \RuntimeHost{hostImportCache} -> RuntimeOk . Map.lookup path <$> readIORef hostImportCache

cacheRuntimeImport :: String -> RuntimeEnv -> Eval ()
cacheRuntimeImport path env = Eval $ \RuntimeHost{hostImportCache} -> do
  atomicModifyIORef' hostImportCache (\cache -> (Map.insert path env cache, ()))
  pure (RuntimeOk ())

projectRuntimeInterface :: ModuleInterface -> RuntimeEnv -> RuntimeEnv
projectRuntimeInterface ModuleInterface{interfaceTerms} RuntimeEnv{runtimeGlobals, runtimeFills} =
  RuntimeEnv
    { runtimeGlobals = Map.restrictKeys runtimeGlobals targets
    , runtimeLocals = Map.empty
    , runtimeFills
    }
 where
  targets = Set.fromList (Map.elems interfaceTerms)

mergeRuntimeEnv :: RuntimeEnv -> RuntimeEnv -> RuntimeEnv
mergeRuntimeEnv left right =
  RuntimeEnv
    { runtimeGlobals = Map.union (runtimeGlobals left) (runtimeGlobals right)
    , runtimeLocals = Map.union (runtimeLocals left) (runtimeLocals right)
    , runtimeFills = Map.union (runtimeFills left) (runtimeFills right)
    }

-- imports and inert declarations form the module environment before any
-- top-level computation. fills are closed separately so all local fills share
-- that environment without replacing imported modules' private closures.
hoistRuntimeDecl :: RuntimeEnv -> CoreDecl -> RuntimeEnv
hoistRuntimeDecl env = \case
  CoreData constructors -> addConstructors constructors env
  CoreEffect operations -> addEffectOps operations env
  CoreShape _ _ members -> addShapeSelectors members env
  CoreForeignLet term hostKey arity -> addGlobalValue term (nativeValue hostKey arity) env
  _ -> env

closeRuntimeFills :: [CoreDecl] -> RuntimeEnv -> RuntimeEnv
closeRuntimeFills declarations env = closedEnv
 where
  closedEnv = foldl' add env declarations
  add current = \case
    CoreFill key _ members ->
      current
        { runtimeFills =
            Map.insert key (RuntimeFill (Map.fromList members) closedEnv) (runtimeFills current)
        }
    _ -> current

evalGlobalBoundValue :: TermExport -> CoreExpr -> RuntimeEnv -> Eval RuntimeValue
evalGlobalBoundValue term expr env = case anonymousCoreMatchCases expr of
  Just cases ->
    let value = newMatcher (compileMatchCases cases) (addGlobalValue term value env)
     in pure value
  Nothing -> compileExpr expr env

addShapeSelectors :: [TermExport] -> RuntimeEnv -> RuntimeEnv
addShapeSelectors members env = foldl' add env members
 where
  add current term = addGlobalValue term (shapeMemberValue (shapeMemberName term)) current

shapeMemberName :: TermExport -> String
shapeMemberName = lastQualifiedSegment . symbolName . termExportTarget

compileExpr :: CoreExpr -> RuntimeExpr
compileExpr = \case
  CoreInteger i -> const (pure (VInteger i))
  CoreFloat s -> const (pure (VFloat s))
  CoreUnicode codePoint -> const (pure (VUnicode codePoint))
  CoreText s -> const (pure (VText (Text.pack s)))
  CoreVar name -> evalCoreName name
  CoreApply function arguments ->
    let compiledFunction = compileExpr function
        compiledArguments = map compileExpr (NE.toList arguments)
     in \env -> compiledFunction env >>= \value -> evalCompiledArguments value compiledArguments env
  CoreRecord fields ->
    let compiledFields = map (fmap compileExpr) fields
     in \env -> evalCompiledRecord compiledFields env Map.empty
  CoreField base field ->
    let compiledBase = compileExpr base
     in compiledBase >=> evalRecordField field
  CoreUpdate base updates ->
    let compiledBase = compileExpr base
        compiledUpdates = map compileRecordUpdate updates
     in \env -> compiledBase env >>= \value -> evalCompiledRecordUpdate value compiledUpdates env
  CoreTry body returnCase cases -> evalTry body returnCase cases
  CoreMatch scrutinees cases ->
    let compiledScrutinees = map compileExpr scrutinees
        compiledCases = compileMatchCases cases
     in \env ->
          if null compiledScrutinees
            then pure (newMatcher compiledCases env)
            else traverse ($ env) compiledScrutinees >>= \values -> evalMatchCases compiledCases values env
  CoreBlock declarations body ->
    let compiledDeclarations = map compileLocalDecl declarations
        compiledBody = compileExpr body
     in evalCompiledLocalDecls compiledDeclarations >=> compiledBody
  CoreDictionary key frame required parents ->
    let compiledRequired = map compileExpr required
        compiledParents = map compileExpr parents
     in \env -> do
          arguments <- traverse ($ env) compiledRequired
          parentValues <- traverse ($ env) compiledParents
          makeDictionary key frame arguments parentValues env

evalCompiledArguments :: RuntimeValue -> [RuntimeExpr] -> RuntimeEnv -> Eval RuntimeValue
evalCompiledArguments value arguments env = foldM step value arguments
 where
  step function argument = argument env >>= applyOne function

type RuntimeRecordUpdate = Either String (String, RuntimeExpr)

compileRecordUpdate :: CoreRecordUpdate -> RuntimeRecordUpdate
compileRecordUpdate = \case
  CoreRecordRemove name -> Left name
  CoreRecordSet name expr -> Right (name, compileExpr expr)

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
  = RuntimeLocalLet LocalId RuntimeExpr
  | RuntimeLocalMatcher LocalId [RuntimeMatchCase]

compileLocalDecl :: CoreLocalDecl -> RuntimeLocalDecl
compileLocalDecl (CoreLocalLet name expr) = case anonymousCoreMatchCases expr of
  Just cases -> RuntimeLocalMatcher name (compileMatchCases cases)
  Nothing -> RuntimeLocalLet name (compileExpr expr)

anonymousCoreMatchCases :: CoreExpr -> Maybe [CoreMatchCase]
anonymousCoreMatchCases = \case
  CoreMatch [] cases -> Just cases
  _ -> Nothing

evalCompiledLocalDecls :: [RuntimeLocalDecl] -> RuntimeEnv -> Eval RuntimeEnv
evalCompiledLocalDecls declarations env = foldM step env declarations
 where
  step env (RuntimeLocalLet name expr) = addLocalValue name <$> expr env <*> pure env
  step env (RuntimeLocalMatcher name cases) =
    let value = newMatcher cases (addLocalValue name value env)
     in pure (addLocalValue name value env)

compileMatchCases :: [CoreMatchCase] -> [RuntimeMatchCase]
compileMatchCases = map (\(CoreMatchCase patterns body) -> RuntimeMatchCase patterns (compileExpr body))

newMatcher :: [RuntimeMatchCase] -> RuntimeEnv -> RuntimeValue
newMatcher cases env = callableValue "<function>" arity (\values -> evalMatchCases cases values env)
 where
  arity = case cases of
    RuntimeMatchCase patterns _ : _ -> NE.length patterns
    [] -> 0

evalRecordField :: String -> RuntimeValue -> Eval RuntimeValue
evalRecordField field = \case
  VRecord fields -> maybe (runtimeError ("unknown record field '" ++ field ++ "'")) pure (Map.lookup field fields)
  value -> runtimeError ("record field access expected record, found " ++ showRuntimeValue value)

makeDictionary :: FillId -> SymbolId -> [RuntimeValue] -> [RuntimeValue] -> RuntimeEnv -> Eval RuntimeValue
makeDictionary key frame arguments parentValues env = do
  parentDictionaries <- traverse expectDictionary parentValues
  case Map.lookup key (runtimeFills env) of
    Just fill -> pure (VDictionary (FillDictionary fill arguments parentDictionaries))
    Nothing -> case key of
      PrimitiveFillId{fillIdPrimitiveType}
        | null arguments -> pure (VDictionary (PrimitiveDictionary frame fillIdPrimitiveType parentDictionaries))
      _ -> runtimeError ("internal missing selected fill '" ++ renderFillId key ++ "'")
 where
  expectDictionary (VDictionary dictionary) = pure dictionary
  expectDictionary _ = runtimeError "internal parent fill value is not a dictionary"

evalCoreName :: CoreName -> RuntimeEnv -> Eval RuntimeValue
evalCoreName (CoreLocalName local) env = case Map.lookup local (runtimeLocals env) of
  Just value -> pure value
  Nothing -> runtimeError ("internal missing local '" ++ localIdText local ++ "'")
evalCoreName (CoreGlobalName term) env = case lookupGlobalValue term env of
  Just value -> pure value
  Nothing -> runtimeError ("internal missing global '" ++ symbolName (termExportTarget term) ++ "'")

-- 'applyOne' is the sole application boundary. supplied arguments are retained
-- in reverse order until the callable is saturated.
applyOne :: RuntimeValue -> RuntimeValue -> Eval RuntimeValue
applyOne fn arg = case fn of
  VCallable display finish remaining supplied
    | remaining > 1 -> pure (VCallable display finish (remaining - 1) (arg : supplied))
    | remaining == 1 -> finish (reverse (arg : supplied))
    | otherwise -> runtimeError "too many arguments"
  _ -> runtimeError ("cannot apply " ++ showRuntimeValue fn)

callableValue :: String -> Int -> ([RuntimeValue] -> Eval RuntimeValue) -> RuntimeValue
callableValue display arity finish = VCallable display finish arity []

unaryValue :: String -> (RuntimeValue -> Eval RuntimeValue) -> RuntimeValue
unaryValue display action = callableValue display 1 $ \case
  [value] -> action value
  _ -> runtimeError "internal unary callable arity mismatch"

nativeValue :: String -> Int -> RuntimeValue
nativeValue name arity = callableValue ("<native " ++ name ++ ">") arity (evalNative name)

shapeMemberValue :: String -> RuntimeValue
shapeMemberValue member = unaryValue ("<frame member " ++ member ++ ">") $ \case
  VDictionary dictionary -> evalDictionaryMember member dictionary
  _ -> runtimeError ("frame member '" ++ member ++ "' received non-dictionary evidence")

evalDictionaryMember :: String -> RuntimeDictionary -> Eval RuntimeValue
evalDictionaryMember member dictionary =
  fromMaybe missing (dictionaryMember dictionary)
 where
  missing = case dictionary of
    PrimitiveDictionary frame typeName _ -> runtimeError ("primitive fill '" ++ symbolName frame ++ " " ++ typeName ++ "' hath no member '" ++ member ++ "'")
    FillDictionary{} -> runtimeError ("selected fill hath no member '" ++ member ++ "'")

  dictionaryMember (PrimitiveDictionary frame typeName parents) =
    (pure <$> primitiveDictionaryValue frame typeName member)
      <|> asum (map dictionaryMember parents)
  dictionaryMember current@(FillDictionary fill requirements parents) =
    case fillMemberExpr member fill of
      Just expr -> Just do
        let evidence = VDictionary current : requirements
            memberEnv = foldl' addEvidence (runtimeFillEnv fill) (zip [(0 :: Int) ..] evidence)
        compileExpr expr memberEnv
      Nothing -> asum (map dictionaryMember parents)

  addEvidence env (index, value) = addLocalValue (EvidenceLocal index) value env

fillMemberExpr :: String -> RuntimeFill -> Maybe CoreExpr
fillMemberExpr member RuntimeFill{runtimeFillMembers} = Map.lookup member runtimeFillMembers

primitiveDictionaryValue :: SymbolId -> String -> String -> Maybe RuntimeValue
primitiveDictionaryValue frame typeName member = do
  _ <- find matches primitiveFillSpecs
  case (typeName, member) of
    ("ℤ", "zero") -> pure (VInteger 0)
    ("float", "zero") -> pure (VFloat 0)
    ("ℤ", "one") -> pure (VInteger 1)
    ("float", "one") -> pure (VFloat 1)
    _ -> nativeValue member . hostArity <$> find ((== member) . hostName) baseNativeBindings
 where
  matches PrimitiveFillSpec{primitiveFillShapeTarget, primitiveFillType, primitiveFillMember} =
    primitiveFillShapeTarget == frame && primitiveFillType == typeName && primitiveFillMember == member

-- handlers are deep because an escaping operation is rewrapped with 'handleEval'.
-- the captured continuation is an ordinary reusable runtime value.
evalTry :: CoreExpr -> Maybe CoreReturnCase -> [CoreHandlerCase] -> RuntimeEnv -> Eval RuntimeValue
evalTry body returnCase cases env = handleEval (compileExpr body env)
 where
  handleEval action = Eval $ \host -> runEval action host >>= \result -> runEval (handleRuntimeResult result) host
  handleRuntimeResult result = case result of
    RuntimeOk value -> evalReturnCase returnCase value env
    RuntimeErr err -> raiseRuntime err
    RuntimeOp effect operation args resume -> case findHandler effect operation cases of
      Nothing -> Eval (const (pure (RuntimeOp effect operation args (handleEval . resume))))
      Just handlerCase -> evalHandlerCase handlerCase args (handleEval . resume) env

evalReturnCase :: Maybe CoreReturnCase -> RuntimeValue -> RuntimeEnv -> Eval RuntimeValue
evalReturnCase Nothing value _ = pure value
evalReturnCase (Just (CoreReturnCase pat body)) value env =
  case bindRuntimePattern pat value env of
    Nothing -> runtimeError "handler yield value did not match pattern"
    Just handlerEnv -> compileExpr body handlerEnv

findHandler :: SymbolId -> SymbolId -> [CoreHandlerCase] -> Maybe CoreHandlerCase
findHandler effect operation = find (\(CoreHandlerCase target _ _ _) -> handlerMatches target effect operation)

handlerMatches :: HandlerTarget -> SymbolId -> SymbolId -> Bool
handlerMatches (EffectTarget expected) effect _ = expected == effect
handlerMatches (OperationTarget expected) _ operation = expected == operation

evalHandlerCase :: CoreHandlerCase -> [RuntimeValue] -> (RuntimeValue -> Eval RuntimeValue) -> RuntimeEnv -> Eval RuntimeValue
evalHandlerCase (CoreHandlerCase _ continuation patterns body) args resume env =
  case bindHandlerRuntimePatterns patterns args env of
    Nothing -> runtimeError "handler operation arguments did not match pattern"
    Just handlerEnv -> compileExpr body (addLocalValue continuation (unaryValue "<continuation>" resume) handlerEnv)

bindHandlerRuntimePatterns :: [CorePattern] -> [RuntimeValue] -> RuntimeEnv -> Maybe RuntimeEnv
bindHandlerRuntimePatterns [] _ env = Just env
bindHandlerRuntimePatterns patterns values env = bindRuntimePatterns patterns values env

evalMatchCases :: [RuntimeMatchCase] -> [RuntimeValue] -> RuntimeEnv -> Eval RuntimeValue
evalMatchCases cases values env = foldr step noMatch cases
 where
  -- coverage checking should make this defensive branch unreachable for typed data.
  noMatch = runtimeError ("non-exhaustive match " ++ show (map (NE.toList . runtimeMatchPatterns) cases) ++ " on " ++ showRuntimeValues values)
  runtimeMatchPatterns (RuntimeMatchCase patterns _) = patterns
  step (RuntimeMatchCase patterns body) fallback = maybe fallback body (bindRuntimePatterns (NE.toList patterns) values env)

bindRuntimePatterns :: [CorePattern] -> [RuntimeValue] -> RuntimeEnv -> Maybe RuntimeEnv
bindRuntimePatterns patterns values env
  | length patterns /= length values = Nothing
  | otherwise = foldM (\current (pat, value) -> bindRuntimePattern pat value current) env (zip patterns values)

bindRuntimePattern :: CorePattern -> RuntimeValue -> RuntimeEnv -> Maybe RuntimeEnv
bindRuntimePattern CoreWildcard _ env = Just env
bindRuntimePattern (CoreBind local) value env = Just (addLocalValue local value env)
bindRuntimePattern (CoreIntegerPattern expected) value env = case value of
  VInteger actual | actual == expected -> Just env
  _ -> Nothing
bindRuntimePattern (CoreTextPattern expected) value env = case value of
  VText actual | actual == Text.pack expected -> Just env
  _ -> Nothing
bindRuntimePattern (CoreConstructorPattern constructor args) value env = case value of
  VData identity fields | constructor == identity -> bindRuntimePatterns args fields env
  _ -> Nothing

-- fremmed lets reach this boundary only after their keys and full signatures
-- pass the host registry; base natives and deeds use the same host catalogue.
evalNative :: String -> [RuntimeValue] -> Eval RuntimeValue
evalNative name args = case (name, args) of
  ("add-integer", [VInteger a, VInteger b]) -> pure (VInteger (a + b))
  ("subtract-integer", [VInteger a, VInteger b]) -> pure (VInteger (a - b))
  ("multiply-integer", [VInteger a, VInteger b]) -> pure (VInteger (a * b))
  ("divide-remainder-integer", [VInteger a, VInteger b]) -> evalIntegerDivideRemainder a b
  ("add-float", [VFloat a, VFloat b]) -> evalFloatBinary (+) a b
  ("subtract-float", [VFloat a, VFloat b]) -> evalFloatBinary (-) a b
  ("multiply-float", [VFloat a, VFloat b]) -> evalFloatBinary (*) a b
  ("divide-float", [VFloat a, VFloat b]) -> pure (VFloat (a / b))
  ("exponent-float", [VFloat a]) -> evalFloatUnary exp a
  ("sine-float", [VFloat a]) -> evalFloatUnary sin a
  ("arctan-float", [VFloat a, VFloat b]) -> evalFloatBinary arctanFloat a b
  ("logariþm-float", [VFloat a]) -> evalFloatUnary log a
  ("exponent", [VFloat a]) -> evalFloatUnary exp a
  ("sine", [VFloat a]) -> evalFloatUnary sin a
  ("logariþm", [VFloat a]) -> evalFloatUnary log a
  ("⌊", [VFloat a]) -> evalFloatFloor a
  ("integer-to-text", [VInteger a]) -> pure (VText (Text.pack (show a)))
  ("float-to-text", [VFloat a]) -> pure (VText (Text.pack (show a)))
  ("unicode-to-integer", [VUnicode codePoint]) -> pure (VInteger (toInteger (ord codePoint)))
  ("integer-to-unicode", [VInteger value]) -> integerToUnicode value
  ("equal-integer", [VInteger a, VInteger b]) -> pure (runtimeBool (a == b))
  ("behead-text", [VText value]) -> pure (runtimeTextBehead value)
  ("text-to-list", [VText value]) -> pure (runtimeList (map VUnicode (Text.unpack value)))
  ("list-to-text", [value]) -> runtimeListToText value
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
  ("÷", [VFloat a, VFloat b]) -> pure (VFloat (a / b))
  ("%", [VInteger a, VInteger b]) -> evalIntegerDivideRemainder a b
  ("≡", [a, b]) -> pure (runtimeBool (a == b))
  ("≤", [VInteger a, VInteger b]) -> pure (runtimeBool (a <= b))
  ("≤", [VFloat a, VFloat b]) -> pure (runtimeBool (a <= b))
  ("from-text", [VText text]) -> floatFromText text
  ("to-text", [value]) -> pure (VText (Text.pack (showRuntimeValue value)))
  _ -> runtimeError ("native '" ++ name ++ "' doth not support these arguments")

evalEffectOp :: SymbolId -> SymbolId -> [RuntimeValue] -> Eval RuntimeValue
evalEffectOp effect operation args = case (effect, operation, args) of
  (SymbolId RuntimeModule "console", SymbolId RuntimeModule "write", [VText text]) -> writeConsole text
  (SymbolId RuntimeModule "console", SymbolId RuntimeModule "read", [VData identity []]) | identity == onlyConstructorId -> VText <$> liftIO readConsoleLine
  (SymbolId RuntimeModule "random", SymbolId RuntimeModule "random", [VData identity []]) | identity == onlyConstructorId -> VFloat <$> liftIO nextRandomFloat
  (SymbolId RuntimeModule "async", SymbolId RuntimeModule "sleep", [VInteger ms]) -> sleepMilliseconds ms
  (SymbolId RuntimeModule "async", SymbolId RuntimeModule "fork", [work]) -> forkConcurrent work
  (SymbolId RuntimeModule "async", SymbolId RuntimeModule "wait", [VTask task]) -> waitTask task
  (SymbolId RuntimeModule "async", SymbolId RuntimeModule "fordo", [VTask task]) -> cancelTask task
  (SymbolId RuntimeModule "async", SymbolId RuntimeModule "wait-for", [VTask task, VInteger milliseconds]) -> waitForTask task milliseconds
  (SymbolId RuntimeModule "file", SymbolId RuntimeModule "read-file", [VText path]) -> ioRuntime (VText <$> TextIO.readFile (Text.unpack path))
  (SymbolId RuntimeModule "file", SymbolId RuntimeModule "write-file", [VText path, VText text]) -> ioRuntime (TextIO.writeFile (Text.unpack path) text >> pure runtimeOnly)
  (SymbolId RuntimeModule "file", SymbolId RuntimeModule "append-file", [VText path, VText text]) -> ioRuntime (TextIO.appendFile (Text.unpack path) text >> pure runtimeOnly)
  (SymbolId RuntimeModule "system", SymbolId RuntimeModule "arguments", [VData identity []]) | identity == onlyConstructorId -> runtimeList . map (VText . Text.pack) <$> readHostArguments
  (SymbolId RuntimeModule "system", SymbolId RuntimeModule "environment", [VText name]) -> runtimeOption . fmap (VText . Text.pack) <$> liftIO (Environment.lookupEnv (Text.unpack name))
  (SymbolId RuntimeModule "system", SymbolId RuntimeModule "exit", [VInteger status]) -> liftIO (exitWith (if status == 0 then ExitSuccess else ExitFailure (boundedInt status)))
  (SymbolId RuntimeModule "clock", SymbolId RuntimeModule "unix-time", [VData identity []]) | identity == onlyConstructorId -> VInteger . floor <$> liftIO getPOSIXTime
  (SymbolId RuntimeModule "process", SymbolId RuntimeModule "run-process", [VText command, arguments]) -> runProcessRuntime command arguments
  (SymbolId RuntimeModule "web", SymbolId RuntimeModule "serve", [VInteger port, handler]) -> serveWeb port handler
  _ -> performRuntime effect operation args

readHostArguments :: Eval [String]
readHostArguments = Eval (pure . RuntimeOk . hostArguments)

failRuntime :: String -> Eval RuntimeValue
failRuntime message = performRuntime (runtimeSymbol "fail") (runtimeSymbol "fail") [VText (Text.pack message)]

integerToUnicode :: Integer -> Eval RuntimeValue
integerToUnicode value = maybe (failRuntime "invalid unicode scalar value") (pure . VUnicode) (unicodeScalar value)

ioRuntime :: IO RuntimeValue -> Eval RuntimeValue
ioRuntime action = do
  result <- liftIO (try action)
  case (result :: Either IOException RuntimeValue) of
    Left err -> failRuntime (show err)
    Right value -> pure value

runProcessRuntime :: Text.Text -> RuntimeValue -> Eval RuntimeValue
runProcessRuntime command arguments = do
  texts <- runtimeListOf "run-process" "text" runtimeText arguments
  ioRuntime do
    (status, output, errors) <- Process.readProcessWithExitCode (Text.unpack command) (map Text.unpack texts) ""
    pure
      ( VData
          processResultConstructorId
          [VInteger (exitStatus status), VText (Text.pack output), VText (Text.pack errors)]
      )

serveWeb :: Integer -> RuntimeValue -> Eval RuntimeValue
serveWeb port handler
  | port < 1 || port > 65535 = failRuntime "web port must be between 1 and 65535"
  | otherwise = Eval $ \host -> do
      outcome <- tryIOException (Web.serve port (handleWebRequest host handler))
      case outcome of
        Left exception -> runEval (failRuntime (displayException exception)) host
        Right result -> pure result

handleWebRequest :: RuntimeHost -> RuntimeValue -> Web.Request -> IO (Either (RuntimeResult RuntimeValue) Web.Response)
handleWebRequest host handler request =
  runEval (applyOne handler (runtimeWebRequest request)) host >>= \case
    RuntimeOk value -> pure case runtimeWebResponse value of
      Left message -> Left (RuntimeErr (RuntimeError message))
      Right response -> Right response
    result -> pure (Left result)

runtimeWebRequest :: Web.Request -> RuntimeValue
runtimeWebRequest request =
  VData
    requestConstructorId
    [ VText (Web.requestMethod request)
    , VText (Web.requestTarget request)
    , runtimeWebHeaderTable (Web.requestHeaders request)
    , VText (Web.requestBody request)
    ]

runtimeWebResponse :: RuntimeValue -> Either String Web.Response
runtimeWebResponse = \case
  VData identity [VInteger status, headers, VText body]
    | identity /= responseConstructorId -> Left "web handler returned an invalid response"
    | status < 100 || status > 599 -> Left "web response status must be between 100 and 599"
    | otherwise -> Web.Response status <$> runtimeWebHeaderTableValues headers <*> pure body
  _ -> Left "web handler returned an invalid response"

runtimeWebHeaderTableValues :: RuntimeValue -> Either String [(Text.Text, Text.Text)]
runtimeWebHeaderTableValues = \case
  VData identity [entries]
    | identity == tableConstructorId -> runtimeWebHeaderList entries
  _ -> Left "web response containeþ an invalid header table"

runtimeWebHeaderList :: RuntimeValue -> Either String [(Text.Text, Text.Text)]
runtimeWebHeaderList value = case runtimeListValues value of
  Nothing -> Left "web response containeþ an invalid header list"
  Just headers -> traverse runtimeWebHeader headers

runtimeWebHeader :: RuntimeValue -> Either String (Text.Text, Text.Text)
runtimeWebHeader = \case
  VData identity [VText name, VText value]
    | identity /= productConstructorId -> Left "web response containeþ a non-text header"
    | Web.validHeaderName name && Web.validHeaderValue value -> Right (name, value)
    | otherwise -> Left "web response containeþ an invalid header"
  _ -> Left "web response containeþ a non-text header"

runtimeWebHeaderTable :: [(Text.Text, Text.Text)] -> RuntimeValue
runtimeWebHeaderTable headers =
  VData
    tableConstructorId
    [runtimeList [runtimeProduct (VText name) (VText value) | (name, value) <- headers]]

tryIOException :: IO a -> IO (Either IOException a)
tryIOException = try

exitStatus :: ExitCode -> Integer
exitStatus ExitSuccess = 0
exitStatus (ExitFailure status) = toInteger status

writeConsole :: Text.Text -> Eval RuntimeValue
writeConsole text = liftIO (TextIO.putStr text >> hFlush stdout) >> pure runtimeOnly

readConsoleLine :: IO Text.Text
readConsoleLine = hFlush stdout >> TextIO.getLine

sleepMilliseconds :: Integer -> Eval RuntimeValue
sleepMilliseconds ms = liftIO (sleepMicroseconds (max 0 ms * 1000)) >> pure runtimeOnly

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
          finishTask host True task =<< runTaskWith restore host work
        readMVar started
        putMVar runtimeTaskThread (Just thread)
      else do
        putMVar runtimeTaskThread Nothing
        finishTask host False task =<< runTask host work
    pure task
  pure (RuntimeOk (VTask task))

runTask :: RuntimeHost -> RuntimeValue -> IO TaskResult
runTask = runTaskWith id

runTaskWith :: (IO (RuntimeResult RuntimeValue) -> IO (RuntimeResult RuntimeValue)) -> RuntimeHost -> RuntimeValue -> IO TaskResult
runTaskWith restore host work = do
  outcome <- try (restore (runEval (applyOne work runtimeOnly) host))
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
cancelTask task = liftIO (cancelTaskIO task) >> pure runtimeOnly

cancelTaskIO :: RuntimeTask -> IO ()
cancelTaskIO task = readMVar (runtimeTaskThread task) >>= mapM_ killThread

taskResult :: TaskResult -> Eval RuntimeValue
taskResult = \case
  TaskFinished result -> Eval (const (pure result))
  TaskCancelled -> failRuntime "task cancelled"
  TaskCrashed message -> failRuntime ("task failed: " ++ message)

boundedMicroseconds :: Integer -> Int
boundedMicroseconds milliseconds = fromInteger (min (toInteger (maxBound :: Int)) (max 0 milliseconds * 1000))

runtimeOnly :: RuntimeValue
runtimeOnly = VData onlyConstructorId []

runtimeList :: [RuntimeValue] -> RuntimeValue
runtimeList = foldr (\value rest -> VData consConstructorId [value, rest]) (VData emptyConstructorId [])

runtimeOption :: Maybe RuntimeValue -> RuntimeValue
runtimeOption = maybe (VData noneConstructorId []) (\value -> VData someConstructorId [value])

runtimeProduct :: RuntimeValue -> RuntimeValue -> RuntimeValue
runtimeProduct left right = VData productConstructorId [left, right]

runtimeTextBehead :: Text.Text -> RuntimeValue
runtimeTextBehead value = runtimeOption do
  (codePoint, rest) <- Text.uncons value
  pure (runtimeProduct (VUnicode codePoint) (VText rest))

runtimeListToText :: RuntimeValue -> Eval RuntimeValue
runtimeListToText value = VText . Text.pack <$> runtimeListOf "list-to-text" "unicode" runtimeUnicode value

runtimeListOf :: String -> String -> (RuntimeValue -> Maybe a) -> RuntimeValue -> Eval [a]
runtimeListOf native element project value =
  maybe (runtimeError ("native '" ++ native ++ "' received an invalid " ++ element ++ " list")) pure (runtimeListValues value >>= traverse project)

runtimeListValues :: RuntimeValue -> Maybe [RuntimeValue]
runtimeListValues = \case
  VData identity [] | identity == emptyConstructorId -> Just []
  VData identity [value, rest] | identity == consConstructorId -> (value :) <$> runtimeListValues rest
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

evalFloatFloor :: Double -> Eval RuntimeValue
evalFloatFloor value
  | isNaN value || isInfinite value = failRuntime "cannot floor a non-finite float"
  | otherwise = pure (VInteger (floor value))

floatFromText :: Text.Text -> Eval RuntimeValue
floatFromText text = case (readMaybe (Text.unpack text) :: Maybe Double) of
  Just x -> pure (VFloat x)
  Nothing -> failRuntime "could not parse float"

runtimeBool :: Bool -> RuntimeValue
runtimeBool True = VData yeaConstructorId []
runtimeBool False = VData nayConstructorId []

addConstructors :: [TermExport] -> RuntimeEnv -> RuntimeEnv
addConstructors constructors env = foldl' step env constructors
 where
  step current term@TermExport{termExportTarget = identity, termExportKind = ConstructorTerm arity} =
    let name = lastQualifiedSegment (symbolName identity)
        value = if arity == 0 then VData identity [] else callableValue ("<constructor " ++ name ++ ">") arity (pure . VData identity)
     in addGlobalValue term value current
  step current _ = current

addEffectOps :: [TermExport] -> RuntimeEnv -> RuntimeEnv
addEffectOps ops env = foldl' step env ops
 where
  step current term@TermExport{termExportTarget = operation, termExportKind = EffectOperationTerm effect arity} =
    let name = lastQualifiedSegment (symbolName operation)
        value = callableValue ("<effect " ++ name ++ ">") arity (evalEffectOp effect operation)
     in addGlobalValue term value current
  step current _ = current

baseRuntimeEnv :: RuntimeEnv
baseRuntimeEnv = foldl' add base hostBindings
 where
  base = RuntimeEnv{runtimeGlobals = Map.empty, runtimeLocals = Map.empty, runtimeFills = Map.empty}
  add current binding = case hostRole binding of
    BaseNative -> addGlobalValue (TermExport (runtimeSymbol name) OrdinaryTerm) (nativeValue name arity) current
    BaseEffect effectName ->
      let effect = runtimeSymbol effectName
          operation = runtimeSymbol name
          term = TermExport operation (EffectOperationTerm effect arity)
       in addGlobalValue term (callableValue ("<effect " ++ name ++ ">") arity (evalEffectOp effect operation)) current
    _ -> current
   where
    name = hostName binding
    arity = hostArity binding

addGlobalValue :: TermExport -> RuntimeValue -> RuntimeEnv -> RuntimeEnv
addGlobalValue term value env@RuntimeEnv{runtimeGlobals} = env{runtimeGlobals = Map.insert term value runtimeGlobals}

addLocalValue :: LocalId -> RuntimeValue -> RuntimeEnv -> RuntimeEnv
addLocalValue local value env@RuntimeEnv{runtimeLocals} = env{runtimeLocals = Map.insert local value runtimeLocals}

lookupGlobalValue :: TermExport -> RuntimeEnv -> Maybe RuntimeValue
lookupGlobalValue term RuntimeEnv{runtimeGlobals} = Map.lookup term runtimeGlobals

runtimeSymbol :: String -> SymbolId
runtimeSymbol = SymbolId RuntimeModule

showUnhandledOperation :: SymbolId -> SymbolId -> String
showUnhandledOperation effect operation =
  "unhandled effect '" ++ renderEffectId effect ++ "' operation '" ++ lastQualifiedSegment (symbolName operation) ++ "'"

showRuntimeValue :: RuntimeValue -> String
showRuntimeValue = \case
  VUnit -> "()"
  VInteger i -> show i
  VFloat value -> show value
  VUnicode codePoint -> "`" ++ escapeUnicodeCodePoint codePoint
  VText s -> "'" ++ concatMap escapeTextCharacter (Text.unpack s) ++ "'"
  VRecord fields -> "r(" ++ showRuntimeFields fields ++ ")"
  VData identity [] -> lastQualifiedSegment (symbolName identity)
  VData identity args -> "(" ++ showRuntimeValues args ++ " " ++ lastQualifiedSegment (symbolName identity) ++ ")"
  VCallable display _ _ _ -> display
  VTask{} -> "<task>"
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
