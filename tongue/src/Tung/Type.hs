{- | static semantics: imports and visibility, Hindley-Milner inference,
effect rows, coverage, shapes and fills, and elaboration to dictionary passing.
-}
module Tung.Type (
  Ty (..),
  Scheme (..),
  EnvLookup (..),
  TcContext (..),
  TcState (..),
  TcResult (..),
  TypeFailure (..),
  ShapeInfo (..),
  check,
  checkWithImports,
  checkEditorWithImports,
  checkEditorProgramWithImportsDetailed,
  checkRunnableWithImports,
  elaborateProgramWithImports,
  elaborateInteractiveProgramWithImports,
  typeOfWithImports,
  baseContext,
  inferProgramContext,
  lookupEnv,
  canonicalTypeName,
  showTy,
  showEffects,
  namespaceImportShapes,
  baseRuntimeNames,
  baseEffectNames,
  runtimeNativeSpecs,
  runtimeEffectOpSpecs,
  findShapeMember,
  fillKeyFor,
)
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, replicateM, unless, void, zipWithM, zipWithM_)
import Control.Monad.Trans.State.Strict (StateT (..), get, put, runStateT)
import Data.Bifunctor (bimap)
import Data.Char (isAscii, isDigit, isLower)
import Data.IntMap.Strict qualified as IntMap
import Data.List (find, intercalate, isPrefixOf, nub, partition)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Traversable (mapAccumM)
import Tung.Core (CoreProgram, makeCoreProgram)
import Tung.Coverage qualified as Coverage
import Tung.Import (ImportStack, enterImport)
import Tung.Name (constructorNamesMatch, importNamespace, isQualifiedName, lastQualifiedSegment, splitFieldAccessName)
import Tung.Parse (parse)
import Tung.Primitive
import Tung.Syntax
import Tung.Token (SourceSpan)
import Tung.Validate (validateProgram)

-- function effects are latent on 'TyFun'; 'Inferred' carrieth only effects caused
-- while evaluating the current expression, plus unresolved shape requirements.
data Ty
  = TyMeta Int
  | TyVar String
  | TyCon String
  | TyApp String [Ty]
  | TyRecord (Map.Map String Ty)
  | TyFun (NonEmpty Ty) [Ty] Ty
  deriving (Eq, Show)

data Need = Need [Ty] String deriving (Eq, Show)

data Inferred = Inferred
  { inferredTy :: Ty
  , inferredEffects :: [Ty]
  , inferredNeeds :: [Need]
  , inferredExpr :: Expr
  }
  deriving (Eq, Show)

data Scheme
  = Forall [String] [Need] Ty
  | EffectOpForall String [String] [Need] Ty
  deriving (Eq, Show)

data EnvLookup = EnvFound Scheme | EnvMissing | EnvAmbiguous String deriving (Eq, Show)

data EnvChoices = EnvChoices [Scheme] | ChoicesMissing | ChoicesAmbiguous String deriving (Eq, Show)

data TcResult a = TcOk a TcState | TcErr String deriving (Eq, Show)

data TypeFailure = TypeFailure
  { typeFailureMessage :: String
  , typeFailureSpan :: Maybe SourceSpan
  }
  deriving (Eq, Show)

-- these modes share inference but impose different top-level computation bounds.
data ProgramMode = Bookhoard | Interactive | Runnable

data TyHead = TyHead String [Ty] deriving (Eq, Show)

-- 'TcContext' is lexical knowledge, while 'TcState' holds substitutions,
-- open effect rows, import results, and evidence holes shared by one check.
data TcState = TcState
  { tcNextMeta :: !Int
  , tcMetaSubst :: !(IntMap.IntMap Ty)
  , tcTypeHeads :: !(Map.Map String TyHead)
  , tcEffectRows :: !(Map.Map String [Ty])
  , tcImportCache :: !(Map.Map String TcContext)
  , tcEvidenceNeeds :: !(IntMap.IntMap Need)
  , tcCurrentSpan :: !(Maybe SourceSpan)
  }
  deriving (Eq, Show)

-- primitive entries exist only when a module explicitly showeth a built-in type;
-- unlike aliases, they remain opaque during unification.
data TypeInfo
  = Primitive String
  | Alias String [String] Ty
  | DataInfo String [String] [(String, [TypeExpr])]
  | EffectInfo String [String]
  | ShownType TypeInfo
  deriving (Eq, Show)

data ShapeInfo = ShapeInfo
  { shapeParams :: [String]
  , shapeNeeds :: [ShapeNeed]
  , shapeMembers :: [ShapeMember]
  , shapeExported :: Bool
  }
  deriving (Eq, Show)

data FillInfo = FillInfo
  { fillShape :: String
  , fillTypes :: [TypeExpr]
  , fillNeeds :: [ShapeNeed]
  , fillOrigin :: String
  , fillKey :: String
  , fillDirect :: Bool
  , fillPrimitive :: Bool
  , fillDepth :: Int
  , fillParents :: [ShapeNeed]
  , fillMembers :: [String]
  }
  deriving (Eq, Show)

data TcContext = TcContext
  { tcEnv :: [(String, Scheme, Int, String)]
  , tcTypes :: Map.Map String TypeInfo
  , tcTypeAmbiguities :: Map.Map String [String]
  , tcShapes :: Map.Map String ShapeInfo
  , tcFills :: Map.Map String [FillInfo]
  }
  deriving (Eq, Show)

newtype Tc a = Tc {unTc :: StateT TcState (Either TypeFailure) a}
  deriving newtype (Functor, Applicative, Monad)

initialTcState :: TcState
initialTcState =
  TcState
    { tcNextMeta = 0
    , tcMetaSubst = IntMap.empty
    , tcTypeHeads = Map.empty
    , tcEffectRows = Map.empty
    , tcImportCache = Map.empty
    , tcEvidenceNeeds = IntMap.empty
    , tcCurrentSpan = Nothing
    }

runTc :: Tc a -> TcState -> TcResult a
runTc computation st = case runStateT (unTc computation) st of
  Left failure -> TcErr (typeFailureMessage failure)
  Right (value, st2) -> TcOk value st2

failTc :: String -> Tc a
failTc message = Tc $ StateT $ \state -> Left (TypeFailure message (tcCurrentSpan state))

getTc :: Tc TcState
getTc = Tc get

putTc :: TcState -> Tc ()
putTc = Tc . put

mapTcError :: Tc a -> (String -> String) -> Tc a
mapTcError computation f = Tc $ StateT $ \st -> case runStateT (unTc computation) st of
  Left failure -> Left failure{typeFailureMessage = f (typeFailureMessage failure)}
  ok -> ok

recoverTc :: Tc a -> (String -> Tc a) -> Tc a
recoverTc computation f = Tc $ StateT $ \st -> case runStateT (unTc computation) st of
  Left failure -> runStateT (unTc (f (typeFailureMessage failure))) st
  ok -> ok

succeedsTc :: Tc a -> Tc Bool
succeedsTc computation = Tc $ StateT $ \st ->
  Right (either (const False) (const True) (runStateT (unTc computation) st), st)

withTcSpan :: SourceSpan -> Tc a -> Tc a
withTcSpan span computation = Tc $ StateT $ \state ->
  case runStateT (unTc computation) state{tcCurrentSpan = Just span} of
    Left failure -> Left failure
    Right (value, state2) -> Right (value, state2{tcCurrentSpan = tcCurrentSpan state})

curriedFunction :: [Ty] -> [Ty] -> Ty -> Ty
curriedFunction args effects ret =
  maybe ret (\domain -> TyFun domain effects ret) (NE.nonEmpty args)

mapTyChildren :: (Ty -> Ty) -> (Ty -> Ty) -> Ty -> Ty
mapTyChildren mapTy mapEffect = \case
  TyApp name args -> TyApp name (map mapTy args)
  TyRecord fields -> TyRecord (Map.map mapTy fields)
  TyFun args effects ret -> TyFun (fmap mapTy args) (map mapEffect effects) (mapTy ret)
  ty -> ty

skipEffectVar :: (Ty -> Ty) -> Ty -> Ty
skipEffectVar f ty = if isEffectVarTy ty then ty else f ty

applyStateEffects :: [Ty] -> TcState -> [Ty]
applyStateEffects effects st = foldl' (flip addEffect) [] (concatMap (`applyStateEffect` st) effects)

applyStateEffect :: Ty -> TcState -> [Ty]
applyStateEffect (TyVar name) st@TcState{tcEffectRows}
  | isEffectVarName name = maybe [TyVar name] (`applyStateEffects` st) (Map.lookup name tcEffectRows)
applyStateEffect effect st = [applyState effect st]

applyStateNeed :: Need -> TcState -> Need
applyStateNeed (Need args name) st = Need (applyStateAll args st) name

applyStateNeeds :: [Need] -> TcState -> [Need]
applyStateNeeds needs st = map (`applyStateNeed` st) needs

unionEffects :: [Ty] -> [Ty] -> [Ty]
unionEffects = foldl' (flip addEffect)

removeEffect :: String -> [Ty] -> [Ty]
removeEffect name = filter (not . effectHasName name)

addEffect :: Ty -> [Ty] -> [Ty]
addEffect = addUnique

unionNeeds :: [Need] -> [Need] -> [Need]
unionNeeds = foldl' (flip addNeed)

addNeed :: Need -> [Need] -> [Need]
addNeed = addUnique

-- effect rows are unordered sets represented as lists. a row variable may absorb
-- missing labels, but concrete effects with the same head must still unify.
unifyEffectsM :: [Ty] -> [Ty] -> Tc ()
unifyEffectsM xs0 ys0 = do
  st <- getTc
  let xs = applyStateEffects xs0 st
      ys = applyStateEffects ys0 st
      xLabels = concreteEffects xs
      yLabels = concreteEffects ys
      xVars = effectVars xs
      yVars = effectVars ys
      xMissing = missingEffects xLabels yLabels
      yMissing = missingEffects yLabels xLabels
  unifyCommonEffectsM xLabels yLabels
  case (xVars, yVars) of
    ([], []) -> unless (null xMissing && null yMissing) mismatch
    (x : _, []) -> unless (null xMissing) mismatch >> bindEffectVarM x yMissing
    ([], y : _) -> unless (null yMissing) mismatch >> bindEffectVarM y xMissing
    (x : _, y : _)
      | x == y -> bindEffectVarM x (unionEffects xMissing yMissing)
      | otherwise -> bindEffectVarM x yMissing >> bindEffectVarM y xMissing
 where
  mismatch = failTc ("cannot unify effects {" ++ showEffects xs0 ++ "} with {" ++ showEffects ys0 ++ "}")

concreteEffects :: [Ty] -> [Ty]
concreteEffects = filter (not . isEffectVarTy)

effectVars :: [Ty] -> [String]
effectVars effects = nub [name | TyVar name <- effects, isEffectVarName name]

missingEffects :: [Ty] -> [Ty] -> [Ty]
missingEffects xs ys = filter (not . (`containsEffectHead` ys)) xs

bindEffectVarM :: String -> [Ty] -> Tc ()
bindEffectVarM name effects = do
  st@TcState{tcEffectRows} <- getTc
  let normalized = filter (/= TyVar name) (applyStateEffects effects st)
  if any (effectVarOccurs name) normalized
    then failTc "recursive effect"
    else case Map.lookup name tcEffectRows of
      Just existing -> unifyEffectsM existing normalized
      Nothing -> putTc st{tcEffectRows = Map.insert name normalized tcEffectRows}

effectVarOccurs :: String -> Ty -> Bool
effectVarOccurs name = \case
  TyVar found -> name == found
  TyApp _ args -> any (effectVarOccurs name) args
  TyRecord fields -> any (effectVarOccurs name) (Map.elems fields)
  TyFun args effects ret -> any (effectVarOccurs name) args || any (effectVarOccurs name) effects || effectVarOccurs name ret
  _ -> False

isEffectVarTy :: Ty -> Bool
isEffectVarTy (TyVar name) = isEffectVarName name
isEffectVarTy _ = False

isEffectVarName :: String -> Bool
isEffectVarName "e" = True
isEffectVarName ('e' : rest) = all isDigit rest
isEffectVarName _ = False

unifyCommonEffectsM :: [Ty] -> [Ty] -> Tc ()
unifyCommonEffectsM xs ys =
  mapM_ (\x -> maybe (pure ()) (unifyEffectM x) (findEffectByHead x ys)) xs

unifyEffectM :: Ty -> Ty -> Tc ()
unifyEffectM x y = case (effectNameAndArgs x, effectNameAndArgs y) of
  (Just (xn, xs), Just (yn, ys)) | xn == yn -> unifyListsM xs ys
  _ -> failTc ("cannot unify effect " ++ showEffect x ++ " with " ++ showEffect y)

findEffectByHead :: Ty -> [Ty] -> Maybe Ty
findEffectByHead x = find (sameEffectHead x)

containsEffectHead :: Ty -> [Ty] -> Bool
containsEffectHead x = any (sameEffectHead x)

sameEffectHead :: Ty -> Ty -> Bool
sameEffectHead x y = case (effectNameAndArgs x, effectNameAndArgs y) of
  (Just (xn, _), Just (yn, _)) -> xn == yn
  _ -> False

effectHasName :: String -> Ty -> Bool
effectHasName name effect = case effectNameAndArgs effect of
  Just (effectName, _) -> name == effectName
  Nothing -> False

effectNameAndArgs :: Ty -> Maybe (String, [Ty])
effectNameAndArgs (TyApp name args) = Just (name, args)
effectNameAndArgs (TyCon name) = Just (name, [])
effectNameAndArgs _ = Nothing

showEffects :: [Ty] -> String
showEffects = intercalate ", " . map showEffect

showEffect :: Ty -> String
showEffect (TyVar name) = name
showEffect (TyApp name []) = name
showEffect (TyApp name args) = showTyList args ++ " " ++ name
showEffect other = showTy other

-- ordinary unification also handleth higher-kinded type heads, closed records,
-- non-empty function domains, and latent effect rows.
unifyM :: Ty -> Ty -> Tc ()
unifyM a b = do
  st <- getTc
  let aa = normalizeFun (applyState a st)
      bb = normalizeFun (applyState b st)
  case (aa, bb) of
    (TyMeta x, TyMeta y) | x == y -> pure ()
    (TyMeta x, t) -> bindMetaM x t
    (t, TyMeta x) -> bindMetaM x t
    (TyVar x, TyVar y) | x == y -> pure ()
    (TyCon x, TyCon y) | x == y -> pure ()
    (TyApp x xs, TyApp y ys) | x == y -> unifyListsM xs ys
    (TyApp x xs, TyApp y ys) | isTypeVarName x -> unifyTypeHeadM x y xs ys
    (TyApp x xs, TyApp y ys) | isTypeVarName y -> unifyTypeHeadM y x ys xs
    (TyRecord xs, TyRecord ys) -> unifyRecordFieldsM xs ys
    (TyApp x xs, TyFun ys yEffs y) | isTypeVarName x -> unifyTypeHeadWithFunM x xs ys yEffs y
    (TyFun xs xEffs x, TyApp y ys) | isTypeVarName y -> unifyTypeHeadWithFunM y ys xs xEffs x
    (TyFun xs xEffs x, TyFun ys yEffs y) -> unifyFunM xs xEffs x ys yEffs y
    _ -> failTc ("cannot unify " ++ showTy aa ++ " with " ++ showTy bb)

unifyFunM :: NonEmpty Ty -> [Ty] -> Ty -> NonEmpty Ty -> [Ty] -> Ty -> Tc ()
unifyFunM xs xEffs xRet ys yEffs yRet =
  case compare (length xArgs) (length yArgs) of
    EQ -> unifyEffectsM xEffs yEffs >> unifyListsM xArgs yArgs >> unifyM xRet yRet
    LT -> do
      unifyEffectsM xEffs []
      let (prefix, suffix) = splitAt (length xArgs) yArgs
      unifyListsM xArgs prefix
      suffixArgs <- maybe (failTc "internal empty function suffix") pure (NE.nonEmpty suffix)
      unifyM xRet (TyFun suffixArgs yEffs yRet)
    GT -> do
      unifyEffectsM yEffs []
      let (prefix, suffix) = splitAt (length yArgs) xArgs
      unifyListsM prefix yArgs
      suffixArgs <- maybe (failTc "internal empty function suffix") pure (NE.nonEmpty suffix)
      unifyM (TyFun suffixArgs xEffs xRet) yRet
 where
  xArgs = NE.toList xs
  yArgs = NE.toList ys

normalizeFun :: Ty -> Ty
normalizeFun (TyFun args effects ret) = case normalizeFun ret of
  TyFun moreArgs moreEffects finalRet | null effects -> TyFun (args <> moreArgs) moreEffects finalRet
  other -> TyFun args effects other
normalizeFun (TyApp name args) = case tyAppOrFunc name (map normalizeFun args) of
  TyApp appName appArgs -> TyApp appName appArgs
  funTy -> normalizeFun funTy
normalizeFun (TyRecord fields) = TyRecord (Map.map normalizeFun fields)
normalizeFun ty = ty

unifyListsM :: [Ty] -> [Ty] -> Tc ()
unifyListsM xs ys
  | length xs == length ys = zipWithM_ unifyM xs ys
  | otherwise = failTc ("arity mismatch: [" ++ showTyList xs ++ "] vs [" ++ showTyList ys ++ "]")

unifyTypeHeadM :: String -> String -> [Ty] -> [Ty] -> Tc ()
unifyTypeHeadM varName actualName varArgs actualArgs = do
  st <- getTc
  case lookupTypeHead varName st of
    Just boundHead -> unifyM (applyTypeHead boundHead varArgs) (tyAppOrFunc actualName actualArgs)
    Nothing | isTypeVarName actualName -> unifyListsM varArgs actualArgs
    Nothing
      | length actualArgs >= length varArgs -> do
          let (captured, supplied) = splitAt (length actualArgs - length varArgs) actualArgs
          bindTypeHeadM varName (TyHead actualName captured)
          unifyListsM varArgs supplied
      | otherwise -> unifyListsM varArgs actualArgs

unifyTypeHeadWithFunM :: String -> [Ty] -> NonEmpty Ty -> [Ty] -> Ty -> Tc ()
unifyTypeHeadWithFunM varName varArgs funArgs effects ret
  | not (null effects) = failTc "effectful functions cannot be used as func"
  | otherwise = case NE.toList funArgs of
      [] -> failTc "internal error: function type with no arguments"
      arg : rest -> do
        let result = curriedFunction rest [] ret
            fullFun = TyFun (arg :| rest) [] ret
        st <- getTc
        case lookupTypeHead varName st of
          Just boundHead -> unifyM (applyTypeHead boundHead varArgs) fullFun
          Nothing -> case varArgs of
            [onlyResult] -> bindTypeHeadM varName (TyHead "func" [arg]) >> unifyM onlyResult result
            [fromArg, toArg] -> bindTypeHeadM varName (TyHead "func" []) >> unifyM fromArg arg >> unifyM toArg result
            _ -> failTc "func expecteth one or two type arguments in function unification"

lookupTypeHead :: String -> TcState -> Maybe TyHead
lookupTypeHead name TcState{tcTypeHeads} = Map.lookup name tcTypeHeads

bindTypeHeadM :: String -> TyHead -> Tc ()
bindTypeHeadM name value = do
  st@TcState{tcTypeHeads} <- getTc
  putTc st{tcTypeHeads = Map.insert name value tcTypeHeads}

applyTypeHead :: TyHead -> [Ty] -> Ty
applyTypeHead (TyHead name prefixArgs) args = tyAppOrFunc name (prefixArgs ++ args)

tyAppOrFunc :: String -> [Ty] -> Ty
tyAppOrFunc name [] = TyCon name
tyAppOrFunc "func" [arg, ret] = TyFun (arg :| []) [] ret
tyAppOrFunc name args = TyApp name args

unifyRecordFieldsM :: Map.Map String Ty -> Map.Map String Ty -> Tc ()
unifyRecordFieldsM xs ys
  | Map.size xs /= Map.size ys = failTc "record arity mismatch"
  | otherwise = mapM_ one (Map.toList xs)
 where
  one (name, xTy) = case Map.lookup name ys of
    Nothing -> failTc ("record field mismatch '" ++ name ++ "'")
    Just yTy -> unifyM xTy yTy

bindMetaM :: Int -> Ty -> Tc ()
bindMetaM ident ty = do
  st@TcState{tcMetaSubst} <- getTc
  if occursMeta ident ty st
    then failTc "recursive type"
    else putTc st{tcMetaSubst = IntMap.insert ident ty tcMetaSubst}

occursMeta :: Int -> Ty -> TcState -> Bool
occursMeta ident ty st = case applyState ty st of
  TyMeta found -> found == ident
  TyVar _ -> False
  TyCon _ -> False
  TyApp _ args -> any (\child -> occursMeta ident child st) args
  TyRecord fields -> any (\field -> occursMeta ident field st) fields
  TyFun args effects ret -> any (\arg -> occursMeta ident arg st) args || any (\effect -> occursMeta ident effect st) (concreteEffects effects) || occursMeta ident ret st

applyState :: Ty -> TcState -> Ty
applyState ty st@TcState{tcMetaSubst, tcTypeHeads} = go ty
 where
  go = \case
    TyMeta ident -> maybe (TyMeta ident) go (IntMap.lookup ident tcMetaSubst)
    TyVar name -> maybe (TyVar name) (`applyTypeHead` []) (Map.lookup name tcTypeHeads)
    TyCon name -> TyCon name
    TyApp name args -> maybe (tyAppOrFunc name (map go args)) (\headSubst -> go (applyTypeHead headSubst (map go args))) (Map.lookup name tcTypeHeads)
    TyRecord fields -> TyRecord (Map.map go fields)
    TyFun args effects ret -> TyFun (fmap go args) (applyStateEffects effects st) (go ret)

applyStateAll :: [Ty] -> TcState -> [Ty]
applyStateAll tys st = map (`applyState` st) tys

freshM :: Tc Ty
freshM = do
  st@TcState{tcNextMeta} <- getTc
  putTc st{tcNextMeta = tcNextMeta + 1}
  pure (TyMeta tcNextMeta)

freshManyM :: Int -> Tc [Ty]
freshManyM n = replicateM n freshM

schemeEffectOwner :: Scheme -> Maybe String
schemeEffectOwner = \case
  EffectOpForall owner _ _ _ -> Just owner
  Forall{} -> Nothing

schemeParts :: Scheme -> ([String], [Need], Ty)
schemeParts = \case
  Forall vars needs ty -> (vars, needs, ty)
  EffectOpForall _ vars needs ty -> (vars, needs, ty)

markEffectOpScheme :: String -> Scheme -> Scheme
markEffectOpScheme owner = \case
  Forall vars needs ty -> EffectOpForall owner vars needs ty
  EffectOpForall _ vars needs ty -> EffectOpForall owner vars needs ty

instantiateM :: Scheme -> Tc (Ty, [Need])
instantiateM scheme = do
  let (vars, needs, ty) = schemeParts scheme
      appliedVars = filter (`elem` vars) (typeAppHeadsInNeeds needs (typeAppHeadsInTy ty []))
      headVars = nub (appliedVars ++ typeHeadVarsInNeeds needs (typeHeadVarsInTy ty []))
      rowVars = effectRowVarsInNeeds needs (effectRowVarsInTy ty [])
  repls <- freshForNamesM headVars rowVars vars
  pure (replaceVars ty repls, map (`replaceNeedVars` repls) needs)

freshForNamesM :: [String] -> [String] -> [String] -> Tc (Map.Map String Ty)
freshForNamesM headVars rowVars vars =
  Map.fromList <$> traverse freshPair vars
 where
  freshPair v
    | v `elem` rowVars = do
        name <- freshEffectVarNameM
        pure (v, TyVar name)
    | v `elem` headVars = do
        name <- freshTypeHeadNameM
        pure (v, TyVar name)
    | otherwise = do
        t <- freshM
        pure (v, t)

freshTypeHeadNameM :: Tc String
freshTypeHeadNameM = do
  st@TcState{tcNextMeta} <- getTc
  putTc st{tcNextMeta = tcNextMeta + 1}
  pure ("h" ++ show tcNextMeta)

freshEffectVarNameM :: Tc String
freshEffectVarNameM = do
  st@TcState{tcNextMeta} <- getTc
  putTc st{tcNextMeta = tcNextMeta + 1}
  pure ("e" ++ show tcNextMeta)

replaceVars :: Ty -> Map.Map String Ty -> Ty
replaceVars ty repls = case ty of
  TyVar v -> Map.findWithDefault ty v repls
  TyApp name args
    | Just (TyVar newName) <- Map.lookup name repls -> TyApp newName (map (`replaceVars` repls) args)
    | Just (TyCon newName) <- Map.lookup name repls -> TyApp newName (map (`replaceVars` repls) args)
    | otherwise -> TyApp name (map (`replaceVars` repls) args)
  _ -> mapTyChildren (`replaceVars` repls) (`replaceVars` repls) ty

replaceNeedVars :: Need -> Map.Map String Ty -> Need
replaceNeedVars (Need args name) repls = Need (map (`replaceVars` repls) args) name

typeAnnScheme :: TypeAnn -> TcContext -> Scheme
typeAnnScheme (TypeAnn t needs) ctx = generalizeTyWithNeeds (map (`convertShapeNeed` ctx) needs) (convertTypeExpr t ctx)

convertShapeNeed :: ShapeNeed -> TcContext -> Need
convertShapeNeed (ShapeNeed args name) ctx = Need (map (`convertTypeExpr` ctx) args) (canonicalShapeName name ctx)

canonicalShapeName :: String -> TcContext -> String
canonicalShapeName name TcContext{tcShapes}
  | isQualifiedName name = name
  | otherwise = case [key | key <- Map.keys tcShapes, isQualifiedName key, lastQualifiedSegment key == name] of
      [key] -> key
      _ -> name

convertTypeExpr :: TypeExpr -> TcContext -> Ty
convertTypeExpr t ctx = case t of
  TypeName name -> atomTypeName name ctx
  TypeApply name args -> atomAppliedTypeName name (map (`convertTypeExpr` ctx) args) ctx
  TypeRecord fields -> TyRecord (Map.fromList [(name, convertTypeExpr ty ctx) | (name, ty) <- fields])
  TypeArrow args effects ret -> TyFun (fmap (`convertTypeExpr` ctx) args) (map (`convertEffectExpr` ctx) effects) (convertTypeExpr ret ctx)

convertEffectExpr :: TypeExpr -> TcContext -> Ty
convertEffectExpr t ctx = case t of
  TypeName name | isEffectVarName name -> TyVar name
  TypeName name -> TyApp (fromMaybe name (canonicalEffectName name ctx)) []
  TypeApply name args -> TyApp (fromMaybe name (canonicalEffectName name ctx)) (map (`convertTypeExpr` ctx) args)
  _ -> convertTypeExpr t ctx

typeExprFromTy :: Ty -> TcState -> TypeExpr
typeExprFromTy ty st = typeExprFromAppliedTy (normalizeFun (applyState ty st))

shapeNeedsFromNeeds :: [Need] -> TcState -> [ShapeNeed]
shapeNeedsFromNeeds needs st = map needFromAppliedNeed (applyStateNeeds needs st)

needFromAppliedNeed :: Need -> ShapeNeed
needFromAppliedNeed (Need args name) = ShapeNeed (map typeExprFromAppliedTy args) name

typeExprFromAppliedTy :: Ty -> TypeExpr
typeExprFromAppliedTy = \case
  TyMeta ident -> TypeName ("t" ++ show ident)
  TyVar name -> TypeName name
  TyCon name -> TypeName name
  TyApp name args -> TypeApply name (map typeExprFromAppliedTy args)
  TyRecord fields -> TypeRecord [(name, typeExprFromAppliedTy fieldTy) | (name, fieldTy) <- Map.toList fields]
  TyFun args effects ret -> TypeArrow (fmap typeExprFromAppliedTy args) (map typeExprFromAppliedTy effects) (typeExprFromAppliedTy ret)

specializeShapeType :: [String] -> [TypeExpr] -> TypeExpr -> TypeExpr
specializeShapeType params fillTypes = replaceShapeParams (Map.fromList (zip params fillTypes))

specializeShapeTypeAnn :: [String] -> [TypeExpr] -> TypeAnn -> TypeAnn
specializeShapeTypeAnn params fillTypes (TypeAnn t needs) =
  TypeAnn (specializeShapeType params fillTypes t) (map (specializeShapeNeed params fillTypes) needs)

specializeShapeNeed :: [String] -> [TypeExpr] -> ShapeNeed -> ShapeNeed
specializeShapeNeed params fillTypes (ShapeNeed args name) =
  ShapeNeed (map (specializeShapeType params fillTypes) args) name

replaceShapeParams :: Map.Map String TypeExpr -> TypeExpr -> TypeExpr
replaceShapeParams repls t = case t of
  TypeName name -> Map.findWithDefault t name repls
  TypeApply name args ->
    let args2 = map (replaceShapeParams repls) args
     in maybe (TypeApply name args2) (`applyReplacementType` args2) (Map.lookup name repls)
  TypeRecord fields -> TypeRecord [(name, replaceShapeParams repls fieldTy) | (name, fieldTy) <- fields]
  TypeArrow args effects ret -> TypeArrow (fmap (replaceShapeParams repls) args) (map (replaceShapeParams repls) effects) (replaceShapeParams repls ret)

applyReplacementType :: TypeExpr -> [TypeExpr] -> TypeExpr
applyReplacementType replacement args = case replacement of
  TypeName name -> TypeApply name args
  TypeApply name existing -> TypeApply name (existing ++ args)
  _ -> replacement

atomTypeName :: String -> TcContext -> Ty
atomTypeName name ctx
  | name == "func" = TyCon "func"
  | isPrimitiveConcreteTypeName name = TyCon name
  | Just ([], target) <- aliasInfo name ctx = target
  | Just canonical <- canonicalTypeName name ctx = TyCon canonical
  | isQualifiedName name = TyCon name
  | otherwise = TyVar name

atomAppliedTypeName :: String -> [Ty] -> TcContext -> Ty
atomAppliedTypeName "func" args _ = tyAppOrFunc "func" args
atomAppliedTypeName name args ctx = case aliasInfo name ctx of
  Just (params, target)
    | length params == length args -> replaceVars target (Map.fromList (zip params args))
  _ -> maybe (TyApp name args) (`TyApp` args) (canonicalTypeName name ctx)

aliasInfo :: String -> TcContext -> Maybe ([String], Ty)
aliasInfo name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (Alias _ params target) -> Just (params, target)
  _ -> Nothing

isPrimitiveConcreteTypeName :: String -> Bool
isPrimitiveConcreteTypeName name = name `elem` primitiveTypeNames

canonicalTypeName :: String -> TcContext -> Maybe String
canonicalTypeName name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (Primitive canonical) -> Just canonical
  Just (Alias canonical _ _) -> Just canonical
  Just (DataInfo canonical _ _) -> Just canonical
  Just (EffectInfo _ _) -> Nothing
  Just (ShownType _) -> Nothing
  Nothing -> Nothing

canonicalEffectName :: String -> TcContext -> Maybe String
canonicalEffectName name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (EffectInfo canonical _) -> Just canonical
  _ -> Nothing

findTypeInfoForTypeSystem :: String -> TcContext -> Maybe TypeInfo
findTypeInfoForTypeSystem name TcContext{tcTypes} = Map.lookup name tcTypes

typeVarsInTy :: Ty -> [String] -> [String]
typeVarsInTy ty acc = case ty of
  TyVar v -> addUnique v acc
  TyApp name args -> typeVarsInList args (if isTypeVarName name then addUnique name acc else acc)
  TyRecord fields -> foldl' (flip typeVarsInTy) acc (Map.elems fields)
  TyFun args effects ret -> typeVarsInTy ret (typeVarsInEffects effects (typeVarsInList (NE.toList args) acc))
  _ -> acc

typeVarsInEffects :: [Ty] -> [String] -> [String]
typeVarsInEffects effects acc = foldl' (flip typeVarsInTy) acc effects

typeVarsInNeed :: Need -> [String] -> [String]
typeVarsInNeed (Need args _) = typeVarsInList args

typeVarsInNeeds :: [Need] -> [String] -> [String]
typeVarsInNeeds needs acc = foldl' (flip typeVarsInNeed) acc needs

typeHeadVarsInTy :: Ty -> [String] -> [String]
typeHeadVarsInTy ty acc = case ty of
  TyApp name args -> foldl' (flip typeHeadVarsInTy) (if isTypeVarName name then addUnique name acc else acc) args
  TyRecord fields -> foldl' (flip typeHeadVarsInTy) acc (Map.elems fields)
  TyFun args effects ret ->
    typeHeadVarsInTy ret $
      foldl' (flip typeHeadVarsInTy) (foldl' (flip typeHeadVarsInTy) acc (NE.toList args)) (concreteEffects effects)
  _ -> acc

typeHeadVarsInNeed :: Need -> [String] -> [String]
typeHeadVarsInNeed (Need args _) acc = foldl' (flip typeHeadVarsInTy) acc args

typeHeadVarsInNeeds :: [Need] -> [String] -> [String]
typeHeadVarsInNeeds needs acc = foldl' (flip typeHeadVarsInNeed) acc needs

typeAppHeadsInTy :: Ty -> [String] -> [String]
typeAppHeadsInTy ty acc = case ty of
  TyApp name args -> foldl' (flip typeAppHeadsInTy) (addUnique name acc) args
  TyRecord fields -> foldl' (flip typeAppHeadsInTy) acc (Map.elems fields)
  TyFun args effects ret -> typeAppHeadsInTy ret (foldl' (flip typeAppHeadsInTy) (foldl' (flip typeAppHeadsInTy) acc (NE.toList args)) effects)
  _ -> acc

typeAppHeadsInNeed :: Need -> [String] -> [String]
typeAppHeadsInNeed (Need args _) acc = foldl' (flip typeAppHeadsInTy) acc args

typeAppHeadsInNeeds :: [Need] -> [String] -> [String]
typeAppHeadsInNeeds needs acc = foldl' (flip typeAppHeadsInNeed) acc needs

effectRowVarsInTy :: Ty -> [String] -> [String]
effectRowVarsInTy ty acc = case ty of
  TyApp _ args -> foldl' (flip effectRowVarsInTy) acc args
  TyRecord fields -> foldl' (flip effectRowVarsInTy) acc (Map.elems fields)
  TyFun args effects ret ->
    effectRowVarsInTy ret $
      foldl' (flip effectRowVarsInEffect) (foldl' (flip effectRowVarsInTy) acc (NE.toList args)) effects
  _ -> acc

effectRowVarsInEffect :: Ty -> [String] -> [String]
effectRowVarsInEffect effect acc = case effect of
  TyVar name | isEffectVarName name -> addUnique name acc
  other -> effectRowVarsInTy other acc

effectRowVarsInNeed :: Need -> [String] -> [String]
effectRowVarsInNeed (Need args _) acc = foldl' (flip effectRowVarsInTy) acc args

effectRowVarsInNeeds :: [Need] -> [String] -> [String]
effectRowVarsInNeeds needs acc = foldl' (flip effectRowVarsInNeed) acc needs

typeVarsInList :: [Ty] -> [String] -> [String]
typeVarsInList tys acc = foldl' (flip typeVarsInTy) acc tys

addUnique :: (Eq a) => a -> [a] -> [a]
addUnique x xs = if x `elem` xs then xs else x : xs

showTy :: Ty -> String
showTy = \case
  TyMeta ident -> "?" ++ show ident
  TyVar v -> v
  TyCon c -> c
  TyApp name args -> name ++ "<" ++ showTyList args ++ ">"
  TyRecord fields -> "[" ++ showRecordFields fields ++ "]"
  TyFun args effects ret ->
    let effectText = if null effects then "" else "{" ++ showEffects effects ++ "} "
     in "(" ++ showTyList (NE.toList args) ++ " \\" ++ effectText ++ showTy ret ++ ")"

showRecordFields :: Map.Map String Ty -> String
showRecordFields fields = intercalate ", " [name ++ ": " ++ showTy ty | (name, ty) <- Map.toList fields]

showTyList :: [Ty] -> String
showTyList = intercalate ", " . map showTy

baseEnvNames, baseRuntimeNames, baseEffectNames, runnerEffectNames, primitiveTypeNames :: [String]
baseEnvNames = baseRuntimeNames
baseRuntimeNames = nub (map fst runtimeNativeSpecs ++ [opName | (_, opName, _) <- runtimeEffectOpSpecs])
baseEffectNames = ["console", "random", "fail", "state", "async", "file", "system", "clock", "process"]
runnerEffectNames = ["console", "random", "async", "file", "system", "clock", "process"]
-- literal-backed types are available before bookhoard loading.
primitiveTypeNames = ["integer", "float", "unicode", "text", "func"]

-- this base contract must agree with evaluator arities and primitive dictionary
-- support; the bookhoard supplieth the public declarations around these names.
baseContext :: TcContext
baseContext =
  TcContext
    { tcEnv = baseEnv
    , tcTypes = Map.empty
    , tcTypeAmbiguities = Map.empty
    , tcShapes = Map.empty
    , tcFills = indexFills primitiveFills
    }

primitiveFills :: [FillInfo]
primitiveFills =
  [ FillInfo shape [TypeName ty] [] shape (primitiveFillKey shape ty) True True 0 [] []
  | (ty, shapes) <-
      [ ("integer", ["equal", "order-partial", "add", "zero", "subtract", "multiply", "one", "semiring", "ring", "divide", "to-text"])
      , ("float", ["equal", "order-partial", "add", "zero", "subtract", "multiply", "one", "semiring", "ring", "divide", "field", "from-text", "to-text"])
      ]
  , shape <- shapes
  ]

primitiveFillKey :: String -> String -> String
primitiveFillKey shapeName typeName = "$primitive@" ++ shapeName ++ "@" ++ typeName

fillKeyFor :: String -> [TypeExpr] -> String
fillKeyFor shapeName types = "$fill@" ++ shapeName ++ "@" ++ intercalate ";" (map show types)

runtimeNativeSpecs :: [(String, Int)]
runtimeNativeSpecs = nub [(hostName binding, hostArity binding) | binding <- baseNativeBindings]

-- source-level `foreign` lets are checked against this host abi before they
-- can enter core. keeping full schemes here preventeth a mistyped binding from
-- reaching the dynamically represented native call boundary.
foreignNativeSchemes :: [(String, Scheme)]
foreignNativeSchemes = [(hostName binding, hostScheme (hostSignature binding)) | binding <- foreignBindings]

runtimeEffectOpSpecs :: [(String, String, Int)]
runtimeEffectOpSpecs =
  [(effectName, hostName binding, hostArity binding) | binding@HostBinding{hostRole = BaseEffect effectName} <- baseEffectBindings]

-- an imported context contributeth shown names only. each visible name receiveth
-- a qualified binding and a lower-priority bare alias for ambiguity checking.
namespaceImportContext :: String -> TcContext -> TcContext
namespaceImportContext ns TcContext{..} =
  TcContext
    { tcEnv = namespaceImportEnv ns tcEnv
    , tcTypes = namespaceImportTypes ns tcTypes
    , tcTypeAmbiguities = Map.empty
    , tcShapes = namespaceImportShapes ns tcShapes
    , tcFills = Map.map (namespaceImportFills ns) tcFills
    }

namespaceImportEnv :: String -> [(String, Scheme, Int, String)] -> [(String, Scheme, Int, String)]
namespaceImportEnv ns env = concatMap one env
 where
  one (name, scheme, rank, origin)
    | isBaseEnvBinding name origin = []
    | rank /= exportEnvRank || not (isShownOrigin origin) = []
    | otherwise =
        let scheme2 = namespaceScheme ns scheme
            origin2 = ns ++ "@" ++ origin
            exact = (ns ++ "@" ++ name, scheme2, importEnvRank, origin2)
         in (lastQualifiedSegment name, scheme2, aliasEnvRank, origin2) : [exact]

isShownOrigin :: String -> Bool
isShownOrigin origin = "show@" `isPrefixOf` origin

namespaceImportTypes :: String -> Map.Map String TypeInfo -> Map.Map String TypeInfo
namespaceImportTypes ns =
  Map.foldlWithKey'
    step
    Map.empty
 where
  step acc name info =
    let info2 = unshownTypeInfo (namespaceTypeInfo ns info)
        withExact = Map.insert (ns ++ "@" ++ name) info2 acc
     in if typeInfoIsShown info && not (isQualifiedName name)
          then Map.insert (lastQualifiedSegment name) info2 withExact
          else acc

namespaceTypeInfo :: String -> TypeInfo -> TypeInfo
namespaceTypeInfo ns = \case
  Primitive name -> Primitive (namespaceTypeName ns name)
  Alias name params target ->
    Alias (namespaceTypeName ns name) params (namespaceTyWith params ns target)
  DataInfo name params ctors -> DataInfo (namespaceTypeName ns name) params (namespaceCtorInfos ns params (isQualifiedName name) ctors)
  EffectInfo name params -> EffectInfo (namespaceEffectHead ns name) params
  ShownType info -> ShownType (namespaceTypeInfo ns info)

namespaceTypeName :: String -> String -> String
namespaceTypeName ns name
  | isPrimitiveTypeName name || isQualifiedName name = name
  | otherwise = ns ++ "@" ++ name

typeInfoIsShown :: TypeInfo -> Bool
typeInfoIsShown = \case
  ShownType _ -> True
  _ -> False

namespaceCtorInfos :: String -> [String] -> Bool -> [(String, [TypeExpr])] -> [(String, [TypeExpr])]
namespaceCtorInfos ns params externalType =
  map (bimap (namespaceCtorName ns externalType) (map (namespaceTypeExprWith params ns)))

namespaceCtorName :: String -> Bool -> String -> String
namespaceCtorName ns externalType name
  | externalType && isQualifiedName name = name
  | otherwise = ns ++ "@" ++ name

namespaceImportShapes :: String -> Map.Map String ShapeInfo -> Map.Map String ShapeInfo
namespaceImportShapes ns =
  Map.foldlWithKey' step Map.empty
 where
  step acc name ShapeInfo{..} =
    let info = ShapeInfo shapeParams (namespaceShapeNeedsWith shapeParams ns shapeNeeds) (namespaceShapeMembers shapeParams ns shapeMembers) False
     in if isQualifiedName name
          then Map.insert name ShapeInfo{shapeExported = False, ..} acc
          else
            if shapeExported
              then Map.insert (lastQualifiedSegment name) info (Map.insert (ns ++ "@" ++ name) info acc)
              else acc

namespaceShapeNeedsWith :: [String] -> String -> [ShapeNeed] -> [ShapeNeed]
namespaceShapeNeedsWith bound ns = map (\(ShapeNeed args name) -> ShapeNeed (map (namespaceTypeExprWith bound ns) args) (namespaceShapeName ns name))

namespaceShapeName :: String -> String -> String
namespaceShapeName ns name = if isQualifiedName name then name else ns ++ "@" ++ name

namespaceShapeMembers :: [String] -> String -> [ShapeMember] -> [ShapeMember]
namespaceShapeMembers bound ns = map $ \case
  ShapeSpec name ann -> ShapeSpec name (namespaceTypeAnnWith (typeAnnVars ann ++ bound) ns ann)
  ShapeDefault name (Just ann) expr -> ShapeDefault name (Just (namespaceTypeAnnWith (typeAnnVars ann ++ bound) ns ann)) expr
  ShapeDefault name Nothing expr -> ShapeDefault name Nothing expr
  ShapeLaw parameters left right ->
    let lawBound = nub (bound ++ concatMap (typeExprVars . snd) parameters)
     in ShapeLaw [(name, namespaceTypeExprWith lawBound ns ty) | (name, ty) <- parameters] left right

namespaceTypeAnnWith :: [String] -> String -> TypeAnn -> TypeAnn
namespaceTypeAnnWith bound ns (TypeAnn t needs) = TypeAnn (namespaceTypeExprWith bound ns t) (namespaceShapeNeedsWith bound ns needs)

typeAnnVars :: TypeAnn -> [String]
typeAnnVars (TypeAnn t needs) = nub (typeExprVars t ++ shapeNeedVars needs)

namespaceImportFills :: String -> [FillInfo] -> [FillInfo]
namespaceImportFills ns = map $ \FillInfo{..} ->
  let bound = nub (concatMap typeExprVars fillTypes ++ shapeNeedVars fillNeeds ++ shapeNeedVars fillParents)
   in FillInfo
        (namespaceShapeName ns fillShape)
        (map (namespaceTypeExprWith bound ns) fillTypes)
        (namespaceShapeNeedsWith bound ns fillNeeds)
        (namespaceShapeName ns fillOrigin)
        (if fillPrimitive then fillKey else ns ++ "@" ++ fillKey)
        fillDirect
        fillPrimitive
        fillDepth
        (namespaceShapeNeedsWith bound ns fillParents)
        fillMembers

typeExprVars :: TypeExpr -> [String]
typeExprVars = \case
  TypeName name | isTypeVarName name -> [name]
  TypeName _ -> []
  TypeApply _ args -> nub (concatMap typeExprVars args)
  TypeRecord fields -> nub (concatMap (typeExprVars . snd) fields)
  TypeArrow args effects ret -> nub (concatMap typeExprVars (NE.toList args ++ effects ++ [ret]))

shapeNeedVars :: [ShapeNeed] -> [String]
shapeNeedVars needs = nub [name | ShapeNeed args _ <- needs, name <- concatMap typeExprVars args]

namespaceScheme :: String -> Scheme -> Scheme
namespaceScheme ns = \case
  Forall vars needs ty -> Forall vars (namespaceNeedsWith vars ns needs) (namespaceTyWith vars ns ty)
  EffectOpForall owner vars needs ty ->
    EffectOpForall (namespaceEffectHead ns owner) vars (namespaceNeedsWith vars ns needs) (namespaceTyWith vars ns ty)

namespaceNeedsWith :: [String] -> String -> [Need] -> [Need]
namespaceNeedsWith bound ns = map (namespaceNeedWith bound ns)

namespaceNeedWith :: [String] -> String -> Need -> Need
namespaceNeedWith bound ns (Need args name) = Need (map (namespaceTyWith bound ns) args) (namespaceShapeName ns name)

namespaceTyWith :: [String] -> String -> Ty -> Ty
namespaceTyWith bound ns = \case
  TyCon name -> TyCon (namespaceTypeHeadWith bound ns name)
  TyApp name args -> TyApp (namespaceTypeHeadWith bound ns name) (map (namespaceTyWith bound ns) args)
  TyRecord fields -> TyRecord (Map.map (namespaceTyWith bound ns) fields)
  TyFun args effects ret -> TyFun (fmap (namespaceTyWith bound ns) args) (namespaceEffectsWith bound ns effects) (namespaceTyWith bound ns ret)
  ty -> ty

namespaceTypeHeadWith :: [String] -> String -> String -> String
namespaceTypeHeadWith bound ns name
  | name `elem` bound || isPrimitiveTypeName name || isQualifiedName name = name
  | otherwise = ns ++ "@" ++ name

namespaceEffectsWith :: [String] -> String -> [Ty] -> [Ty]
namespaceEffectsWith bound ns = map (namespaceEffectTyWith bound ns)

namespaceEffectTyWith :: [String] -> String -> Ty -> Ty
namespaceEffectTyWith bound ns = \case
  effect@(TyVar name) | isEffectVarName name -> effect
  TyApp name args -> TyApp (namespaceEffectHead ns name) (map (namespaceTyWith bound ns) args)
  TyCon name -> TyApp (namespaceEffectHead ns name) []
  effect -> namespaceTyWith bound ns effect

namespaceEffectHead :: String -> String -> String
namespaceEffectHead ns name
  | isBaseEffectName name || isQualifiedName name = name
  | otherwise = ns ++ "@" ++ name

namespaceTypeExprWith :: [String] -> String -> TypeExpr -> TypeExpr
namespaceTypeExprWith bound ns = \case
  TypeName name -> TypeName (namespaceTypeHeadWith bound ns name)
  TypeApply name args -> TypeApply (namespaceTypeHeadWith bound ns name) (map (namespaceTypeExprWith bound ns) args)
  TypeRecord fields -> TypeRecord [(name, namespaceTypeExprWith bound ns fieldTy) | (name, fieldTy) <- fields]
  TypeArrow args effects ret -> TypeArrow (fmap (namespaceTypeExprWith bound ns) args) (map (namespaceEffectTypeExprWith bound ns) effects) (namespaceTypeExprWith bound ns ret)

namespaceEffectTypeExprWith :: [String] -> String -> TypeExpr -> TypeExpr
namespaceEffectTypeExprWith bound ns = \case
  t@(TypeName name) | isEffectVarName name || isBaseEffectName name || isQualifiedName name -> t
  TypeName name -> TypeName (namespaceEffectHead ns name)
  TypeApply name args -> TypeApply (namespaceEffectHead ns name) (map (namespaceTypeExprWith bound ns) args)
  t -> namespaceTypeExprWith bound ns t

mergeContext :: TcContext -> TcContext -> TcContext
mergeContext left right =
  TcContext
    { tcEnv = tcEnv left ++ tcEnv right
    , tcTypes = Map.union (tcTypes left) (tcTypes right)
    , tcTypeAmbiguities = Map.unionsWith unionNames [tcTypeAmbiguities left, tcTypeAmbiguities right, collisions]
    , tcShapes = Map.union (tcShapes left) (tcShapes right)
    , tcFills = Map.unionWith (++) (tcFills left) (tcFills right)
    }
 where
  collisions =
    Map.fromList
      [ (name, unionNames (typeInfoNames leftInfo) (typeInfoNames rightInfo))
      | (name, leftInfo) <- Map.toList (tcTypes left)
      , not (isQualifiedName name)
      , Just rightInfo <- [Map.lookup name (tcTypes right)]
      , typeInfoNames leftInfo /= typeInfoNames rightInfo
      ]
  unionNames xs ys = nub (xs ++ ys)

typeInfoNames :: TypeInfo -> [String]
typeInfoNames = \case
  Primitive name -> [name]
  Alias name _ _ -> [name]
  DataInfo name _ _ -> [name]
  EffectInfo name _ -> [name]
  ShownType info -> typeInfoNames info

isBaseEnvName :: String -> Bool
isBaseEnvName name = name `elem` baseEnvNames

isBaseEnvBinding :: String -> String -> Bool
isBaseEnvBinding name origin = isBaseEnvName name && origin == "base@" ++ name

isBaseEffectName :: String -> Bool
isBaseEffectName name = name `elem` baseEffectNames

isPrimitiveTypeName :: String -> Bool
isPrimitiveTypeName name = name `elem` primitiveTypeNames

isTypeVarName :: String -> Bool
isTypeVarName [c] = isAscii c && isLower c
isTypeVarName (c : rest) = isAscii c && isLower c && all isDigit rest
isTypeVarName [] = False

-- declaration collection buildeth the namespace used by later checking; export
-- ranks preserve local shadowing and qualify-if-needed lookup.
collectProgramContext :: [Decl] -> TcContext -> TcContext
collectProgramContext ds ctx = foldl' (flip collectDeclContext) ctx ds

collectDeclContext :: Decl -> TcContext -> TcContext
collectDeclContext decl ctx = case decl of
  Import _ -> ctx
  Export declaration -> exportDeclContext declaration (collectDeclContext declaration ctx)
  ReExport _ -> ctx
  ReExportType _ -> ctx
  DataDecl params name ctors -> collectConstructors name params ctors (addDataTypeInfo name params (dataCtorInfos name ctors) ctx)
  TypeAlias params name target -> addTypeAliasInfo params name target ctx
  EffectDecl params name ops -> collectEffectOps params name ops (addEffectTypeInfo name params ctx)
  ShapeDecl params name needs members -> addShapeMembers name params needs members ctx
  Let name (Just ann) _ -> addEnv name (typeAnnScheme ann ctx) ctx
  Let _ Nothing _ -> ctx
  FillDecl tyArgs shapeName needs members -> addFillInfo shapeName tyArgs needs members ctx
  ElaboratedFill _ tyArgs shapeName needs members -> addFillInfo shapeName tyArgs needs members ctx

exportDeclContext :: Decl -> TcContext -> TcContext
exportDeclContext declaration ctx = case declaration of
  Import _ -> ctx
  Export nested -> exportDeclContext nested ctx
  ReExport name -> either (const ctx) id (reExportName name ctx)
  ReExportType name -> either (const ctx) id (reExportTypeName name ctx)
  Let name _ _ -> addShownEnv name ctx
  TypeAlias _ name _ -> addShownType name ctx
  DataDecl _ name constructors -> foldl' (flip addShownEnv) (addShownType name ctx) [ctorName | Ctor ctorName _ <- constructors]
  EffectDecl _ name operations -> foldl' (flip addShownEnv) (addShownType name ctx) [opName | EffectOp opName _ <- operations]
  ShapeDecl _ name _ members -> foldl' (flip addShownEnv) (markShapeExported name ctx) (shapeMemberNames members)
  FillDecl{} -> ctx
  ElaboratedFill{} -> ctx

markShapeExported :: String -> TcContext -> TcContext
markShapeExported name ctx@TcContext{tcShapes} =
  case findShapeEntry name ctx of
    Just (_, info) -> ctx{tcShapes = Map.insert (lastQualifiedSegment name) info{shapeExported = True} tcShapes}
    Nothing -> ctx

reExportName :: String -> TcContext -> Either String TcContext
reExportName name ctx = reExportTerm name ctx >>= maybe (Left ("unknown name '" ++ name ++ "'")) Right

reExportTypeName :: String -> TcContext -> Either String TcContext
reExportTypeName name ctx@TcContext{tcTypes, tcTypeAmbiguities}
  | not (isQualifiedName name) && isPrimitiveTypeName name && Map.notMember name tcTypes =
      Right
        ctx
          { tcTypes = Map.insert name (ShownType (Primitive name)) tcTypes
          , tcTypeAmbiguities = Map.delete name tcTypeAmbiguities
          }
  | otherwise =
      reExportTypeInfo name ctx >>= \case
        Nothing -> Left ("unknown type '" ++ name ++ "'")
        Just _ -> Right (addShownType name ctx)

reExportTypeInfo :: String -> TcContext -> Either String (Maybe TypeInfo)
reExportTypeInfo name ctx@TcContext{tcTypeAmbiguities}
  | isQualifiedName name = Right (shownTypeInfo name ctx)
  | Just choices <- Map.lookup name tcTypeAmbiguities =
      Left ("ambiguous type '" ++ name ++ "'; qualify it as one of: " ++ intercalate ", " choices)
  | otherwise = Right (shownTypeInfo name ctx)

reExportTerm :: String -> TcContext -> Either String (Maybe TcContext)
reExportTerm name ctx@TcContext{tcEnv}
  | isQualifiedName name = Right (addShownEnv name ctx <$ lookupEnvExact name tcEnv)
  | otherwise = case shownBareEnvBindings name tcEnv of
      Right _ -> Right (Just (addShownEnv name ctx))
      Left message
        | null (collectEnvCandidates name tcEnv) -> Right Nothing
        | otherwise -> Left message

findShapeEntry :: String -> TcContext -> Maybe (String, ShapeInfo)
findShapeEntry name TcContext{tcShapes} = case Map.lookup name tcShapes of
  Just info -> Just (name, info)
  Nothing -> case nub [(key, info) | (key, info) <- Map.toList tcShapes, shapeNamesMatch key name] of
    [entry] -> Just entry
    _ -> Nothing

dataCtorInfos :: String -> [Ctor] -> [(String, [TypeExpr])]
dataCtorInfos dataName = map (\(Ctor name fields) -> (dataName ++ "@" ++ name, fields))

collectEffectOps :: [String] -> String -> [EffectOp] -> TcContext -> TcContext
collectEffectOps params effectName ops ctx = foldl' step ctx ops
 where
  step current (EffectOp opName t) =
    let effect = TyApp effectName (map TyVar params)
        opTy = addLatentEffect effect (convertTypeExpr t current)
        scheme = markEffectOpScheme effectName (addForallVars params (generalizeTy opTy))
     in addEnvAlias opName (effectName ++ "@" ++ opName) scheme (addEnv (effectName ++ "@" ++ opName) scheme current)

addLatentEffect :: Ty -> Ty -> Ty
addLatentEffect effect ty = case normalizeFun ty of
  TyFun args effects ret -> TyFun args (addEffect effect effects) ret
  other -> other

collectConstructors :: String -> [String] -> [Ctor] -> TcContext -> TcContext
collectConstructors dataName params ctors ctx = foldl' step ctx ctors
 where
  step current (Ctor name fields) =
    let result = dataResultType dataName params
        fieldTys = map (`convertTypeExpr` current) fields
        ty = if null fieldTys then result else curriedFunction fieldTys [] result
        scheme = Forall params [] ty
     in addEnvAlias name (dataName ++ "@" ++ name) scheme (addEnv (dataName ++ "@" ++ name) scheme current)

dataResultType :: String -> [String] -> Ty
dataResultType name [] = TyCon name
dataResultType name params = TyApp name (map TyVar params)

addTypeAliasInfo :: [String] -> String -> TypeExpr -> TcContext -> TcContext
addTypeAliasInfo params name target ctx = addTypeInfo name (Alias name params (convertTypeExpr target ctx)) ctx

addDataTypeInfo :: String -> [String] -> [(String, [TypeExpr])] -> TcContext -> TcContext
addDataTypeInfo name params ctors ctx =
  addTypeInfo name (DataInfo name params (map (fmap (map (canonicalTypeExpr ctx))) ctors)) ctx

addEffectTypeInfo :: String -> [String] -> TcContext -> TcContext
addEffectTypeInfo name params = addTypeInfo name (EffectInfo name params)

addTypeInfo :: String -> TypeInfo -> TcContext -> TcContext
addTypeInfo name info ctx@TcContext{tcTypes, tcTypeAmbiguities} =
  ctx
    { tcTypes = Map.insert name info tcTypes
    , tcTypeAmbiguities = Map.delete name tcTypeAmbiguities
    }

addFillInfo :: String -> [TypeExpr] -> [ShapeNeed] -> [Decl] -> TcContext -> TcContext
addFillInfo shapeName tyArgs needs members ctx@TcContext{tcFills} =
  let actualShape = canonicalShapeName shapeName ctx
      actualTypes = map (canonicalTypeExpr ctx) tyArgs
      actualNeeds = map (canonicalShapeNeed ctx) needs
      key = fillKeyFor actualShape actualTypes
      parents = fillParentNeeds actualShape actualTypes ctx
      provided = [name | Let name _ _ <- members]
      newFills = fillClosure key actualShape actualShape actualTypes actualNeeds parents provided ctx [] 0
   in ctx{tcFills = Map.unionWith (++) (indexFills newFills) tcFills}

indexFills :: [FillInfo] -> Map.Map String [FillInfo]
indexFills = foldr insertFill Map.empty
 where
  insertFill info = Map.insertWith (++) (lastQualifiedSegment (fillShape info)) [info]

fillsForShape :: String -> TcContext -> [FillInfo]
fillsForShape name = Map.findWithDefault [] (lastQualifiedSegment name) . tcFills

fillParentNeeds :: String -> [TypeExpr] -> TcContext -> [ShapeNeed]
fillParentNeeds shapeName types ctx = case findShapeInfo shapeName ctx of
  Nothing -> []
  Just ShapeInfo{shapeParams, shapeNeeds} -> map (specializeShapeNeed shapeParams types) shapeNeeds

fillClosure :: String -> String -> String -> [TypeExpr] -> [ShapeNeed] -> [ShapeNeed] -> [String] -> TcContext -> [String] -> Int -> [FillInfo]
fillClosure key origin shapeName tyArgs needs parents provided ctx seen depth
  | shapeName `elem` seen = []
  | otherwise = FillInfo shapeName tyArgs needs origin key (shapeName == origin) False depth parents provided : inherited
 where
  inherited = case findShapeInfo shapeName ctx of
    Just ShapeInfo{shapeParams, shapeNeeds} -> concatMap closeNeed shapeNeeds
     where
      closeNeed (ShapeNeed arguments neededShape) =
        let neededTypes = map (specializeShapeType shapeParams tyArgs) arguments
         in fillClosure key origin neededShape neededTypes needs parents provided ctx (shapeName : seen) (depth + 1)
    Nothing -> []

addEnv :: String -> Scheme -> TcContext -> TcContext
addEnv name scheme = addEnvWith name scheme localEnvRank name

addEnvAlias :: String -> String -> Scheme -> TcContext -> TcContext
addEnvAlias name qualifiedName scheme = addEnvWith name scheme localEnvRank qualifiedName

addEnvWith :: String -> Scheme -> Int -> String -> TcContext -> TcContext
addEnvWith name scheme rank origin ctx@TcContext{tcEnv} = ctx{tcEnv = (name, scheme, rank, origin) : tcEnv}

addShownEnv :: String -> TcContext -> TcContext
addShownEnv name ctx = foldl' addOne ctx (shownEnvBindings name ctx)
 where
  exportName = lastQualifiedSegment name
  addOne current scheme = addEnvWith exportName scheme exportEnvRank ("show@" ++ exportName) current

shownEnvBindings :: String -> TcContext -> [Scheme]
shownEnvBindings name TcContext{tcEnv}
  | isQualifiedName name = maybeToList (lookupEnvExact name tcEnv)
  | otherwise = case shownBareEnvBindings name tcEnv of
      Right schemes -> schemes
      Left _ -> []

shownBareEnvBindings :: String -> [(String, Scheme, Int, String)] -> Either String [Scheme]
shownBareEnvBindings name env = fst <$> rankEnvCandidates name (collectEnvCandidates name env)

addShownType :: String -> TcContext -> TcContext
addShownType name ctx@TcContext{tcTypes, tcTypeAmbiguities}
  | not (isQualifiedName name) && Map.member name tcTypeAmbiguities = ctx
  | otherwise = case shownTypeInfo name ctx of
      Just info -> ctx{tcTypes = Map.insert (lastQualifiedSegment name) (ShownType info) tcTypes}
      Nothing -> ctx

shownTypeInfo :: String -> TcContext -> Maybe TypeInfo
shownTypeInfo name TcContext{tcTypes} = Map.lookup name tcTypes

localEnvRank, aliasEnvRank, importEnvRank, baseEnvRank, selfAliasEnvRank, exportEnvRank :: Int
localEnvRank = 0
aliasEnvRank = 1
importEnvRank = 1
baseEnvRank = 2
selfAliasEnvRank = 3
exportEnvRank = 4

lookupEnv :: String -> TcContext -> EnvLookup
lookupEnv name ctx = case lookupEnvChoices name ctx of
  ChoicesMissing -> EnvMissing
  ChoicesAmbiguous msg -> EnvAmbiguous msg
  EnvChoices [] -> EnvMissing
  EnvChoices (scheme : _) -> EnvFound scheme

lookupEnvChoices :: String -> TcContext -> EnvChoices
lookupEnvChoices name TcContext{tcEnv}
  | isQualifiedName name = maybe ChoicesMissing (EnvChoices . (: [])) (lookupEnvExact name tcEnv)
  | otherwise = selectEnvChoices name (collectEnvCandidates name tcEnv)

lookupEnvExact :: String -> [(String, Scheme, Int, String)] -> Maybe Scheme
lookupEnvExact name env = case find (\(n, _, _, _) -> n == name) env of
  Just (_, scheme, _, _) -> Just scheme
  Nothing -> Nothing

collectEnvCandidates :: String -> [(String, Scheme, Int, String)] -> [(Scheme, Int, String)]
collectEnvCandidates name env = [(scheme, rank, origin) | (found, scheme, rank, origin) <- env, found == name]

selectEnvChoices :: String -> [(Scheme, Int, String)] -> EnvChoices
selectEnvChoices _ [] = ChoicesMissing
selectEnvChoices name candidates = case rankEnvCandidates name candidates of
  Left message -> ChoicesAmbiguous message
  Right (best, lower) -> EnvChoices (best ++ lower)

rankEnvCandidates :: String -> [(Scheme, Int, String)] -> Either String ([Scheme], [Scheme])
rankEnvCandidates name [] = Left ("unknown name '" ++ name ++ "'")
rankEnvCandidates name candidates =
  let bestRank = minimum [rank | (_, rank, _) <- candidates]
      (best, lower) = partition (\(_, rank, _) -> rank == bestRank) candidates
      origins = nub [origin | (_, _, origin) <- best]
   in case origins of
        [origin] -> Right ([scheme | (scheme, _, foundOrigin) <- best, foundOrigin == origin], [scheme | (scheme, _, _) <- lower])
        _ -> Left ("ambiguous name '" ++ name ++ "'; qualify it as one of: " ++ intercalate ", " origins)

baseEnv :: [(String, Scheme, Int, String)]
baseEnv =
  map (uncurry baseBinding) baseNativeEnv ++ map baseEffectOpBinding baseEffectOpEnv

baseNativeEnv :: [(String, Scheme)]
baseNativeEnv = [(hostName binding, hostScheme (hostSignature binding)) | binding <- baseNativeBindings]

baseEffectOpEnv :: [(String, String, Scheme)]
baseEffectOpEnv =
  [ (effectName, hostName binding, hostScheme (hostSignature binding))
  | binding@HostBinding{hostRole = BaseEffect effectName} <- baseEffectBindings
  ]

baseBinding :: String -> Scheme -> (String, Scheme, Int, String)
baseBinding name scheme = (name, scheme, baseEnvRank, "base@" ++ name)

baseEffectOpBinding :: (String, String, Scheme) -> (String, Scheme, Int, String)
baseEffectOpBinding (effectName, opName, scheme) =
  (opName, markEffectOpScheme effectName scheme, baseEnvRank, "base@" ++ opName)

hostScheme :: HostSignature -> Scheme
hostScheme HostSignature{..} =
  Forall hostVariables [] (curriedFunction (map hostTy hostArguments) (map hostTy hostEffects) (hostTy hostResult))

hostTy :: HostType -> Ty
hostTy = \case
  HostVar name -> TyVar name
  HostCon name -> TyCon name
  HostApp name arguments -> TyApp name (map hostTy arguments)

addShapeMembers :: String -> [String] -> [ShapeNeed] -> [ShapeMember] -> TcContext -> TcContext
addShapeMembers shapeName params needs members ctx = addShapeMembersToEnv shapeName members (addShapeInfo shapeName params needs members ctx)

addShapeInfo :: String -> [String] -> [ShapeNeed] -> [ShapeMember] -> TcContext -> TcContext
addShapeInfo shapeName params needs members ctx@TcContext{tcShapes} =
  ctx{tcShapes = Map.insert shapeName (ShapeInfo params (map (canonicalShapeNeed ctx) needs) (map (canonicalShapeMember ctx) members) False) tcShapes}

canonicalShapeNeed :: TcContext -> ShapeNeed -> ShapeNeed
canonicalShapeNeed ctx (ShapeNeed args name) = ShapeNeed (map (canonicalTypeExpr ctx) args) (canonicalShapeName name ctx)

canonicalShapeMember :: TcContext -> ShapeMember -> ShapeMember
canonicalShapeMember ctx = \case
  ShapeSpec name ann -> ShapeSpec name (canonicalTypeAnn ctx ann)
  ShapeDefault name ann expr -> ShapeDefault name (canonicalTypeAnn ctx <$> ann) expr
  ShapeLaw parameters left right -> ShapeLaw [(name, canonicalTypeExpr ctx ty) | (name, ty) <- parameters] left right

canonicalTypeAnn :: TcContext -> TypeAnn -> TypeAnn
canonicalTypeAnn ctx (TypeAnn ty needs) = TypeAnn (canonicalTypeExpr ctx ty) (map (canonicalShapeNeed ctx) needs)

canonicalTypeExpr :: TcContext -> TypeExpr -> TypeExpr
canonicalTypeExpr ctx = typeExprFromAppliedTy . (`convertTypeExpr` ctx)

addShapeMembersToEnv :: String -> [ShapeMember] -> TcContext -> TcContext
addShapeMembersToEnv shapeName members ctx = foldl' step ctx (mapMaybe shapeMemberSignature members)
 where
  step current (name, annotation) =
    let scheme = shapeMemberScheme shapeName annotation current
     in addEnvAlias name (shapeName ++ "@" ++ name) scheme (addEnv (shapeName ++ "@" ++ name) scheme current)

shapeMemberScheme :: String -> TypeAnn -> TcContext -> Scheme
shapeMemberScheme shapeName ann ctx =
  let scheme = typeAnnScheme (typeAnnWithOwnShapeNeed shapeName ann ctx) ctx
   in case findShapeInfo shapeName ctx of
        Just ShapeInfo{shapeParams} -> addForallVars shapeParams scheme
        Nothing -> scheme

addForallVars :: [String] -> Scheme -> Scheme
addForallVars extra = \case
  Forall vars needs ty -> Forall (nub (extra ++ vars)) needs ty
  EffectOpForall owner vars needs ty -> EffectOpForall owner (nub (extra ++ vars)) needs ty

typeAnnWithOwnShapeNeed :: String -> TypeAnn -> TcContext -> TypeAnn
typeAnnWithOwnShapeNeed shapeName ann ctx = typeAnnWithInternalNeeds (ownShapeNeed shapeName ctx) ann

typeAnnWithInternalNeeds :: [ShapeNeed] -> TypeAnn -> TypeAnn
typeAnnWithInternalNeeds internal (TypeAnn ty needs) = TypeAnn ty (internal ++ needs)

ownShapeNeed :: String -> TcContext -> [ShapeNeed]
ownShapeNeed shapeName ctx = case findShapeInfo shapeName ctx of
  Just ShapeInfo{shapeParams} -> [ShapeNeed (map TypeName shapeParams) shapeName]
  Nothing -> []

findShapeInfo :: String -> TcContext -> Maybe ShapeInfo
findShapeInfo shapeName TcContext{tcShapes} = case Map.lookup shapeName tcShapes of
  Just info -> Just info
  Nothing -> case nub [info | (name, info) <- Map.toList tcShapes, shapeNamesMatch name shapeName] of
    [info] -> Just info
    _ -> Nothing

findShapeMember :: String -> [ShapeMember] -> Maybe TypeAnn
findShapeMember name = lookup name . mapMaybe shapeMemberSignature

-- pattern binding and coverage use the same constructor metadata so nullary
-- constructors are never mistaken for binders.
bindPatternsM :: [Pattern] -> [Ty] -> TcContext -> Tc TcContext
bindPatternsM patterns tys ctx
  | length patterns == length tys = fst <$> foldM step (ctx, []) (zip patterns tys)
  | otherwise = failTc "pattern arity mismatch"
 where
  step (currentCtx, bound) (pat, ty) = bindPatternLinearM pat ty currentCtx bound

bindPatternM :: Pattern -> Ty -> TcContext -> Tc TcContext
bindPatternM pat expected ctx = fst <$> bindPatternLinearM pat expected ctx []

bindPatternLinearM :: Pattern -> Ty -> TcContext -> [String] -> Tc (TcContext, [String])
bindPatternLinearM (PVar "_") _ ctx bound = pure (ctx, bound)
bindPatternLinearM (PVar name) expected ctx bound = case knownConstructorArity name ctx of
  Just 0 -> inferVarM name ctx >>= \(Inferred actual _ _ _) -> unifyM actual expected >> pure (ctx, bound)
  Just _ -> failTc ("constructor pattern '" ++ name ++ "' needeth arguments")
  Nothing -> bindPatternVariableM name expected ctx bound
bindPatternLinearM (PInteger _) expected ctx bound = unifyM (TyCon "integer") expected >> pure (ctx, bound)
bindPatternLinearM (PCon name args) expected ctx bound = do
  unless (isJust (knownConstructorArity name ctx)) (failTc ("unknown constructor pattern '" ++ name ++ "'"))
  Inferred conTy _ _ _ <- inferVarM name ctx
  case normalizeFun conTy of
    TyFun fields _ result -> do
      unifyM result expected
      bindPatternListM args (NE.toList fields) ctx bound
    _ -> case args of
      [] -> unifyM conTy expected >> pure (ctx, bound)
      _ -> failTc ("constructor '" ++ name ++ "' is not a function")

bindPatternListM :: [Pattern] -> [Ty] -> TcContext -> [String] -> Tc (TcContext, [String])
bindPatternListM patterns tys ctx bound
  | length patterns == length tys = foldM step (ctx, bound) (zip patterns tys)
  | otherwise = failTc "constructor pattern arity mismatch"
 where
  step (currentCtx, currentBound) (pat, ty) = bindPatternLinearM pat ty currentCtx currentBound

bindPatternVariableM :: String -> Ty -> TcContext -> [String] -> Tc (TcContext, [String])
bindPatternVariableM name expected ctx bound
  | name `elem` bound = failTc ("pattern bindeth '" ++ name ++ "' more than once")
  | otherwise = pure (addEnv name (Forall [] [] expected) ctx, name : bound)

knownConstructorArity :: String -> TcContext -> Maybe Int
knownConstructorArity name TcContext{tcTypes} = case nub arities of
  [arity] -> Just arity
  _ -> Nothing
 where
  arities =
    [ length fields
    | info <- Map.elems tcTypes
    , DataInfo _ _ constructors <- [unshownTypeInfo info]
    , (constructorName, fields) <- constructors
    , constructorNamesMatch name constructorName
    ]

-- coverage followeth the constructor-specialisation matrix algorithm and returneth
-- one missing witness, including products from multi-scrutinee matches.
checkExhaustiveM :: [Ty] -> [MatchCase] -> TcContext -> Tc ()
checkExhaustiveM scrutTys cases ctx = do
  st <- getTc
  let constructors ty = constructorsForType ty ctx st
      tys = applyStateAll scrutTys st
      rows = matchRows cases
  case Coverage.firstRedundantRow constructors tys rows of
    Just index ->
      let patterns = fromMaybe [] (listToMaybe (drop (index - 1) rows))
       in failTc ("redundant match case " ++ show index ++ "; pattern " ++ Coverage.showPatternList patterns ++ " is unreachable")
    Nothing -> case Coverage.uncoveredPatterns constructors tys rows of
      Nothing -> pure ()
      Just witness -> failTc ("non-exhaustive match; missing case " ++ Coverage.showPatternList witness)

constructorsForType :: Ty -> TcContext -> TcState -> Maybe [(String, [Ty])]
constructorsForType ty ctx st = do
  (name, args) <- typeHead (applyState ty st)
  case unshownTypeInfo <$> findTypeInfo name ctx of
    Just (DataInfo _ params ctors) -> Just (specializeCtorInfos params args ctors ctx)
    _ -> Nothing

typeHead :: Ty -> Maybe (String, [Ty])
typeHead ty = case normalizeFun ty of
  TyCon name -> Just (name, [])
  TyApp name args -> Just (name, args)
  _ -> Nothing

findTypeInfo :: String -> TcContext -> Maybe TypeInfo
findTypeInfo name TcContext{tcTypes} =
  Map.lookup name tcTypes <|> find (typeInfoHasCanonicalName name) (Map.elems tcTypes)

typeInfoHasCanonicalName :: String -> TypeInfo -> Bool
typeInfoHasCanonicalName name info = case unshownTypeInfo info of
  Primitive canonical -> canonical == name
  Alias canonical _ _ -> canonical == name
  DataInfo canonical _ _ -> canonical == name
  EffectInfo canonical _ -> canonical == name
  ShownType inner -> typeInfoHasCanonicalName name inner

unshownTypeInfo :: TypeInfo -> TypeInfo
unshownTypeInfo = \case
  ShownType info -> unshownTypeInfo info
  info -> info

specializeCtorInfos :: [String] -> [Ty] -> [(String, [TypeExpr])] -> TcContext -> [(String, [Ty])]
specializeCtorInfos params args ctors ctx =
  let repls = Map.fromList (zip params args)
   in [(name, map (replaceTyVarsForCoverage repls . (`convertTypeExpr` ctx)) fields) | (name, fields) <- ctors]

replaceTyVarsForCoverage :: Map.Map String Ty -> Ty -> Ty
replaceTyVarsForCoverage repls ty = case ty of
  TyVar name -> Map.findWithDefault ty name repls
  _ -> mapTyChildren (replaceTyVarsForCoverage repls) (skipEffectVar (replaceTyVarsForCoverage repls)) ty

-- expression inference produceth an elaborated expression skeleton alongside
-- its value type, immediate effects, and dictionary requirements.
inferExprM :: Expr -> TcContext -> Tc Inferred
inferExprM expr ctx = case expr of
  ELocated span inner -> withTcSpan span do
    inferred <- inferExprM inner ctx
    pure inferred{inferredExpr = ELocated span (inferredExpr inferred)}
  EInteger _ -> pure (Inferred (TyCon "integer") [] [] expr)
  EFloat _ -> pure (Inferred (TyCon "float") [] [] expr)
  EUnicode _ -> pure (Inferred (TyCon "unicode") [] [] expr)
  EText _ -> pure (Inferred (TyCon "text") [] [] expr)
  EForeign -> failTc "foreign is only allowed as the direct body of an annotated top-level let"
  EVar name -> inferVarM name ctx
  EApply f args -> inferApplyM f (NE.toList args) ctx
  ERecord fields -> inferRecordFieldsM fields ctx
  EField{} -> failTc "internal field access appeareth before elaboration"
  EUpdate base updates -> inferRecordUpdateM base updates ctx
  ETry body returnCase cases -> inferTryHandleM body returnCase cases ctx
  EMatch scrutinees cases -> inferMatchM scrutinees cases ctx
  EBlock ds body -> inferBlockM ds body ctx
  EWithEvidence{} -> failTc "internal evidence appeareth before elaboration"

inferRecordFieldsM :: [(String, Expr)] -> TcContext -> Tc Inferred
inferRecordFieldsM fields ctx = do
  inferred <- traverse (traverse (`inferExprM` ctx)) fields
  let recordTy = Map.fromList [(name, inferredTy field) | (name, field) <- inferred]
      effects = foldl' unionEffects [] (map (inferredEffects . snd) inferred)
      needs = foldl' unionNeeds [] (map (inferredNeeds . snd) inferred)
      fields2 = [(name, inferredExpr field) | (name, field) <- inferred]
  pure (Inferred (TyRecord recordTy) effects needs (ERecord fields2))

inferRecordUpdateM :: Expr -> [RecordUpdate] -> TcContext -> Tc Inferred
inferRecordUpdateM base updates ctx = do
  Inferred baseTy baseEffects baseNeeds base2 <- inferExprM base ctx
  st <- getTc
  case normalizeFun (applyState baseTy st) of
    TyRecord fields -> inferRecordUpdatesM base2 updates fields ctx baseEffects baseNeeds
    other -> failTc ("record update expected record, found " ++ showTy other)

inferRecordUpdatesM :: Expr -> [RecordUpdate] -> Map.Map String Ty -> TcContext -> [Ty] -> [Need] -> Tc Inferred
inferRecordUpdatesM base updates fields ctx effects0 needs0 = do
  ((updatedFields, effects, needs), updates2) <- mapAccumM step (fields, effects0, needs0) updates
  pure (Inferred (TyRecord updatedFields) effects needs (EUpdate base updates2))
 where
  step (currentFields, effects, needs) = \case
    RecordRemove name ->
      if Map.member name currentFields
        then pure ((Map.delete name currentFields, effects, needs), RecordRemove name)
        else failTc ("unknown record field '" ++ name ++ "'")
    RecordSet name expr -> do
      Inferred fieldTy fieldEffects fieldNeeds expr2 <- inferExprM expr ctx
      pure
        ( (Map.insert name fieldTy currentFields, unionEffects effects fieldEffects, unionNeeds needs fieldNeeds)
        , RecordSet name expr2
        )

inferBlockM :: [Decl] -> Expr -> TcContext -> Tc Inferred
inferBlockM ds body ctx = do
  (_, declEffects) <- checkLocalDeclsM ds ctx
  (ds2, ctx2) <- elaborateDeclsContextM ds ctx
  Inferred bodyTy bodyEffects bodyNeeds body2 <- inferExprM body ctx2
  pure (Inferred bodyTy (unionEffects declEffects bodyEffects) bodyNeeds (EBlock ds2 body2))

inferVarM :: String -> TcContext -> Tc Inferred
inferVarM name ctx = case lookupEnv name ctx of
  EnvMissing -> inferFieldAccessNameM name ctx
  EnvAmbiguous msg -> failTc msg
  EnvFound scheme -> do
    (ty, needs) <- instantiateM scheme
    holes <- traverse freshEvidenceHoleM needs
    let core = if null holes then EVar name else EWithEvidence (EVar name) holes
    pure (Inferred ty [] needs core)

freshEvidenceHoleM :: Need -> Tc Evidence
freshEvidenceHoleM need = do
  st@TcState{tcEvidenceNeeds} <- getTc
  let ident = maybe 0 ((+ 1) . fst) (IntMap.lookupMax tcEvidenceNeeds)
  putTc st{tcEvidenceNeeds = IntMap.insert ident need tcEvidenceNeeds}
  pure (EvidenceHole ident)

inferFieldAccessNameM :: String -> TcContext -> Tc Inferred
inferFieldAccessNameM name ctx = case splitFieldAccessName name of
  Just (baseName, fieldName) -> inferRecordFieldAccessM name baseName fieldName ctx
  Nothing -> failTc ("unknown name '" ++ name ++ "'")

inferRecordFieldAccessM :: String -> String -> String -> TcContext -> Tc Inferred
inferRecordFieldAccessM originalName baseName fieldName ctx = do
  Inferred baseTy effects needs base <- inferVarM baseName ctx
  st <- getTc
  case normalizeFun (applyState baseTy st) of
    TyRecord fields -> case Map.lookup fieldName fields of
      Just fieldTy -> pure (Inferred fieldTy effects needs (EField base fieldName))
      Nothing -> failTc ("unknown record field '" ++ fieldName ++ "' in '" ++ originalName ++ "'")
    other -> failTc ("record field access expected record, found " ++ showTy other)

inferApplyM :: Expr -> [Expr] -> TcContext -> Tc Inferred
inferApplyM f args ctx = case flatApply f args of
  (EVar name, allArgs) -> inferNamedApplyM name allArgs ctx
  (other, allArgs) -> inferExprM other ctx >>= \inferred -> inferCurriedApplyM inferred allArgs ctx

flatApply :: Expr -> [Expr] -> (Expr, [Expr])
flatApply (EApply inner innerArgs) args =
  let (base, baseArgs) = flatApply inner (NE.toList innerArgs)
   in (base, baseArgs ++ args)
flatApply f args = (f, args)

inferNamedApplyM :: String -> [Expr] -> TcContext -> Tc Inferred
inferNamedApplyM name args ctx = case lookupEnvChoices name ctx of
  ChoicesMissing -> inferFieldAccessNameM name ctx >>= \inferred -> inferCurriedApplyM inferred args ctx
  ChoicesAmbiguous msg -> failTc msg
  EnvChoices schemes -> tryApplySchemesM name args schemes ctx Nothing

tryApplySchemesM :: String -> [Expr] -> [Scheme] -> TcContext -> Maybe String -> Tc Inferred
tryApplySchemesM name args schemes ctx = foldr tryScheme noMore schemes
 where
  noMore err = failTc (fromMaybe ("unknown name '" ++ name ++ "'") err)
  tryScheme scheme fallback err =
    recoverTc
      ( do
          (t, needs) <- instantiateM scheme
          holes <- traverse freshEvidenceHoleM needs
          let core = if null holes then EVar name else EWithEvidence (EVar name) holes
          inferCurriedApplyM (Inferred t [] needs core) args ctx
      )
      (fallback . keepFirstError err)

keepFirstError :: Maybe String -> String -> Maybe String
keepFirstError Nothing msg = Just msg
keepFirstError some@(Just _) _ = some

inferCurriedApplyM :: Inferred -> [Expr] -> TcContext -> Tc Inferred
inferCurriedApplyM fn args ctx = foldM step fn args
 where
  step (Inferred fTy fEffs fNeeds f) arg = do
    Inferred argTy argEffs argNeeds arg2 <- inferExprM arg ctx
    retTy <- freshM
    latentEffects <- unifyApplyFunctionM fTy argTy retTy
    let effects = unionEffects fEffs (unionEffects argEffs latentEffects)
        needs = unionNeeds fNeeds argNeeds
    st <- getTc
    pure (Inferred (applyState retTy st) effects (applyStateNeeds needs st) (EApply f (arg2 :| [])))

unifyApplyFunctionM :: Ty -> Ty -> Ty -> Tc [Ty]
unifyApplyFunctionM fTy argTy retTy = do
  st <- getTc
  case normalizeFun (applyState fTy st) of
    TyFun (param :| restParams) effects ret -> do
      let remaining = case restParams of
            [] -> ret
            p : ps -> TyFun (p :| ps) effects ret
          latentEffects = if null restParams then effects else []
      unifyM param argTy
      unifyM remaining retTy
      pure latentEffects
    _ -> unifyM fTy (TyFun (argTy :| []) [] retTy) >> pure []

inferExprListM :: [Expr] -> TcContext -> Tc [Inferred]
inferExprListM exprs ctx = traverse (`inferExprM` ctx) exprs

inferTryHandleM :: Expr -> Maybe ReturnCase -> [HandlerCase] -> TcContext -> Tc Inferred
inferTryHandleM body returnCase cases ctx = do
  Inferred bodyTy bodyEffects bodyNeeds body2 <- inferExprM body ctx
  st <- getTc
  Inferred answerTy returnEffects returnNeeds returnBody <- inferReturnCaseM returnCase (applyState bodyTy st) ctx
  let returnCase2 = case returnCase of
        Nothing -> Nothing
        Just (ReturnCase pattern _) -> Just (ReturnCase pattern returnBody)
  inferHandlerCasesM body2 returnCase2 cases answerTy bodyEffects (unionNeeds bodyNeeds returnNeeds) returnEffects ctx

inferReturnCaseM :: Maybe ReturnCase -> Ty -> TcContext -> Tc Inferred
inferReturnCaseM Nothing bodyTy _ = pure (Inferred bodyTy [] [] (EVar "$return"))
inferReturnCaseM (Just (ReturnCase pat body)) bodyTy ctx = do
  ensureIrrefutablePatternM "handler return" pat ctx
  ctx2 <- bindPatternM pat bodyTy ctx
  inferExprM body ctx2

ensureIrrefutablePatternM :: String -> Pattern -> TcContext -> Tc ()
ensureIrrefutablePatternM owner pattern ctx = case pattern of
  PVar "_" -> pure ()
  PVar name
    | isJust (knownConstructorArity name ctx) -> refutable
    | otherwise -> pure ()
  PInteger{} -> refutable
  PCon{} -> refutable
 where
  refutable = failTc (owner ++ " pattern must be a binder or '_'")

data HandlerCoverage = WholeEffect String | OneOperation String String deriving (Eq, Show)

inferHandlerCasesM :: Expr -> Maybe ReturnCase -> [HandlerCase] -> Ty -> [Ty] -> [Need] -> [Ty] -> TcContext -> Tc Inferred
inferHandlerCasesM body returnCase cases answerTy effects bodyNeeds returnEffects ctx = do
  coverage <- traverse (handlerCoverageM effects ctx) cases
  let fullyHandled = completeHandledEffects coverage ctx
  ((finalAnswerTy, accumulatedEffects, accumulatedNeeds), cases2) <- mapAccumM (step fullyHandled) (answerTy, returnEffects, bodyNeeds) cases
  st <- getTc
  let remainingEffects = removeEffectNames fullyHandled (applyStateEffects effects st)
  pure (Inferred (applyState finalAnswerTy st) (unionEffects remainingEffects accumulatedEffects) (applyStateNeeds accumulatedNeeds st) (ETry body returnCase cases2))
 where
  step fullyHandled (currentAnswerTy, accumulatedEffects, accumulatedNeeds) (HandlerCase name patterns handlerBody) = do
    handlerCtx <- handlerCaseContextM fullyHandled name patterns currentAnswerTy effects ctx
    Inferred handlerTy caseEffects caseNeeds handlerBody2 <- inferExprM handlerBody handlerCtx
    mapTcError (unifyM currentAnswerTy handlerTy) (\msg -> "in handler for '" ++ name ++ "': " ++ msg)
    st <- getTc
    pure
      (
        ( applyState currentAnswerTy st
        , unionEffects accumulatedEffects (applyStateEffects caseEffects st)
        , unionNeeds accumulatedNeeds (applyStateNeeds caseNeeds st)
        )
      , HandlerCase name patterns handlerBody2
      )

handlerCoverageM :: [Ty] -> TcContext -> HandlerCase -> Tc HandlerCoverage
handlerCoverageM effects ctx (HandlerCase name patterns _) = do
  st <- getTc
  let actualEffects = applyStateEffects effects st
      whole = WholeEffect name <$ find (effectHasName name) actualEffects
  case lookupEnv name ctx of
    EnvMissing -> maybe (failTc ("handler for absent effect '" ++ name ++ "'")) pure whole
    EnvAmbiguous message -> failTc message
    EnvFound scheme -> case schemeEffectOwner scheme of
      Just owner
        | null patterns && owner == name && isJust whole -> pure (WholeEffect name)
        | Just actual <- find (effectHasName owner) actualEffects -> pure (OneOperation (handledEffectName actual st) (lastQualifiedSegment name))
      _ -> maybe (failTc ("handler case '" ++ name ++ "' is not an effect operation")) pure whole

completeHandledEffects :: [HandlerCoverage] -> TcContext -> [String]
completeHandledEffects coverage ctx =
  [ effectName
  | effectName <- nub [name | item <- coverage, name <- coverageEffect item]
  , WholeEffect effectName `elem` coverage || all (`elem` handledOperations effectName) (effectOperationNames effectName ctx)
  ]
 where
  coverageEffect = \case
    WholeEffect name -> [name]
    OneOperation name _ -> [name]
  handledOperations effectName = [operation | OneOperation owner operation <- coverage, owner == effectName]

effectOperationNames :: String -> TcContext -> [String]
effectOperationNames effectName TcContext{tcEnv} =
  nub
    [ lastQualifiedSegment boundName
    | (boundName, scheme, _, _) <- tcEnv
    , schemeEffectOwner scheme == Just effectName
    ]

handlerCaseContextM :: [String] -> String -> [Pattern] -> Ty -> [Ty] -> TcContext -> Tc TcContext
handlerCaseContextM fullyHandled name patterns answerTy effects ctx = case lookupEnv name ctx of
  EnvMissing -> effectHandlerCaseContextM name patterns effects ctx
  EnvAmbiguous msg -> failTc msg
  EnvFound scheme
    | null patterns -> recoverTc (operationHandlerCaseContextM fullyHandled name patterns answerTy effects scheme ctx) (\_ -> effectHandlerCaseContextM name patterns effects ctx)
    | otherwise -> operationHandlerCaseContextM fullyHandled name patterns answerTy effects scheme ctx

effectHandlerCaseContextM :: String -> [Pattern] -> [Ty] -> TcContext -> Tc TcContext
effectHandlerCaseContextM effectName patterns effects ctx = case patterns of
  [] -> do
    st <- getTc
    if any (effectHasName effectName) (applyStateEffects effects st)
      then pure ctx
      else failTc ("handler for absent effect '" ++ effectName ++ "'")
  _ -> failTc ("effect-level handler '" ++ effectName ++ "' cannot bind operation arguments")

operationHandlerCaseContextM :: [String] -> String -> [Pattern] -> Ty -> [Ty] -> Scheme -> TcContext -> Tc TcContext
operationHandlerCaseContextM fullyHandled name patterns answerTy effects scheme ctx = do
  case schemeEffectOwner scheme of
    Nothing -> failTc ("handler case '" ++ name ++ "' is not an effect operation")
    Just ownerEffect -> do
      (t, _) <- instantiateM scheme
      case normalizeFun t of
        TyFun argTys opEffects retTy -> finish ownerEffect (NE.toList argTys) opEffects retTy
        _ -> failTc ("handler case '" ++ name ++ "' is not an effect operation")
 where
  finish ownerEffect argTys opEffects retTy = do
    _ <- findHandledOperationEffectM name ownerEffect opEffects effects
    mapM_ (\pattern -> ensureIrrefutablePatternM ("handler for '" ++ name ++ "'") pattern ctx) patterns
    ctx2 <- mapTcError (bindHandlerPatternsM patterns argTys ctx) (\msg -> "in handler for '" ++ name ++ "': " ++ msg)
    st <- getTc
    let resumeEffects = removeEffectNames fullyHandled (applyStateEffects effects st)
        resumeTy = curriedFunction [applyState retTy st] resumeEffects (applyState answerTy st)
    pure (addEnv "resume" (Forall [] [] resumeTy) ctx2)

bindHandlerPatternsM :: [Pattern] -> [Ty] -> TcContext -> Tc TcContext
bindHandlerPatternsM [] _ ctx = pure ctx
bindHandlerPatternsM patterns argTys ctx = bindPatternsM patterns argTys ctx

findHandledOperationEffectM :: String -> String -> [Ty] -> [Ty] -> Tc Ty
findHandledOperationEffectM name ownerEffect opEffects effects = do
  ownEffect <- maybe notFound pure (find (effectHasName ownerEffect) opEffects)
  st <- getTc
  case findEffectByHead ownEffect (applyStateEffects effects st) of
    Nothing -> failTc ("handler for absent effect '" ++ ownerEffect ++ "' in operation '" ++ name ++ "'")
    Just actualEffect -> unifyEffectM ownEffect actualEffect >> (applyState actualEffect <$> getTc)
 where
  notFound = failTc ("handler case '" ++ name ++ "' is not an effect operation")

handledEffectName :: Ty -> TcState -> String
handledEffectName effect st = maybe (showEffect effect) fst (effectNameAndArgs (applyState effect st))

removeEffectNames :: [String] -> [Ty] -> [Ty]
removeEffectNames names effects = foldl' (flip removeEffect) effects names

inferMatchM :: [Expr] -> [MatchCase] -> TcContext -> Tc Inferred
inferMatchM [] cases ctx = do
  arity <- maybe (failTc "empty anonymous match") pure (matchCaseArity cases)
  scrutTys <- freshManyM arity
  Inferred branchTy branchEffects branchNeeds core <- inferMatchCasesM cases scrutTys ctx
  st <- getTc
  checkExhaustiveM (applyStateAll scrutTys st) cases ctx
  st2 <- getTc
  pure (Inferred (curriedFunction (applyStateAll scrutTys st2) branchEffects branchTy) [] (applyStateNeeds branchNeeds st2) core)
inferMatchM scrutinees cases ctx = do
  inferred <- inferExprListM scrutinees ctx
  let scrutTys = [t | Inferred t _ _ _ <- inferred]
      scrutEffects = foldl' unionEffects [] [effects | Inferred _ effects _ _ <- inferred]
      scrutNeeds = foldl' unionNeeds [] [needs | Inferred _ _ needs _ <- inferred]
      scrutinees2 = map inferredExpr inferred
  Inferred branchTy branchEffects branchNeeds core <- inferMatchCasesM cases scrutTys ctx
  let cases2 = case core of
        EMatch _ transformed -> transformed
        _ -> []
  st <- getTc
  checkExhaustiveM (applyStateAll scrutTys st) cases ctx
  st2 <- getTc
  pure (Inferred branchTy (unionEffects (applyStateEffects scrutEffects st2) branchEffects) (unionNeeds (applyStateNeeds scrutNeeds st2) branchNeeds) (EMatch scrutinees2 cases2))

inferMatchCasesM :: [MatchCase] -> [Ty] -> TcContext -> Tc Inferred
inferMatchCasesM cases scrutTys ctx = do
  ((branchTy, _, effects, needs), cases2) <- mapAccumM step (Nothing, scrutTys, [], []) cases
  st <- getTc
  case branchTy of
    Just t -> pure (Inferred (applyState t st) effects (applyStateNeeds needs st) (EMatch [] cases2))
    Nothing -> do
      t <- freshM
      pure (Inferred (applyState t st) [] [] (EMatch [] []))
 where
  step (branchTy, currentScrutTys, effects, needs) (MatchCase ps e) = do
    ctx2 <- bindPatternsM (NE.toList ps) currentScrutTys ctx
    Inferred t caseEffects caseNeeds e2 <- inferExprM e ctx2
    mapM_ (`unifyM` t) branchTy
    st <- getTc
    pure
      (
        ( Just (applyState t st)
        , applyStateAll currentScrutTys st
        , unionEffects effects caseEffects
        , unionNeeds needs caseNeeds
        )
      , MatchCase ps e2
      )

checkDeclsM :: [Decl] -> TcContext -> Tc ()
checkDeclsM ds ctx = void (checkDeclsContextM ds ctx)

checkInteractiveDeclsM :: [Decl] -> TcContext -> Tc ()
checkInteractiveDeclsM ds ctx = void (checkDeclsContextWithM True ds ctx)

-- checking establisheth schemes and rejecteth bad source; elaboration then walketh
-- declarations sequentially to insert dictionaries and selected fills.
elaborateDeclsM :: [Decl] -> TcContext -> Tc [Decl]
elaborateDeclsM declarations ctx = fst <$> elaborateDeclsContextM declarations ctx

elaborateDeclsContextM :: [Decl] -> TcContext -> Tc ([Decl], TcContext)
elaborateDeclsContextM declarations ctx = do
  (ctx2, elaborated) <- mapAccumM step ctx declarations
  pure (elaborated, ctx2)
 where
  step current declaration = do
    (declaration2, current2) <- elaborateSequentialDeclM declaration current
    pure (current2, declaration2)

elaborateSequentialDeclM :: Decl -> TcContext -> Tc (Decl, TcContext)
elaborateSequentialDeclM (Export nested) ctx = do
  (nested2, ctx2) <- elaborateSequentialDeclM nested ctx
  pure (Export nested2, exportDeclContext nested ctx2)
elaborateSequentialDeclM declaration@(Let name (Just ann) EForeign) ctx =
  pure (declaration, addEnv name (typeAnnScheme ann ctx) ctx)
elaborateSequentialDeclM (Let name annotation expr) ctx = do
  case annotation of
    Just ann -> do
      let scheme = typeAnnScheme ann ctx
          ctx2 = addEnv name scheme ctx
      expr2 <- elaborateBindingWithSchemeM name scheme expr ctx2
      pure (Let name annotation expr2, ctx2)
    Nothing -> do
      (inferredCtx, _) <- inferUnannotatedBindingM name expr ctx
      Inferred _ _ needs core <- inferNamedM name expr inferredCtx
      normalizedNeeds <- normalizeNeedsM inferredCtx needs
      let bindings = evidenceBindings normalizedNeeds
      expr2 <- wrapEvidence bindings <$> resolveExprEvidenceM inferredCtx bindings core
      pure (Let name annotation expr2, inferredCtx)
elaborateSequentialDeclM declaration ctx = (,ctx) <$> elaborateDeclM declaration ctx

elaborateDeclM :: Decl -> TcContext -> Tc Decl
elaborateDeclM declaration ctx = case declaration of
  Export nested -> Export <$> elaborateDeclM nested ctx
  Let _ (Just _) EForeign -> pure declaration
  Let name annotation expr -> Let name annotation <$> elaborateNamedBindingM name expr ctx
  ShapeDecl params shapeName needs members -> ShapeDecl params shapeName needs <$> traverse (elaborateShapeMemberM shapeName ctx) members
  FillDecl types shapeName needs members -> do
    members2 <- elaborateFillMembersM types shapeName needs members ctx
    let key = fillKeyFor (canonicalShapeName shapeName ctx) (map (canonicalTypeExpr ctx) types)
    pure (ElaboratedFill key types shapeName needs members2)
  ElaboratedFill{} -> pure declaration
  other -> pure other

elaborateNamedBindingM :: String -> Expr -> TcContext -> Tc Expr
elaborateNamedBindingM name expr ctx = do
  scheme <- case lookupEnv name ctx of
    EnvFound found -> pure found
    EnvMissing -> failTc ("internal missing binding '" ++ name ++ "' during elaboration")
    EnvAmbiguous message -> failTc message
  elaborateBindingWithSchemeM name scheme expr ctx

elaborateBindingWithSchemeM :: String -> Scheme -> Expr -> TcContext -> Tc Expr
elaborateBindingWithSchemeM name scheme expr ctx = do
  (expected, needs) <- instantiateM scheme
  (_, _, checkedNeeds, core) <- checkExprAgainstM name expected [] needs expr ctx
  let bindings = evidenceBindings checkedNeeds
  wrapEvidence bindings <$> resolveExprEvidenceM ctx bindings core

elaborateShapeMemberM :: String -> TcContext -> ShapeMember -> Tc ShapeMember
elaborateShapeMemberM shapeName ctx = \case
  ShapeDefault name annotation@(Just ann) expr -> do
    let internalNeeds = ownShapeNeed shapeName ctx
    (expected, needs) <- instantiateM (shapeMemberScheme shapeName ann ctx)
    (_, _, checkedNeeds, core) <- checkExprAgainstM name expected [] needs expr ctx
    let bindings = evidenceBindings checkedNeeds
    resolved <- resolveExprEvidenceM ctx bindings core
    pure (ShapeDefault name annotation (wrapExternalEvidence internalNeeds bindings resolved))
  member -> pure member

elaborateFillMembersM :: [TypeExpr] -> String -> [ShapeNeed] -> [Decl] -> TcContext -> Tc [Decl]
elaborateFillMembersM types shapeName fillNeeds members ctx = case fillSpecsForShape shapeName types ctx [] of
  Nothing -> failTc ("internal missing fill shape '" ++ shapeName ++ "' during elaboration")
  Just specs -> traverse (elaborateMember specs) members
 where
  -- inherited members receive the dictionary for the declared fill. member
  -- names themselves are not recursive term bindings.
  internalNeeds = fillMemberInternalNeeds types shapeName fillNeeds
  elaborateMember specs (Let name annotation expr) = do
    FillSpec{fillSpecType} <- requireFillSpecM name specs
    let memberAnn = typeAnnWithInternalNeeds internalNeeds fillSpecType
    (expected, _, needs) <- instantiateAnnotationM memberAnn ctx
    (_, _, checkedNeeds, core) <- checkExprAgainstM name expected [] needs expr ctx
    let bindings = evidenceBindings checkedNeeds
    resolved <- resolveExprEvidenceM ctx bindings core
    pure (Let name annotation (wrapExternalEvidence internalNeeds bindings resolved))
  elaborateMember _ _ = failTc "fill member must be a let"

checkRunnableDeclsM :: [Decl] -> TcContext -> Tc ()
checkRunnableDeclsM ds ctx = do
  unless (any declaresMain ds) (failTc "runnable file must declare main")
  checkedCtx <- checkDeclsContextM ds ctx
  checkMainM checkedCtx

declaresMain :: Decl -> Bool
declaresMain = \case
  Let "main" _ _ -> True
  Export declaration -> declaresMain declaration
  _ -> False

checkMainM :: TcContext -> Tc ()
checkMainM ctx = case lookupEnv "main" ctx of
  EnvMissing -> failTc "runnable file must declare main"
  EnvAmbiguous message -> failTc message
  EnvFound scheme -> do
    (mainTy, needs) <- instantiateM scheme
    st <- getTc
    let actual = normalizeFun (applyState mainTy st)
        unitTy = maybe (TyCon "𝟙") TyCon (canonicalTypeName "𝟙" ctx)
    case actual of
      TyFun arguments effects result
        | [argument] <- NE.toList arguments -> do
            recoverTc
              (unifyM argument unitTy >> unifyM result unitTy)
              (\_ -> invalidMainType actual)
            checkRunnableEffectsM effects
            requireNoNeedsM ctx needs
      _ -> invalidMainType actual

invalidMainType :: Ty -> Tc a
invalidMainType actual =
  failTc
    ( "main must have type 𝟙 → 𝟙, optionally with console, random, async, file, system, clock, or process effects; found "
        ++ showTy actual
    )

checkDeclsContextM :: [Decl] -> TcContext -> Tc TcContext
checkDeclsContextM = checkDeclsContextWithM False

checkDeclsContextWithM :: Bool -> [Decl] -> TcContext -> Tc TcContext
checkDeclsContextWithM allowImmediate ds ctx = foldM step ctx ds
 where
  step currentCtx (Export declaration) = do
    checkedCtx <- step currentCtx declaration
    pure (exportDeclContext declaration checkedCtx)
  step currentCtx (ReExport name) = either failTc pure (reExportName name currentCtx)
  step currentCtx (ReExportType name) = either failTc pure (reExportTypeName name currentCtx)
  step _ (Let name Nothing EForeign) = failTc ("foreign let '" ++ name ++ "' requireth a type annotation")
  step currentCtx (Let name (Just ann) EForeign) = do
    checkForeignLetM name ann currentCtx
    pure (addEnv name (typeAnnScheme ann currentCtx) currentCtx)
  step currentCtx (Let name Nothing expr) = do
    (ctx2, effects) <- inferUnannotatedBindingM name expr currentCtx
    unless allowImmediate (requirePureImmediateEffectsM ("let '" ++ name ++ "'") effects)
    pure ctx2
  step currentCtx (Let name (Just ann) expr) = do
    checkTypeAnnNeedsM ("let '" ++ name ++ "'") ann currentCtx
    _ <- checkAnnotatedExprM name ann expr currentCtx
    pure (addEnv name (typeAnnScheme ann currentCtx) currentCtx)
  step currentCtx d = mapTcError (checkDeclM d currentCtx) (\msg -> "while checking " ++ declLabel d ++ ": " ++ msg) >> pure currentCtx

declLabel :: Decl -> String
declLabel = \case
  Import path -> "bring '" ++ path ++ "'"
  Let name _ _ -> "let '" ++ name ++ "'"
  TypeAlias _ name _ -> "let-ilk '" ++ name ++ "'"
  DataDecl _ name _ -> "kin '" ++ name ++ "'"
  EffectDecl _ name _ -> "deed '" ++ name ++ "'"
  ShapeDecl _ name _ _ -> "shape '" ++ name ++ "'"
  FillDecl tyArgs shapeName _ _ -> "fill '" ++ showTyArgs tyArgs ++ " " ++ shapeName ++ "'"
  ElaboratedFill _ tyArgs shapeName _ _ -> "fill '" ++ showTyArgs tyArgs ++ " " ++ shapeName ++ "'"
  Export declaration -> "show " ++ declLabel declaration
  ReExport name -> "show '" ++ name ++ "'"
  ReExportType name -> "show-ilk '" ++ name ++ "'"

showTyArgs :: [TypeExpr] -> String
showTyArgs = unwords . map showTypeExprLabel

showTypeExprLabel :: TypeExpr -> String
showTypeExprLabel = \case
  TypeName name -> name
  TypeApply name _ -> name
  TypeRecord{} -> "record"
  TypeArrow{} -> "function"

checkRunnableEffectsM :: [Ty] -> Tc ()
checkRunnableEffectsM effects = do
  st <- getTc
  let actualEffects = applyStateEffects effects st
  if allRunnableEffects actualEffects
    then pure ()
    else failTc ("main hath unsupported effects {" ++ showEffects actualEffects ++ "}; only console, random, async, file, system, clock, and process are allowed")

allRunnableEffects :: [Ty] -> Bool
allRunnableEffects = all isRunnableEffect

isRunnableEffect :: Ty -> Bool
isRunnableEffect effect = case effectNameAndArgs effect of
  Just (name, []) -> name `elem` runnerEffectNames
  _ -> False

checkDeclM :: Decl -> TcContext -> Tc Decl
checkDeclM d ctx = case d of
  Export declaration -> checkDeclM declaration ctx >> pure d
  ReExport name -> either failTc (const (pure d)) (reExportName name ctx)
  ReExportType name -> either failTc (const (pure d)) (reExportTypeName name ctx)
  Let name ann expr -> checkLetM name ann expr ctx
  TypeAlias _ _ target -> checkTypeExprNamesM "type alias" target ctx >> pure d
  DataDecl _ dataName constructors -> mapM_ (checkCtor dataName) constructors >> pure d
  EffectDecl _ effectName ops -> checkEffectDeclM effectName ops ctx d
  ShapeDecl params shapeName needs members -> checkShapeDeclM params shapeName needs members ctx d
  FillDecl tyArgs shapeName needs members -> checkFillM tyArgs shapeName needs members ctx
  _ -> pure d
 where
  checkCtor dataName (Ctor _ fields) = mapM_ (\field -> checkTypeExprNamesM ("data '" ++ dataName ++ "'") field ctx) fields

checkEffectDeclM :: String -> [EffectOp] -> TcContext -> Decl -> Tc Decl
checkEffectDeclM effectName ops ctx whole = mapM_ checkOp ops >> pure whole
 where
  checkOp (EffectOp opName t) = do
    checkTypeExprNamesM ("effect operation '" ++ effectName ++ "@" ++ opName ++ "'") t ctx
    unless (isFunctionTypeExpr t) (failTc ("effect operation '" ++ effectName ++ "@" ++ opName ++ "' must have a function type"))

isFunctionTypeExpr :: TypeExpr -> Bool
isFunctionTypeExpr TypeArrow{} = True
isFunctionTypeExpr _ = False

inferShapeDefaultDeclsM :: [Decl] -> TcContext -> Tc ([Decl], TcContext)
inferShapeDefaultDeclsM ds ctx = do
  (acc, finalCtx) <- foldM step ([], ctx) ds
  pure (reverse acc, finalCtx)
 where
  step (acc, currentCtx) (Export declaration@ShapeDecl{}) = do
    (typed, ctx2) <- inferShape (acc, currentCtx) declaration
    case typed of
      declaration2 : rest -> pure (Export declaration2 : rest, exportDeclContext declaration2 ctx2)
      [] -> pure (typed, ctx2)
  step (acc, currentCtx) (ShapeDecl params shapeName needs members) = do
    inferShape (acc, currentCtx) (ShapeDecl params shapeName needs members)
  step (acc, currentCtx) d =
    pure (d : acc, collectDeclContext d currentCtx)

  inferShape (acc, currentCtx) (ShapeDecl params shapeName needs members) = do
    let knownCtx = addShapeMembers shapeName params needs members currentCtx
    typedMembers <- inferShapeDefaultMembersM params shapeName members knownCtx
    let typedDecl = ShapeDecl params shapeName needs typedMembers
        ctx2 = addShapeMembers shapeName params needs typedMembers currentCtx
    pure (typedDecl : acc, ctx2)
  inferShape state _ = pure state

inferShapeDefaultMembersM :: [String] -> String -> [ShapeMember] -> TcContext -> Tc [ShapeMember]
inferShapeDefaultMembersM shapeParams shapeName members ctx =
  reverse . fst <$> foldM step ([], ctx) members
 where
  step (acc, currentCtx) member = case member of
    ShapeDefault name (Just t) expr -> do
      let typedMember = ShapeDefault name (Just t) expr
      void (checkRecursiveLetAnnotatedAliasM name (shapeName ++ "@" ++ name) (typeAnnWithOwnShapeNeed shapeName t currentCtx) expr currentCtx)
      pure (typedMember : acc, addShapeMembersToEnv shapeName [typedMember] currentCtx)
    ShapeDefault name Nothing expr -> do
      t <- mapTcError (inferShapeDefaultTypeM shapeParams name expr currentCtx) (\msg -> "in shape default '" ++ name ++ "': " ++ msg)
      let typedMember = ShapeDefault name (Just t) expr
      pure (typedMember : acc, addShapeMembersToEnv shapeName [typedMember] currentCtx)
    _ -> pure (member : acc, currentCtx)

inferShapeDefaultTypeM :: [String] -> String -> Expr -> TcContext -> Tc TypeAnn
inferShapeDefaultTypeM shapeParams name expr ctx = case expr of
  EMatch [] [MatchCase ps body] -> do
    let argTypeExprs = replicate (NE.length ps) (defaultShapeArgumentType shapeParams)
        argTys = map (`convertTypeExpr` ctx) argTypeExprs
    ctx2 <- bindPatternsM (NE.toList ps) argTys ctx
    Inferred ret effects needs _ <- inferNamedM name body ctx2
    st <- getTc
    pure (TypeAnn (typeExprFromTy (curriedFunction argTys effects ret) st) (shapeNeedsFromNeeds needs st))
  _ -> do
    Inferred t effects needs _ <- inferNamedM name expr ctx
    st <- getTc
    if null (applyStateEffects effects st)
      then pure (TypeAnn (typeExprFromTy t st) (shapeNeedsFromNeeds needs st))
      else failTc ("effectful shape default '" ++ name ++ "' must be a function")

defaultShapeArgumentType :: [String] -> TypeExpr
defaultShapeArgumentType (param : _) = TypeName param
defaultShapeArgumentType [] = TypeName "t0"

checkShapeDeclM :: [String] -> String -> [ShapeNeed] -> [ShapeMember] -> TcContext -> Decl -> Tc Decl
checkShapeDeclM shapeParams shapeName needs members ctx whole = do
  checkShapeNeedsM ("shape '" ++ shapeName ++ "'") needs ctx
  checkShapeMemberNeedsM shapeName members ctx
  checkShapeDefaultsM members
  checkShapeLawsM shapeParams shapeName needs members ctx
  pure whole

checkShapeNeedsM :: String -> [ShapeNeed] -> TcContext -> Tc ()
checkShapeNeedsM owner needs ctx = mapM_ checkNeed needs
 where
  checkNeed (ShapeNeed args neededName) = do
    mapM_ (\argument -> checkTypeExprNamesM owner argument ctx) args
    case findShapeInfo neededName ctx of
      Nothing -> failTc (owner ++ " hath unknown graith shape '" ++ neededName ++ "'")
      Just ShapeInfo{shapeParams} -> do
        let expected = length shapeParams
            actual = length args
        unless (actual == expected) $
          failTc (owner ++ " hath graith '" ++ neededName ++ "' with " ++ show actual ++ " arguments, but '" ++ neededName ++ "' hath " ++ show expected ++ " parameters")

checkShapeMemberNeedsM :: String -> [ShapeMember] -> TcContext -> Tc ()
checkShapeMemberNeedsM shapeName members ctx = mapM_ checkMember (mapMaybe shapeMemberSignature members)
 where
  checkMember (name, annotation) = checkTypeAnnNeedsM ("shape member '" ++ shapeName ++ "@" ++ name ++ "'") annotation ctx

checkTypeAnnNeedsM :: String -> TypeAnn -> TcContext -> Tc ()
checkTypeAnnNeedsM owner (TypeAnn value needs) ctx = checkTypeExprNamesM owner value ctx >> checkShapeNeedsM owner needs ctx

checkTypeExprNamesM :: String -> TypeExpr -> TcContext -> Tc ()
checkTypeExprNamesM owner expr ctx = case expr of
  TypeName name -> checkTypeNameM owner name ctx >> checkAliasArityM owner name 0 ctx
  TypeApply name arguments -> do
    checkTypeNameM owner name ctx
    checkAliasArityM owner name (length arguments) ctx
    mapM_ (\argument -> checkTypeExprNamesM owner argument ctx) arguments
  TypeRecord fields -> mapM_ (\(_, fieldType) -> checkTypeExprNamesM owner fieldType ctx) fields
  TypeArrow arguments effects result -> do
    mapM_ (\argument -> checkTypeExprNamesM owner argument ctx) arguments
    mapM_ (\effect -> checkTypeExprNamesM owner effect ctx) effects
    checkTypeExprNamesM owner result ctx

checkAliasArityM :: String -> String -> Int -> TcContext -> Tc ()
checkAliasArityM owner name actual ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (Alias canonical params _) -> do
    let expected = length params
    unless (actual == expected) $
      failTc (owner ++ " useth type alias '" ++ canonical ++ "' with " ++ show actual ++ " arguments, but it hath " ++ show expected ++ " parameters")
  _ -> pure ()

checkTypeNameM :: String -> String -> TcContext -> Tc ()
checkTypeNameM owner name TcContext{tcTypeAmbiguities}
  | isQualifiedName name = pure ()
  | Just choices <- Map.lookup name tcTypeAmbiguities =
      failTc (owner ++ " useth ambiguous type '" ++ name ++ "'; qualify it as one of: " ++ intercalate ", " choices)
  | otherwise = pure ()

checkShapeDefaultsM :: [ShapeMember] -> Tc ()
checkShapeDefaultsM = mapM_ checkMember
 where
  checkMember (ShapeDefault name Nothing _) = failTc ("shape default '" ++ name ++ "' hath no inferred type")
  checkMember _ = pure ()

checkShapeLawsM :: [String] -> String -> [ShapeNeed] -> [ShapeMember] -> TcContext -> Tc ()
checkShapeLawsM shapeParams shapeName shapeNeeds members ctx = zipWithM_ checkLaw [(1 :: Int) ..] laws
 where
  laws = [(parameters, left, right) | ShapeLaw parameters left right <- members]
  availableNeeds = map (`convertShapeNeed` ctx) (ownShapeNeed shapeName ctx ++ shapeNeeds)

  checkLaw number (parameters, left, right) = mapTcError check (\message -> "in law " ++ show number ++ " of shape '" ++ shapeName ++ "': " ++ message)
   where
    check = do
      mapM_ (\(_, ty) -> checkTypeExprNamesM ("law of shape '" ++ shapeName ++ "'") ty ctx) parameters
      let rawParameterTypes = map ((`convertTypeExpr` ctx) . snd) parameters
          variables = nub (shapeParams ++ concatMap (typeExprVars . snd) parameters)
          rowVariables = nub (foldl' (flip effectRowVarsInTy) [] rawParameterTypes)
      replacements <- Map.fromList <$> traverse (freshLawVariable rowVariables) variables
      let parameterTypes = map (`replaceVars` replacements) rawParameterTypes
          lawCtx = foldl' addParameter ctx (zip (map fst parameters) parameterTypes)
      Inferred leftType leftEffects leftNeeds _ <- inferNamedM "law left side" left lawCtx
      Inferred rightType rightEffects rightNeeds _ <- inferNamedM "law right side" right lawCtx
      mapTcError (unifyM leftType rightType) ("law sides have different types: " ++)
      mapTcError (unifyEffectsM leftEffects rightEffects) ("law sides have different effects: " ++)
      checkNeedsAgainstM "law left side" ctx leftNeeds availableNeeds
      checkNeedsAgainstM "law right side" ctx rightNeeds availableNeeds
    addParameter current (name, ty) = addEnv name (Forall [] [] ty) current

  freshLawVariable rowVariables variable
    | variable `elem` rowVariables = (variable,) . TyVar <$> freshEffectVarNameM
    | otherwise = freshSkolem [] variable

-- unannotated lets obey the value restriction through immediate-effect purity;
-- annotations are checked with rigid variables before their scheme is trusted.
checkLetM :: String -> Maybe TypeAnn -> Expr -> TcContext -> Tc Decl
checkLetM name Nothing EForeign _ = failTc ("foreign let '" ++ name ++ "' requireth a type annotation")
checkLetM name (Just ann) EForeign ctx = checkForeignLetM name ann ctx >> pure (Let name (Just ann) EForeign)
checkLetM name Nothing expr ctx = do
  Inferred _ effects needs _ <- inferNamedM name expr ctx
  requirePureImmediateEffectsM ("let '" ++ name ++ "'") effects
  _ <- normalizeNeedsM ctx needs
  pure (Let name Nothing expr)
checkLetM name (Just ann) expr ctx = do
  checkTypeAnnNeedsM ("let '" ++ name ++ "'") ann ctx
  checkAnnotatedExprM name ann expr ctx
  pure (Let name (Just ann) expr)

checkForeignLetM :: String -> TypeAnn -> TcContext -> Tc ()
checkForeignLetM name ann ctx = do
  checkTypeAnnNeedsM ("foreign let '" ++ name ++ "'") ann ctx
  let actual = typeAnnScheme ann ctx
  case lookup name foreignNativeSchemes of
    Nothing -> failTc ("foreign let '" ++ name ++ "' hath no host implementation")
    Just expected ->
      unless (actual == expected) $
        failTc
          ( "foreign let '"
              ++ name
              ++ "' hath type "
              ++ schemeType actual
              ++ ", but its host implementation hath type "
              ++ schemeType expected
          )
 where
  schemeType scheme = showTy (let (_, _, ty) = schemeParts scheme in ty)

requirePureImmediateEffectsM :: String -> [Ty] -> Tc ()
requirePureImmediateEffectsM owner effects = do
  st <- getTc
  let actualEffects = applyStateEffects effects st
  unless (null actualEffects) $
    failTc (owner ++ " hath immediate effects {" ++ showEffects actualEffects ++ "}; use a function type or run it from a runnable file")

checkFillM :: [TypeExpr] -> String -> [ShapeNeed] -> [Decl] -> TcContext -> Tc Decl
checkFillM tyArgs shapeName needs members ctx = do
  mapM_ (\tyArg -> checkTypeExprNamesM ("fill '" ++ shapeName ++ "'") tyArg ctx) tyArgs
  checkShapeNeedsM ("fill '" ++ shapeName ++ "'") needs ctx
  let actualShape = canonicalShapeName shapeName ctx
      key = fillKeyFor actualShape (map (canonicalTypeExpr ctx) tyArgs)
      duplicateCount = length [() | FillInfo{fillKey, fillDirect = True} <- fillsForShape actualShape ctx, fillKey == key]
  unless (duplicateCount == 1) (failTc ("duplicate fill '" ++ showTyArgs tyArgs ++ " " ++ shapeName ++ "'"))
  case findShapeInfo shapeName ctx of
    Nothing -> failTc ("unknown shape '" ++ shapeName ++ "' in fill")
    Just ShapeInfo{shapeParams} -> do
      checkFillArityM shapeName shapeParams tyArgs
      case fillSpecsForShape shapeName tyArgs ctx [] of
        Nothing -> failTc ("fill '" ++ shapeName ++ "' hath an unknown required shape")
        Just specs -> checkFillMembersM shapeName tyArgs needs members specs ctx (FillDecl tyArgs shapeName needs members)

checkFillArityM :: String -> [String] -> [TypeExpr] -> Tc ()
checkFillArityM shapeName shapeParams tyArgs =
  unless (length tyArgs == length shapeParams) $
    failTc ("fill '" ++ shapeName ++ "' hath " ++ show (length tyArgs) ++ " type arguments, but '" ++ shapeName ++ "' hath " ++ show (length shapeParams) ++ " parameters")

data FillSpec = FillSpec
  { fillSpecName :: String
  , fillSpecShape :: String
  , fillSpecTypes :: [TypeExpr]
  , fillSpecType :: TypeAnn
  , fillSpecRequired :: Bool
  }
  deriving (Eq, Show)

fillSpecsForShape :: String -> [TypeExpr] -> TcContext -> [String] -> Maybe [FillSpec]
fillSpecsForShape shapeName fillTypes ctx seen
  | shapeName `elem` seen = Just []
  | otherwise = case findShapeInfo shapeName ctx of
      Nothing -> Nothing
      Just ShapeInfo{..}
        | length fillTypes /= length shapeParams -> Nothing
        | otherwise -> do
            inherited <- fillSpecsForNeeds shapeNeeds shapeParams fillTypes ctx (shapeName : seen)
            pure (fillMemberSpecs shapeName shapeParams fillTypes shapeMembers ++ inherited)

fillSpecsForNeeds :: [ShapeNeed] -> [String] -> [TypeExpr] -> TcContext -> [String] -> Maybe [FillSpec]
fillSpecsForNeeds needs currentParams currentFillTypes ctx seen =
  concat <$> traverse specsForNeed needs
 where
  specsForNeed (ShapeNeed args neededName) = do
    let neededFillTypes = map (specializeShapeType currentParams currentFillTypes) args
    fillSpecsForShape neededName neededFillTypes ctx seen

fillMemberSpecs :: String -> [String] -> [TypeExpr] -> [ShapeMember] -> [FillSpec]
fillMemberSpecs shapeName shapeParams fillTypes = mapMaybe makeSpec
 where
  makeSpec = \case
    ShapeSpec name annotation -> Just (make name annotation True)
    ShapeDefault name (Just annotation) _ -> Just (make name annotation False)
    ShapeDefault _ Nothing _ -> Nothing
    ShapeLaw{} -> Nothing
  make name annotation =
    FillSpec name shapeName fillTypes (specializeShapeTypeAnn shapeParams fillTypes annotation)

findFillSpec :: String -> [FillSpec] -> Maybe FillSpec
findFillSpec name = find ((== name) . fillSpecName)

requireFillSpecM :: String -> [FillSpec] -> Tc FillSpec
requireFillSpecM name = maybe (failTc ("fill defineth unknown member '" ++ name ++ "'")) pure . findFillSpec name

checkFillMembersM :: String -> [TypeExpr] -> [ShapeNeed] -> [Decl] -> [FillSpec] -> TcContext -> Decl -> Tc Decl
checkFillMembersM shapeName tyArgs needs members specs ctx whole = do
  requireFillMembersM shapeName tyArgs members specs ctx
  mapM_ (checkFillMemberM tyArgs shapeName needs specs ctx) members
  checkRedundantFillNeedsM shapeName tyArgs needs members specs ctx
  pure whole

checkFillMemberM :: [TypeExpr] -> String -> [ShapeNeed] -> [FillSpec] -> TcContext -> Decl -> Tc ()
checkFillMemberM tyArgs shapeName needs specs ctx = \case
  Let name _ expr -> do
    FillSpec{fillSpecShape, fillSpecType} <- requireFillSpecM name specs
    let memberAnn = typeAnnWithInternalNeeds (fillMemberInternalNeeds tyArgs shapeName needs) fillSpecType
    mapTcError
      (void (checkAnnotatedExprM name memberAnn expr ctx))
      (\msg -> "in fill member '" ++ fillSpecShape ++ "@" ++ name ++ "': " ++ msg)
  _ -> failTc "fill member must be a let"

checkRedundantFillNeedsM :: String -> [TypeExpr] -> [ShapeNeed] -> [Decl] -> [FillSpec] -> TcContext -> Tc ()
checkRedundantFillNeedsM shapeName tyArgs needs members specs ctx = do
  redundant <- findRedundant (zip [(0 :: Int) ..] needs)
  case redundant of
    Just need -> failTc (owner ++ " hath redundant graith " ++ showSourceNeed (convertShapeNeed need ctx))
    Nothing -> pure ()
 where
  owner = "fill '" ++ showTyArgs tyArgs ++ " " ++ shapeName ++ "'"
  ownKey = canonicalFillKey (fillKeyFor (canonicalShapeName shapeName ctx) (map (canonicalTypeExpr ctx) tyArgs))
  parentNeeds = fillParentNeeds shapeName tyArgs ctx
  checkWithout index = do
    let remaining = [need | (otherIndex, need) <- zip [(0 :: Int) ..] needs, otherIndex /= index]
    mapM_ (checkFillMemberM tyArgs shapeName remaining specs ctx) members
    checkParentEvidenceM remaining
  checkParentEvidenceM available = do
    let external = map (`convertShapeNeed` ctx) available
        parents = map (`convertShapeNeed` ctx) parentNeeds
        scheme = generalizeTyWithNeeds (external ++ parents) (TyCon "integer")
    (_, instantiated) <- skolemizeM scheme
    let (external2, parents2) = splitAt (length external) instantiated
        bindings = evidenceBindings external2
    mapM_ (void . resolveNeedEvidenceSeenM ctx bindings [ownKey]) parents2
  findRedundant [] = pure Nothing
  findRedundant ((index, need) : rest) = do
    redundant <- succeedsTc (checkWithout index)
    if redundant then pure (Just need) else findRedundant rest

requireFillMembersM :: String -> [TypeExpr] -> [Decl] -> [FillSpec] -> TcContext -> Tc ()
requireFillMembersM currentShape currentTypes members specs ctx = case missing of
  spec : _ -> failTc ("fill '" ++ currentShape ++ "' is missing required member '" ++ fillSpecName spec ++ "'")
  [] -> pure ()
 where
  provided = [name | Let name _ _ <- members]
  missing =
    [ spec
    | spec@FillSpec{..} <- specs
    , fillSpecRequired
    , fillSpecName `notElem` provided
    , fillSpecName `notElem` inheritedProvided
    , fillSpecShape == currentShape || not (hasDirectFill fillSpecShape fillSpecTypes ctx)
    ]
  inheritedProvided = case findShapeInfo currentShape ctx of
    Nothing -> []
    Just ShapeInfo{shapeParams, shapeNeeds} -> concatMap providedByNeed shapeNeeds
     where
      providedByNeed (ShapeNeed arguments neededShape) =
        let neededTypes = map (specializeShapeType shapeParams currentTypes) arguments
         in if hasDirectFill neededShape neededTypes ctx
              then maybe [] (map fillSpecName) (fillSpecsForShape neededShape neededTypes ctx [])
              else []

hasDirectFill :: String -> [TypeExpr] -> TcContext -> Bool
hasDirectFill wantedShape wantedTypes ctx = any matches (fillsForShape wantedShape ctx)
 where
  wanted = map (`convertTypeExpr` ctx) wantedTypes
  matches FillInfo{..} =
    shapeNamesMatch fillShape wantedShape
      && shapeNamesMatch fillOrigin wantedShape
      && isJust (zipWithExact typePatternBindings (map (`convertTypeExpr` ctx) fillTypes) wanted >>= mergeBindingMaps)

fillMemberInternalNeeds :: [TypeExpr] -> String -> [ShapeNeed] -> [ShapeNeed]
fillMemberInternalNeeds types shapeName = (ShapeNeed types shapeName :)

checkLocalDeclsM :: [Decl] -> TcContext -> Tc (TcContext, [Ty])
checkLocalDeclsM ds ctx = foldM step (ctx, []) ds
 where
  step (currentCtx, currentEffects) (Let name ann expr) = do
    (ctx2, letEffects) <- inferLocalLetM name ann expr currentCtx
    pure (ctx2, unionEffects currentEffects letEffects)
  step acc _ = pure acc

inferLocalLetM :: String -> Maybe TypeAnn -> Expr -> TcContext -> Tc (TcContext, [Ty])
inferLocalLetM name _ EForeign _ = failTc ("foreign let '" ++ name ++ "' is only allowed at file level")
inferLocalLetM name Nothing expr ctx = inferUnannotatedBindingM name expr ctx
inferLocalLetM name (Just ann) expr ctx = do
  checkTypeAnnNeedsM ("let '" ++ name ++ "'") ann ctx
  (_, effects, _, _) <- checkAnnotatedExprM name ann expr ctx
  pure (addEnv name (typeAnnScheme ann ctx) ctx, effects)

inferUnannotatedBindingM :: String -> Expr -> TcContext -> Tc (TcContext, [Ty])
inferUnannotatedBindingM name expr@(EMatch [] _) ctx = do
  selfTy <- freshM
  let ctxSelf = addEnv name (Forall [] [] selfTy) ctx
  Inferred t effects needs _ <- inferNamedM name expr ctxSelf
  mapTcError (unifyM selfTy t) (\msg -> "in '" ++ name ++ "': " ++ msg)
  st <- getTc
  normalizedNeeds <- normalizeNeedsM ctx needs
  pure (addEnv name (bindingScheme t normalizedNeeds effects st) ctx, effects)
inferUnannotatedBindingM name expr ctx = do
  Inferred t effects needs _ <- inferNamedM name expr ctx
  st <- getTc
  normalizedNeeds <- normalizeNeedsM ctx needs
  pure (addEnv name (bindingScheme t normalizedNeeds effects st) ctx, effects)

bindingScheme :: Ty -> [Need] -> [Ty] -> TcState -> Scheme
bindingScheme ty needs effects st
  | null (applyStateEffects effects st) = generalizeApplied ty needs st
  | otherwise = Forall [] (applyStateNeeds needs st) (applyState ty st)

inferNamedM :: String -> Expr -> TcContext -> Tc Inferred
inferNamedM name expr ctx = mapTcError (inferExprM expr ctx) (\msg -> "in '" ++ name ++ "': " ++ msg)

instantiateAnnotationM :: TypeAnn -> TcContext -> Tc (Ty, [Ty], [Need])
instantiateAnnotationM ann ctx = do
  (ty, needs) <- skolemizeM (typeAnnScheme ann ctx)
  pure (ty, [], needs)

skolemizeM :: Scheme -> Tc (Ty, [Need])
skolemizeM scheme = do
  -- annotation variables are rigid here; only parser-generated holes remain flexible.
  let (vars, needs, ty) = schemeParts scheme
      rowVars = effectRowVarsInNeeds needs (effectRowVarsInTy ty [])
  replacements <- Map.fromList <$> traverse (freshSkolem rowVars) vars
  pure (replaceVars ty replacements, map (`replaceNeedVars` replacements) needs)

freshSkolem :: [String] -> String -> Tc (String, Ty)
freshSkolem rowVars variable
  | isImplicitAnnotationVariable variable = (variable,) <$> freshM
  | otherwise = do
      st@TcState{tcNextMeta} <- getTc
      putTc st{tcNextMeta = tcNextMeta + 1}
      let suffix = show tcNextMeta
          skolem = if variable `elem` rowVars then TyApp ("$effect" ++ suffix) [] else TyCon ("$type" ++ suffix)
      pure (variable, skolem)

isImplicitAnnotationVariable :: String -> Bool
isImplicitAnnotationVariable ('t' : digits) = not (null digits) && all isDigit digits
isImplicitAnnotationVariable _ = False

checkAnnotatedExprM :: String -> TypeAnn -> Expr -> TcContext -> Tc (Ty, [Ty], [Need], Expr)
checkAnnotatedExprM name ann expr ctx = do
  (expectedValue, expectedEffects, expectedNeeds) <- instantiateAnnotationM ann ctx
  checkExprAgainstM name expectedValue expectedEffects expectedNeeds expr ctx

checkExprAgainstM :: String -> Ty -> [Ty] -> [Need] -> Expr -> TcContext -> Tc (Ty, [Ty], [Need], Expr)
checkExprAgainstM name expectedValue expectedEffects expectedNeeds (ELocated span expression) ctx = withTcSpan span do
  (value, effects, needs, core) <- checkExprAgainstM name expectedValue expectedEffects expectedNeeds expression ctx
  pure (value, effects, needs, ELocated span core)
checkExprAgainstM name expectedValue expectedEffects expectedNeeds (EMatch [] cases) ctx
  | TyFun args latentEffects ret <- normalizeFun expectedValue
  , anonymousCasesFitFunction (NE.length args) cases = do
      (checkedNeeds, core) <- checkMatchFunctionAgainstM name args latentEffects ret expectedNeeds cases ctx
      st <- getTc
      pure (applyState expectedValue st, applyStateEffects expectedEffects st, checkedNeeds, core)
checkExprAgainstM name expectedValue expectedEffects expectedNeeds expr ctx = do
  Inferred actual effects needs core <- inferNamedM name expr ctx
  mapTcError (unifyM actual expectedValue) (\msg -> "in '" ++ name ++ "': " ++ msg ++ "; actual " ++ showTy actual ++ ", expected " ++ showTy expectedValue)
  (checkedValue, checkedEffects) <- checkEffectsAgainstM name expectedValue effects expectedEffects
  checkNeedsAgainstM name ctx needs expectedNeeds
  st <- getTc
  pure (checkedValue, checkedEffects, applyStateNeeds expectedNeeds st, core)

checkMatchFunctionAgainstM :: String -> NonEmpty Ty -> [Ty] -> Ty -> [Need] -> [MatchCase] -> TcContext -> Tc ([Need], Expr)
checkMatchFunctionAgainstM name args latentEffects ret expectedNeeds cases ctx = do
  (actualNeeds, cases2) <- mapAccumM step [] cases
  st <- getTc
  checkExhaustiveM (applyStateAll patternArgs st) cases ctx
  checkNeedsAgainstM name ctx actualNeeds expectedNeeds
  checked <- applyStateNeeds expectedNeeds <$> getTc
  pure (checked, EMatch [] cases2)
 where
  arity = functionCaseArity (NE.length args) cases
  (patternArgs, remainingArgs) = splitAt arity (NE.toList args)
  step needs (MatchCase ps body) = do
    ctx2 <- bindPatternsM (NE.toList ps) patternArgs ctx
    (bodyNeeds, body2) <- case NE.nonEmpty remainingArgs of
      Nothing -> do
        Inferred bodyTy bodyEffects inferredNeeds core <- inferNamedM name body ctx2
        mapTcError (unifyM bodyTy ret) (\msg -> "in '" ++ name ++ "': " ++ msg ++ "; actual " ++ showTy bodyTy ++ ", expected " ++ showTy ret)
        _ <- checkEffectsAgainstM name ret bodyEffects latentEffects
        pure (inferredNeeds, core)
      Just rest -> do
        (_, _, inferredNeeds, core) <- checkExprAgainstM name (TyFun rest latentEffects ret) [] expectedNeeds body ctx2
        pure (inferredNeeds, core)
    pure (unionNeeds needs bodyNeeds, MatchCase ps body2)

anonymousCasesFitFunction :: Int -> [MatchCase] -> Bool
anonymousCasesFitFunction arity cases = case cases of
  [] -> True
  _ -> let caseArity = functionCaseArity arity cases in caseArity <= arity && all (\(MatchCase ps _) -> NE.length ps == caseArity) cases

functionCaseArity :: Int -> [MatchCase] -> Int
functionCaseArity fallback = fromMaybe fallback . matchCaseArity

checkEffectsAgainstM :: String -> Ty -> [Ty] -> [Ty] -> Tc (Ty, [Ty])
checkEffectsAgainstM name expectedValue actualEffects0 expectedEffects0 = do
  st <- getTc
  let actualEffects = applyStateEffects actualEffects0 st
      expectedEffects = applyStateEffects expectedEffects0 st
  recoverTc
    ( do
        checkEffectSubsetM actualEffects expectedEffects
        st2 <- getTc
        pure (applyState expectedValue st2, applyStateEffects actualEffects st2)
    )
    (\_ -> failTc ("in '" ++ name ++ "': cannot unify effects {" ++ showEffects actualEffects ++ "} with {" ++ showEffects expectedEffects ++ "}"))

checkEffectSubsetM :: [Ty] -> [Ty] -> Tc ()
checkEffectSubsetM actual expected = mapM_ checkOne actual
 where
  checkOne (TyVar name)
    | isEffectVarName name = bindEffectVarM name expected
  checkOne effect = maybe (failTc ("unexpected effect " ++ showEffect effect)) (unifyEffectM effect) (findEffectByHead effect expected)

checkNeedsAgainstM :: String -> TcContext -> [Need] -> [Need] -> Tc ()
checkNeedsAgainstM name ctx actualNeeds0 expectedNeeds0 = do
  st <- getTc
  let actualNeeds = applyStateNeeds actualNeeds0 st
      expectedNeeds = applyStateNeeds expectedNeeds0 st
      unexpected = filter (not . needResolvedBy expectedNeeds ctx) actualNeeds
  unless (null unexpected) $
    failTc ("in '" ++ name ++ "': missing graith " ++ showNeeds unexpected ++ "; available graiths " ++ showNeeds expectedNeeds)

normalizeNeedsM :: TcContext -> [Need] -> Tc [Need]
normalizeNeedsM ctx needs0 = do
  st <- getTc
  foldM step [] (applyStateNeeds needs0 st)
 where
  step acc need =
    do
      normalized <- normalizeNeedM ctx [] need
      pure (unionNeeds acc normalized)

normalizeNeedM :: TcContext -> [Need] -> Need -> Tc [Need]
normalizeNeedM ctx seen need
  | need `elem` seen = if needMayRemainOpen need then pure [need] else failTc ("missing graith " ++ showNeed need)
  | otherwise = case fillNeedsForNeed ctx need of
      Just needs -> foldM step [] needs
      Nothing
        | needMayRemainOpen need -> pure [need]
        | otherwise -> failTc ("missing graith " ++ showNeed need)
 where
  step acc nested = do
    normalized <- normalizeNeedM ctx (need : seen) nested
    pure (unionNeeds acc normalized)

requireNoNeedsM :: TcContext -> [Need] -> Tc ()
requireNoNeedsM ctx needs = do
  remaining <- normalizeNeedsM ctx needs
  if null remaining
    then pure ()
    else failTc ("runnable file hath unresolved graiths " ++ showNeeds remaining)

needResolvedBy :: [Need] -> TcContext -> Need -> Bool
needResolvedBy expected ctx = go []
 where
  go seen need
    | any (\graith -> needCovers ctx [] graith need) expected = True
    | need `elem` seen = False
    | otherwise = case fillNeedsForNeed ctx need of
        Just needs -> all (go (need : seen)) needs
        Nothing -> False

needSubsumes :: Need -> Need -> Bool
needSubsumes (Need expectedArgs expectedName) (Need actualArgs actualName) =
  shapeNamesMatch expectedName actualName
    && length expectedArgs == length actualArgs
    && hasConsistentTypePatternBindings expectedArgs actualArgs

needCovers :: TcContext -> [String] -> Need -> Need -> Bool
needCovers ctx seen graith@(Need graithArgs graithName) wanted
  | needSubsumes graith wanted = True
  | graithName `elem` seen = False
  | otherwise = case findShapeInfo graithName ctx of
      Nothing -> False
      Just ShapeInfo{shapeParams, shapeNeeds} ->
        any (\need -> needCovers ctx (graithName : seen) (specializeNeed shapeParams graithArgs need) wanted) shapeNeeds

specializeNeed :: [String] -> [Ty] -> ShapeNeed -> Need
specializeNeed params args (ShapeNeed needArgs needName) =
  let repls = Map.fromList (zip params args)
   in Need (map ((`replaceVars` repls) . (`convertTypeExpr` baseContext)) needArgs) needName

fillNeedsForNeed :: TcContext -> Need -> Maybe [Need]
fillNeedsForNeed ctx (Need tys shapeName) =
  if all hasConcreteHead tys then listToMaybe (mapMaybe matches (fillsForShape shapeName ctx)) else Nothing
 where
  matches FillInfo{..}
    | not (shapeNamesMatch fillShape shapeName) = Nothing
    | otherwise = do
        bindings <- zipWithExact typePatternBindings (map (`convertTypeExpr` ctx) fillTypes) tys >>= mergeBindingMaps
        pure (map (needWithBindings ctx bindings) fillNeeds)

-- evidence holes keep inference independent of fill selection. resolution turns
-- them into hidden local parameters or a statically chosen dictionary tree.
type EvidenceBinding = (Need, String)

-- evidence parameters are ordinary hidden pure arguments after elaboration.
evidenceBindings :: [Need] -> [EvidenceBinding]
evidenceBindings needs = zip needs ["$evidence" ++ show index | index <- [(0 :: Int) ..]]

wrapEvidence :: [EvidenceBinding] -> Expr -> Expr
wrapEvidence [] expr = expr
wrapEvidence ((_, name) : bindings) expr = EMatch [] [MatchCase (PVar name :| map (PVar . snd) bindings) expr]

wrapExternalEvidence :: [ShapeNeed] -> [EvidenceBinding] -> Expr -> Expr
wrapExternalEvidence internal = wrapEvidence . drop (length internal)

resolveEvidenceM :: TcContext -> [EvidenceBinding] -> Evidence -> Tc Evidence
resolveEvidenceM ctx available = \case
  evidence@EvidenceLocal{} -> pure evidence
  EvidenceFill key nested parents -> EvidenceFill key <$> traverse (resolveEvidenceM ctx available) nested <*> traverse (resolveEvidenceM ctx available) parents
  EvidenceHole ident -> do
    st@TcState{tcEvidenceNeeds} <- getTc
    need <- maybe (failTc "internal missing evidence hole") pure (IntMap.lookup ident tcEvidenceNeeds)
    resolveNeedEvidenceM ctx available (applyStateNeed need st)

resolveNeedEvidenceM :: TcContext -> [EvidenceBinding] -> Need -> Tc Evidence
resolveNeedEvidenceM ctx available = resolveNeedEvidenceSeenM ctx available []

resolveNeedEvidenceSeenM :: TcContext -> [EvidenceBinding] -> [String] -> Need -> Tc Evidence
resolveNeedEvidenceSeenM ctx available seen wanted = case bestLocalEvidence ctx available wanted of
  Left message -> failTc message
  Right (Just name) -> pure (EvidenceLocal name)
  Right Nothing -> do
    (info, nestedNeeds, parentNeeds) <- selectFillEvidenceM ctx wanted
    buildFillEvidenceM ctx available seen info nestedNeeds parentNeeds

buildFillEvidenceM :: TcContext -> [EvidenceBinding] -> [String] -> FillInfo -> [Need] -> [Need] -> Tc Evidence
buildFillEvidenceM ctx available seen info nestedNeeds parentNeeds = do
  let key = canonicalFillKey (fillKey info)
  whenMember (key `elem` seen) (failTc ("recursive fill evidence for " ++ fillKey info))
  nested <- traverse (resolveNeedEvidenceSeenM ctx available (key : seen)) nestedNeeds
  parents <- mapMaybeM (resolveParent key) parentNeeds
  pure (EvidenceFill (fillKey info) nested parents)
 where
  resolveParent ownKey parentNeed = case bestLocalEvidence ctx available parentNeed of
    Left message -> failTc message
    Right (Just name) -> pure (Just (EvidenceLocal name))
    Right Nothing -> do
      (parentInfo, nested, parents) <- selectFillEvidenceM ctx parentNeed
      if canonicalFillKey (fillKey parentInfo) == ownKey
        then pure Nothing
        else Just <$> buildFillEvidenceM ctx available (ownKey : seen) parentInfo nested parents

whenMember :: Bool -> Tc () -> Tc ()
whenMember condition action = if condition then action else pure ()

mapMaybeM :: (a -> Tc (Maybe b)) -> [a] -> Tc [b]
mapMaybeM f = fmap (mapMaybe id) . traverse f

bestLocalEvidence :: TcContext -> [EvidenceBinding] -> Need -> Either String (Maybe String)
bestLocalEvidence ctx available wanted = choose (filter (covers . fst) available)
 where
  covers given = needCovers ctx [] given wanted
  choose [] = Right Nothing
  choose candidates =
    let bestRank = minimum (map (localEvidenceRank . fst) candidates)
        names = [name | (need, name) <- candidates, localEvidenceRank need == bestRank]
     in case (bestRank, names) of
          (_, []) -> Right Nothing
          (0, name : _) -> Right (Just name)
          (_, [name]) -> Right (Just name)
          _ -> Left ("ambiguous graith evidence for " ++ showNeed wanted)
  localEvidenceRank given = if needSubsumes given wanted then (0 :: Int) else 1

selectFillEvidenceM :: TcContext -> Need -> Tc (FillInfo, [Need], [Need])
selectFillEvidenceM ctx wanted@(Need actualTypes shapeName)
  | not (all hasConcreteHead actualTypes) = failTc ("missing graith " ++ showNeed wanted)
  | otherwise = case bestCandidates of
      [] -> failTc ("missing graith " ++ showNeed wanted)
      [(info, needs, parents)] -> pure (info, needs, parents)
      choices -> failTc ("ambiguous fills for " ++ showNeed wanted ++ ": " ++ intercalate ", " (nub [fillKey info | (info, _, _) <- choices]))
 where
  -- selection is static: inheritance depth ranks fills, while equal best choices
  -- are rejected instead of depending on import or runtime argument order.
  candidates = mapMaybe match (fillsForShape shapeName ctx)
  match info@FillInfo{..}
    | not (shapeNamesMatch fillShape shapeName) = Nothing
    | not (fillSuppliesShape ctx info) = Nothing
    | otherwise = do
        bindings <- zipWithExact typePatternBindings (map (`convertTypeExpr` ctx) fillTypes) actualTypes >>= mergeBindingMaps
        pure (info, map (needWithBindings ctx bindings) fillNeeds, map (needWithBindings ctx bindings) fillParents)
  candidateRank (FillInfo{fillPrimitive = True}, _, _) = maxBound :: Int
  candidateRank (FillInfo{fillDepth}, _, _) = fillDepth
  ranked = case candidates of
    [] -> []
    _ -> let rank = minimum (map candidateRank candidates) in filter ((== rank) . candidateRank) candidates
  bestCandidates = nubByFillKey ranked

fillSuppliesShape :: TcContext -> FillInfo -> Bool
fillSuppliesShape _ FillInfo{fillDirect = True} = True
fillSuppliesShape _ FillInfo{fillPrimitive = True} = True
fillSuppliesShape ctx FillInfo{fillShape, fillMembers} = case findShapeInfo fillShape ctx of
  Nothing -> False
  Just ShapeInfo{shapeMembers} ->
    let methods = shapeMemberNames shapeMembers
     in null methods || any (`elem` fillMembers) methods

nubByFillKey :: [(FillInfo, [Need], [Need])] -> [(FillInfo, [Need], [Need])]
nubByFillKey = Map.elems . foldl' add Map.empty
 where
  add rest candidate@(info, _, _) = Map.insertWith shorter (canonicalFillKey (fillKey info)) candidate rest
  shorter new old = if length (fillKey (first3 new)) < length (fillKey (first3 old)) then new else old
  first3 (value, _, _) = value

canonicalFillKey :: String -> String
canonicalFillKey key = case splitFillMarker key of
  Nothing -> key
  Just (prefix, suffix) ->
    let owner = reverse (takeWhile (/= '@') (reverse (dropTrailingAt prefix)))
     in if null owner then "$fill@" ++ suffix else owner ++ "@$fill@" ++ suffix
 where
  dropTrailingAt value = case reverse value of
    '@' : rest -> reverse rest
    _ -> value

splitFillMarker :: String -> Maybe (String, String)
splitFillMarker = go []
 where
  go _ [] = Nothing
  go prefix rest
    | "$fill@" `isPrefixOf` rest = Just (reverse prefix, drop (length "$fill@") rest)
    | c : tailText <- rest = go (c : prefix) tailText

resolveExprEvidenceM :: TcContext -> [EvidenceBinding] -> Expr -> Tc Expr
resolveExprEvidenceM ctx available = go
 where
  go = \case
    ELocated span expression -> ELocated span <$> go expression
    literal@EInteger{} -> pure literal
    literal@EFloat{} -> pure literal
    literal@EUnicode{} -> pure literal
    literal@EText{} -> pure literal
    EForeign -> pure EForeign
    variable@EVar{} -> pure variable
    EApply function arguments -> EApply <$> go function <*> traverse go arguments
    ERecord fields -> ERecord <$> traverse (traverse go) fields
    EField base field -> EField <$> go base <*> pure field
    EUpdate base updates -> EUpdate <$> go base <*> traverse resolveUpdate updates
    ETry body returnCase cases -> ETry <$> go body <*> traverse resolveReturn returnCase <*> traverse resolveHandler cases
    EMatch scrutinees cases -> EMatch <$> traverse go scrutinees <*> traverse resolveCase cases
    EBlock declarations body -> EBlock declarations <$> go body
    EWithEvidence function evidence -> EWithEvidence <$> go function <*> traverse (resolveEvidenceM ctx available) evidence
  resolveUpdate = \case
    RecordSet name expr -> RecordSet name <$> go expr
    update@RecordRemove{} -> pure update
  resolveReturn (ReturnCase pattern body) = ReturnCase pattern <$> go body
  resolveHandler (HandlerCase name patterns body) = HandlerCase name patterns <$> go body
  resolveCase (MatchCase patterns body) = MatchCase patterns <$> go body

needWithBindings :: TcContext -> Map.Map String Ty -> ShapeNeed -> Need
needWithBindings ctx bindings (ShapeNeed args name) =
  Need (map (replaceVarsWithBindings bindings . (`convertTypeExpr` ctx)) args) name

replaceVarsWithBindings :: Map.Map String Ty -> Ty -> Ty
replaceVarsWithBindings bindings ty = case ty of
  TyVar name -> Map.findWithDefault ty name bindings
  TyApp name args
    | Just (TyCon headName) <- Map.lookup name bindings -> TyApp headName (map (replaceVarsWithBindings bindings) args)
    | Just (TyVar headName) <- Map.lookup name bindings -> TyApp headName (map (replaceVarsWithBindings bindings) args)
    | otherwise -> TyApp name (map (replaceVarsWithBindings bindings) args)
  _ -> mapTyChildren (replaceVarsWithBindings bindings) (skipEffectVar (replaceVarsWithBindings bindings)) ty

typePatternBindings :: Ty -> Ty -> Maybe (Map.Map String Ty)
typePatternBindings patternTy actualTy = case (normalizeFun patternTy, normalizeFun actualTy) of
  (TyVar name, ty) -> Just (Map.singleton name ty)
  (TyMeta ident, ty) -> Just (Map.singleton ("?" ++ show ident) ty)
  (TyCon x, TyCon y) | x == y -> Just Map.empty
  (TyApp x xs, TyApp y ys)
    | x == y && length xs == length ys -> zipWithExact typePatternBindings xs ys >>= mergeBindingMaps
    | isTypeVarName x && length xs == length ys -> do
        bindings <- zipWithExact typePatternBindings xs ys >>= mergeBindingMaps
        mergeBindingMaps [Map.singleton x (TyCon y), bindings]
  (TyRecord xs, TyRecord ys)
    | Map.keys xs == Map.keys ys -> traverse recordField (Map.toList xs) >>= mergeBindingMaps
   where
    recordField (name, x) = Map.lookup name ys >>= typePatternBindings x
  (TyFun xs xEffs xRet, TyFun ys yEffs yRet)
    | length xs == length ys && length xEffs == length yEffs ->
        zipWithExact typePatternBindings (NE.toList xs ++ xEffs ++ [xRet]) (NE.toList ys ++ yEffs ++ [yRet]) >>= mergeBindingMaps
  _ -> Nothing

zipWithExact :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithExact f xs ys
  | length xs == length ys = zipWithM f xs ys
  | otherwise = Nothing

mergeBindingMaps :: [Map.Map String Ty] -> Maybe (Map.Map String Ty)
mergeBindingMaps = foldM mergeOne Map.empty
 where
  mergeOne acc bindings = foldM insertOne acc (Map.toList bindings)
  insertOne acc (name, ty) = case Map.lookup name acc of
    Nothing -> Just (Map.insert name ty acc)
    Just existing | existing == ty -> Just acc
    Just _ -> Nothing

hasConsistentTypePatternBindings :: [Ty] -> [Ty] -> Bool
hasConsistentTypePatternBindings expected actual = isJust (zipWithExact typePatternBindings expected actual >>= mergeBindingMaps)

hasConcreteHead :: Ty -> Bool
hasConcreteHead ty = case normalizeFun ty of
  TyCon name -> not ("$type" `isPrefixOf` name)
  TyApp _ _ -> True
  TyFun{} -> True
  _ -> False

needMayRemainOpen :: Need -> Bool
needMayRemainOpen (Need args _) = not (all hasConcreteHead args)

shapeNamesMatch :: String -> String -> Bool
shapeNamesMatch x y
  | x == y = True
  | not (isQualifiedName x) || not (isQualifiedName y) = lastQualifiedSegment x == lastQualifiedSegment y
  | otherwise = canonicalShapeIdentity x == canonicalShapeIdentity y

canonicalShapeIdentity :: String -> String
-- a shape identity is its final owner@shape suffix; source tails avoid copies.
canonicalShapeIdentity name = go name name name
 where
  go identity _ [] = identity
  go _ segment ('@' : rest) = go segment rest rest
  go identity segment (_ : rest) = go identity segment rest

showNeeds :: [Need] -> String
showNeeds = intercalate ", " . map showNeed

showNeed :: Need -> String
showNeed (Need args name) = showTyList args ++ " " ++ name

generalizeApplied :: Ty -> [Need] -> TcState -> Scheme
generalizeApplied ty needs st = generalizeTyWithNeeds (applyStateNeeds needs st) (applyState ty st)

generalizeTy :: Ty -> Scheme
generalizeTy = generalizeTyWithNeeds []

generalizeTyWithNeeds :: [Need] -> Ty -> Scheme
generalizeTyWithNeeds needs ty =
  let (needs2, ty2) = replaceMetasForGeneralization needs ty
   in Forall (typeVarsInNeeds needs2 (typeVarsInTy ty2 [])) needs2 ty2

replaceMetasForGeneralization :: [Need] -> Ty -> ([Need], Ty)
replaceMetasForGeneralization needs ty =
  let used = typeVarsInNeeds needs (typeVarsInTy ty [])
      metaIds = metaIdsInNeeds needs (metaIdsInTy ty [])
      names = metaVarNames used metaIds
      repls = IntMap.fromList (zip metaIds names)
   in (map (replaceNeedMetas repls) needs, replaceTyMetas repls ty)

metaIdsInNeeds :: [Need] -> [Int] -> [Int]
metaIdsInNeeds needs acc = foldl' (flip metaIdsInNeed) acc needs

metaIdsInNeed :: Need -> [Int] -> [Int]
metaIdsInNeed (Need args _) acc = foldl' (flip metaIdsInTy) acc args

metaIdsInTy :: Ty -> [Int] -> [Int]
metaIdsInTy ty acc = case ty of
  TyMeta ident -> addUnique ident acc
  TyApp _ args -> foldl' (flip metaIdsInTy) acc args
  TyRecord fields -> foldl' (flip metaIdsInTy) acc (Map.elems fields)
  TyFun args effects ret -> metaIdsInTy ret (foldl' (flip metaIdsInTy) (foldl' (flip metaIdsInTy) acc (NE.toList args)) effects)
  _ -> acc

metaVarNames :: [String] -> [Int] -> [String]
metaVarNames used ids = reverse names
 where
  (_, names) = foldl' step (used, []) ids
  step (taken, acc) ident =
    let name = firstMetaVarName taken ident
     in (name : taken, name : acc)

firstMetaVarName :: [String] -> Int -> String
firstMetaVarName taken = go
 where
  go n =
    let name = "m" ++ show n
     in if name `elem` taken then go (n + 1) else name

replaceNeedMetas :: IntMap.IntMap String -> Need -> Need
replaceNeedMetas repls (Need args name) = Need (map (replaceTyMetas repls) args) name

replaceTyMetas :: IntMap.IntMap String -> Ty -> Ty
replaceTyMetas repls ty = case ty of
  TyMeta ident -> TyVar (IntMap.findWithDefault ("m" ++ show ident) ident repls)
  _ -> mapTyChildren (replaceTyMetas repls) (replaceTyMetas repls) ty

checkRecursiveLetAnnotatedAliasM :: String -> String -> TypeAnn -> Expr -> TcContext -> Tc Decl
checkRecursiveLetAnnotatedAliasM name qualifiedName expectedExpr expr ctx = do
  checkTypeAnnNeedsM ("let '" ++ name ++ "'") expectedExpr ctx
  (expectedValue, expectedEffects, expectedNeeds) <- instantiateAnnotationM expectedExpr ctx
  let selfScheme = typeAnnScheme expectedExpr ctx
      ctx2 = addSelfEnv name qualifiedName selfScheme ctx
  checkExprAgainstM name expectedValue expectedEffects expectedNeeds expr ctx2
  pure (Let name (Just expectedExpr) expr)

addSelfEnv :: String -> String -> Scheme -> TcContext -> TcContext
addSelfEnv name qualifiedName scheme ctx
  | name == qualifiedName = addEnv name scheme ctx
  | otherwise = addEnvWith qualifiedName scheme localEnvRank qualifiedName (addEnvWith name scheme selfAliasEnvRank qualifiedName ctx)

-- public entry points differ only in their top-level mode; every successful path
-- parseth, validateth, checketh, elaborateth, and crosseth the 'CoreProgram' boundary.
check :: String -> String
check source = checkWithImports source Map.empty

checkWithImports :: String -> Map.Map String String -> String
checkWithImports source imports = checkPrepared source imports False

checkEditorWithImports :: String -> Map.Map String String -> String
checkEditorWithImports source imports = case parse source of
  Left msg -> "parse error: " ++ msg
  Right program
    | looksRunnable program ->
        let runnableResult = checkParsed program imports True
         in if runnableResult == "type ok" then "type ok" else fallbackBookhoard runnableResult program
    | otherwise -> checkParsed program imports False
 where
  fallbackBookhoard runnableResult program =
    let bookhoardResult = checkParsed program imports False
     in if bookhoardResult == "type ok" then "type ok" else runnableResult

checkEditorProgramWithImportsDetailed :: Program -> Map.Map String String -> Maybe TypeFailure
checkEditorProgramWithImportsDetailed program imports
  | looksRunnable program = case checkMode Runnable of
      Right _ -> Nothing
      Left runnableFailure -> case checkMode Bookhoard of
        Right _ -> Nothing
        Left _ -> Just runnableFailure
  | otherwise = either Just (const Nothing) (checkMode Bookhoard)
 where
  checkMode mode = inferProgramWithModeDetailed program imports mode

checkRunnableWithImports :: String -> Map.Map String String -> String
checkRunnableWithImports source imports = checkPrepared source imports True

typeOfWithImports :: String -> Map.Map String String -> String -> Either String String
typeOfWithImports source imports name = do
  program <- parse source
  case inferProgramContext program imports baseContext of
    TcErr message -> Left message
    TcOk context state -> case lookupEnv name context of
      EnvFound scheme -> Right (showScheme state scheme)
      EnvMissing -> Left ("unknown name '" ++ name ++ "'")
      EnvAmbiguous message -> Left message

showScheme :: TcState -> Scheme -> String
showScheme state scheme =
  let (_, needs, ty) = schemeParts scheme
      typeText = showSourceTy (applyState ty state)
      graithText = if null needs then "" else "graith " ++ intercalate ", " (map showSourceNeed (applyStateNeeds needs state)) ++ "; "
   in graithText ++ typeText

showSourceTy :: Ty -> String
showSourceTy = \case
  TyMeta ident -> "?" ++ show ident
  TyVar name -> name
  TyCon name -> name
  TyApp name arguments -> unwords (map showSourceTypeArgument arguments ++ [name])
  TyRecord fields -> "[" ++ intercalate ", " [name ++ ": " ++ showSourceTy ty | (name, ty) <- Map.toList fields] ++ "]"
  TyFun arguments effects result ->
    intercalate " → " (map showSourceTypeArgument (NE.toList arguments) ++ [showSourceTy result])
      ++ if null effects then "" else " ! " ++ intercalate ", " (map showSourceTy effects)

showSourceTypeArgument :: Ty -> String
showSourceTypeArgument ty@TyFun{} = "(" ++ showSourceTy ty ++ ")"
showSourceTypeArgument ty = showSourceTy ty

showSourceNeed :: Need -> String
showSourceNeed (Need arguments name) = unwords (map showSourceTypeArgument arguments ++ [name])

checkPrepared :: String -> Map.Map String String -> Bool -> String
checkPrepared source imports runnable = case parse source of
  Left msg -> "parse error: " ++ msg
  Right p -> checkParsed p imports runnable

checkParsed :: Program -> Map.Map String String -> Bool -> String
checkParsed p imports runnable =
  case inferProgramWithMode p imports (if runnable then Runnable else Bookhoard) of
    TcErr msg -> "type error: " ++ msg
    TcOk _ _ -> "type ok"

looksRunnable :: Program -> Bool
looksRunnable (Program ds) = any declaresMain ds

inferProgramWithMode :: Program -> Map.Map String String -> ProgramMode -> TcResult CoreProgram
inferProgramWithMode p imports mode = runTc (inferProgramWithModeM p imports mode) initialTcState

inferProgramWithModeDetailed :: Program -> Map.Map String String -> ProgramMode -> Either TypeFailure CoreProgram
inferProgramWithModeDetailed program imports mode =
  fst <$> runStateT (unTc (inferProgramWithModeM program imports mode)) initialTcState

elaborateProgramWithImports :: Program -> Map.Map String String -> Bool -> Either String CoreProgram
elaborateProgramWithImports program imports runnable = elaborateWithMode program imports (if runnable then Runnable else Bookhoard)

elaborateInteractiveProgramWithImports :: Program -> Map.Map String String -> Either String CoreProgram
elaborateInteractiveProgramWithImports program imports = elaborateWithMode program imports Interactive

elaborateWithMode :: Program -> Map.Map String String -> ProgramMode -> Either String CoreProgram
elaborateWithMode program imports mode = case inferProgramWithMode program imports mode of
  TcErr message -> Left message
  TcOk elaborated _ -> Right elaborated

inferProgramWithModeM :: Program -> Map.Map String String -> ProgramMode -> Tc CoreProgram
inferProgramWithModeM (Program ds) imports mode = do
  (typedDs, ctx) <- prepareDeclsM [] ds imports baseContext
  case mode of
    Bookhoard -> checkDeclsM typedDs ctx
    Interactive -> checkInteractiveDeclsM typedDs ctx
    Runnable -> checkRunnableDeclsM typedDs ctx
  importCtx <- collectImportContextsM [] typedDs imports baseContext
  let elaborationCtx = foldl' (flip collectElaborationContext) importCtx typedDs
  elaborated <- Program <$> elaborateDeclsM typedDs elaborationCtx
  -- core construction is the final assertion that no inference placeholder leaked.
  either failTc pure (makeCoreProgram elaborated)

collectElaborationContext :: Decl -> TcContext -> TcContext
collectElaborationContext declaration ctx = case declaration of
  Let{} -> ctx
  Export Let{} -> ctx
  Export nested -> exportDeclContext nested (collectElaborationContext nested ctx)
  other -> collectDeclContext other ctx

inferProgramContext :: Program -> Map.Map String String -> TcContext -> TcResult TcContext
inferProgramContext p imports baseCtx = runTc (inferProgramContextM p imports baseCtx) initialTcState

inferProgramContextM :: Program -> Map.Map String String -> TcContext -> Tc TcContext
inferProgramContextM = inferProgramContextFromM []

inferProgramContextFromM :: ImportStack -> Program -> Map.Map String String -> TcContext -> Tc TcContext
inferProgramContextFromM importStack (Program ds) imports baseCtx = do
  (typedDs, ctx) <- prepareDeclsM importStack ds imports baseCtx
  checkDeclsContextM typedDs ctx

prepareDeclsM :: ImportStack -> [Decl] -> Map.Map String String -> TcContext -> Tc ([Decl], TcContext)
prepareDeclsM importStack ds imports baseCtx = do
  either failTc pure (validateProgram (Program ds))
  importCtx <- collectImportContextsM importStack ds imports baseCtx
  (typedDs, _) <- inferShapeDefaultDeclsM ds importCtx
  pure (typedDs, collectProgramContext typedDs importCtx)

-- imports are checked in isolated states and cached by public path. only their
-- resulting contexts cross back into the importing check.
collectImportContextsM :: ImportStack -> [Decl] -> Map.Map String String -> TcContext -> Tc TcContext
collectImportContextsM importStack ds imports ctx = foldM step ctx ds
 where
  step currentCtx (Import path) = importContext currentCtx path
  step currentCtx _ = pure currentCtx

  importContext currentCtx path = do
    nextStack <- either failTc pure (enterImport importStack path)
    cached <- Map.lookup path . tcImportCache <$> getTc
    importedCtx <- case cached of
      Just context -> pure context
      Nothing -> do
        source <- maybe (failTc ("missing bring '" ++ path ++ "'")) pure (Map.lookup path imports)
        let importError message = failTc ("in bring '" ++ path ++ "': " ++ message)
        p <- either importError pure (parse source)
        importedContextM nextStack path p imports
    let namespaced = namespaceImportContext (importNamespace path) importedCtx
    pure (mergeContext namespaced currentCtx)

importedContextM :: ImportStack -> String -> Program -> Map.Map String String -> Tc TcContext
importedContextM importStack path p imports = Tc $ StateT $ \st ->
  let importedState = initialTcState{tcImportCache = tcImportCache st}
   in case runStateT (unTc imported) importedState of
        Left failure -> Left failure{typeFailureMessage = "in bring '" ++ path ++ "': " ++ typeFailureMessage failure, typeFailureSpan = Nothing}
        Right (importedCtx, importedSt) ->
          let cache = Map.insert path importedCtx (tcImportCache importedSt)
           in Right (importedCtx, st{tcImportCache = cache})
 where
  imported = inferProgramContextFromM importStack p imports baseContext
