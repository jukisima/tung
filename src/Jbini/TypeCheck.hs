module Jbini.TypeCheck (
  Ty (..),
  Scheme (..),
  EnvLookup (..),
  TcContext (..),
  TcState (..),
  TcResult (..),
  BoneInfo (..),
  check,
  checkWithImports,
  checkRunnableWithImports,
  baseContext,
  inferProgramContext,
  lookupEnv,
  canonicalTypeName,
  showTy,
  showEffects,
  libraryImportFiles,
  importNamespace,
  namespaceImportBones,
  lastQualifiedSegment,
  baseRuntimeNames,
  baseEffectNames,
  runtimeNativeSpecs,
  runtimeEffectOpSpecs,
  findBoneMember,
)
where

import Control.Monad (foldM, replicateM, unless, zipWithM_)
import Control.Monad.Trans.State.Strict (StateT (..), runStateT)
import Data.Char (isAscii, isDigit, isLower)
import Data.List ((\\), find, intercalate, isPrefixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Jbini.Parse (parse)
import Jbini.Syntax

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
  }
  deriving (Eq, Show)

data Scheme = Forall [String] [Need] Ty deriving (Eq, Show)

data EnvLookup = EnvFound Scheme | EnvMissing | EnvAmbiguous String deriving (Eq, Show)

data EnvChoices = EnvChoices [Scheme] | ChoicesMissing | ChoicesAmbiguous String deriving (Eq, Show)

data TcResult a = TcOk a TcState | TcErr String deriving (Eq, Show)

data TyHead = TyHead String [Ty] deriving (Eq, Show)

data TcState = TcState
  { tcNextMeta :: Int
  , tcMetaSubst :: Map.Map Int Ty
  , tcTypeHeads :: Map.Map String TyHead
  , tcEffectRows :: Map.Map String [Ty]
  }
  deriving (Eq, Show)

data TypeInfo = Alias String [String] | DataInfo String [String] [(String, [TypeExpr])] | ShownType TypeInfo deriving (Eq, Show)

data BoneInfo = BoneInfo
  { boneParams :: [String]
  , boneNeeds :: [BoneNeed]
  , boneMembers :: [BoneMember]
  }
  deriving (Eq, Show)

data FleshInfo = FleshInfo
  { fleshBone :: String
  , fleshType :: TypeExpr
  , fleshNeeds :: [BoneNeed]
  }
  deriving (Eq, Show)

data TcContext = TcContext
  { tcEnv :: [(String, Scheme, Int, String)]
  , tcTypes :: Map.Map String TypeInfo
  , tcBones :: Map.Map String BoneInfo
  , tcFleshes :: [FleshInfo]
  }
  deriving (Eq, Show)

newtype Tc a = Tc {unTc :: StateT TcState (Either String) a}
  deriving newtype (Functor, Applicative, Monad)

initialTcState :: TcState
initialTcState = TcState{tcNextMeta = 0, tcMetaSubst = Map.empty, tcTypeHeads = Map.empty, tcEffectRows = Map.empty}

runTc :: Tc a -> TcState -> TcResult a
runTc computation st = case runStateT (unTc computation) st of
  Left msg -> TcErr msg
  Right (value, st2) -> TcOk value st2

failTc :: String -> Tc a
failTc msg = Tc $ StateT $ \_ -> Left msg

getTc :: Tc TcState
getTc = Tc $ StateT $ \st -> Right (st, st)

putTc :: TcState -> Tc ()
putTc st = Tc $ StateT $ \_ -> Right ((), st)

mapTcError :: Tc a -> (String -> String) -> Tc a
mapTcError computation f = Tc $ StateT $ \st -> case runStateT (unTc computation) st of
  Left msg -> Left (f msg)
  ok -> ok

recoverTc :: Tc a -> (String -> Tc a) -> Tc a
recoverTc computation f = Tc $ StateT $ \st -> case runStateT (unTc computation) st of
  Left msg -> runStateT (unTc (f msg)) st
  ok -> ok

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
      unifyM xRet (TyFun (nonEmptyKnown suffix) yEffs yRet)
    GT -> do
      unifyEffectsM yEffs []
      let (prefix, suffix) = splitAt (length yArgs) xArgs
      unifyListsM prefix yArgs
      unifyM (TyFun (nonEmptyKnown suffix) xEffs xRet) yRet
 where
  xArgs = NE.toList xs
  yArgs = NE.toList ys

nonEmptyKnown :: [a] -> NonEmpty a
nonEmptyKnown (x : xs) = x :| xs
nonEmptyKnown [] = error "internal error: expected non-empty list"

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
    Nothing -> bindTypeHeadM varName (TyHead actualName []) >> unifyListsM varArgs actualArgs

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
            _ -> failTc "func expects one or two type arguments in function unification"

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
    else putTc st{tcMetaSubst = Map.insert ident ty tcMetaSubst}

occursMeta :: Int -> Ty -> TcState -> Bool
occursMeta ident ty st = case applyState ty st of
  TyMeta x -> x == ident
  TyVar _ -> False
  TyCon _ -> False
  TyApp _ args -> any (\t -> occursMeta ident t st) args
  TyRecord fields -> any (\t -> occursMeta ident t st) (Map.elems fields)
  TyFun args effects ret -> any (\t -> occursMeta ident t st) args || any (\t -> occursMeta ident t st) (concreteEffects effects) || occursMeta ident ret st

applyState :: Ty -> TcState -> Ty
applyState ty st@TcState{tcMetaSubst, tcTypeHeads} = go ty
 where
  go = \case
    TyMeta ident -> maybe (TyMeta ident) go (Map.lookup ident tcMetaSubst)
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

instantiateM :: Scheme -> Tc (Ty, [Need])
instantiateM (Forall vars needs ty) = do
  let appliedVars = filter (`elem` vars) (typeAppHeadsInNeeds needs (typeAppHeadsInTy ty []))
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
typeAnnScheme (TypeAnn t needs) ctx = generalizeTyWithNeeds (map (`convertBoneNeed` ctx) needs) (convertTypeExpr t ctx)

convertBoneNeed :: BoneNeed -> TcContext -> Need
convertBoneNeed (BoneNeed args name) ctx = Need (map (`convertTypeExpr` ctx) args) (canonicalBoneName name ctx)

canonicalBoneName :: String -> TcContext -> String
canonicalBoneName name ctx = case findBoneInfo name ctx of
  Just _ -> name
  Nothing -> name

convertTypeExpr :: TypeExpr -> TcContext -> Ty
convertTypeExpr t ctx = case t of
  TypeName name -> atomTypeName name ctx
  TypeApply name args -> atomAppliedTypeName name (map (`convertTypeExpr` ctx) args) ctx
  TypeRecord fields -> TyRecord (Map.fromList [(name, convertTypeExpr ty ctx) | (name, ty) <- fields])
  TypeArrow args effects ret -> TyFun (fmap (`convertTypeExpr` ctx) args) (map (`convertEffectExpr` ctx) effects) (convertTypeExpr ret ctx)
  TypeForall _ body -> convertTypeExpr body ctx

convertEffectExpr :: TypeExpr -> TcContext -> Ty
convertEffectExpr t ctx = case t of
  TypeName name | isEffectVarName name -> TyVar name
  TypeName name -> TyApp name []
  TypeApply name args -> TyApp name (map (`convertTypeExpr` ctx) args)
  _ -> convertTypeExpr t ctx

typeExprFromTy :: Ty -> TcState -> TypeExpr
typeExprFromTy ty st = typeExprFromAppliedTy (normalizeFun (applyState ty st))

boneNeedsFromNeeds :: [Need] -> TcState -> [BoneNeed]
boneNeedsFromNeeds needs st = map needFromAppliedNeed (applyStateNeeds needs st)

needFromAppliedNeed :: Need -> BoneNeed
needFromAppliedNeed (Need args name) = BoneNeed (map typeExprFromAppliedTy args) name

typeExprFromAppliedTy :: Ty -> TypeExpr
typeExprFromAppliedTy = \case
  TyMeta ident -> TypeName ("t" ++ show ident)
  TyVar name -> TypeName name
  TyCon name -> TypeName name
  TyApp name args -> TypeApply name (map typeExprFromAppliedTy args)
  TyRecord fields -> TypeRecord [(name, typeExprFromAppliedTy fieldTy) | (name, fieldTy) <- Map.toList fields]
  TyFun args effects ret -> TypeArrow (fmap typeExprFromAppliedTy args) (map typeExprFromAppliedTy effects) (typeExprFromAppliedTy ret)

specializeBoneType :: [String] -> TypeExpr -> TypeExpr -> TypeExpr
specializeBoneType (param : _) fleshType memberType = substituteTypeExpr param fleshType memberType
specializeBoneType [] _ memberType = memberType

specializeBoneTypeAnn :: [String] -> TypeExpr -> TypeAnn -> TypeAnn
specializeBoneTypeAnn params fleshType (TypeAnn t needs) =
  TypeAnn (specializeAllBoneParams params fleshType t) (map (specializeBoneNeed params fleshType) needs)

specializeBoneNeed :: [String] -> TypeExpr -> BoneNeed -> BoneNeed
specializeBoneNeed params fleshType (BoneNeed args name) =
  BoneNeed (map (specializeAllBoneParams params fleshType) args) name

specializeAllBoneParams :: [String] -> TypeExpr -> TypeExpr -> TypeExpr
specializeAllBoneParams params fleshType ty = foldl' (\current param -> substituteTypeExpr param fleshType current) ty params

substituteTypeExpr :: String -> TypeExpr -> TypeExpr -> TypeExpr
substituteTypeExpr param replacement t = case t of
  TypeName name | name == param -> replacement
  TypeName _ -> t
  TypeApply name args | name == param -> applyReplacementType replacement (map (substituteTypeExpr param replacement) args)
  TypeApply name args -> TypeApply name (map (substituteTypeExpr param replacement) args)
  TypeRecord fields -> TypeRecord [(name, substituteTypeExpr param replacement fieldTy) | (name, fieldTy) <- fields]
  TypeArrow args effects ret -> TypeArrow (fmap (substituteTypeExpr param replacement) args) (map (substituteTypeExpr param replacement) effects) (substituteTypeExpr param replacement ret)
  TypeForall params body -> TypeForall params (substituteTypeExpr param replacement body)

applyReplacementType :: TypeExpr -> [TypeExpr] -> TypeExpr
applyReplacementType replacement args = case replacement of
  TypeName name -> TypeApply name args
  TypeApply name existing -> TypeApply name (existing ++ args)
  _ -> replacement

atomTypeName :: String -> TcContext -> Ty
atomTypeName name ctx
  | name == "func" = TyCon "func"
  | isPrimitiveConcreteTypeName name = TyCon name
  | otherwise = maybe (TyVar name) TyCon (canonicalTypeName name ctx)

atomAppliedTypeName :: String -> [Ty] -> TcContext -> Ty
atomAppliedTypeName "func" args _ = tyAppOrFunc "func" args
atomAppliedTypeName name args ctx = maybe (TyApp name args) (`TyApp` args) (canonicalTypeName name ctx)

isPrimitiveConcreteTypeName :: String -> Bool
isPrimitiveConcreteTypeName name = name `elem` primitiveTypeNames

canonicalTypeName :: String -> TcContext -> Maybe String
canonicalTypeName name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (Alias canonical _) -> Just canonical
  Just (DataInfo canonical _ _) -> Just canonical
  Just (ShownType _) -> Nothing
  Nothing -> Nothing

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
typeVarsInNeed (Need args _) acc = typeVarsInList args acc

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
baseEnvNames = ["to-string", "from-string", "floor", "≤", "+", "-", "×", "÷", "exponent", "logarithm", "sine", "write", "read", "sleep", "random"]
baseRuntimeNames = ["to-string", "from-string", "floor", "≤", "≡", "÷", "×", "-", "+", "exponent", "logarithm", "sine", "read", "write", "sleep", "random"]
baseEffectNames = ["io", "console", "random", "fail", "state", "async"]
runnerEffectNames = ["console", "random", "async"]
primitiveTypeNames = ["integer", "float", "char", "character", "string", "func", "𝟘", "𝟙", "𝟚"]

baseContext :: TcContext
baseContext =
  TcContext
    { tcEnv = baseEnv
    , tcTypes = Map.empty
    , tcBones = Map.empty
    , tcFleshes = primitiveFleshes
    }

primitiveFleshes :: [FleshInfo]
primitiveFleshes =
  [ FleshInfo bone (TypeName ty) []
  | (ty, bones) <-
      [ ("integer", ["equal", "order", "add", "zero", "subtract", "multiply", "one", "semiring", "ring", "mod", "divide", "to-string"])
      , ("float", ["equal", "order", "add", "zero", "subtract", "multiply", "one", "semiring", "ring", "divide", "field", "from-string", "to-string"])
      ]
  , bone <- bones
  ]

runtimeNativeSpecs :: [(String, Int)]
runtimeNativeSpecs =
  [ ("to-string", 1)
  , ("from-string", 1)
  , ("floor", 1)
  , ("≤", 2)
  , ("≡", 2)
  , ("÷", 2)
  , ("exponent", 1)
  , ("logarithm", 1)
  , ("sine", 1)
  , ("×", 2)
  , ("-", 2)
  , ("+", 2)
  ]

runtimeEffectOpSpecs :: [(String, String, Int)]
runtimeEffectOpSpecs =
  [ ("async", "wait", 1)
  , ("async", "fork", 1)
  , ("async", "sleep", 1)
  , ("random", "random", 1)
  , ("console", "read", 1)
  , ("console", "write", 1)
  ]

libraryImportFiles :: [(FilePath, String)]
libraryImportFiles =
  [ ("library/ground.jbini", "ground.jbini")
  , ("library/_foreign.jbini", "foreign.jbini")
  , ("library/data/zero.jbini", "data/zero.jbini")
  , ("library/data/zero.jbini", "zero.jbini")
  , ("library/data/one.jbini", "data/one.jbini")
  , ("library/data/one.jbini", "one.jbini")
  , ("library/data/two.jbini", "data/two.jbini")
  , ("library/data/two.jbini", "two.jbini")
  , ("library/data/product.jbini", "data/product.jbini")
  , ("library/data/product.jbini", "product.jbini")
  , ("library/data/sum.jbini", "data/sum.jbini")
  , ("library/data/sum.jbini", "sum.jbini")
  , ("library/string.jbini", "string.jbini")
  , ("library/string/to-string.jbini", "string/to-string.jbini")
  , ("library/string/from-string.jbini", "string/from-string.jbini")
  , ("library/equal.jbini", "equal.jbini")
  , ("library/order.jbini", "order.jbini")
  , ("library/algebra/category.jbini", "algebra/category.jbini")
  , ("library/algebra/field.jbini", "algebra/field.jbini")
  , ("library/algebra/group.jbini", "algebra/group.jbini")
  , ("library/algebra/monoid.jbini", "algebra/monoid.jbini")
  , ("library/algebra/ring.jbini", "algebra/ring.jbini")
  , ("library/algebra/semigroup.jbini", "algebra/semigroup.jbini")
  , ("library/algebra/semigroupoid.jbini", "algebra/semigroupoid.jbini")
  , ("library/algebra/semiring.jbini", "algebra/semiring.jbini")
  , ("library/collection/applicative.jbini", "collection/applicative.jbini")
  , ("library/collection/catamorphism.jbini", "collection/catamorphism.jbini")
  , ("library/collection/filter.jbini", "collection/filter.jbini")
  , ("library/collection/functor.jbini", "collection/functor.jbini")
  , ("library/collection/list.jbini", "collection/list.jbini")
  , ("library/collection/list.jbini", "list.jbini")
  , ("library/collection/monad.jbini", "collection/monad.jbini")
  , ("library/collection/size.jbini", "collection/size.jbini")
  , ("library/collection/traverse.jbini", "collection/traverse.jbini")
  , ("library/combinator.jbini", "combinator.jbini")
  , ("library/data/natural.jbini", "data/natural.jbini")
  , ("library/data/natural.jbini", "natural.jbini")
  , ("library/data/option.jbini", "data/option.jbini")
  , ("library/data/option.jbini", "option.jbini")
  , ("library/numeric/absolute.jbini", "numeric/absolute.jbini")
  , ("library/numeric/complex.jbini", "numeric/complex.jbini")
  , ("library/numeric/complex.jbini", "complex.jbini")
  , ("library/numeric/constant.jbini", "numeric/constant.jbini")
  , ("library/numeric/constant.jbini", "constant.jbini")
  , ("library/numeric/float.jbini", "numeric/float.jbini")
  , ("library/numeric/integer.jbini", "numeric/integer.jbini")
  ]

importNamespace :: String -> String
importNamespace path = takeWhile (/= '.') path

namespaceImportContext :: String -> TcContext -> TcContext
namespaceImportContext ns TcContext{..} =
  TcContext
    { tcEnv = namespaceImportEnv ns tcEnv
    , tcTypes = namespaceImportTypes ns tcTypes
    , tcBones = namespaceImportBones ns tcBones
    , tcFleshes = namespaceImportFleshes ns tcFleshes
    }

namespaceImportEnv :: String -> [(String, Scheme, Int, String)] -> [(String, Scheme, Int, String)]
namespaceImportEnv ns env = concatMap one env
 where
  hasShownValues = any (\(_, _, rank, origin) -> rank == localEnvRank && isShownOrigin origin) env
  one (name, scheme, rank, origin)
    | isBaseEnvBinding name origin = []
    | otherwise =
        let scheme2 = namespaceScheme ns scheme
            origin2 = ns ++ "@" ++ origin
            exact = (ns ++ "@" ++ name, scheme2, importEnvRank, origin2)
         in if rank == localEnvRank && (not hasShownValues || isShownOrigin origin)
              then (lastQualifiedSegment name, scheme2, aliasEnvRank, origin2) : [exact]
              else [exact]

isShownOrigin :: String -> Bool
isShownOrigin origin = "show@" `isPrefixOf` origin

namespaceImportTypes :: String -> Map.Map String TypeInfo -> Map.Map String TypeInfo
namespaceImportTypes ns types =
  Map.foldlWithKey' step Map.empty
    types
 where
  step acc name info =
    let info2 = namespaceTypeInfo ns info
        withExact = Map.insert (ns ++ "@" ++ name) info2 acc
     in if (typeInfoIsShown info || typeInfoCanonicalName info == name) && not (isQualifiedEnvName name)
          then Map.insert (lastQualifiedSegment name) info2 withExact
          else withExact

namespaceTypeInfo :: String -> TypeInfo -> TypeInfo
namespaceTypeInfo ns = \case
  Alias name params -> Alias (namespaceTypeName ns name) params
  DataInfo name params ctors -> DataInfo (namespaceTypeName ns name) params (namespaceCtorInfos ns params (isQualifiedEnvName name) ctors)
  ShownType info -> ShownType (namespaceTypeInfo ns info)

namespaceTypeName :: String -> String -> String
namespaceTypeName ns name
  | isPrimitiveTypeName name || isQualifiedEnvName name = name
  | otherwise = ns ++ "@" ++ name

typeInfoCanonicalName :: TypeInfo -> String
typeInfoCanonicalName = \case
  Alias name _ -> name
  DataInfo name _ _ -> name
  ShownType info -> typeInfoCanonicalName info

typeInfoIsShown :: TypeInfo -> Bool
typeInfoIsShown = \case
  ShownType _ -> True
  _ -> False

namespaceCtorInfos :: String -> [String] -> Bool -> [(String, [TypeExpr])] -> [(String, [TypeExpr])]
namespaceCtorInfos ns params externalType = map (\(name, fields) -> (namespaceCtorName ns externalType name, map (namespaceTypeExprWith params ns) fields))

namespaceCtorName :: String -> Bool -> String -> String
namespaceCtorName ns externalType name
  | externalType && isQualifiedEnvName name = name
  | otherwise = ns ++ "@" ++ name

namespaceImportBones :: String -> Map.Map String BoneInfo -> Map.Map String BoneInfo
namespaceImportBones ns =
  Map.foldlWithKey' step Map.empty
 where
  step acc name BoneInfo{..} =
    let info = BoneInfo boneParams (namespaceBoneNeedsWith boneParams ns boneNeeds) (namespaceBoneMembers boneParams ns boneMembers)
     in Map.insert (lastQualifiedSegment name) info (Map.insert (ns ++ "@" ++ name) info acc)

namespaceBoneNeedsWith :: [String] -> String -> [BoneNeed] -> [BoneNeed]
namespaceBoneNeedsWith bound ns = map (\(BoneNeed args name) -> BoneNeed (map (namespaceTypeExprWith bound ns) args) (namespaceBoneName ns name))

namespaceBoneName :: String -> String -> String
namespaceBoneName ns name = if isQualifiedEnvName name then name else ns ++ "@" ++ name

namespaceBoneMembers :: [String] -> String -> [BoneMember] -> [BoneMember]
namespaceBoneMembers bound ns = map $ \case
  BoneSpec name ann -> BoneSpec name (namespaceTypeAnnWith bound ns ann)
  BoneDefault name (Just ann) expr -> BoneDefault name (Just (namespaceTypeAnnWith bound ns ann)) expr
  BoneDefault name Nothing expr -> BoneDefault name Nothing expr

namespaceTypeAnnWith :: [String] -> String -> TypeAnn -> TypeAnn
namespaceTypeAnnWith bound ns (TypeAnn t needs) = TypeAnn (namespaceTypeExprWith bound ns t) (namespaceBoneNeedsWith bound ns needs)

namespaceImportFleshes :: String -> [FleshInfo] -> [FleshInfo]
namespaceImportFleshes ns = map $ \FleshInfo{..} ->
  let bound = typeExprVars fleshType ++ boneNeedVars fleshNeeds
   in FleshInfo (namespaceBoneName ns fleshBone) (namespaceTypeExprWith bound ns fleshType) (namespaceBoneNeedsWith bound ns fleshNeeds)

typeExprVars :: TypeExpr -> [String]
typeExprVars = \case
  TypeName name | isTypeVarName name -> [name]
  TypeName _ -> []
  TypeApply _ args -> nub (concatMap typeExprVars args)
  TypeRecord fields -> nub (concatMap (typeExprVars . snd) fields)
  TypeArrow args effects ret -> nub (concatMap typeExprVars (NE.toList args ++ effects ++ [ret]))
  TypeForall params body -> typeExprVars body \\ typeParamNames params

boneNeedVars :: [BoneNeed] -> [String]
boneNeedVars needs = nub [name | BoneNeed args _ <- needs, name <- concatMap typeExprVars args]

namespaceScheme :: String -> Scheme -> Scheme
namespaceScheme ns (Forall vars needs ty) = Forall vars (namespaceNeedsWith vars ns needs) (namespaceTyWith vars ns ty)

namespaceNeedsWith :: [String] -> String -> [Need] -> [Need]
namespaceNeedsWith bound ns = map (namespaceNeedWith bound ns)

namespaceNeedWith :: [String] -> String -> Need -> Need
namespaceNeedWith bound ns (Need args name) = Need (map (namespaceTyWith bound ns) args) (namespaceBoneName ns name)

namespaceTyWith :: [String] -> String -> Ty -> Ty
namespaceTyWith bound ns = \case
  TyCon name -> TyCon (namespaceTyConHeadWith bound ns name)
  TyApp name args -> TyApp (namespaceTyAppHeadWith bound ns name) (map (namespaceTyWith bound ns) args)
  TyRecord fields -> TyRecord (Map.map (namespaceTyWith bound ns) fields)
  TyFun args effects ret -> TyFun (fmap (namespaceTyWith bound ns) args) (namespaceEffectsWith bound ns effects) (namespaceTyWith bound ns ret)
  ty -> ty

namespaceTyConHeadWith :: [String] -> String -> String -> String
namespaceTyConHeadWith bound ns name
  | name `elem` bound || isPrimitiveTypeName name || isQualifiedEnvName name = name
  | otherwise = ns ++ "@" ++ name

namespaceTyAppHeadWith :: [String] -> String -> String -> String
namespaceTyAppHeadWith bound ns name
  | name `elem` bound || isPrimitiveTypeName name || isQualifiedEnvName name = name
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
  | isBaseEffectName name || isQualifiedEnvName name = name
  | otherwise = ns ++ "@" ++ name

namespaceTypeExprWith :: [String] -> String -> TypeExpr -> TypeExpr
namespaceTypeExprWith bound ns = \case
  TypeName name -> TypeName (namespaceTyAppHeadWith bound ns name)
  TypeApply name args -> TypeApply (namespaceTyAppHeadWith bound ns name) (map (namespaceTypeExprWith bound ns) args)
  TypeRecord fields -> TypeRecord [(name, namespaceTypeExprWith bound ns fieldTy) | (name, fieldTy) <- fields]
  TypeArrow args effects ret -> TypeArrow (fmap (namespaceTypeExprWith bound ns) args) (map (namespaceEffectTypeExprWith bound ns) effects) (namespaceTypeExprWith bound ns ret)
  TypeForall params body ->
    let bound2 = typeParamNames params ++ bound
     in TypeForall params (namespaceTypeExprWith bound2 ns body)

namespaceEffectTypeExprWith :: [String] -> String -> TypeExpr -> TypeExpr
namespaceEffectTypeExprWith bound ns = \case
  t@(TypeName name) | isEffectVarName name || isBaseEffectName name || isQualifiedEnvName name -> t
  TypeName name -> TypeName (namespaceEffectHead ns name)
  TypeApply name args -> TypeApply (namespaceEffectHead ns name) (map (namespaceTypeExprWith bound ns) args)
  t -> namespaceTypeExprWith bound ns t

typeParamNames :: [TypeExpr] -> [String]
typeParamNames = mapMaybe \case
  TypeName name -> Just name
  _ -> Nothing

mergeContext :: TcContext -> TcContext -> TcContext
mergeContext left right =
  TcContext
    { tcEnv = tcEnv left ++ tcEnv right
    , tcTypes = Map.union (tcTypes left) (tcTypes right)
    , tcBones = Map.union (tcBones left) (tcBones right)
    , tcFleshes = tcFleshes left ++ tcFleshes right
    }

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

collectProgramContext :: [Decl] -> TcContext -> TcContext
collectProgramContext ds ctx = foldl' (flip collectDeclContext) ctx ds

collectDeclContext :: Decl -> TcContext -> TcContext
collectDeclContext decl ctx = case decl of
  Import _ -> ctx
  ShowDecl _ -> ctx
  ShowTypeDecl _ -> ctx
  DataDecl params name ctors -> collectConstructors name params ctors (addDataTypeInfo name params (dataCtorInfos name ctors) ctx)
  TypeAlias name _ -> addTypeAliasInfo name [] ctx
  EffectDecl params name ops -> collectEffectOps params name ops ctx
  ForeignDecl members -> addForeignMembers members ctx
  BoneDecl params name needs members -> addBoneMembers name params needs members ctx
  Let name (Just ann) _ -> addEnv name (typeAnnScheme ann ctx) ctx
  Let _ Nothing _ -> ctx
  FleshDecl ty boneName needs _ -> addFleshInfo boneName ty needs ctx

addForeignMembers :: [ForeignMember] -> TcContext -> TcContext
addForeignMembers members ctx = foldl' step ctx members
 where
  step current (ForeignMember name ann) = addEnv name (typeAnnScheme ann current) current

dataCtorInfos :: String -> [Ctor] -> [(String, [TypeExpr])]
dataCtorInfos dataName = map (\(Ctor name fields) -> (dataName ++ "@" ++ name, fields))

collectEffectOps :: [String] -> String -> [EffectOp] -> TcContext -> TcContext
collectEffectOps params effectName ops ctx = foldl' step ctx ops
 where
  step current (EffectOp opName t) =
    let opTy = addLatentEffect params effectName (convertTypeExpr t current)
        scheme = generalizeTy opTy
     in addEnvAlias opName (effectName ++ "@" ++ opName) scheme (addEnv (effectName ++ "@" ++ opName) scheme current)

addLatentEffect :: [String] -> String -> Ty -> Ty
addLatentEffect params effectName ty =
  let effect = TyApp effectName (map TyVar params)
   in case normalizeFun ty of
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

addTypeAliasInfo :: String -> [String] -> TcContext -> TcContext
addTypeAliasInfo name params ctx@TcContext{tcTypes} = ctx{tcTypes = Map.insert name (Alias name params) tcTypes}

addDataTypeInfo :: String -> [String] -> [(String, [TypeExpr])] -> TcContext -> TcContext
addDataTypeInfo name params ctors ctx@TcContext{tcTypes} = ctx{tcTypes = Map.insert name (DataInfo name params ctors) tcTypes}

addFleshInfo :: String -> TypeExpr -> [BoneNeed] -> TcContext -> TcContext
addFleshInfo boneName ty needs ctx@TcContext{tcFleshes} = ctx{tcFleshes = FleshInfo boneName ty needs : tcFleshes}

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
  addOne current scheme = addEnvWith exportName scheme localEnvRank ("show@" ++ exportName) current

shownEnvBindings :: String -> TcContext -> [Scheme]
shownEnvBindings name TcContext{tcEnv}
  | isQualifiedEnvName name = maybe [] (: []) (lookupEnvExact name tcEnv)
  | otherwise = case shownBareEnvBindings name tcEnv of
      Right schemes -> schemes
      Left _ -> []

shownBareEnvBindings :: String -> [(String, Scheme, Int, String)] -> Either String [Scheme]
shownBareEnvBindings name env =
  let candidates = collectEnvCandidates name env
   in case candidates of
        [] -> Left ("unknown name '" ++ name ++ "'")
        _ ->
          let bestRank = minimum [rank | (_, rank, _) <- candidates]
              best = [(scheme, origin) | (scheme, rank, origin) <- candidates, rank == bestRank]
              origins = nub (map snd best)
           in case origins of
                [origin] -> Right [scheme | (scheme, foundOrigin) <- best, foundOrigin == origin]
                _ -> Left ("ambiguous name '" ++ name ++ "'; qualify it as one of: " ++ intercalate ", " origins)

addShownType :: String -> TcContext -> TcContext
addShownType name ctx@TcContext{tcTypes} = case shownTypeInfo name ctx of
  Just info -> ctx{tcTypes = Map.insert (lastQualifiedSegment name) (ShownType info) tcTypes}
  Nothing -> ctx

shownTypeInfo :: String -> TcContext -> Maybe TypeInfo
shownTypeInfo name TcContext{tcTypes} = Map.lookup name tcTypes

localEnvRank, aliasEnvRank, importEnvRank, baseEnvRank, selfAliasEnvRank :: Int
localEnvRank = 0
aliasEnvRank = 1
importEnvRank = 1
baseEnvRank = 2
selfAliasEnvRank = 3

lookupEnv :: String -> TcContext -> EnvLookup
lookupEnv name ctx = case lookupEnvChoices name ctx of
  ChoicesMissing -> EnvMissing
  ChoicesAmbiguous msg -> EnvAmbiguous msg
  EnvChoices [] -> EnvMissing
  EnvChoices (scheme : _) -> EnvFound scheme

lookupEnvChoices :: String -> TcContext -> EnvChoices
lookupEnvChoices name TcContext{tcEnv}
  | isQualifiedEnvName name = maybe ChoicesMissing (EnvChoices . (: [])) (lookupEnvExact name tcEnv)
  | otherwise = selectEnvChoices name (collectEnvCandidates name tcEnv)

lookupEnvExact :: String -> [(String, Scheme, Int, String)] -> Maybe Scheme
lookupEnvExact name env = case find (\(n, _, _, _) -> n == name) env of
  Just (_, scheme, _, _) -> Just scheme
  Nothing -> Nothing

collectEnvCandidates :: String -> [(String, Scheme, Int, String)] -> [(Scheme, Int, String)]
collectEnvCandidates name = mapMaybe $ \(n, s, rank, origin) ->
  if n == name then Just (s, rank, origin) else Nothing

selectEnvChoices :: String -> [(Scheme, Int, String)] -> EnvChoices
selectEnvChoices _ [] = ChoicesMissing
selectEnvChoices name candidates =
  let bestRank = minimum [rank | (_, rank, _) <- candidates]
   in case [origin | (_, rank, origin) <- candidates, rank == bestRank] of
        [] -> ChoicesMissing
        bestOrigin : _ ->
          let ambiguous = nub [origin | (_, rank, origin) <- candidates, rank == bestRank, origin /= bestOrigin]
           in case ambiguous of
                [] ->
                  let bestChoices = [scheme | (scheme, rank, origin) <- candidates, rank == bestRank, origin == bestOrigin]
                      lowerChoices = [scheme | (scheme, rank, _) <- candidates, rank > bestRank]
                   in EnvChoices (bestChoices ++ lowerChoices)
                _ -> ChoicesAmbiguous ("ambiguous name '" ++ name ++ "'; qualify it as one of: " ++ intercalate ", " (bestOrigin : ambiguous))

isQualifiedEnvName :: String -> Bool
isQualifiedEnvName = elem '@'

lastQualifiedSegment :: String -> String
lastQualifiedSegment name = reverse (takeWhile (/= '@') (reverse name))

baseEnv :: [(String, Scheme, Int, String)]
baseEnv =
  map
    (uncurry baseBinding)
    [ ("to-string", native1 "integer" [] "string")
    , ("to-string", native1 "float" [] "string")
    , ("from-string", native1 "string" [nativeEffect1 "fail" "string"] "float")
    , ("floor", native1 "float" [] "integer")
    , ("≤", native2 "integer" "integer" [] "𝟚")
    , ("≤", native2 "float" "float" [] "𝟚")
    , ("+", native2 "integer" "integer" [] "integer")
    , ("+", native2 "float" "float" [] "float")
    , ("×", native2 "integer" "integer" [] "integer")
    , ("×", native2 "float" "float" [] "float")
    , ("-", native2 "integer" "integer" [] "integer")
    , ("-", native2 "float" "float" [] "float")
    , ("÷", native2 "integer" "integer" [nativeEffect1 "fail" "string"] "integer")
    , ("÷", native2 "float" "float" [nativeEffect1 "fail" "string"] "float")
    , ("exponent", native1 "float" [] "float")
    , ("logarithm", native1 "float" [] "float")
    , ("sine", native1 "float" [] "float")
    , ("write", native1 "string" [nativeEffect "console"] "𝟙")
    , ("read", native1 "𝟙" [nativeEffect "console"] "string")
    , ("sleep", native1 "integer" [nativeEffect "async"] "𝟙")
    , ("random", native1 "𝟙" [nativeEffect "random"] "float")
    ]

baseBinding :: String -> Scheme -> (String, Scheme, Int, String)
baseBinding name scheme = (name, scheme, baseEnvRank, "base@" ++ name)

nativeEffect :: String -> Ty
nativeEffect name = TyApp name []

nativeEffect1 :: String -> String -> Ty
nativeEffect1 name arg = TyApp name [TyCon arg]

native1 :: String -> [Ty] -> String -> Scheme
native1 arg effects ret = Forall [] [] (curriedFunction [TyCon arg] effects (TyCon ret))

native2 :: String -> String -> [Ty] -> String -> Scheme
native2 a b effects ret = Forall [] [] (curriedFunction [TyCon a, TyCon b] effects (TyCon ret))

addBoneMembers :: String -> [String] -> [BoneNeed] -> [BoneMember] -> TcContext -> TcContext
addBoneMembers boneName params needs members ctx = addBoneMembersToEnv boneName members (addBoneInfo boneName params needs members ctx)

addBoneInfo :: String -> [String] -> [BoneNeed] -> [BoneMember] -> TcContext -> TcContext
addBoneInfo boneName params needs members ctx@TcContext{tcBones} = ctx{tcBones = Map.insert boneName (BoneInfo params needs members) tcBones}

addBoneMembersToEnv :: String -> [BoneMember] -> TcContext -> TcContext
addBoneMembersToEnv boneName members ctx = foldl' step ctx members
 where
  step current member = case boneMemberSignature member of
    Nothing -> current
    Just (name, ann) ->
      let scheme = boneMemberScheme boneName ann current
       in addEnvAlias name (boneName ++ "@" ++ name) scheme (addEnv (boneName ++ "@" ++ name) scheme current)

boneMemberScheme :: String -> TypeAnn -> TcContext -> Scheme
boneMemberScheme boneName ann ctx =
  let scheme = typeAnnScheme (typeAnnWithOwnBoneNeed boneName ann ctx) ctx
   in case findBoneInfo boneName ctx of
        Just BoneInfo{boneParams} -> addForallVars boneParams scheme
        Nothing -> scheme

addForallVars :: [String] -> Scheme -> Scheme
addForallVars extra (Forall vars needs ty) = Forall (nub (extra ++ vars)) needs ty

typeAnnWithOwnBoneNeed :: String -> TypeAnn -> TcContext -> TypeAnn
typeAnnWithOwnBoneNeed boneName (TypeAnn t needs) ctx = TypeAnn t (ownBoneNeed boneName ctx ++ needs)

ownBoneNeed :: String -> TcContext -> [BoneNeed]
ownBoneNeed boneName ctx = case findBoneInfo boneName ctx of
  Just BoneInfo{boneParams} -> [BoneNeed (map TypeName boneParams) boneName]
  Nothing -> []

boneMemberSignature :: BoneMember -> Maybe (String, TypeAnn)
boneMemberSignature = \case
  BoneSpec name t -> Just (name, t)
  BoneDefault name (Just t) _ -> Just (name, t)
  BoneDefault _ Nothing _ -> Nothing

findBoneInfo :: String -> TcContext -> Maybe BoneInfo
findBoneInfo boneName TcContext{tcBones} = case Map.lookup boneName tcBones of
  Just info -> Just info
  Nothing -> case nub [info | (name, info) <- Map.toList tcBones, boneNamesMatch name boneName] of
    [info] -> Just info
    _ -> Nothing

findBoneMember :: String -> [BoneMember] -> Maybe TypeAnn
findBoneMember name members = do
  (_, t) <- find ((== name) . fst) (mapMaybe boneMemberSignature members)
  pure t

bindPatternsM :: [Pattern] -> [Ty] -> TcContext -> Tc TcContext
bindPatternsM patterns tys ctx
  | length patterns == length tys = foldM step ctx (zip patterns tys)
  | otherwise = failTc "pattern arity mismatch"
 where
  step currentCtx (pattern, ty) = bindPatternM pattern ty currentCtx

bindPatternM :: Pattern -> Ty -> TcContext -> Tc TcContext
bindPatternM (PVar "_") _ ctx = pure ctx
bindPatternM (PVar name) expected ctx = bindBarePatternNameM name expected ctx
bindPatternM (PCon name args) expected ctx = do
  Inferred conTy _ _ <- inferVarM name ctx
  case normalizeFun conTy of
    TyFun fields _ result -> unifyM result expected >> bindPatternsM args (NE.toList fields) ctx
    _ -> case args of
      [] -> unifyM conTy expected >> pure ctx
      _ -> failTc ("constructor '" ++ name ++ "' is not a function")

bindBarePatternNameM :: String -> Ty -> TcContext -> Tc TcContext
bindBarePatternNameM name expected ctx = case lookupEnv name ctx of
  EnvMissing -> pure (addEnv name (Forall [] [] expected) ctx)
  EnvAmbiguous msg -> failTc msg
  EnvFound scheme -> do
    st0 <- getTc
    (t, _) <- instantiateM scheme
    case normalizeFun t of
      TyFun _ _ _ -> putTc st0 >> pure (addEnv name (Forall [] [] expected) ctx)
      actual -> recoverTc (unifyM actual expected >> pure ctx) (\_ -> putTc st0 >> pure (addEnv name (Forall [] [] expected) ctx))

checkExhaustiveM :: [Ty] -> NonEmpty MatchCase -> TcContext -> Tc ()
checkExhaustiveM scrutTys cases ctx = do
  st <- getTc
  case uncoveredPatterns (applyStateAll scrutTys st) (matchRows cases) ctx st of
    Nothing -> pure ()
    Just witness -> failTc ("non-exhaustive match; missing case " ++ showPatternList witness)

uncoveredPatterns :: [Ty] -> [[Pattern]] -> TcContext -> TcState -> Maybe [Pattern]
uncoveredPatterns [] rows _ _ = if any null rows then Nothing else Just []
uncoveredPatterns (ty : restTys) rows ctx st =
  case constructorsForType ty ctx st of
    Nothing -> (PVar "_" :) <$> uncoveredPatterns restTys (defaultRows rows) ctx st
    Just [] -> Nothing
    Just ctors ->
      case uncoveredPatterns restTys (defaultRowsForConstructors (ctorNames ctors) rows) ctx st of
        Nothing -> Nothing
        Just _ -> uncoveredConstructor ctors (ctorNames ctors) restTys rows ctx st

uncoveredConstructor :: [(String, [Ty])] -> [String] -> [Ty] -> [[Pattern]] -> TcContext -> TcState -> Maybe [Pattern]
uncoveredConstructor ctors allCtorNames restTys rows ctx st = foldr firstUncovered Nothing ctors
 where
  firstUncovered (ctorName, fieldTys) fallback =
    case uncoveredPatterns (fieldTys ++ restTys) (specializeRows ctorName (length fieldTys) allCtorNames rows) ctx st of
      Nothing -> fallback
      Just witness ->
        let (args, restWitness) = splitAt (length fieldTys) witness
         in Just (PCon (lastQualifiedSegment ctorName) args : restWitness)

ctorNames :: [(String, [Ty])] -> [String]
ctorNames = map fst

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
findTypeInfo name TcContext{tcTypes} = Map.lookup name tcTypes

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

specializeRows :: String -> Int -> [String] -> [[Pattern]] -> [[Pattern]]
specializeRows ctorName arity allCtorNames = mapMaybe (specializeRow ctorName arity allCtorNames)

specializeRow :: String -> Int -> [String] -> [Pattern] -> Maybe [Pattern]
specializeRow _ _ _ [] = Nothing
specializeRow ctorName arity allCtorNames (PVar name : rest)
  | isNullaryConstructorPatternFor name ctorName arity = Just rest
  | isConstructorPatternName name allCtorNames = Nothing
  | otherwise = Just (replicate arity (PVar "_") ++ rest)
specializeRow ctorName _ _ (PCon name args : rest)
  | constructorNamesMatch name ctorName = Just (args ++ rest)
specializeRow _ _ _ _ = Nothing

defaultRows :: [[Pattern]] -> [[Pattern]]
defaultRows = mapMaybe $ \case
  PVar _ : ps -> Just ps
  _ -> Nothing

defaultRowsForConstructors :: [String] -> [[Pattern]] -> [[Pattern]]
defaultRowsForConstructors ctorNames0 = mapMaybe $ \case
  PVar name : _ | isConstructorPatternName name ctorNames0 -> Nothing
  PVar _ : ps -> Just ps
  _ -> Nothing

constructorNamesMatch :: String -> String -> Bool
constructorNamesMatch patternName ctorName =
  if isQualifiedEnvName patternName
    then patternName == ctorName
    else patternName == lastQualifiedSegment ctorName

isNullaryConstructorPatternFor :: String -> String -> Int -> Bool
isNullaryConstructorPatternFor patternName ctorName arity = arity == 0 && isConstructorPatternName patternName [ctorName]

isConstructorPatternName :: String -> [String] -> Bool
isConstructorPatternName patternName = any (constructorNamesMatch patternName)

showPatternList :: [Pattern] -> String
showPatternList [] = "()"
showPatternList ps = intercalate ", " (map showPattern ps)

showPattern :: Pattern -> String
showPattern = \case
  PVar name -> name
  PCon name [] -> name
  PCon name args -> "$" ++ name ++ " " ++ showPatternList args

inferExprM :: Expr -> TcContext -> Tc Inferred
inferExprM expr ctx = case expr of
  EInteger _ -> pure (Inferred (TyCon "integer") [] [])
  EFloat _ -> pure (Inferred (TyCon "float") [] [])
  EChar _ -> pure (Inferred (TyCon "character") [] [])
  EString _ -> pure (Inferred (TyCon "string") [] [])
  EVar name -> inferVarM name ctx
  EApply f args -> inferApplyM f (NE.toList args) ctx
  ERecord fields -> inferRecordFieldsM fields ctx
  EUpdate base updates -> inferRecordUpdateM base updates ctx
  ETry body returnCase cases -> inferTryHandleM body returnCase cases ctx
  EMatch scrutinees cases -> inferMatchM scrutinees cases ctx
  EBlock ds body -> inferBlockM ds body ctx

inferRecordFieldsM :: [(String, Expr)] -> TcContext -> Tc Inferred
inferRecordFieldsM fields ctx = do
  (recordTy, effects, needs) <- foldM step (Map.empty, [], []) fields
  pure (Inferred (TyRecord recordTy) effects needs)
 where
  step (acc, effects, needs) (name, expr) = do
    Inferred t fieldEffects fieldNeeds <- inferExprM expr ctx
    pure (Map.insert name t acc, unionEffects effects fieldEffects, unionNeeds needs fieldNeeds)

inferRecordUpdateM :: Expr -> [RecordUpdate] -> TcContext -> Tc Inferred
inferRecordUpdateM base updates ctx = do
  Inferred baseTy baseEffects baseNeeds <- inferExprM base ctx
  st <- getTc
  case normalizeFun (applyState baseTy st) of
    TyRecord fields -> inferRecordUpdatesM updates fields ctx baseEffects baseNeeds
    other -> failTc ("record update expected record, found " ++ showTy other)

inferRecordUpdatesM :: [RecordUpdate] -> Map.Map String Ty -> TcContext -> [Ty] -> [Need] -> Tc Inferred
inferRecordUpdatesM updates fields ctx effects0 needs0 = do
  (updatedFields, effects, needs) <- foldM step (fields, effects0, needs0) updates
  pure (Inferred (TyRecord updatedFields) effects needs)
 where
  step (currentFields, effects, needs) = \case
    RecordRemove name ->
      if Map.member name currentFields
        then pure (Map.delete name currentFields, effects, needs)
        else failTc ("unknown record field '" ++ name ++ "'")
    RecordSet name expr -> do
      Inferred fieldTy fieldEffects fieldNeeds <- inferExprM expr ctx
      pure (Map.insert name fieldTy currentFields, unionEffects effects fieldEffects, unionNeeds needs fieldNeeds)

inferBlockM :: [Decl] -> Expr -> TcContext -> Tc Inferred
inferBlockM ds body ctx = do
  (ctx2, declEffects) <- checkLocalDeclsM ds ctx
  Inferred bodyTy bodyEffects bodyNeeds <- inferExprM body ctx2
  pure (Inferred bodyTy (unionEffects declEffects bodyEffects) bodyNeeds)

inferVarM :: String -> TcContext -> Tc Inferred
inferVarM name ctx = case lookupEnv name ctx of
  EnvMissing -> failTc ("unknown name '" ++ name ++ "'")
  EnvAmbiguous msg -> failTc msg
  EnvFound scheme -> do
    (ty, needs) <- instantiateM scheme
    pure (Inferred ty [] needs)

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
  ChoicesMissing -> failTc ("unknown name '" ++ name ++ "'")
  ChoicesAmbiguous msg -> failTc msg
  EnvChoices schemes -> tryApplySchemesM name args schemes ctx Nothing

tryApplySchemesM :: String -> [Expr] -> [Scheme] -> TcContext -> Maybe String -> Tc Inferred
tryApplySchemesM name args schemes ctx firstError = foldr tryScheme noMore schemes firstError
 where
  noMore err = failTc (fromMaybe ("unknown name '" ++ name ++ "'") err)
  tryScheme scheme fallback err =
    recoverTc
      (instantiateM scheme >>= \(t, needs) -> inferCurriedApplyM (Inferred t [] needs) args ctx)
      (\msg -> fallback (keepFirstError err msg))

keepFirstError :: Maybe String -> String -> Maybe String
keepFirstError Nothing msg = Just msg
keepFirstError some@(Just _) _ = some

inferCurriedApplyM :: Inferred -> [Expr] -> TcContext -> Tc Inferred
inferCurriedApplyM fn args ctx = foldM step fn args
 where
  step (Inferred fTy fEffs fNeeds) arg = do
    Inferred argTy argEffs argNeeds <- inferExprM arg ctx
    retTy <- freshM
    latentEffects <- unifyApplyFunctionM fTy argTy retTy
    let effects = unionEffects fEffs (unionEffects argEffs latentEffects)
        needs = unionNeeds fNeeds argNeeds
    st <- getTc
    pure (Inferred (applyState retTy st) effects (applyStateNeeds needs st))

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
  Inferred bodyTy bodyEffects bodyNeeds <- inferExprM body ctx
  st <- getTc
  Inferred answerTy returnEffects returnNeeds <- inferReturnCaseM returnCase (applyState bodyTy st) ctx
  inferHandlerCasesM cases answerTy bodyEffects (unionNeeds bodyNeeds returnNeeds) returnEffects ctx

inferReturnCaseM :: Maybe ReturnCase -> Ty -> TcContext -> Tc Inferred
inferReturnCaseM Nothing bodyTy _ = pure (Inferred bodyTy [] [])
inferReturnCaseM (Just (ReturnCase pattern body)) bodyTy ctx = do
  ctx2 <- bindPatternM pattern bodyTy ctx
  inferExprM body ctx2

inferHandlerCasesM :: [HandlerCase] -> Ty -> [Ty] -> [Need] -> [Ty] -> TcContext -> Tc Inferred
inferHandlerCasesM cases answerTy effects bodyNeeds returnEffects ctx = do
  (finalAnswerTy, handled, accumulatedEffects, accumulatedNeeds) <- foldM step (answerTy, [], returnEffects, bodyNeeds) cases
  st <- getTc
  let remainingEffects = removeEffectNames handled (applyStateEffects effects st)
  pure (Inferred (applyState finalAnswerTy st) (unionEffects remainingEffects accumulatedEffects) (applyStateNeeds accumulatedNeeds st))
 where
  step (currentAnswerTy, handled, accumulatedEffects, accumulatedNeeds) (HandlerCase name patterns handlerBody) = do
    (handledName, handlerCtx) <- handlerCaseContextM name patterns currentAnswerTy effects ctx
    Inferred handlerTy caseEffects caseNeeds <- inferExprM handlerBody handlerCtx
    mapTcError (unifyM currentAnswerTy handlerTy) (\msg -> "in handler for '" ++ name ++ "': " ++ msg)
    st <- getTc
    pure
      ( applyState currentAnswerTy st
      , addHandledEffectName handledName handled
      , unionEffects accumulatedEffects (applyStateEffects caseEffects st)
      , unionNeeds accumulatedNeeds (applyStateNeeds caseNeeds st)
      )

handlerCaseContextM :: String -> [Pattern] -> Ty -> [Ty] -> TcContext -> Tc (String, TcContext)
handlerCaseContextM name patterns answerTy effects ctx = case lookupEnv name ctx of
  EnvMissing -> effectHandlerCaseContextM name patterns effects ctx
  EnvAmbiguous msg -> failTc msg
  EnvFound scheme
    | null patterns -> recoverTc (operationHandlerCaseContextM name patterns answerTy effects scheme ctx) (\_ -> effectHandlerCaseContextM name patterns effects ctx)
    | otherwise -> operationHandlerCaseContextM name patterns answerTy effects scheme ctx

effectHandlerCaseContextM :: String -> [Pattern] -> [Ty] -> TcContext -> Tc (String, TcContext)
effectHandlerCaseContextM effectName patterns effects ctx = case patterns of
  [] -> do
    st <- getTc
    if any (effectHasName effectName) (applyStateEffects effects st)
      then pure (effectName, ctx)
      else failTc ("handler for absent effect '" ++ effectName ++ "'")
  _ -> failTc ("effect-level handler '" ++ effectName ++ "' cannot bind operation arguments")

operationHandlerCaseContextM :: String -> [Pattern] -> Ty -> [Ty] -> Scheme -> TcContext -> Tc (String, TcContext)
operationHandlerCaseContextM name patterns answerTy effects scheme ctx = do
  (t, _) <- instantiateM scheme
  case normalizeFun t of
    TyFun argTys opEffects retTy -> do
      handledEffect <- findHandledOperationEffectM name opEffects effects
      ctx2 <- mapTcError (bindHandlerPatternsM patterns (NE.toList argTys) ctx) (\msg -> "in handler for '" ++ name ++ "': " ++ msg)
      st <- getTc
      let handledName = handledEffectName handledEffect st
          resumeEffects = removeEffect handledName (applyStateEffects effects st)
          resumeTy = curriedFunction [applyState retTy st] resumeEffects (applyState answerTy st)
      pure (handledName, addEnv "resume" (Forall [] [] resumeTy) ctx2)
    _ -> failTc ("handler case '" ++ name ++ "' is not an effect operation")

bindHandlerPatternsM :: [Pattern] -> [Ty] -> TcContext -> Tc TcContext
bindHandlerPatternsM [] _ ctx = pure ctx
bindHandlerPatternsM patterns argTys ctx = bindPatternsM patterns argTys ctx

findHandledOperationEffectM :: String -> [Ty] -> [Ty] -> Tc Ty
findHandledOperationEffectM name opEffects effects =
  foldr tryEffect notFound opEffects
 where
  notFound = failTc ("handler case '" ++ name ++ "' is not an effect operation")
  tryEffect opEffect fallback = do
    st <- getTc
    case findEffectByHead opEffect (applyStateEffects effects st) of
      Nothing -> fallback
      Just actualEffect -> unifyEffectM opEffect actualEffect >> (applyState actualEffect <$> getTc)

handledEffectName :: Ty -> TcState -> String
handledEffectName effect st = maybe (showEffect effect) fst (effectNameAndArgs (applyState effect st))

removeEffectNames :: [String] -> [Ty] -> [Ty]
removeEffectNames names effects = foldl' (flip removeEffect) effects names

addHandledEffectName :: String -> [String] -> [String]
addHandledEffectName = addUnique

inferMatchM :: [Expr] -> NonEmpty MatchCase -> TcContext -> Tc Inferred
inferMatchM [] cases ctx = do
  scrutTys <- freshManyM (matchCaseArity cases)
  Inferred branchTy branchEffects branchNeeds <- inferMatchCasesM cases scrutTys ctx
  st <- getTc
  checkExhaustiveM (applyStateAll scrutTys st) cases ctx
  st2 <- getTc
  pure (Inferred (curriedFunction (applyStateAll scrutTys st2) branchEffects branchTy) [] (applyStateNeeds branchNeeds st2))
inferMatchM scrutinees cases ctx = do
  inferred <- inferExprListM scrutinees ctx
  let scrutTys = [t | Inferred t _ _ <- inferred]
      scrutEffects = foldl' unionEffects [] [effects | Inferred _ effects _ <- inferred]
      scrutNeeds = foldl' unionNeeds [] [needs | Inferred _ _ needs <- inferred]
  Inferred branchTy branchEffects branchNeeds <- inferMatchCasesM cases scrutTys ctx
  st <- getTc
  checkExhaustiveM (applyStateAll scrutTys st) cases ctx
  st2 <- getTc
  pure (Inferred branchTy (unionEffects (applyStateEffects scrutEffects st2) branchEffects) (unionNeeds (applyStateNeeds scrutNeeds st2) branchNeeds))

inferMatchCasesM :: NonEmpty MatchCase -> [Ty] -> TcContext -> Tc Inferred
inferMatchCasesM cases scrutTys ctx = do
  (branchTy, _, effects, needs) <- foldM step (Nothing, scrutTys, [], []) (NE.toList cases)
  st <- getTc
  case branchTy of
    Just t -> pure (Inferred (applyState t st) effects (applyStateNeeds needs st))
    Nothing -> failTc "empty match"
 where
  step (branchTy, currentScrutTys, effects, needs) (MatchCase ps e) = do
    ctx2 <- bindPatternsM (NE.toList ps) currentScrutTys ctx
    Inferred t caseEffects caseNeeds <- inferExprM e ctx2
    mapM_ (`unifyM` t) branchTy
    st <- getTc
    pure
      ( Just (applyState t st)
      , applyStateAll currentScrutTys st
      , unionEffects effects caseEffects
      , unionNeeds needs caseNeeds
      )

checkDeclsM :: [Decl] -> TcContext -> Tc Program
checkDeclsM ds ctx = checkDeclsContextM ds ctx >> pure (Program [])

checkRunnableDeclsM :: [Decl] -> TcContext -> Tc Program
checkRunnableDeclsM ds ctx = do
  (_, effects, needs, value) <- foldM inferRunnableDeclM (ctx, [], [], TyCon "𝟙") ds
  checkRunnableTypeM value effects needs ctx

checkDeclsContextM :: [Decl] -> TcContext -> Tc TcContext
checkDeclsContextM ds ctx = foldM step ctx ds
 where
  step currentCtx (Let name Nothing expr) = fst <$> inferUnannotatedBindingM name expr currentCtx
  step currentCtx (Let name (Just ann) expr) = do
    checkTypeAnnNeedsM ("let '" ++ name ++ "'") ann currentCtx
    (checkedValue, _, checkedNeeds) <- checkAnnotatedExprM name ann expr currentCtx
    scheme <- schemeFromCheckedAnn checkedValue checkedNeeds
    pure (addEnv name scheme currentCtx)
  step currentCtx (ShowDecl name) = checkShowDeclM name currentCtx >> pure (addShownEnv name currentCtx)
  step currentCtx (ShowTypeDecl name) = checkShowTypeDeclM name currentCtx >> pure (addShownType name currentCtx)
  step currentCtx d = checkDeclM d currentCtx >> pure currentCtx

inferRunnableDeclM :: (TcContext, [Ty], [Need], Ty) -> Decl -> Tc (TcContext, [Ty], [Need], Ty)
inferRunnableDeclM (ctx, effects, needs, fileValue) (Let name ann expr) = do
  (ctx2, value, letEffects, letNeeds) <- inferRunnableLetM name ann expr ctx
  let nextValue = if name == "_" then value else fileValue
  pure (ctx2, unionEffects effects letEffects, unionNeeds needs letNeeds, nextValue)
inferRunnableDeclM (ctx, effects, needs, fileValue) (ShowDecl name) =
  checkShowDeclM name ctx >> pure (addShownEnv name ctx, effects, needs, fileValue)
inferRunnableDeclM (ctx, effects, needs, fileValue) (ShowTypeDecl name) =
  checkShowTypeDeclM name ctx >> pure (addShownType name ctx, effects, needs, fileValue)
inferRunnableDeclM (ctx, effects, needs, fileValue) d =
  checkDeclM d ctx >> pure (ctx, effects, needs, fileValue)

inferRunnableLetM :: String -> Maybe TypeAnn -> Expr -> TcContext -> Tc (TcContext, Ty, [Ty], [Need])
inferRunnableLetM name ann expr ctx = case ann of
  Nothing -> do
    Inferred value effects needs <- inferNamedM name expr ctx
    st <- getTc
    normalizedNeeds <- normalizeNeedsM ctx needs
    let emittedNeeds = if name == "_" then normalizedNeeds else []
    pure (addEnv name (generalizeApplied value normalizedNeeds st) ctx, value, effects, emittedNeeds)
  Just ann -> do
    checkTypeAnnNeedsM ("let '" ++ name ++ "'") ann ctx
    (checkedValue, effects, checkedNeeds) <- checkAnnotatedExprM name ann expr ctx
    scheme <- schemeFromCheckedAnn checkedValue checkedNeeds
    pure (addEnv name scheme ctx, checkedValue, effects, [])

checkRunnableTypeM :: Ty -> [Ty] -> [Need] -> TcContext -> Tc Program
checkRunnableTypeM value effects needs ctx = do
  st <- getTc
  let actualValue = applyState value st
  recoverTc (unifyM actualValue (TyCon "𝟙")) (\_ -> failTc ("runnable file must return 𝟙, found " ++ showTy actualValue))
  checkRunnableEffectsM effects
  requireNoNeedsM ctx needs

checkRunnableEffectsM :: [Ty] -> Tc Program
checkRunnableEffectsM effects = do
  st <- getTc
  let actualEffects = applyStateEffects effects st
  if allRunnableEffects actualEffects
    then pure (Program [])
    else failTc ("runnable file has unhandled effects {" ++ showEffects actualEffects ++ "}; only console, random, and async may remain")

allRunnableEffects :: [Ty] -> Bool
allRunnableEffects = all isRunnableEffect

isRunnableEffect :: Ty -> Bool
isRunnableEffect effect = case effectNameAndArgs effect of
  Just (name, []) -> name `elem` runnerEffectNames
  _ -> False

checkDeclM :: Decl -> TcContext -> Tc Decl
checkDeclM d ctx = case d of
  Let name ann expr -> checkLetM name ann expr ctx
  ShowDecl name -> checkShowDeclM name ctx >> pure d
  ShowTypeDecl name -> checkShowTypeDeclM name ctx >> pure d
  EffectDecl _ effectName ops -> checkEffectDeclM effectName ops d
  ForeignDecl members -> checkForeignDeclM members ctx d
  BoneDecl _ boneName needs members -> checkBoneDeclM boneName needs members ctx d
  FleshDecl ty boneName needs members -> checkFleshM ty boneName needs members ctx
  _ -> pure d

checkShowDeclM :: String -> TcContext -> Tc ()
checkShowDeclM name ctx = case shownEnvBindingsResult name ctx of
  Right _ -> pure ()
  Left msg -> failTc msg

checkShowTypeDeclM :: String -> TcContext -> Tc ()
checkShowTypeDeclM name ctx = case shownTypeInfo name ctx of
  Just _ -> pure ()
  Nothing -> failTc ("unknown type '" ++ name ++ "'")

shownEnvBindingsResult :: String -> TcContext -> Either String [Scheme]
shownEnvBindingsResult name TcContext{tcEnv}
  | isQualifiedEnvName name = case lookupEnvExact name tcEnv of
      Just scheme -> Right [scheme]
      Nothing -> Left ("unknown name '" ++ name ++ "'")
  | otherwise = shownBareEnvBindings name tcEnv

checkEffectDeclM :: String -> [EffectOp] -> Decl -> Tc Decl
checkEffectDeclM effectName ops whole = mapM_ checkOp ops >> pure whole
 where
  checkOp (EffectOp opName t)
    | not (isFunctionTypeExpr t) = failTc ("effect operation '" ++ effectName ++ "@" ++ opName ++ "' must have a function type")
    | hasTopLevelArrowEffects t = failTc ("effect operation '" ++ effectName ++ "@" ++ opName ++ "' cannot declare extra top-level effects")
    | otherwise = pure ()

isFunctionTypeExpr :: TypeExpr -> Bool
isFunctionTypeExpr (TypeForall _ body) = isFunctionTypeExpr body
isFunctionTypeExpr (TypeArrow _ _ _) = True
isFunctionTypeExpr _ = False

hasTopLevelArrowEffects :: TypeExpr -> Bool
hasTopLevelArrowEffects (TypeForall _ body) = hasTopLevelArrowEffects body
hasTopLevelArrowEffects (TypeArrow _ effects _) = not (null effects)
hasTopLevelArrowEffects _ = False

checkForeignDeclM :: [ForeignMember] -> TcContext -> Decl -> Tc Decl
checkForeignDeclM members ctx whole = mapM_ checkMember members >> pure whole
 where
  checkMember (ForeignMember name ann@(TypeAnn t _)) = do
    checkTypeAnnNeedsM ("foreign '" ++ name ++ "'") ann ctx
    unless (isFunctionTypeExpr t) $
      failTc ("foreign '" ++ name ++ "' must have a function type")

inferBoneDefaultDeclsM :: [Decl] -> TcContext -> Tc ([Decl], TcContext)
inferBoneDefaultDeclsM ds ctx = do
  (acc, finalCtx) <- foldM step ([], ctx) ds
  pure (reverse acc, finalCtx)
 where
  step (acc, currentCtx) (BoneDecl params boneName needs members) = do
    let knownCtx = addBoneMembers boneName params needs members currentCtx
    typedMembers <- inferBoneDefaultMembersM params boneName members knownCtx
    let typedDecl = BoneDecl params boneName needs typedMembers
        ctx2 = addBoneMembers boneName params needs typedMembers currentCtx
    pure (typedDecl : acc, ctx2)
  step (acc, currentCtx) d =
    pure (d : acc, collectDeclContext d currentCtx)

inferBoneDefaultMembersM :: [String] -> String -> [BoneMember] -> TcContext -> Tc [BoneMember]
inferBoneDefaultMembersM boneParams boneName members ctx =
  reverse . fst <$> foldM step ([], ctx) members
 where
  step (acc, currentCtx) member = case member of
    BoneSpec name t -> pure (BoneSpec name t : acc, currentCtx)
    BoneDefault name (Just t) expr -> do
      let typedMember = BoneDefault name (Just t) expr
      pure (typedMember : acc, addBoneMembersToEnv boneName [typedMember] currentCtx)
    BoneDefault name Nothing expr -> do
      t <- mapTcError (inferBoneDefaultTypeM boneParams name expr currentCtx) (\msg -> "in bone default '" ++ name ++ "': " ++ msg)
      let typedMember = BoneDefault name (Just t) expr
      pure (typedMember : acc, addBoneMembersToEnv boneName [typedMember] currentCtx)

inferBoneDefaultTypeM :: [String] -> String -> Expr -> TcContext -> Tc TypeAnn
inferBoneDefaultTypeM boneParams name expr ctx = case expr of
  EMatch [] (MatchCase ps body :| []) -> do
    let argTypeExprs = replicate (NE.length ps) (defaultBoneArgumentType boneParams)
        argTys = map (`convertTypeExpr` ctx) argTypeExprs
    ctx2 <- bindPatternsM (NE.toList ps) argTys ctx
    Inferred ret effects needs <- inferNamedM name body ctx2
    st <- getTc
    pure (TypeAnn (typeExprFromTy (curriedFunction argTys effects ret) st) (boneNeedsFromNeeds needs st))
  _ -> do
    Inferred t effects needs <- inferNamedM name expr ctx
    st <- getTc
    if null (applyStateEffects effects st)
      then pure (TypeAnn (typeExprFromTy t st) (boneNeedsFromNeeds needs st))
      else failTc ("effectful bone default '" ++ name ++ "' must be a function")

defaultBoneArgumentType :: [String] -> TypeExpr
defaultBoneArgumentType (param : _) = TypeName param
defaultBoneArgumentType [] = TypeName "t0"

checkBoneDeclM :: String -> [BoneNeed] -> [BoneMember] -> TcContext -> Decl -> Tc Decl
checkBoneDeclM boneName needs members ctx whole =
  checkBoneNeedsM ("bone '" ++ boneName ++ "'") needs ctx >> checkBoneMemberNeedsM boneName members ctx >> checkBoneDefaultsM boneName members ctx whole

checkBoneNeedsM :: String -> [BoneNeed] -> TcContext -> Tc ()
checkBoneNeedsM owner needs ctx = mapM_ checkNeed needs
 where
  checkNeed (BoneNeed args neededName) = case findBoneInfo neededName ctx of
    Nothing -> failTc (owner ++ " has unknown given bone '" ++ neededName ++ "'")
    Just BoneInfo{boneParams} -> do
      let expected = length boneParams
          actual = length args
      unless (actual == expected) $
        failTc (owner ++ " has given '" ++ neededName ++ "' with " ++ show actual ++ " arguments, but '" ++ neededName ++ "' has " ++ show expected ++ " parameters")

checkBoneMemberNeedsM :: String -> [BoneMember] -> TcContext -> Tc ()
checkBoneMemberNeedsM boneName members ctx = mapM_ checkMember members
 where
  checkMember member = case boneMemberSignature member of
    Nothing -> pure ()
    Just (name, TypeAnn _ needs) -> checkBoneNeedsM ("bone member '" ++ boneName ++ "@" ++ name ++ "'") needs ctx

checkTypeAnnNeedsM :: String -> TypeAnn -> TcContext -> Tc ()
checkTypeAnnNeedsM owner (TypeAnn _ needs) = checkBoneNeedsM owner needs

checkBoneDefaultsM :: String -> [BoneMember] -> TcContext -> Decl -> Tc Decl
checkBoneDefaultsM boneName members ctx whole = mapM_ checkMember members >> pure whole
 where
  checkMember (BoneSpec _ _) = pure ()
  checkMember (BoneDefault name (Just t) expr) =
    checkRecursiveLetAnnotatedAliasM name (boneName ++ "@" ++ name) (typeAnnWithOwnBoneNeed boneName t ctx) expr ctx >> pure ()
  checkMember (BoneDefault name Nothing _) = failTc ("bone default '" ++ name ++ "' has no inferred type")

checkLetM :: String -> Maybe TypeAnn -> Expr -> TcContext -> Tc Decl
checkLetM name Nothing expr ctx = do
  Inferred _ _ needs <- inferNamedM name expr ctx
  _ <- normalizeNeedsM ctx needs
  pure (Let name Nothing expr)
checkLetM name (Just ann) expr ctx = do
  checkTypeAnnNeedsM ("let '" ++ name ++ "'") ann ctx
  checkAnnotatedExprM name ann expr ctx
  pure (Let name (Just ann) expr)

checkFleshM :: TypeExpr -> String -> [BoneNeed] -> [Decl] -> TcContext -> Tc Decl
checkFleshM tyExpr boneName needs members ctx = do
  checkBoneNeedsM ("flesh '" ++ boneName ++ "'") needs ctx
  case findBoneInfo boneName ctx of
    Nothing -> checkLooseFleshMembersM members ctx (FleshDecl tyExpr boneName needs members)
    Just _ -> case fleshSpecsForBone boneName tyExpr ctx [] of
      Nothing -> failTc ("flesh '" ++ boneName ++ "' has an unknown required bone")
      Just specs -> checkFleshMembersM tyExpr needs members specs ctx (FleshDecl tyExpr boneName needs members)

checkLooseFleshMembersM :: [Decl] -> TcContext -> Decl -> Tc Decl
checkLooseFleshMembersM members ctx whole = mapM_ checkMember members >> pure whole
 where
  checkMember (Let name _ expr) = checkRecursiveLetM name expr ctx >> pure ()
  checkMember _ = failTc "flesh member must be a let"

data FleshSpec = FleshSpec String String TypeAnn deriving (Eq, Show)

fleshSpecsForBone :: String -> TypeExpr -> TcContext -> [String] -> Maybe [FleshSpec]
fleshSpecsForBone boneName fleshType ctx seen
  | boneName `elem` seen = Just []
  | otherwise = case findBoneInfo boneName ctx of
      Nothing -> Nothing
      Just BoneInfo{..} -> do
        inherited <- fleshSpecsForNeeds boneNeeds boneParams fleshType ctx (boneName : seen)
        pure (fleshMemberSpecs boneName boneParams fleshType boneMembers ++ inherited)

fleshSpecsForNeeds :: [BoneNeed] -> [String] -> TypeExpr -> TcContext -> [String] -> Maybe [FleshSpec]
fleshSpecsForNeeds needs currentParams currentFleshType ctx seen =
  concat <$> traverse specsForNeed needs
 where
  specsForNeed (BoneNeed args neededName) = do
    neededFleshType <- case map (specializeBoneType currentParams currentFleshType) args of
      [] -> Nothing
      x : _ -> Just x
    fleshSpecsForBone neededName neededFleshType ctx seen

fleshMemberSpecs :: String -> [String] -> TypeExpr -> [BoneMember] -> [FleshSpec]
fleshMemberSpecs boneName boneParams fleshType members =
  [FleshSpec name boneName (specializeBoneTypeAnn boneParams fleshType ann) | (name, ann) <- mapMaybe boneMemberSignature members]

findFleshSpec :: String -> [FleshSpec] -> Maybe FleshSpec
findFleshSpec name = find (\(FleshSpec n _ _) -> n == name)

checkFleshMembersM :: TypeExpr -> [BoneNeed] -> [Decl] -> [FleshSpec] -> TcContext -> Decl -> Tc Decl
checkFleshMembersM tyExpr needs members specs ctx whole = mapM_ checkMember members >> pure whole
 where
  checkMember (Let name _ expr) = case findFleshSpec name specs of
    Nothing -> failTc ("flesh defines unknown member '" ++ name ++ "'")
    Just (FleshSpec _ ownerName memberType) ->
      checkRecursiveLetAnnotatedAliasM name (ownerName ++ "@" ++ name) (typeAnnWithFleshNeeds tyExpr ownerName needs memberType) expr ctx >> pure ()
  checkMember _ = failTc "flesh member must be a let"

typeAnnWithFleshNeeds :: TypeExpr -> String -> [BoneNeed] -> TypeAnn -> TypeAnn
typeAnnWithFleshNeeds tyExpr boneName needs (TypeAnn t memberNeeds) =
  TypeAnn t (BoneNeed [tyExpr] boneName : needs ++ memberNeeds)

checkLocalDeclsM :: [Decl] -> TcContext -> Tc (TcContext, [Ty])
checkLocalDeclsM ds ctx = foldM step (ctx, []) ds
 where
  step (currentCtx, currentEffects) (Let name ann expr) = do
    (ctx2, letEffects) <- inferLocalLetM name ann expr currentCtx
    pure (ctx2, unionEffects currentEffects letEffects)
  step acc _ = pure acc

inferLocalLetM :: String -> Maybe TypeAnn -> Expr -> TcContext -> Tc (TcContext, [Ty])
inferLocalLetM name Nothing expr ctx = inferUnannotatedBindingM name expr ctx
inferLocalLetM name (Just ann) expr ctx = do
  checkTypeAnnNeedsM ("let '" ++ name ++ "'") ann ctx
  (checkedValue, effects, checkedNeeds) <- checkAnnotatedExprM name ann expr ctx
  scheme <- schemeFromCheckedAnn checkedValue checkedNeeds
  pure (addEnv name scheme ctx, effects)

schemeFromCheckedAnn :: Ty -> [Need] -> Tc Scheme
schemeFromCheckedAnn checkedValue checkedNeeds = do
  st <- getTc
  pure (generalizeApplied checkedValue checkedNeeds st)

inferUnannotatedBindingM :: String -> Expr -> TcContext -> Tc (TcContext, [Ty])
inferUnannotatedBindingM name expr@(EMatch [] _) ctx = do
  selfTy <- freshM
  let ctxSelf = addEnv name (Forall [] [] selfTy) ctx
  Inferred t effects needs <- inferNamedM name expr ctxSelf
  mapTcError (unifyM selfTy t) (\msg -> "in '" ++ name ++ "': " ++ msg)
  st <- getTc
  normalizedNeeds <- normalizeNeedsM ctx needs
  pure (addEnv name (generalizeApplied t normalizedNeeds st) ctx, effects)
inferUnannotatedBindingM name expr ctx = do
  Inferred t effects needs <- inferNamedM name expr ctx
  st <- getTc
  normalizedNeeds <- normalizeNeedsM ctx needs
  pure (addEnv name (generalizeApplied t normalizedNeeds st) ctx, effects)

inferNamedM :: String -> Expr -> TcContext -> Tc Inferred
inferNamedM name expr ctx = mapTcError (inferExprM expr ctx) (\msg -> "in '" ++ name ++ "': " ++ msg)

instantiateAnnotationM :: TypeAnn -> TcContext -> Tc (Ty, [Ty], [Need])
instantiateAnnotationM ann ctx = do
  (ty, needs) <- instantiateM (typeAnnScheme ann ctx)
  pure (ty, [], needs)

checkAnnotatedExprM :: String -> TypeAnn -> Expr -> TcContext -> Tc (Ty, [Ty], [Need])
checkAnnotatedExprM name ann expr ctx = do
  (expectedValue, expectedEffects, expectedNeeds) <- instantiateAnnotationM ann ctx
  checkExprAgainstM name expectedValue expectedEffects expectedNeeds expr ctx

checkExprAgainstM :: String -> Ty -> [Ty] -> [Need] -> Expr -> TcContext -> Tc (Ty, [Ty], [Need])
checkExprAgainstM name expectedValue expectedEffects expectedNeeds expr ctx = do
  Inferred actual effects needs <- inferNamedM name expr ctx
  mapTcError (unifyM actual expectedValue) (\msg -> "in '" ++ name ++ "': " ++ msg ++ "; actual " ++ showTy actual ++ ", expected " ++ showTy expectedValue)
  (checkedValue, checkedEffects) <- checkEffectsAgainstM name expectedValue effects expectedEffects
  checkNeedsAgainstM name ctx needs expectedNeeds
  st <- getTc
  pure (checkedValue, checkedEffects, applyStateNeeds expectedNeeds st)

checkEffectsAgainstM :: String -> Ty -> [Ty] -> [Ty] -> Tc (Ty, [Ty])
checkEffectsAgainstM name expectedValue actualEffects0 expectedEffects0 = do
  st <- getTc
  let actualEffects = applyStateEffects actualEffects0 st
      expectedEffects = applyStateEffects expectedEffects0 st
  recoverTc
    ( do
        unifyEffectsM actualEffects expectedEffects
        st2 <- getTc
        pure (applyState expectedValue st2, applyStateEffects actualEffects st2)
    )
    (\_ -> failTc ("in '" ++ name ++ "': cannot unify effects {" ++ showEffects actualEffects ++ "} with {" ++ showEffects expectedEffects ++ "}"))

checkNeedsAgainstM :: String -> TcContext -> [Need] -> [Need] -> Tc ()
checkNeedsAgainstM name ctx actualNeeds0 expectedNeeds0 = do
  st <- getTc
  let actualNeeds = applyStateNeeds actualNeeds0 st
      expectedNeeds = applyStateNeeds expectedNeeds0 st
      unexpected = filter (not . needResolvedBy expectedNeeds ctx) actualNeeds
  unless (null unexpected) $
    failTc ("in '" ++ name ++ "': missing given " ++ showNeeds unexpected)

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
  | need `elem` seen = if needMayRemainOpen need then pure [need] else failTc ("missing given " ++ showNeed need)
  | otherwise = case fleshNeedsForNeed ctx need of
      Just needs -> foldM step [] needs
      Nothing
        | needMayRemainOpen need -> pure [need]
        | otherwise -> failTc ("missing given " ++ showNeed need)
 where
  step acc nested = do
    normalized <- normalizeNeedM ctx (need : seen) nested
    pure (unionNeeds acc normalized)

requireNoNeedsM :: TcContext -> [Need] -> Tc Program
requireNoNeedsM ctx needs = do
  remaining <- normalizeNeedsM ctx needs
  if null remaining
    then pure (Program [])
    else failTc ("runnable file has unresolved givens " ++ showNeeds remaining)

needResolvedBy :: [Need] -> TcContext -> Need -> Bool
needResolvedBy expected ctx = go []
 where
  go seen need
    | any (\given -> needCovers ctx [] given need) expected = True
    | need `elem` seen = False
    | otherwise = case fleshNeedsForNeed ctx need of
        Just needs -> all (go (need : seen)) needs
        Nothing -> False

needSubsumes :: Need -> Need -> Bool
needSubsumes (Need expectedArgs expectedName) (Need actualArgs actualName) =
  boneNamesMatch expectedName actualName
    && length expectedArgs == length actualArgs
    && and (zipWith typePatternMatches expectedArgs actualArgs)

needCovers :: TcContext -> [String] -> Need -> Need -> Bool
needCovers ctx seen given@(Need givenArgs givenName) wanted
  | needSubsumes given wanted = True
  | givenName `elem` seen = False
  | otherwise = case findBoneInfo givenName ctx of
      Nothing -> False
      Just BoneInfo{boneParams, boneNeeds} ->
        any (\need -> needCovers ctx (givenName : seen) (specializeNeed boneParams givenArgs need) wanted) boneNeeds

specializeNeed :: [String] -> [Ty] -> BoneNeed -> Need
specializeNeed params args (BoneNeed needArgs needName) =
  let repls = Map.fromList (zip params args)
   in Need (map ((`replaceVars` repls) . (`convertTypeExpr` baseContext)) needArgs) needName

fleshNeedsForNeed :: TcContext -> Need -> Maybe [Need]
fleshNeedsForNeed ctx@TcContext{tcFleshes} (Need [ty] boneName) =
  if hasConcreteHead ty then firstJust (map matches tcFleshes) else Nothing
 where
  matches FleshInfo{..}
    | not (boneNamesMatch fleshBone boneName) = Nothing
    | otherwise = do
        bindings <- typePatternBindings (convertTypeExpr fleshType ctx) ty
        pure (map (needWithBindings ctx bindings) fleshNeeds)
fleshNeedsForNeed _ _ = Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : xs) = firstJust xs

needWithBindings :: TcContext -> Map.Map String Ty -> BoneNeed -> Need
needWithBindings ctx bindings (BoneNeed args name) =
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
  (TyMeta _, _) -> Just Map.empty
  (_, TyMeta _) -> Just Map.empty
  (TyCon x, TyCon y) | typeHeadsMatch x y -> Just Map.empty
  (TyApp x xs, TyApp y ys)
    | typeHeadsMatch x y && length xs == length ys -> mergeBindingMaps <$> zipWithMMaybe typePatternBindings xs ys
    | isTypeVarName x && length xs == length ys -> Map.insert x (TyCon y) . mergeBindingMaps <$> zipWithMMaybe typePatternBindings xs ys
  (TyRecord xs, TyRecord ys)
    | Map.keys xs == Map.keys ys -> mergeBindingMaps <$> traverse recordField (Map.toList xs)
   where
    recordField (name, x) = Map.lookup name ys >>= typePatternBindings x
  (TyFun xs xEffs xRet, TyFun ys yEffs yRet)
    | length xs == length ys && length xEffs == length yEffs ->
        mergeBindingMaps <$> zipWithMMaybe typePatternBindings (NE.toList xs ++ xEffs ++ [xRet]) (NE.toList ys ++ yEffs ++ [yRet])
  _ -> Nothing

zipWithMMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithMMaybe _ [] [] = Just []
zipWithMMaybe f (x : xs) (y : ys) = (:) <$> f x y <*> zipWithMMaybe f xs ys
zipWithMMaybe _ _ _ = Nothing

mergeBindingMaps :: [Map.Map String Ty] -> Map.Map String Ty
mergeBindingMaps = foldl' (Map.unionWith preferSpecific) Map.empty
 where
  preferSpecific left right = if left == right then left else right

hasConcreteHead :: Ty -> Bool
hasConcreteHead ty = case normalizeFun ty of
  TyCon _ -> True
  TyApp _ _ -> True
  TyFun _ _ _ -> True
  _ -> False

needMayRemainOpen :: Need -> Bool
needMayRemainOpen (Need args _) = any (not . hasConcreteHead) args

typePatternMatches :: Ty -> Ty -> Bool
typePatternMatches patternTy actualTy = case (normalizeFun patternTy, normalizeFun actualTy) of
  (TyVar _, _) -> True
  (TyMeta _, _) -> True
  (TyCon x, TyCon y) -> typeHeadsMatch x y
  (TyApp x xs, TyApp y ys) -> typeHeadsMatch x y && length xs == length ys && and (zipWith typePatternMatches xs ys)
  (TyRecord xs, TyRecord ys) -> Map.keys xs == Map.keys ys && and [maybe False (typePatternMatches x) (Map.lookup name ys) | (name, x) <- Map.toList xs]
  (TyFun xs xEffs xRet, TyFun ys yEffs yRet) ->
    length xs == length ys
      && and (zipWith typePatternMatches (NE.toList xs) (NE.toList ys))
      && length xEffs == length yEffs
      && and (zipWith typePatternMatches xEffs yEffs)
      && typePatternMatches xRet yRet
  _ -> False

boneNamesMatch :: String -> String -> Bool
boneNamesMatch x y = x == y || lastQualifiedSegment x == lastQualifiedSegment y

typeHeadsMatch :: String -> String -> Bool
typeHeadsMatch = boneNamesMatch

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
      repls = Map.fromList (zip metaIds names)
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
firstMetaVarName taken ident = go ident
 where
  go n =
    let name = "m" ++ show n
     in if name `elem` taken then go (n + 1) else name

replaceNeedMetas :: Map.Map Int String -> Need -> Need
replaceNeedMetas repls (Need args name) = Need (map (replaceTyMetas repls) args) name

replaceTyMetas :: Map.Map Int String -> Ty -> Ty
replaceTyMetas repls ty = case ty of
  TyMeta ident -> TyVar (Map.findWithDefault ("m" ++ show ident) ident repls)
  _ -> mapTyChildren (replaceTyMetas repls) (replaceTyMetas repls) ty

checkRecursiveLetM :: String -> Expr -> TcContext -> Tc Decl
checkRecursiveLetM name expr ctx = do
  selfTy <- freshM
  let ctx2 = addEnv name (Forall [] [] selfTy) ctx
  Inferred actual _ needs <- inferNamedM name expr ctx2
  mapTcError (unifyM selfTy actual) (\msg -> "in '" ++ name ++ "': " ++ msg)
  _ <- normalizeNeedsM ctx needs
  pure (Let name Nothing expr)

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

check :: String -> String
check source = checkWithImports source Map.empty

checkWithImports :: String -> Map.Map String String -> String
checkWithImports source imports = checkPrepared source imports False

checkRunnableWithImports :: String -> Map.Map String String -> String
checkRunnableWithImports source imports = checkPrepared source imports True

checkPrepared :: String -> Map.Map String String -> Bool -> String
checkPrepared source imports runnable = case parse source of
  Left msg -> "parse error: " ++ msg
  Right p -> case inferProgramWithMode p imports runnable of
    TcErr msg -> "type error: " ++ msg
    TcOk _ _ -> "type ok"

inferProgramWithMode :: Program -> Map.Map String String -> Bool -> TcResult Program
inferProgramWithMode p imports runnable = runTc (inferProgramWithModeM p imports runnable) initialTcState

inferProgramWithModeM :: Program -> Map.Map String String -> Bool -> Tc Program
inferProgramWithModeM (Program ds) imports runnable = do
  (typedDs, ctx) <- prepareDeclsM ds imports baseContext
  if runnable then checkRunnableDeclsM typedDs ctx else checkDeclsM typedDs ctx

inferProgramContext :: Program -> Map.Map String String -> TcContext -> TcResult TcContext
inferProgramContext p imports baseCtx = runTc (inferProgramContextM p imports baseCtx) initialTcState

inferProgramContextM :: Program -> Map.Map String String -> TcContext -> Tc TcContext
inferProgramContextM (Program ds) imports baseCtx = do
  (typedDs, ctx) <- prepareDeclsM ds imports baseCtx
  checkDeclsContextM typedDs ctx

prepareDeclsM :: [Decl] -> Map.Map String String -> TcContext -> Tc ([Decl], TcContext)
prepareDeclsM ds imports baseCtx = do
  importCtx <- collectImportContextsM ds imports baseCtx
  (typedDs, _) <- inferBoneDefaultDeclsM ds importCtx
  pure (typedDs, collectProgramContext typedDs importCtx)

collectImportContextsM :: [Decl] -> Map.Map String String -> TcContext -> Tc TcContext
collectImportContextsM ds imports ctx = foldM step ctx ds
 where
  step currentCtx (Import path) = case Map.lookup path imports of
    Nothing -> failTc ("missing bring '" ++ path ++ "'")
    Just source -> case parse source of
      Left msg -> failTc ("in bring '" ++ path ++ "': " ++ msg)
      Right p -> do
        importedCtx <- importedContextM path p imports
        pure (mergeContext (namespaceImportContext (importNamespace path) importedCtx) currentCtx)
  step currentCtx _ = pure currentCtx

importedContextM :: String -> Program -> Map.Map String String -> Tc TcContext
importedContextM path p imports = Tc $ StateT $ \st -> case inferProgramContext p imports baseContext of
  TcErr msg -> Left ("in bring '" ++ path ++ "': " ++ msg)
  TcOk importedCtx _ -> Right (importedCtx, st)
