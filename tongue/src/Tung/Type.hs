module Tung.Type (
  Ty (..),
  Scheme (..),
  EnvLookup (..),
  TcContext (..),
  TcState (..),
  TcResult (..),
  ShapeInfo (..),
  check,
  checkWithImports,
  checkEditorWithImports,
  checkRunnableWithImports,
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
)
where

import Control.Monad (foldM, replicateM, unless, void, zipWithM, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..), get, put, runStateT)
import Data.Bifunctor (bimap)
import Data.Char (isAscii, isDigit, isLower)
import Data.List (find, intercalate, isPrefixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Tung.Import (ImportStack, enterImport)
import Tung.Name (importNamespace, isQualifiedName, lastQualifiedSegment, splitFieldAccessName)
import Tung.Parse (parse)
import Tung.Syntax
import Tung.Validate (validateProgram)

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

data Scheme
  = Forall [String] [Need] Ty
  | EffectOpForall String [String] [Need] Ty
  deriving (Eq, Show)

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

data TypeInfo
  = Alias String Ty
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
  }
  deriving (Eq, Show)

data TcContext = TcContext
  { tcEnv :: [(String, Scheme, Int, String)]
  , tcTypes :: Map.Map String TypeInfo
  , tcTypeAmbiguities :: Map.Map String [String]
  , tcShapes :: Map.Map String ShapeInfo
  , tcFills :: [FillInfo]
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
failTc = Tc . lift . Left

getTc :: Tc TcState
getTc = Tc get

putTc :: TcState -> Tc ()
putTc = Tc . put

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
nonEmptyKnown = fromMaybe (error "internal error: expected non-empty list") . NE.nonEmpty

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
  TypeAnn (specializeAllShapeParams params fillTypes t) (map (specializeShapeNeed params fillTypes) needs)

specializeShapeNeed :: [String] -> [TypeExpr] -> ShapeNeed -> ShapeNeed
specializeShapeNeed params fillTypes (ShapeNeed args name) =
  ShapeNeed (map (specializeAllShapeParams params fillTypes) args) name

specializeAllShapeParams :: [String] -> [TypeExpr] -> TypeExpr -> TypeExpr
specializeAllShapeParams params fillTypes = replaceShapeParams (Map.fromList (zip params fillTypes))

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
  | Just target <- aliasTarget name ctx = target
  | Just canonical <- canonicalTypeName name ctx = TyCon canonical
  | isQualifiedName name = TyCon name
  | otherwise = TyVar name

atomAppliedTypeName :: String -> [Ty] -> TcContext -> Ty
atomAppliedTypeName "func" args _ = tyAppOrFunc "func" args
atomAppliedTypeName name args ctx = maybe (TyApp name args) (`TyApp` args) (canonicalTypeName name ctx)

aliasTarget :: String -> TcContext -> Maybe Ty
aliasTarget name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (Alias _ target) -> Just target
  _ -> Nothing

isPrimitiveConcreteTypeName :: String -> Bool
isPrimitiveConcreteTypeName name = name `elem` primitiveTypeNames

canonicalTypeName :: String -> TcContext -> Maybe String
canonicalTypeName name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (Alias canonical _) -> Just canonical
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
baseEnvNames = ["to-string", "from-string", "⌊", "≤", "+", "-", "×", "÷", "exponent", "logarithm", "sine", "write", "read", "sleep", "random", "read-file", "write-file", "append-file"]
baseRuntimeNames = ["to-string", "from-string", "⌊", "≤", "≡", "÷", "×", "-", "+", "exponent", "logarithm", "sine", "read", "write", "sleep", "random", "read-file", "write-file", "append-file"]
baseEffectNames = ["io", "console", "random", "fail", "state", "async", "file"]
runnerEffectNames = ["console", "random", "async", "file"]
primitiveTypeNames = ["integer", "float", "character", "string", "func"]

baseContext :: TcContext
baseContext =
  TcContext
    { tcEnv = baseEnv
    , tcTypes = Map.empty
    , tcTypeAmbiguities = Map.empty
    , tcShapes = Map.empty
    , tcFills = primitiveFills
    }

primitiveFills :: [FillInfo]
primitiveFills =
  [ FillInfo shape [TypeName ty] [] shape
  | (ty, shapes) <-
      [ ("integer", ["equal", "order", "add", "zero", "subtract", "multiply", "one", "semiring", "ring", "mod", "divide", "to-string"])
      , ("float", ["equal", "order", "add", "zero", "subtract", "multiply", "one", "semiring", "ring", "divide", "field", "from-string", "to-string"])
      ]
  , shape <- shapes
  ]

runtimeNativeSpecs :: [(String, Int)]
runtimeNativeSpecs =
  [ ("to-string", 1)
  , ("from-string", 1)
  , ("⌊", 1)
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
  , ("file", "read-file", 1)
  , ("file", "write-file", 2)
  , ("file", "append-file", 2)
  ]

namespaceImportContext :: String -> TcContext -> TcContext
namespaceImportContext ns TcContext{..} =
  TcContext
    { tcEnv = namespaceImportEnv ns tcEnv
    , tcTypes = namespaceImportTypes ns tcTypes
    , tcTypeAmbiguities = Map.empty
    , tcShapes = namespaceImportShapes ns tcShapes
    , tcFills = namespaceImportFills ns tcFills
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
  Alias name target ->
    let bound = typeVarsInTy target []
     in Alias (namespaceTypeName ns name) (namespaceTyWith bound ns target)
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

namespaceTypeAnnWith :: [String] -> String -> TypeAnn -> TypeAnn
namespaceTypeAnnWith bound ns (TypeAnn t needs) = TypeAnn (namespaceTypeExprWith bound ns t) (namespaceShapeNeedsWith bound ns needs)

typeAnnVars :: TypeAnn -> [String]
typeAnnVars (TypeAnn t needs) = nub (typeExprVars t ++ shapeNeedVars needs)

namespaceImportFills :: String -> [FillInfo] -> [FillInfo]
namespaceImportFills ns = map $ \FillInfo{..} ->
  let bound = nub (concatMap typeExprVars fillTypes ++ shapeNeedVars fillNeeds)
   in FillInfo
        (namespaceShapeName ns fillShape)
        (map (namespaceTypeExprWith bound ns) fillTypes)
        (namespaceShapeNeedsWith bound ns fillNeeds)
        (namespaceShapeName ns fillOrigin)

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
  TyCon name -> TyCon (namespaceTyConHeadWith bound ns name)
  TyApp name args -> TyApp (namespaceTyAppHeadWith bound ns name) (map (namespaceTyWith bound ns) args)
  TyRecord fields -> TyRecord (Map.map (namespaceTyWith bound ns) fields)
  TyFun args effects ret -> TyFun (fmap (namespaceTyWith bound ns) args) (namespaceEffectsWith bound ns effects) (namespaceTyWith bound ns ret)
  ty -> ty

namespaceTyConHeadWith :: [String] -> String -> String -> String
namespaceTyConHeadWith bound ns name
  | name `elem` bound || isPrimitiveTypeName name || isQualifiedName name = name
  | otherwise = ns ++ "@" ++ name

namespaceTyAppHeadWith :: [String] -> String -> String -> String
namespaceTyAppHeadWith bound ns name
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
  TypeName name -> TypeName (namespaceTyAppHeadWith bound ns name)
  TypeApply name args -> TypeApply (namespaceTyAppHeadWith bound ns name) (map (namespaceTypeExprWith bound ns) args)
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
    , tcFills = tcFills left ++ tcFills right
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
  Alias name _ -> [name]
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

collectProgramContext :: [Decl] -> TcContext -> TcContext
collectProgramContext ds ctx = foldl' (flip collectDeclContext) ctx ds

collectDeclContext :: Decl -> TcContext -> TcContext
collectDeclContext decl ctx = case decl of
  Import _ -> ctx
  Export declaration -> exportDeclContext declaration (collectDeclContext declaration ctx)
  ReExport _ -> ctx
  ReExportType _ -> ctx
  DataDecl params name ctors -> collectConstructors name params ctors (addDataTypeInfo name params (dataCtorInfos name ctors) ctx)
  TypeAlias name target -> addTypeAliasInfo name target ctx
  EffectDecl params name ops -> collectEffectOps params name ops (addEffectTypeInfo name params ctx)
  ForeignDecl members -> addForeignMembers members ctx
  ShapeDecl params name needs members -> addShapeMembers name params needs members ctx
  Let name (Just ann) _ -> addEnv name (typeAnnScheme ann ctx) ctx
  Let _ Nothing _ -> ctx
  FillDecl tyArgs shapeName needs _ -> addFillInfo shapeName tyArgs needs ctx

exportDeclContext :: Decl -> TcContext -> TcContext
exportDeclContext declaration ctx = case declaration of
  Import _ -> ctx
  Export nested -> exportDeclContext nested ctx
  ReExport name -> either (const ctx) id (reExportName name ctx)
  ReExportType name -> either (const ctx) id (reExportTypeName name ctx)
  Let name _ _ -> addShownEnv name ctx
  TypeAlias name _ -> addShownType name ctx
  DataDecl _ name constructors -> foldl' (flip addShownEnv) (addShownType name ctx) [ctorName | Ctor ctorName _ <- constructors]
  EffectDecl _ name operations -> foldl' (flip addShownEnv) (addShownType name ctx) [opName | EffectOp opName _ <- operations]
  ForeignDecl members -> foldl' (flip addShownEnv) ctx [name | ForeignMember name _ <- members]
  ShapeDecl _ name _ members -> foldl' (flip addShownEnv) (markShapeExported name ctx) (mapMaybe (fmap fst . shapeMemberSignature) members)
  FillDecl{} -> ctx

markShapeExported :: String -> TcContext -> TcContext
markShapeExported name ctx@TcContext{tcShapes} =
  case findShapeEntry name ctx of
    Just (_, info) -> ctx{tcShapes = Map.insert (lastQualifiedSegment name) info{shapeExported = True} tcShapes}
    Nothing -> ctx

reExportName :: String -> TcContext -> Either String TcContext
reExportName name ctx = reExportTerm name ctx >>= maybe (Left ("unknown name '" ++ name ++ "'")) Right

reExportTypeName :: String -> TcContext -> Either String TcContext
reExportTypeName name ctx =
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

markImportedContextExported :: TcContext -> TcContext
markImportedContextExported ctx@TcContext{..} =
  ctx
    { tcEnv = map markEnv tcEnv
    , tcTypes = Map.mapWithKey markType tcTypes
    , tcShapes = Map.mapWithKey markShape tcShapes
    }
 where
  markEnv binding@(name, scheme, rank, _)
    | rank == aliasEnvRank && not (isQualifiedName name) = (name, scheme, exportEnvRank, "show@" ++ name)
    | otherwise = binding
  markType name info
    | not (isQualifiedName name) = ShownType info
    | otherwise = info
  markShape name info
    | not (isQualifiedName name) = info{shapeExported = True}
    | otherwise = info

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
        scheme = markEffectOpScheme effectName (generalizeTy opTy)
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

addTypeAliasInfo :: String -> TypeExpr -> TcContext -> TcContext
addTypeAliasInfo name target ctx@TcContext{tcTypes, tcTypeAmbiguities} =
  ctx
    { tcTypes = Map.insert name (Alias name (convertTypeExpr target ctx)) tcTypes
    , tcTypeAmbiguities = Map.delete name tcTypeAmbiguities
    }

addDataTypeInfo :: String -> [String] -> [(String, [TypeExpr])] -> TcContext -> TcContext
addDataTypeInfo name params ctors ctx@TcContext{tcTypes, tcTypeAmbiguities} =
  ctx
    { tcTypes = Map.insert name (DataInfo name params (map (fmap (map (canonicalTypeExpr ctx))) ctors)) tcTypes
    , tcTypeAmbiguities = Map.delete name tcTypeAmbiguities
    }

addEffectTypeInfo :: String -> [String] -> TcContext -> TcContext
addEffectTypeInfo name params ctx@TcContext{tcTypes, tcTypeAmbiguities} =
  ctx
    { tcTypes = Map.insert name (EffectInfo name params) tcTypes
    , tcTypeAmbiguities = Map.delete name tcTypeAmbiguities
    }

addFillInfo :: String -> [TypeExpr] -> [ShapeNeed] -> TcContext -> TcContext
addFillInfo shapeName tyArgs needs ctx@TcContext{tcFills} =
  let actualShape = canonicalShapeName shapeName ctx
      actualTypes = map (canonicalTypeExpr ctx) tyArgs
      actualNeeds = map (canonicalShapeNeed ctx) needs
   in ctx{tcFills = fillClosure actualShape actualShape actualTypes actualNeeds ctx [] ++ tcFills}

fillClosure :: String -> String -> [TypeExpr] -> [ShapeNeed] -> TcContext -> [String] -> [FillInfo]
fillClosure origin shapeName tyArgs needs ctx seen
  | shapeName `elem` seen = []
  | otherwise = FillInfo shapeName tyArgs needs origin : inherited
 where
  inherited = case findShapeInfo shapeName ctx of
    Just ShapeInfo{shapeParams, shapeNeeds} -> concatMap closeNeed shapeNeeds
     where
      closeNeed (ShapeNeed arguments neededShape) =
        let neededTypes = map (specializeShapeType shapeParams tyArgs) arguments
         in fillClosure origin neededShape neededTypes needs ctx (shapeName : seen)
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

baseEnv :: [(String, Scheme, Int, String)]
baseEnv =
  map (uncurry baseBinding) baseNativeEnv ++ map baseEffectOpBinding baseEffectOpEnv

baseNativeEnv :: [(String, Scheme)]
baseNativeEnv =
  [ ("to-string", native1 "integer" [] "string")
  , ("to-string", native1 "float" [] "string")
  , ("from-string", native1 "string" [nativeEffect1 "fail" "string"] "float")
  , ("⌊", native1 "float" [] "integer")
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
  ]

baseEffectOpEnv :: [(String, String, Scheme)]
baseEffectOpEnv =
  [ ("console", "write", native1 "string" [nativeEffect "console"] "𝟙")
  , ("console", "read", native1 "𝟙" [nativeEffect "console"] "string")
  , ("async", "sleep", native1 "integer" [nativeEffect "async"] "𝟙")
  , ("random", "random", native1 "𝟙" [nativeEffect "random"] "float")
  , ("file", "read-file", native1 "string" [nativeEffect "file", nativeEffect1 "fail" "string"] "string")
  , ("file", "write-file", native2 "string" "string" [nativeEffect "file", nativeEffect1 "fail" "string"] "𝟙")
  , ("file", "append-file", native2 "string" "string" [nativeEffect "file", nativeEffect1 "fail" "string"] "𝟙")
  ]

baseBinding :: String -> Scheme -> (String, Scheme, Int, String)
baseBinding name scheme = (name, scheme, baseEnvRank, "base@" ++ name)

baseEffectOpBinding :: (String, String, Scheme) -> (String, Scheme, Int, String)
baseEffectOpBinding (effectName, opName, scheme) =
  (opName, markEffectOpScheme effectName scheme, baseEnvRank, "base@" ++ opName)

nativeEffect :: String -> Ty
nativeEffect name = TyApp name []

nativeEffect1 :: String -> String -> Ty
nativeEffect1 name arg = TyApp name [TyCon arg]

native1 :: String -> [Ty] -> String -> Scheme
native1 arg effects ret = Forall [] [] (curriedFunction [TyCon arg] effects (TyCon ret))

native2 :: String -> String -> [Ty] -> String -> Scheme
native2 a b effects ret = Forall [] [] (curriedFunction [TyCon a, TyCon b] effects (TyCon ret))

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

canonicalTypeAnn :: TcContext -> TypeAnn -> TypeAnn
canonicalTypeAnn ctx (TypeAnn ty needs) = TypeAnn (canonicalTypeExpr ctx ty) (map (canonicalShapeNeed ctx) needs)

canonicalTypeExpr :: TcContext -> TypeExpr -> TypeExpr
canonicalTypeExpr ctx = typeExprFromAppliedTy . (`convertTypeExpr` ctx)

addShapeMembersToEnv :: String -> [ShapeMember] -> TcContext -> TcContext
addShapeMembersToEnv shapeName members ctx = foldl' step ctx members
 where
  step current member = case shapeMemberSignature member of
    Nothing -> current
    Just (name, ann) ->
      let scheme = shapeMemberScheme shapeName ann current
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
typeAnnWithOwnShapeNeed shapeName (TypeAnn t needs) ctx = TypeAnn t (ownShapeNeed shapeName ctx ++ needs)

ownShapeNeed :: String -> TcContext -> [ShapeNeed]
ownShapeNeed shapeName ctx = case findShapeInfo shapeName ctx of
  Just ShapeInfo{shapeParams} -> [ShapeNeed (map TypeName shapeParams) shapeName]
  Nothing -> []

shapeMemberSignature :: ShapeMember -> Maybe (String, TypeAnn)
shapeMemberSignature = \case
  ShapeSpec name t -> Just (name, t)
  ShapeDefault name (Just t) _ -> Just (name, t)
  ShapeDefault _ Nothing _ -> Nothing

findShapeInfo :: String -> TcContext -> Maybe ShapeInfo
findShapeInfo shapeName TcContext{tcShapes} = case Map.lookup shapeName tcShapes of
  Just info -> Just info
  Nothing -> case nub [info | (name, info) <- Map.toList tcShapes, shapeNamesMatch name shapeName] of
    [info] -> Just info
    _ -> Nothing

findShapeMember :: String -> [ShapeMember] -> Maybe TypeAnn
findShapeMember name = lookup name . mapMaybe shapeMemberSignature

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
  Just 0 -> inferVarM name ctx >>= \(Inferred actual _ _) -> unifyM actual expected >> pure (ctx, bound)
  Just _ -> failTc ("constructor pattern '" ++ name ++ "' needs arguments")
  Nothing -> bindPatternVariableM name expected ctx bound
bindPatternLinearM (PCon name args) expected ctx bound = do
  unless (isJust (knownConstructorArity name ctx)) (failTc ("unknown constructor pattern '" ++ name ++ "'"))
  Inferred conTy _ _ <- inferVarM name ctx
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
  | name `elem` bound = failTc ("pattern binds '" ++ name ++ "' more than once")
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

checkExhaustiveM :: [Ty] -> [MatchCase] -> TcContext -> Tc ()
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
  if isQualifiedName patternName
    then patternName == ctorName || patternName == importedConstructorName ctorName
    else patternName == lastQualifiedSegment ctorName

importedConstructorName :: String -> String
importedConstructorName name = takeWhile (/= '@') name ++ "@" ++ lastQualifiedSegment name

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
  EnvMissing -> inferFieldAccessNameM name ctx
  EnvAmbiguous msg -> failTc msg
  EnvFound scheme -> do
    (ty, needs) <- instantiateM scheme
    pure (Inferred ty [] needs)

inferFieldAccessNameM :: String -> TcContext -> Tc Inferred
inferFieldAccessNameM name ctx = case splitFieldAccessName name of
  Just (baseName, fieldName) -> inferRecordFieldAccessM name baseName fieldName ctx
  Nothing -> failTc ("unknown name '" ++ name ++ "'")

inferRecordFieldAccessM :: String -> String -> String -> TcContext -> Tc Inferred
inferRecordFieldAccessM originalName baseName fieldName ctx = do
  Inferred baseTy effects needs <- inferVarM baseName ctx
  st <- getTc
  case normalizeFun (applyState baseTy st) of
    TyRecord fields -> case Map.lookup fieldName fields of
      Just fieldTy -> pure (Inferred fieldTy effects needs)
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
      (instantiateM scheme >>= \(t, needs) -> inferCurriedApplyM (Inferred t [] needs) args ctx)
      (fallback . keepFirstError err)

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
inferReturnCaseM (Just (ReturnCase pat body)) bodyTy ctx = do
  ctx2 <- bindPatternM pat bodyTy ctx
  inferExprM body ctx2

data HandlerCoverage = WholeEffect String | OneOperation String String deriving (Eq, Show)

inferHandlerCasesM :: [HandlerCase] -> Ty -> [Ty] -> [Need] -> [Ty] -> TcContext -> Tc Inferred
inferHandlerCasesM cases answerTy effects bodyNeeds returnEffects ctx = do
  coverage <- traverse (handlerCoverageM effects ctx) cases
  let fullyHandled = completeHandledEffects coverage ctx
  (finalAnswerTy, accumulatedEffects, accumulatedNeeds) <- foldM (step fullyHandled) (answerTy, returnEffects, bodyNeeds) cases
  st <- getTc
  let remainingEffects = removeEffectNames fullyHandled (applyStateEffects effects st)
  pure (Inferred (applyState finalAnswerTy st) (unionEffects remainingEffects accumulatedEffects) (applyStateNeeds accumulatedNeeds st))
 where
  step fullyHandled (currentAnswerTy, accumulatedEffects, accumulatedNeeds) (HandlerCase name patterns handlerBody) = do
    handlerCtx <- handlerCaseContextM fullyHandled name patterns currentAnswerTy effects ctx
    Inferred handlerTy caseEffects caseNeeds <- inferExprM handlerBody handlerCtx
    mapTcError (unifyM currentAnswerTy handlerTy) (\msg -> "in handler for '" ++ name ++ "': " ++ msg)
    st <- getTc
    pure
      ( applyState currentAnswerTy st
      , unionEffects accumulatedEffects (applyStateEffects caseEffects st)
      , unionNeeds accumulatedNeeds (applyStateNeeds caseNeeds st)
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
        TyFun argTys opEffects retTy -> do
          _ <- findHandledOperationEffectM name ownerEffect opEffects effects
          ctx2 <- mapTcError (bindHandlerPatternsM patterns (NE.toList argTys) ctx) (\msg -> "in handler for '" ++ name ++ "': " ++ msg)
          st <- getTc
          let resumeEffects = removeEffectNames fullyHandled (applyStateEffects effects st)
              resumeTy = curriedFunction [applyState retTy st] resumeEffects (applyState answerTy st)
          pure (addEnv "resume" (Forall [] [] resumeTy) ctx2)
        _ -> failTc ("handler case '" ++ name ++ "' is not an effect operation")

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

inferMatchCasesM :: [MatchCase] -> [Ty] -> TcContext -> Tc Inferred
inferMatchCasesM cases scrutTys ctx = do
  (branchTy, _, effects, needs) <- foldM step (Nothing, scrutTys, [], []) cases
  st <- getTc
  case branchTy of
    Just t -> pure (Inferred (applyState t st) effects (applyStateNeeds needs st))
    Nothing -> do
      t <- freshM
      pure (Inferred (applyState t st) [] [])
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
  step currentCtx (Export declaration) = do
    checkedCtx <- step currentCtx declaration
    pure (exportDeclContext declaration checkedCtx)
  step currentCtx (ReExport name) = either failTc pure (reExportName name currentCtx)
  step currentCtx (ReExportType name) = either failTc pure (reExportTypeName name currentCtx)
  step currentCtx (Let name Nothing expr) = do
    (ctx2, effects) <- inferUnannotatedBindingM name expr currentCtx
    requirePureImmediateEffectsM ("let '" ++ name ++ "'") effects
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
  TypeAlias name _ -> "let-ilk '" ++ name ++ "'"
  DataDecl _ name _ -> "choose '" ++ name ++ "'"
  EffectDecl _ name _ -> "deed '" ++ name ++ "'"
  ForeignDecl{} -> "foreign declaration"
  ShapeDecl _ name _ _ -> "shape '" ++ name ++ "'"
  FillDecl tyArgs shapeName _ _ -> "fill '" ++ showTyArgs tyArgs ++ " " ++ shapeName ++ "'"
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

inferRunnableDeclM :: (TcContext, [Ty], [Need], Ty) -> Decl -> Tc (TcContext, [Ty], [Need], Ty)
inferRunnableDeclM state (Export declaration) = do
  (checkedCtx, effects, needs, value) <- inferRunnableDeclM state declaration
  pure (exportDeclContext declaration checkedCtx, effects, needs, value)
inferRunnableDeclM (ctx, effects, needs, fileValue) (ReExport name) = do
  exported <- either failTc pure (reExportName name ctx)
  pure (exported, effects, needs, fileValue)
inferRunnableDeclM (ctx, effects, needs, fileValue) (ReExportType name) = do
  exported <- either failTc pure (reExportTypeName name ctx)
  pure (exported, effects, needs, fileValue)
inferRunnableDeclM (ctx, effects, needs, fileValue) (Let name ann expr) = do
  (ctx2, value, letEffects, letNeeds) <- inferRunnableLetM name ann expr ctx
  let nextValue = if name == "_" then value else fileValue
  pure (ctx2, unionEffects effects letEffects, unionNeeds needs letNeeds, nextValue)
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
    (_, effects, _) <- checkAnnotatedExprM name ann expr ctx
    let scheme = typeAnnScheme ann ctx
    (value, _) <- instantiateM scheme
    pure (addEnv name scheme ctx, value, effects, [])

checkRunnableTypeM :: Ty -> [Ty] -> [Need] -> TcContext -> Tc Program
checkRunnableTypeM value effects needs ctx = do
  st <- getTc
  let actualValue = applyState value st
      expectedValue = maybe (TyCon "𝟙") TyCon (canonicalTypeName "𝟙" ctx)
  recoverTc (unifyM actualValue expectedValue) (\_ -> failTc ("runnable file must return 𝟙, found " ++ showTy actualValue))
  checkRunnableEffectsM effects
  requireNoNeedsM ctx needs

checkRunnableEffectsM :: [Ty] -> Tc Program
checkRunnableEffectsM effects = do
  st <- getTc
  let actualEffects = applyStateEffects effects st
  if allRunnableEffects actualEffects
    then pure (Program [])
    else failTc ("runnable file has unhandled effects {" ++ showEffects actualEffects ++ "}; only console, random, async, and file may remain")

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
  TypeAlias _ target -> checkTypeExprNamesM "type alias" target ctx >> pure d
  DataDecl _ dataName constructors -> mapM_ (checkCtor dataName) constructors >> pure d
  EffectDecl _ effectName ops -> checkEffectDeclM effectName ops ctx d
  ForeignDecl members -> checkForeignDeclM members ctx d
  ShapeDecl _ shapeName needs members -> checkShapeDeclM shapeName needs members ctx d
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

checkForeignDeclM :: [ForeignMember] -> TcContext -> Decl -> Tc Decl
checkForeignDeclM members ctx whole = mapM_ checkMember members >> pure whole
 where
  checkMember (ForeignMember name ann@(TypeAnn t _)) = do
    checkTypeAnnNeedsM ("foreign '" ++ name ++ "'") ann ctx
    unless (isFunctionTypeExpr t) $
      failTc ("foreign '" ++ name ++ "' must have a function type")

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
    ShapeSpec name t -> pure (ShapeSpec name t : acc, currentCtx)
    ShapeDefault name (Just t) expr -> do
      let typedMember = ShapeDefault name (Just t) expr
      void (checkRecursiveLetAnnotatedAliasM name (shapeName ++ "@" ++ name) (typeAnnWithOwnShapeNeed shapeName t currentCtx) expr currentCtx)
      pure (typedMember : acc, addShapeMembersToEnv shapeName [typedMember] currentCtx)
    ShapeDefault name Nothing expr -> do
      t <- mapTcError (inferShapeDefaultTypeM shapeParams name expr currentCtx) (\msg -> "in shape default '" ++ name ++ "': " ++ msg)
      let typedMember = ShapeDefault name (Just t) expr
      pure (typedMember : acc, addShapeMembersToEnv shapeName [typedMember] currentCtx)

inferShapeDefaultTypeM :: [String] -> String -> Expr -> TcContext -> Tc TypeAnn
inferShapeDefaultTypeM shapeParams name expr ctx = case expr of
  EMatch [] [MatchCase ps body] -> do
    let argTypeExprs = replicate (NE.length ps) (defaultShapeArgumentType shapeParams)
        argTys = map (`convertTypeExpr` ctx) argTypeExprs
    ctx2 <- bindPatternsM (NE.toList ps) argTys ctx
    Inferred ret effects needs <- inferNamedM name body ctx2
    st <- getTc
    pure (TypeAnn (typeExprFromTy (curriedFunction argTys effects ret) st) (shapeNeedsFromNeeds needs st))
  _ -> do
    Inferred t effects needs <- inferNamedM name expr ctx
    st <- getTc
    if null (applyStateEffects effects st)
      then pure (TypeAnn (typeExprFromTy t st) (shapeNeedsFromNeeds needs st))
      else failTc ("effectful shape default '" ++ name ++ "' must be a function")

defaultShapeArgumentType :: [String] -> TypeExpr
defaultShapeArgumentType (param : _) = TypeName param
defaultShapeArgumentType [] = TypeName "t0"

checkShapeDeclM :: String -> [ShapeNeed] -> [ShapeMember] -> TcContext -> Decl -> Tc Decl
checkShapeDeclM shapeName needs members ctx whole =
  checkShapeNeedsM ("shape '" ++ shapeName ++ "'") needs ctx >> checkShapeMemberNeedsM shapeName members ctx >> checkShapeDefaultsM shapeName members ctx whole

checkShapeNeedsM :: String -> [ShapeNeed] -> TcContext -> Tc ()
checkShapeNeedsM owner needs ctx = mapM_ checkNeed needs
 where
  checkNeed (ShapeNeed args neededName) = do
    mapM_ (\argument -> checkTypeExprNamesM owner argument ctx) args
    case findShapeInfo neededName ctx of
      Nothing -> failTc (owner ++ " has unknown graith shape '" ++ neededName ++ "'")
      Just ShapeInfo{shapeParams} -> do
        let expected = length shapeParams
            actual = length args
        unless (actual == expected) $
          failTc (owner ++ " has graith '" ++ neededName ++ "' with " ++ show actual ++ " arguments, but '" ++ neededName ++ "' has " ++ show expected ++ " parameters")

checkShapeMemberNeedsM :: String -> [ShapeMember] -> TcContext -> Tc ()
checkShapeMemberNeedsM shapeName members ctx = mapM_ checkMember members
 where
  checkMember member = case shapeMemberSignature member of
    Nothing -> pure ()
    Just (name, annotation) -> checkTypeAnnNeedsM ("shape member '" ++ shapeName ++ "@" ++ name ++ "'") annotation ctx

checkTypeAnnNeedsM :: String -> TypeAnn -> TcContext -> Tc ()
checkTypeAnnNeedsM owner (TypeAnn value needs) ctx = checkTypeExprNamesM owner value ctx >> checkShapeNeedsM owner needs ctx

checkTypeExprNamesM :: String -> TypeExpr -> TcContext -> Tc ()
checkTypeExprNamesM owner expr ctx = case expr of
  TypeName name -> checkTypeNameM owner name ctx
  TypeApply name arguments -> checkTypeNameM owner name ctx >> mapM_ (\argument -> checkTypeExprNamesM owner argument ctx) arguments
  TypeRecord fields -> mapM_ (\(_, fieldType) -> checkTypeExprNamesM owner fieldType ctx) fields
  TypeArrow arguments effects result -> do
    mapM_ (\argument -> checkTypeExprNamesM owner argument ctx) arguments
    mapM_ (\effect -> checkTypeExprNamesM owner effect ctx) effects
    checkTypeExprNamesM owner result ctx

checkTypeNameM :: String -> String -> TcContext -> Tc ()
checkTypeNameM owner name TcContext{tcTypeAmbiguities}
  | isQualifiedName name = pure ()
  | Just choices <- Map.lookup name tcTypeAmbiguities =
      failTc (owner ++ " uses ambiguous type '" ++ name ++ "'; qualify it as one of: " ++ intercalate ", " choices)
  | otherwise = pure ()

checkShapeDefaultsM :: String -> [ShapeMember] -> TcContext -> Decl -> Tc Decl
checkShapeDefaultsM _ members _ whole = mapM_ checkMember members >> pure whole
 where
  checkMember (ShapeDefault name Nothing _) = failTc ("shape default '" ++ name ++ "' has no inferred type")
  checkMember _ = pure ()

checkLetM :: String -> Maybe TypeAnn -> Expr -> TcContext -> Tc Decl
checkLetM name Nothing expr ctx = do
  Inferred _ effects needs <- inferNamedM name expr ctx
  requirePureImmediateEffectsM ("let '" ++ name ++ "'") effects
  _ <- normalizeNeedsM ctx needs
  pure (Let name Nothing expr)
checkLetM name (Just ann) expr ctx = do
  checkTypeAnnNeedsM ("let '" ++ name ++ "'") ann ctx
  checkAnnotatedExprM name ann expr ctx
  pure (Let name (Just ann) expr)

requirePureImmediateEffectsM :: String -> [Ty] -> Tc ()
requirePureImmediateEffectsM owner effects = do
  st <- getTc
  let actualEffects = applyStateEffects effects st
  unless (null actualEffects) $
    failTc (owner ++ " has immediate effects {" ++ showEffects actualEffects ++ "}; use a function type or run it from a runnable file")

checkFillM :: [TypeExpr] -> String -> [ShapeNeed] -> [Decl] -> TcContext -> Tc Decl
checkFillM tyArgs shapeName needs members ctx = do
  mapM_ (\tyArg -> checkTypeExprNamesM ("fill '" ++ shapeName ++ "'") tyArg ctx) tyArgs
  checkShapeNeedsM ("fill '" ++ shapeName ++ "'") needs ctx
  case findShapeInfo shapeName ctx of
    Nothing -> failTc ("unknown shape '" ++ shapeName ++ "' in fill")
    Just ShapeInfo{shapeParams} -> do
      checkFillArityM shapeName shapeParams tyArgs
      case fillSpecsForShape shapeName tyArgs ctx [] of
        Nothing -> failTc ("fill '" ++ shapeName ++ "' has an unknown required shape")
        Just specs -> checkFillMembersM shapeName tyArgs needs members specs ctx (FillDecl tyArgs shapeName needs members)

checkFillArityM :: String -> [String] -> [TypeExpr] -> Tc ()
checkFillArityM shapeName shapeParams tyArgs =
  unless (length tyArgs == length shapeParams) $
    failTc ("fill '" ++ shapeName ++ "' has " ++ show (length tyArgs) ++ " type arguments, but '" ++ shapeName ++ "' has " ++ show (length shapeParams) ++ " parameters")

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
  make name annotation =
    FillSpec name shapeName fillTypes (specializeShapeTypeAnn shapeParams fillTypes annotation)

findFillSpec :: String -> [FillSpec] -> Maybe FillSpec
findFillSpec name = find ((== name) . fillSpecName)

checkFillMembersM :: String -> [TypeExpr] -> [ShapeNeed] -> [Decl] -> [FillSpec] -> TcContext -> Decl -> Tc Decl
checkFillMembersM shapeName tyArgs needs members specs ctx whole = do
  requireFillMembersM shapeName tyArgs members specs ctx
  mapM_ checkMember members
  pure whole
 where
  checkMember (Let name _ expr) = case findFillSpec name specs of
    Nothing -> failTc ("fill defines unknown member '" ++ name ++ "'")
    Just FillSpec{fillSpecShape, fillSpecTypes, fillSpecType} ->
      mapTcError
        (void (checkRecursiveLetAnnotatedAliasM name (fillSpecShape ++ "@" ++ name) (typeAnnWithFillNeeds fillSpecTypes fillSpecShape needs fillSpecType) expr ctx))
        (\msg -> "in fill member '" ++ fillSpecShape ++ "@" ++ name ++ "': " ++ msg)
  checkMember _ = failTc "fill member must be a let"

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
hasDirectFill wantedShape wantedTypes ctx@TcContext{tcFills} = any matches tcFills
 where
  wanted = map (`convertTypeExpr` ctx) wantedTypes
  matches FillInfo{..} =
    shapeNamesMatch fillShape wantedShape
      && shapeNamesMatch fillOrigin wantedShape
      && isJust (zipWithExact typePatternBindings (map (`convertTypeExpr` ctx) fillTypes) wanted >>= mergeBindingMaps)

typeAnnWithFillNeeds :: [TypeExpr] -> String -> [ShapeNeed] -> TypeAnn -> TypeAnn
typeAnnWithFillNeeds tyArgs shapeName needs (TypeAnn t memberNeeds) =
  TypeAnn t (ShapeNeed tyArgs shapeName : needs ++ memberNeeds)

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
  (_, effects, _) <- checkAnnotatedExprM name ann expr ctx
  pure (addEnv name (typeAnnScheme ann ctx) ctx, effects)

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
  (ty, needs) <- skolemizeM (typeAnnScheme ann ctx)
  pure (ty, [], needs)

skolemizeM :: Scheme -> Tc (Ty, [Need])
skolemizeM scheme = do
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

checkAnnotatedExprM :: String -> TypeAnn -> Expr -> TcContext -> Tc (Ty, [Ty], [Need])
checkAnnotatedExprM name ann expr ctx = do
  (expectedValue, expectedEffects, expectedNeeds) <- instantiateAnnotationM ann ctx
  checkExprAgainstM name expectedValue expectedEffects expectedNeeds expr ctx

checkExprAgainstM :: String -> Ty -> [Ty] -> [Need] -> Expr -> TcContext -> Tc (Ty, [Ty], [Need])
checkExprAgainstM name expectedValue expectedEffects expectedNeeds (EMatch [] cases) ctx
  | TyFun args latentEffects ret <- normalizeFun expectedValue
  , anonymousCasesFitFunction (NE.length args) cases = do
      checkedNeeds <- checkMatchFunctionAgainstM name args latentEffects ret expectedNeeds cases ctx
      st <- getTc
      pure (applyState expectedValue st, applyStateEffects expectedEffects st, checkedNeeds)
checkExprAgainstM name expectedValue expectedEffects expectedNeeds expr ctx = do
  Inferred actual effects needs <- inferNamedM name expr ctx
  mapTcError (unifyM actual expectedValue) (\msg -> "in '" ++ name ++ "': " ++ msg ++ "; actual " ++ showTy actual ++ ", expected " ++ showTy expectedValue)
  (checkedValue, checkedEffects) <- checkEffectsAgainstM name expectedValue effects expectedEffects
  checkNeedsAgainstM name ctx needs expectedNeeds
  st <- getTc
  pure (checkedValue, checkedEffects, applyStateNeeds expectedNeeds st)

checkMatchFunctionAgainstM :: String -> NonEmpty Ty -> [Ty] -> Ty -> [Need] -> [MatchCase] -> TcContext -> Tc [Need]
checkMatchFunctionAgainstM name args latentEffects ret expectedNeeds cases ctx = do
  actualNeeds <- foldM step [] cases
  st <- getTc
  checkExhaustiveM (applyStateAll patternArgs st) cases ctx
  checkNeedsAgainstM name ctx actualNeeds expectedNeeds
  applyStateNeeds expectedNeeds <$> getTc
 where
  arity = functionCaseArity (NE.length args) cases
  (patternArgs, remainingArgs) = splitAt arity (NE.toList args)
  step needs (MatchCase ps body) = do
    ctx2 <- bindPatternsM (NE.toList ps) patternArgs ctx
    bodyNeeds <- case NE.nonEmpty remainingArgs of
      Nothing -> do
        Inferred bodyTy bodyEffects inferredNeeds <- inferNamedM name body ctx2
        mapTcError (unifyM bodyTy ret) (\msg -> "in '" ++ name ++ "': " ++ msg ++ "; actual " ++ showTy bodyTy ++ ", expected " ++ showTy ret)
        _ <- checkEffectsAgainstM name ret bodyEffects latentEffects
        pure inferredNeeds
      Just rest -> do
        (_, _, inferredNeeds) <- checkExprAgainstM name (TyFun rest latentEffects ret) [] expectedNeeds body ctx2
        pure inferredNeeds
    pure (unionNeeds needs bodyNeeds)

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

requireNoNeedsM :: TcContext -> [Need] -> Tc Program
requireNoNeedsM ctx needs = do
  remaining <- normalizeNeedsM ctx needs
  if null remaining
    then pure (Program [])
    else failTc ("runnable file has unresolved graiths " ++ showNeeds remaining)

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
fillNeedsForNeed ctx@TcContext{tcFills} (Need tys shapeName) =
  if all hasConcreteHead tys then listToMaybe (mapMaybe matches tcFills) else Nothing
 where
  matches FillInfo{..}
    | not (shapeNamesMatch fillShape shapeName) = Nothing
    | otherwise = do
        bindings <- zipWithExact typePatternBindings (map (`convertTypeExpr` ctx) fillTypes) tys >>= mergeBindingMaps
        pure (map (needWithBindings ctx bindings) fillNeeds)

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
shapeNamesMatch x y = x == y || lastQualifiedSegment x == lastQualifiedSegment y

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
firstMetaVarName taken = go
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

checkEditorWithImports :: String -> Map.Map String String -> String
checkEditorWithImports source imports = case parse source of
  Left msg -> "parse error: " ++ msg
  Right program ->
    let libraryResult = checkParsed program imports False
        runnableResult = checkParsed program imports True
     in if libraryResult == "type ok" || runnableResult == "type ok"
          then "type ok"
          else if looksRunnable program then runnableResult else libraryResult

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
  case inferProgramWithMode p imports runnable of
    TcErr msg -> "type error: " ++ msg
    TcOk _ _ -> "type ok"

looksRunnable :: Program -> Bool
looksRunnable (Program ds) = case reverse ds of
  Let "_" _ _ : _ -> True
  Export (Let "_" _ _) : _ -> True
  _ -> False

inferProgramWithMode :: Program -> Map.Map String String -> Bool -> TcResult Program
inferProgramWithMode p imports runnable = runTc (inferProgramWithModeM p imports runnable) initialTcState

inferProgramWithModeM :: Program -> Map.Map String String -> Bool -> Tc Program
inferProgramWithModeM (Program ds) imports runnable = do
  (typedDs, ctx) <- prepareDeclsM [] ds imports baseContext
  if runnable then checkRunnableDeclsM typedDs ctx else checkDeclsM typedDs ctx

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

collectImportContextsM :: ImportStack -> [Decl] -> Map.Map String String -> TcContext -> Tc TcContext
collectImportContextsM importStack ds imports ctx = foldM step ctx ds
 where
  step currentCtx (Import path) = importContext False currentCtx path
  step currentCtx (Export (Import path)) = importContext True currentCtx path
  step currentCtx _ = pure currentCtx

  importContext exported currentCtx path = do
    nextStack <- either failTc pure (enterImport importStack path)
    source <- maybe (failTc ("missing bring '" ++ path ++ "'")) pure (Map.lookup path imports)
    let importError message = failTc ("in bring '" ++ path ++ "': " ++ message)
    p <- either importError pure (parse source)
    importedCtx <- importedContextM nextStack path p imports
    let namespaced = namespaceImportContext (importNamespace path) importedCtx
    pure (mergeContext (if exported then markImportedContextExported namespaced else namespaced) currentCtx)

importedContextM :: ImportStack -> String -> Program -> Map.Map String String -> Tc TcContext
importedContextM importStack path p imports = Tc $ StateT $ \st -> case runTc imported initialTcState of
  TcErr msg -> Left ("in bring '" ++ path ++ "': " ++ msg)
  TcOk importedCtx _ -> Right (importedCtx, st)
 where
  imported = inferProgramContextFromM importStack p imports baseContext
