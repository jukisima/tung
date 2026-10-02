{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | static semantics: imports and visibility, Hindley-Milner inference,
-- effect rows, coverage, classes and instances, and elaboration to dictionary passing.
module Tung.Type
  ( Ty (..),
    Scheme (..),
    EnvLookup (..),
    EnvBinding (..),
    TcContext (..),
    TcState (..),
    TcResult (..),
    TypeFailure (..),
    ClassInfo (..),
    check,
    checkWithImports,
    checkProgramPrepared,
    checkEditorProgramPrepared,
    checkRunnableWithImports,
    elaborateProgramWithImports,
    elaborateInteractiveProgramWithImports,
    typeOfWithImports,
    typeOfBundle,
    elaboratePrepared,
    baseContext,
    inferProgramContext,
    lookupEnv,
    canonicalTypeName,
    showTy,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (filterM, foldM, replicateM, unless, void, when, zipWithM_)
import Control.Monad.Trans.State.Strict (StateT (..), get, mapStateT, put, runStateT, state)
import Data.Bifunctor (first)
import Data.Either (fromRight, isRight)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.IntMap.Strict qualified as IntMap
import Data.List (find, intercalate, isPrefixOf, nub, nubBy, partition, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Traversable (mapAccumM)
import Tung.Core (CoreProgram, ModuleInterface (..), makeCoreProgramWithImports)
import Tung.Coverage qualified as Coverage
import Tung.Identity (ClassRef (..), EffectRef (..), InstanceId (..), InstanceType (..), ModuleId (..), SymbolId (..), TermExport (..), TermKind (..), TypeRef (..), isPrimitiveInstanceId, renderInstanceId)
import Tung.Import (ImportStack, enterImport)
import Tung.Kind qualified as Kind
import Tung.Name (importAlias, importNamespace, isQualifiedName, lastQualifiedSegment, replaceNamespace, splitQualifiedName)
import Tung.Parse (ParsedBundle (..), ParsedSource (..), SourceUnit (..), prepareBundle, prepareSource)
import Tung.Primitive
import Tung.Syntax
import Tung.Token (SourceFailure (..), SourceSpan)
import Tung.Validate (validateProgram)

-- function effects are latent on 'TyArrow'; 'Inferred' carrieþ only effects caused
-- while evaluating the current expression, plus unresolved class requirements.
data Ty
  = TyMeta Int
  | TyVar String
  | TyCon String
  | TyApplication ApplicationHead [Ty]
  | TyEffect EffectRef [Ty]
  | TyRecord (Map.Map String Ty)
  | TyArrow Ty EffectRow Ty
  | TyForall [TypeParameter] Ty
  | TyRowVariable RowVariable
  | TyEffectSkolem Int
  deriving (Eq, Show)

-- source names are classified at construction; inference compareþ identities.
data ApplicationHead = VariableHead String | ConstructorHead TypeConstructor
  deriving (Eq, Show)

pattern TyApp :: String -> [Ty] -> Ty
pattern TyApp name args <- TyApplication (sourceHeadName -> Just name) args
  where
    TyApp name args = TyApplication (ConstructorHead (StructuralConstructor name)) args

pattern TyNamed :: TypeRef -> [Ty] -> Ty
pattern TyNamed ref args = TyApplication (ConstructorHead (NamedConstructor ref)) args

-- rigid variables carry identity and binding role; names are diagnostic only.
data SkolemId = UniversalSkolem Int | ExistentialSkolem Int deriving (Eq, Ord, Show)

pattern TySkolem :: SkolemId -> [Ty] -> Ty
pattern TySkolem ident args = TyApplication (ConstructorHead (SkolemConstructor ident)) args

skolemName :: SkolemId -> String
skolemName (UniversalSkolem ident) = "$type" ++ show ident
skolemName (ExistentialSkolem ident) = "$exist" ++ show ident

sourceHeadName :: ApplicationHead -> Maybe String
sourceHeadName (VariableHead name) = Just name
sourceHeadName (ConstructorHead (StructuralConstructor name)) = Just name
sourceHeadName _ = Nothing

-- source headers have several inputs, but each internal arrow hath one input.
-- only pure arrows are grouped for source display and pattern-row checking.
pattern TyFun :: NonEmpty Ty -> EffectRow -> Ty -> Ty
pattern TyFun args effects ret <- (functionView -> Just (args, effects, ret))
  where
    TyFun (arg :| args) effects ret = case args of
      [] -> TyArrow arg effects ret
      next : rest -> TyArrow arg [] (TyFun (next :| rest) effects ret)

functionView :: Ty -> Maybe (NonEmpty Ty, EffectRow, Ty)
functionView (TyArrow arg [] ret)
  | Just (args, effects, result) <- functionView ret = Just (arg NE.<| args, effects, result)
functionView (TyArrow arg effects ret) = Just (arg :| [], effects, ret)
functionView _ = Nothing

{-# COMPLETE TyMeta, TyVar, TyCon, TyApp, TyNamed, TySkolem, TyEffect, TyRecord, TyFun, TyForall, TyRowVariable, TyEffectSkolem #-}

{-# COMPLETE TyMeta, TyVar, TyCon, TyApplication, TyEffect, TyRecord, TyArrow, TyForall, TyRowVariable, TyEffectSkolem #-}

data Constraint = Constraint [Ty] ClassRef deriving (Eq, Show)

-- resolved member annotations preserve constraint identities; surface syntax stayeþ available for Core.
data ResolvedTypeAnn = ResolvedTypeAnn Ty [Constraint] deriving (Eq, Show)

data Inferred = Inferred
  { inferredTy :: Ty,
    inferredEffects :: EffectRow,
    inferredConstraints :: [Constraint],
    inferredExpr :: Expr
  }
  deriving (Eq, Show)

data Scheme
  = Forall [String] [Constraint] Ty
  | EffectOpForall EffectRef [String] [Constraint] Ty
  | Constrained [RowEquality] Scheme
  | Specified [TypeParameter] Scheme
  deriving (Eq, Show)

data RowEquality = RowEquality EffectRow EffectRow deriving (Eq, Show)

data EnvLookup = EnvFound Scheme | EnvMissing | EnvAmbiguous String deriving (Eq, Show)

data EnvBinding = EnvBinding
  { envName :: String,
    envLocalName :: String,
    envScheme :: Scheme,
    envRank :: Int,
    envOrigin :: String,
    envTarget :: Maybe TermExport
  }
  deriving (Eq, Show)

data EnvChoices = EnvChoices [EnvBinding] | ChoicesMissing | ChoicesAmbiguous String deriving (Eq, Show)

data TcResult a = TcOk a TcState | TcErr String deriving (Eq, Show)

data TypeFailure = TypeFailure
  { typeFailureMessage :: String,
    typeFailureSpan :: Maybe SourceSpan,
    typeFailurePath :: Maybe FilePath
  }
  deriving (Eq, Show)

-- these modes share inference but impose different top-level computation bounds.
data ProgramMode = ModuleMode | Interactive | Runnable deriving (Eq)

data TypeConstructor = StructuralConstructor String | NamedConstructor TypeRef | SkolemConstructor SkolemId deriving (Eq, Show)

data TyHead = TyHead ApplicationHead [Ty] deriving (Eq, Show)

data AliasHeader = AliasHeader
  { aliasHeaderName :: String,
    aliasHeaderParams :: [String],
    aliasHeaderTarget :: TypeExpr
  }

data DataHeader = DataHeader
  { dataHeaderName :: String,
    dataHeaderParams :: [String],
    dataHeaderConstructors :: [Ctor]
  }

data DisplayNames = DisplayNames
  { displayTypes :: Map.Map SymbolId String,
    displayEffects :: Map.Map SymbolId String,
    displayClasses :: Map.Map SymbolId String
  }

emptyDisplayNames :: DisplayNames
emptyDisplayNames = DisplayNames Map.empty Map.empty Map.empty

-- rigid effect skolems are structural inference artifacts; every source-level
-- effect head is an 'EffectRef' and therefore compareþ by 'SymbolId'.
data EffectHead = NominalEffect EffectRef | RigidEffect Int | UnresolvedEffect String deriving (Eq, Ord, Show)

newtype RowVariable = RowVariable String deriving (Eq, Ord, Show)

data Effect = EffectLabel EffectHead [Ty] | EffectVariable RowVariable deriving (Eq, Show)

type EffectRow = [Effect]

effectAsTy :: Effect -> Ty
effectAsTy (EffectLabel (NominalEffect ref) args) = TyEffect ref args
effectAsTy (EffectLabel (RigidEffect ident) _) = TyEffectSkolem ident
effectAsTy (EffectLabel (UnresolvedEffect name) args) = TyApp name args
effectAsTy (EffectVariable variable) = TyRowVariable variable

effectFromTy :: Ty -> Effect
effectFromTy (TyEffect ref args) = EffectLabel (NominalEffect ref) args
effectFromTy (TyEffectSkolem ident) = EffectLabel (RigidEffect ident) []
effectFromTy (TyRowVariable variable) = EffectVariable variable
effectFromTy (TyVar name) = EffectVariable (RowVariable name)
effectFromTy (TyApp name args) = EffectLabel (UnresolvedEffect name) args
effectFromTy ty = EffectLabel (UnresolvedEffect (showTy ty)) []

mapEffectTypes :: (Ty -> Ty) -> Effect -> Effect
mapEffectTypes f = effectFromTy . f . effectAsTy

-- 'TcContext' holds static names, types, and resolved runtime identities;
-- 'TcState' holds substitutions, import results, and evidence holes for one check.
data TcState = TcState
  { tcNextMeta :: !Int,
    tcMetaSubst :: !(IntMap.IntMap Ty),
    tcGadtRefinements :: !(Map.Map SkolemId Ty),
    tcTypeHeads :: !(Map.Map String TyHead),
    tcEffectRows :: !(Map.Map RowVariable EffectRow),
    tcRowEqualities :: [RowEquality],
    tcSolvingRows :: Bool,
    tcImportCache :: !(Map.Map String TcContext),
    tcImportCoreCache :: !(Map.Map String CoreProgram),
    tcEvidenceConstraints :: !(IntMap.IntMap Constraint),
    tcRecursiveUses :: !(Set.Set String),
    tcCurrentSpan :: !(Maybe SourceSpan)
  }
  deriving (Eq, Show)

-- primitive entries exist only when a module explicitly showeþ a built-in type;
-- unlike aliases, they remain opaque during unification.
-- effect entries retain all operations even when an import hideþ some names.
data TypeInfo
  = Primitive String
  | Alias String [String] Ty
  | DataInfo TypeRef [String] [ConstructorInfo]
  | EffectInfo EffectRef [String] [SymbolId]
  | ShownType TypeInfo
  deriving (Eq, Show)

data ConstructorInfo = ConstructorInfo
  { constructorInfoTarget :: SymbolId,
    constructorInfoFields :: [Ty],
    constructorInfoResult :: Ty
  }
  deriving (Eq, Show)

data ClassMethod = ClassMethod
  { classMethodName :: String,
    classMethodType :: ResolvedTypeAnn
  }
  deriving (Eq, Show)

data ClassInfo = ClassInfo
  { classParams :: [String],
    classConstraints :: [Constraint],
    classMethods :: [ClassMethod],
    classExported :: Bool,
    classTarget :: SymbolId
  }
  deriving (Eq, Show)

data InstanceInfo = InstanceInfo
  { instanceClass :: ClassRef,
    instanceTypes :: [Ty],
    instanceConstraints :: [Constraint],
    instanceKey :: InstanceId,
    instanceDepth :: Int,
    instanceParents :: [Constraint],
    instanceMembers :: [String]
  }
  deriving (Eq, Show)

data TcContext = TcContext
  { tcModule :: ModuleId,
    tcEnv :: [EnvBinding],
    tcTypes :: Map.Map String TypeInfo,
    tcTypeRegistry :: Map.Map SymbolId TypeInfo,
    tcTypeAmbiguities :: Map.Map String [String],
    tcClasses :: Map.Map String ClassInfo,
    tcKinds :: Kind.KindContext,
    tcClassRegistry :: Map.Map SymbolId ClassInfo,
    tcInstances :: Map.Map String [InstanceInfo]
  }
  deriving (Eq, Show)

newtype Tc a = Tc {unTc :: StateT TcState (Either TypeFailure) a}
  deriving newtype (Functor, Applicative, Monad)

initialTcState :: TcState
initialTcState =
  TcState
    { tcNextMeta = 0,
      tcMetaSubst = IntMap.empty,
      tcGadtRefinements = Map.empty,
      tcTypeHeads = Map.empty,
      tcEffectRows = Map.empty,
      tcRowEqualities = [],
      tcSolvingRows = False,
      tcImportCache = Map.empty,
      tcImportCoreCache = Map.empty,
      tcEvidenceConstraints = IntMap.empty,
      tcRecursiveUses = Set.empty,
      tcCurrentSpan = Nothing
    }

runTc :: Tc a -> TcState -> TcResult a
runTc computation st = case runStateT (unTc computation) st of
  Left failure -> TcErr (typeFailureMessage failure)
  Right (value, st2) -> TcOk value st2

failTc :: String -> Tc a
failTc message = Tc $ StateT $ \state -> Left (TypeFailure message (tcCurrentSpan state) Nothing)

failImportTc :: FilePath -> SourceFailure -> Tc a
failImportTc path SourceFailure {failureMessage, failureSpan} = Tc $ StateT $ \_ -> Left (TypeFailure ("in use '" ++ path ++ "': " ++ failureMessage) failureSpan (Just path))

getTc :: Tc TcState
getTc = Tc get

putTc :: TcState -> Tc ()
putTc = Tc . put

mapTcError :: Tc a -> (String -> String) -> Tc a
mapTcError computation f = Tc $ mapStateT (first (\failure -> failure {typeFailureMessage = f (typeFailureMessage failure)})) (unTc computation)

recoverTc :: Tc a -> (String -> Tc a) -> Tc a
recoverTc computation f = Tc $ StateT $ \st -> case runStateT (unTc computation) st of
  Left failure -> runStateT (unTc (f (typeFailureMessage failure))) st
  ok -> ok

succeedsTc :: Tc a -> Tc Bool
succeedsTc computation = Tc $ StateT $ \st ->
  Right (isRight (runStateT (unTc computation) st), st)

withTcSpan :: SourceSpan -> Tc a -> Tc a
withTcSpan span computation = Tc $ StateT $ \state ->
  case runStateT (unTc computation) state {tcCurrentSpan = Just span} of
    Left failure -> Left failure
    Right (value, state2) -> Right (value, state2 {tcCurrentSpan = tcCurrentSpan state})

curriedFunction :: [Ty] -> EffectRow -> Ty -> Ty
curriedFunction args effects ret =
  maybe ret (\domain -> TyFun domain effects ret) (NE.nonEmpty args)

mapTyChildren :: (Ty -> Ty) -> (Ty -> Ty) -> Ty -> Ty
mapTyChildren mapTy mapEffect = \case
  TyApplication head args -> TyApplication head (map mapTy args)
  TyEffect effect args -> TyEffect effect (map mapTy args)
  TyRecord fields -> TyRecord (Map.map mapTy fields)
  TyArrow arg effects ret -> TyArrow (mapTy arg) (map (mapEffectTypes mapEffect) effects) (mapTy ret)
  TyForall names body -> TyForall names (mapTy body)
  ty -> ty

-- visit a type before its children. þe second visitor applieþ only to direct
-- effect-row entries, so row variables can retain þeir special semantics.
foldTyWith :: (Ty -> a -> a) -> (Ty -> a -> a) -> Ty -> a -> a
foldTyWith visitType visitEffect = foldScopedTyWith (const visitType) (const visitEffect)

foldScopedTyWith :: ([String] -> Ty -> a -> a) -> ([String] -> Ty -> a -> a) -> Ty -> a -> a
foldScopedTyWith visitType visitEffect = go []
  where
    go bound ty acc = descend bound ty (visitType bound ty acc)
    goEffect bound ty acc = descend bound ty (visitEffect bound ty acc)
    descend bound ty acc = case ty of
      TyApplication _ args -> foldl' (flip (go bound)) acc args
      TyEffect _ args -> foldl' (flip (go bound)) acc args
      TyRecord fields -> foldl' (flip (go bound)) acc (Map.elems fields)
      TyArrow arg effects ret ->
        go bound ret (foldl' (\current effect -> goEffect bound (effectAsTy effect) current) (go bound arg acc) effects)
      TyForall (parameterNames -> names) body -> go (names ++ bound) body acc
      _ -> acc

foldFreeNamesWith :: (Ty -> [String] -> [String]) -> (Ty -> [String] -> [String]) -> Ty -> [String] -> [String]
foldFreeNamesWith visitType visitEffect = foldScopedTyWith (free visitType) (free visitEffect)
  where
    free visit bound ty acc = foldr addUnique acc (filter (`notElem` bound) (visit ty []))

foldConstraintTypes :: (Ty -> a -> a) -> [Constraint] -> a -> a
foldConstraintTypes visit constraints acc = foldl' visitConstraint acc constraints
  where
    visitConstraint current (Constraint args _) = foldl' (flip visit) current args

-- import projection changeþ only diagnostic spellings; nominal equality and
-- all runtime identities remain untouched.
relabelTypeRefs :: DisplayNames -> (String -> String) -> Ty -> Ty
relabelTypeRefs names fallback = go
  where
    go (TyNamed ref args) = TyNamed (relabelTypeRef names fallback ref) (map go args)
    go (TyEffect effect args) = TyEffect (relabelEffectRef names fallback effect) (map go args)
    go ty = mapTyChildren go go ty

relabelConstraintRefs :: DisplayNames -> (String -> String) -> Constraint -> Constraint
relabelConstraintRefs names fallback (Constraint args class_) =
  Constraint (map (relabelTypeRefs names fallback) args) (relabelClassRef names fallback class_)

relabelTypeRef :: DisplayNames -> (String -> String) -> TypeRef -> TypeRef
relabelTypeRef DisplayNames {displayTypes} fallback ref@TypeRef {typeIdentity, typeDisplayName} =
  ref {typeDisplayName = Map.findWithDefault (fallback typeDisplayName) typeIdentity displayTypes}

relabelEffectRef :: DisplayNames -> (String -> String) -> EffectRef -> EffectRef
relabelEffectRef DisplayNames {displayEffects} fallback ref@EffectRef {effectIdentity, effectDisplayName} =
  ref {effectDisplayName = Map.findWithDefault (fallback effectDisplayName) effectIdentity displayEffects}

relabelClassRef :: DisplayNames -> (String -> String) -> ClassRef -> ClassRef
relabelClassRef DisplayNames {displayClasses} fallback ref@ClassRef {classIdentity, classDisplayName} =
  ref {classDisplayName = Map.findWithDefault (fallback classDisplayName) classIdentity displayClasses}

relabelSchemeTypeRefs :: DisplayNames -> (String -> String) -> Scheme -> Scheme
relabelSchemeTypeRefs names fallback = \case
  Specified vars scheme -> Specified vars (relabelSchemeTypeRefs names fallback scheme)
  Constrained rows scheme -> Constrained (map (mapRowEquality relabelTy) rows) (relabelSchemeTypeRefs names fallback scheme)
  Forall vars constraints ty -> Forall vars (map relabelConstraint constraints) (relabelTy ty)
  EffectOpForall owner vars constraints ty ->
    EffectOpForall (relabelEffectRef names fallback owner) vars (map relabelConstraint constraints) (relabelTy ty)
  where
    relabelTy = relabelTypeRefs names fallback
    relabelConstraint = relabelConstraintRefs names fallback

skipEffectVar :: (Ty -> Ty) -> Ty -> Ty
skipEffectVar f ty = if isEffectVarTy ty then ty else f ty

applyStateEffects :: EffectRow -> TcState -> EffectRow
applyStateEffects effects st = foldr addEffect [] (concatMap (`applyStateEffect` st) effects)

applyStateEffect :: Effect -> TcState -> EffectRow
applyStateEffect (EffectVariable variable) st@TcState {tcEffectRows} = maybe [EffectVariable variable] (`applyStateEffects` st) (Map.lookup variable tcEffectRows)
applyStateEffect effect st = [mapEffectTypes (`applyState` st) effect]

applyStateConstraint :: Constraint -> TcState -> Constraint
applyStateConstraint (Constraint args name) st = Constraint (applyStateAll args st) name

applyStateConstraints :: [Constraint] -> TcState -> [Constraint]
applyStateConstraints constraints st = map (`applyStateConstraint` st) constraints

unionEffects :: EffectRow -> EffectRow -> EffectRow
unionEffects = foldl' (flip addEffect)

removeEffect :: EffectRef -> EffectRow -> EffectRow
removeEffect effect = filter (not . effectHasRef effect)

addEffect :: Effect -> EffectRow -> EffectRow
addEffect = addUnique

unionConstraints :: [Constraint] -> [Constraint] -> [Constraint]
unionConstraints = foldl' (flip addConstraint)

addConstraint :: Constraint -> [Constraint] -> [Constraint]
addConstraint = addUnique

-- union equations remain constraints until substitution determineþ their rows.
-- choosing one variable in a union would lose valid solutions and polymorphism.
unifyEffectsM :: EffectRow -> EffectRow -> Tc ()
unifyEffectsM xs ys = do
  st <- getTc
  putTc st {tcRowEqualities = addUnique (RowEquality xs ys) (tcRowEqualities st)}
  unless (tcSolvingRows st) do
    modifyTc (\state -> state {tcSolvingRows = True})
    solveRowEqualitiesM
    modifyTc (\state -> state {tcSolvingRows = False})

modifyTc :: (TcState -> TcState) -> Tc ()
modifyTc f = getTc >>= putTc . f

applyRowEquality :: TcState -> RowEquality -> RowEquality
applyRowEquality st (RowEquality xs ys) = RowEquality (applyStateEffects xs st) (applyStateEffects ys st)

solveRowEqualitiesM :: Tc ()
solveRowEqualitiesM = do
  before <- getTc
  modifyTc (\st -> st {tcRowEqualities = []})
  mapM_ simplify (tcRowEqualities before)
  st <- getTc
  let equations = map (applyRowEquality st) (tcRowEqualities st)
      bounds = rowUpperBounds equations
  mapM_ (checkBounds bounds) equations
  mapM_ (\(variable, allowed) -> when (Set.null allowed) (bindEffectVarM variable [])) (Map.toList bounds)
  after <- getTc
  let sameEquations = all (`elem` tcRowEqualities after) (tcRowEqualities before) && all (`elem` tcRowEqualities before) (tcRowEqualities after)
  unless (tcEffectRows before == tcEffectRows after && sameEquations) solveRowEqualitiesM
  where
    simplify equation = do
      st <- getTc
      let normalized@(RowEquality xs ys) = applyRowEquality st equation
      unifyCommonEffectsM (concreteEffects xs) (concreteEffects ys)
      mapM_ (\row -> mapM_ (\label -> unifyCommonEffectsM [label] row) (concreteEffects row)) [xs, ys]
      case (xs, ys) of
        _ | all (`elem` ys) xs && all (`elem` xs) ys -> pure ()
        ([EffectVariable variable], row) | not (any (effectVarOccurs variable) row) -> bindEffectVarM variable row
        (row, [EffectVariable variable]) | not (any (effectVarOccurs variable) row) -> bindEffectVarM variable row
        _ -> modifyTc (\state -> state {tcRowEqualities = addUnique normalized (tcRowEqualities state)})
    checkBounds bounds (RowEquality xs ys) = do
      checkSide bounds xs ys
      checkSide bounds ys xs
    checkSide bounds xs ys = case rowUpperBound bounds ys of
      Nothing -> pure ()
      Just allowed ->
        unless (all (\effect -> maybe True ((`Set.member` allowed) . fst) (effectHeadAndArgs effect)) (concreteEffects xs)) $
          failTc ("cannot unify effects {" ++ showEffects xs ++ "} with {" ++ showEffects ys ++ "}")

-- a missing bound meaneþ any label. equality propagateþ finite upper bounds
-- in both directions; their greatest solution detecteþ incompatible unions.
rowUpperBounds :: [RowEquality] -> Map.Map RowVariable (Set.Set EffectHead)
rowUpperBounds equations = fixed Map.empty
  where
    fixed bounds = let next = foldl' refine bounds equations in if next == bounds then bounds else fixed next
    refine bounds (RowEquality xs ys) = limit (limit bounds xs ys) ys xs
    limit bounds xs ys = case rowUpperBound bounds ys of
      Nothing -> bounds
      Just allowed -> foldl' (\acc variable -> Map.insertWith Set.intersection variable allowed acc) bounds (effectVars xs)

rowUpperBound :: Map.Map RowVariable (Set.Set EffectHead) -> EffectRow -> Maybe (Set.Set EffectHead)
rowUpperBound bounds =
  fmap Set.unions . traverse \case
    EffectVariable variable -> Map.lookup variable bounds
    EffectLabel head _ -> Just (Set.singleton head)

concreteEffects :: EffectRow -> EffectRow
concreteEffects = filter (\case EffectVariable {} -> False; _ -> True)

effectVars :: EffectRow -> [RowVariable]
effectVars effects = nub [variable | EffectVariable variable <- effects]

bindEffectVarM :: RowVariable -> EffectRow -> Tc ()
bindEffectVarM name effects = do
  st@TcState {tcEffectRows} <- getTc
  let normalized = filter (/= EffectVariable name) (applyStateEffects effects st)
  if any (effectVarOccurs name) normalized
    then failTc "recursive effect"
    else case Map.lookup name tcEffectRows of
      Just existing -> unifyEffectsM existing normalized
      Nothing -> putTc st {tcEffectRows = Map.insert name normalized tcEffectRows}

effectVarOccurs :: RowVariable -> Effect -> Bool
effectVarOccurs name effect = foldTyWith found found (effectAsTy effect) False
  where
    found (TyRowVariable actual) acc = actual == name || acc
    found _ acc = acc

isEffectVarTy :: Ty -> Bool
isEffectVarTy TyRowVariable {} = True
isEffectVarTy _ = False

unifyCommonEffectsM :: EffectRow -> EffectRow -> Tc ()
unifyCommonEffectsM xs ys =
  mapM_ (\x -> maybe (pure ()) (unifyEffectM x) (findEffectByHead x ys)) xs

unifyEffectM :: Effect -> Effect -> Tc ()
unifyEffectM x y = case (effectHeadAndArgs x, effectHeadAndArgs y) of
  (Just (xn, xs), Just (yn, ys)) | xn == yn -> unifyListsM xs ys
  _ -> failTc ("cannot unify effect " ++ showEffect x ++ " with " ++ showEffect y)

findEffectByHead :: Effect -> EffectRow -> Maybe Effect
findEffectByHead x = find (sameEffectHead x)

sameEffectHead :: Effect -> Effect -> Bool
sameEffectHead x y = case (effectHeadAndArgs x, effectHeadAndArgs y) of
  (Just (xn, _), Just (yn, _)) -> xn == yn
  _ -> False

effectHasRef :: EffectRef -> Effect -> Bool
effectHasRef expected effect = case effectHeadAndArgs effect of
  Just (NominalEffect actual, _) -> expected == actual
  Nothing -> False
  Just _ -> False

effectHeadAndArgs :: Effect -> Maybe (EffectHead, [Ty])
effectHeadAndArgs (EffectLabel head args) = Just (head, args)
effectHeadAndArgs EffectVariable {} = Nothing

showEffects :: EffectRow -> String
showEffects = intercalate ", " . map showEffect

showEffect :: Effect -> String
showEffect (EffectVariable (RowVariable name)) = name
showEffect (EffectLabel head args) =
  (if null args then "" else showTyList args ++ " ") ++ case head of
    NominalEffect ref -> effectDisplayName ref
    RigidEffect ident -> "$effect" ++ show ident
    UnresolvedEffect name -> name

-- ordinary unification also handleþ higher-kinded type heads, closed records,
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
    (TyEffect x xs, TyEffect y ys) | x == y -> unifyListsM xs ys
    (TyRecord xs, TyRecord ys) -> unifyRecordFieldsM xs ys
    (TyArrow x xEffs xRet, TyArrow y yEffs yRet) -> unifyM x y >> unifyEffectsM xEffs yEffs >> unifyM xRet yRet
    (TyForall xs x, TyForall ys y) | map snd xs == map snd ys ->
      withPolymorphicScopeM xs x [TyForall ys y] $ \replacements rigid ->
        unifyM rigid (replaceVars y (Map.fromList [(right, replacements Map.! left) | (left, right) <- zip (parameterNames xs) (parameterNames ys)]))
    (applicationView -> Just (x, xs), applicationView -> Just (y, ys)) -> unifyApplicationsM x xs y ys
    (TyApplication head args, TyArrow arg effects ret) -> unifyArrowApplicationM head args arg effects ret
    (TyArrow arg effects ret, TyApplication head args) -> unifyArrowApplicationM head args arg effects ret
    _ -> failTc ("cannot unify " ++ showTy aa ++ " with " ++ showTy bb)

normalizeFun :: Ty -> Ty
normalizeFun (TyApplication head args) = applyApplicationHead head (map normalizeFun args)
normalizeFun ty = mapTyChildren normalizeFun normalizeFun ty

unifyListsM :: [Ty] -> [Ty] -> Tc ()
unifyListsM xs ys
  | length xs == length ys = zipWithM_ unifyM xs ys
  | otherwise = failTc ("arity mismatch: [" ++ showTyList xs ++ "] vs [" ++ showTyList ys ++ "]")

-- pure arrows participate in application unification as the binary '→'
-- constructor. prefix capture is shared by every partially applied constructor.
applicationView :: Ty -> Maybe (ApplicationHead, [Ty])
applicationView (TyApplication head args) = Just (head, args)
applicationView (TyArrow arg [] ret) = Just (ConstructorHead (StructuralConstructor "→"), [arg, ret])
applicationView _ = Nothing

unifyArrowApplicationM :: ApplicationHead -> [Ty] -> Ty -> EffectRow -> Ty -> Tc ()
unifyArrowApplicationM head args arg effects ret = do
  unifyEffectsM effects []
  unifyApplicationsM head args (ConstructorHead (StructuralConstructor "→")) [arg, ret]

unifyApplicationsM :: ApplicationHead -> [Ty] -> ApplicationHead -> [Ty] -> Tc ()
unifyApplicationsM x xs y ys
  | x == y = unifyListsM xs ys
  | VariableHead name <- x, length xs <= length ys = bindHead name xs y ys
  | VariableHead name <- y, length ys <= length xs = bindHead name ys x xs
  | otherwise = failTc ("cannot unify " ++ showTy (applyApplicationHead x xs) ++ " with " ++ showTy (applyApplicationHead y ys))
  where
    bindHead name supplied head actual = do
      let (captured, remaining) = splitAt (length actual - length supplied) actual
      bindTypeHeadM name (TyHead head captured)
      unifyListsM supplied remaining

bindTypeHeadM :: String -> TyHead -> Tc ()
bindTypeHeadM name value = do
  st@TcState {tcTypeHeads} <- getTc
  when (name `elem` typeVarsInTy (applyTypeHead value []) []) (failTc "recursive type constructor")
  putTc st {tcTypeHeads = Map.insert name value tcTypeHeads}

applyTypeHead :: TyHead -> [Ty] -> Ty
applyTypeHead (TyHead head prefixArgs) args = applyApplicationHead head (prefixArgs ++ args)

applyApplicationHead :: ApplicationHead -> [Ty] -> Ty
applyApplicationHead (VariableHead name) [] = TyVar name
applyApplicationHead (VariableHead name) args = TyApplication (VariableHead name) args
applyApplicationHead (ConstructorHead constructor) args = applyTypeConstructor constructor args

applyTypeConstructor :: TypeConstructor -> [Ty] -> Ty
applyTypeConstructor (StructuralConstructor name) = tyAppOrArrow name
applyTypeConstructor (NamedConstructor ref) = TyNamed ref
applyTypeConstructor (SkolemConstructor ident) = TySkolem ident

tyAppOrArrow :: String -> [Ty] -> Ty
tyAppOrArrow name [] = TyCon name
tyAppOrArrow "→" [arg, ret] = TyFun (arg :| []) [] ret
tyAppOrArrow name args = TyApp name args

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
  st@TcState {tcMetaSubst} <- getTc
  when (case ty of TyForall {} -> True; _ -> False) (failTc "cannot infer a polymorphic type; an explicit annotation is required")
  if occursMeta ident ty st
    then failTc "recursive type"
    else putTc st {tcMetaSubst = IntMap.insert ident ty tcMetaSubst}

occursMeta :: Int -> Ty -> TcState -> Bool
occursMeta ident ty st = ident `elem` metaIdsInTy (applyState ty st) []

applyState :: Ty -> TcState -> Ty
applyState ty st@TcState {tcMetaSubst, tcTypeHeads} = go ty
  where
    go = \case
      TyMeta ident -> maybe (TyMeta ident) go (IntMap.lookup ident tcMetaSubst)
      TyVar name -> maybe (TyVar name) (go . (`applyTypeHead` [])) (Map.lookup name tcTypeHeads)
      TyRowVariable variable -> TyRowVariable variable
      TyEffectSkolem ident -> TyEffectSkolem ident
      ty@TyCon {} -> ty
      TyApplication head args -> case head of
        VariableHead name -> maybe (applyApplicationHead head (map go args)) (\bound -> go (applyTypeHead bound (map go args))) (Map.lookup name tcTypeHeads)
        ConstructorHead (SkolemConstructor ident)
          | Just bound <- Map.lookup ident (tcGadtRefinements st) -> go (applyReplacementTy bound (map go args))
        ConstructorHead _ -> applyApplicationHead head (map go args)
      TyEffect effect args -> TyEffect effect (map go args)
      TyRecord fields -> TyRecord (Map.map go fields)
      TyArrow arg effects ret -> TyArrow (go arg) (applyStateEffects effects st) (go ret)
      TyForall parameters body ->
        let names = parameterNames parameters
            activeHeads = foldr Map.delete tcTypeHeads names
            activeRows = foldr (Map.delete . RowVariable) (tcEffectRows st) names
            scoped = st {tcTypeHeads = activeHeads, tcEffectRows = activeRows}
            replacements = IntMap.elems tcMetaSubst ++ map (`applyTypeHead` []) (Map.elems activeHeads) ++ map (\row -> rowEqualityType (RowEquality row [])) (Map.elems activeRows)
            (renamed, body2) = renameBoundVariables (foldr typeVarsInTy [] replacements) parameters body
         in TyForall renamed (applyState body2 scoped {tcTypeHeads = foldr Map.delete activeHeads (parameterNames renamed), tcEffectRows = foldr (Map.delete . RowVariable) activeRows (parameterNames renamed)})

applyStateAll :: [Ty] -> TcState -> [Ty]
applyStateAll tys st = map (`applyState` st) tys

freshM :: Tc Ty
freshM = TyMeta <$> freshIdM

freshIdM :: Tc Int
freshIdM = Tc $ state (\st@TcState {tcNextMeta} -> (tcNextMeta, st {tcNextMeta = tcNextMeta + 1}))

schemeEffectOwner :: Scheme -> Maybe EffectRef
schemeEffectOwner = \case
  Specified _ scheme -> schemeEffectOwner scheme
  Constrained _ scheme -> schemeEffectOwner scheme
  EffectOpForall owner _ _ _ -> Just owner
  Forall {} -> Nothing

schemeParts :: Scheme -> ([String], [Constraint], Ty)
schemeParts = \case
  Specified _ scheme -> schemeParts scheme
  Constrained _ scheme -> schemeParts scheme
  Forall vars constraints ty -> (vars, constraints, ty)
  EffectOpForall _ vars constraints ty -> (vars, constraints, ty)

markEffectOpScheme :: EffectRef -> Scheme -> Scheme
markEffectOpScheme owner = \case
  Specified vars scheme -> Specified vars (markEffectOpScheme owner scheme)
  Constrained rows scheme -> Constrained rows (markEffectOpScheme owner scheme)
  Forall vars constraints ty -> EffectOpForall owner vars constraints ty
  EffectOpForall _ vars constraints ty -> EffectOpForall owner vars constraints ty

instantiateM :: Scheme -> Tc (Ty, [Constraint])
instantiateM = instantiateWithTypesM []

type TypeArgument = (Ty, KindExpr)

instantiateWithTypesM :: [TypeArgument] -> Scheme -> Tc (Ty, [Constraint])
instantiateWithTypesM explicit scheme = do
  (_, ty, constraints) <- instantiateWithBindingsM explicit scheme
  pure (ty, constraints)

instantiateWithBindingsM :: [TypeArgument] -> Scheme -> Tc (Map.Map String Ty, Ty, [Constraint])
instantiateWithBindingsM explicit scheme = do
  let (vars, constraints, ty) = schemeParts scheme
      types = ty : map rowEqualityType (schemeRows scheme)
      headVars = typeHeadVarsInConstraints constraints (foldr typeHeadVarsInTy [] types)
      rowVars = effectRowVarsInConstraints constraints (foldr effectRowVarsInTy [] types)
      specified = specifiedVars scheme
  when (length explicit > length specified) (failTc "too many explicit type arguments; declare type parameters wiþ '@name:kind'")
  when (any (\case (TyForall {}, _) -> True; _ -> False) explicit) (failTc "type arguments cannot be universally quantified")
  zipWithM_ (\(_, expected) (_, actual) -> unless (expected == actual) (failTc "explicit type argument hath the wrong kind")) (specifiedParameters scheme) explicit
  repls <- freshForNamesM headVars rowVars vars
  let appliedReplacements = Map.union (Map.fromList (zip specified (map fst explicit))) repls
  mapM_ (\(RowEquality xs ys) -> unifyEffectsM xs ys) (map (mapRowEquality (`replaceVars` appliedReplacements)) (schemeRows scheme))
  pure (appliedReplacements, replaceVars ty appliedReplacements, map (`replaceConstraintVars` appliedReplacements) constraints)

specifiedVars :: Scheme -> [String]
specifiedVars = parameterNames . specifiedParameters

specifiedParameters :: Scheme -> [TypeParameter]
specifiedParameters (Specified parameters _) = parameters
specifiedParameters (Constrained _ scheme) = specifiedParameters scheme
specifiedParameters _ = []

-- an inferred binding may retain a quantified value from an ascription or a
-- polymorphic result. its written binders remain available for type input.
exposeSchemeQuantifiers :: Scheme -> Scheme
exposeSchemeQuantifiers (Constrained rows scheme) = Constrained rows (exposeSchemeQuantifiers scheme)
exposeSchemeQuantifiers (Forall variables constraints (TyForall parameters body)) = Specified parameters (Forall (nub (parameterNames parameters ++ variables)) constraints body)
exposeSchemeQuantifiers scheme = scheme

-- quantifiers are instantiated only at the outer edge. argument quantifiers
-- remain intact until checking a supplied value against its expected type.
instantiateTypeM :: Ty -> Tc Ty
instantiateTypeM (TyForall (parameterNames -> names) body) = instantiateM (Forall names [] body) >>= instantiateTypeM . fst
instantiateTypeM ty = pure ty

schemeRows :: Scheme -> [RowEquality]
schemeRows (Constrained rows scheme) = rows ++ schemeRows scheme
schemeRows (Specified _ scheme) = schemeRows scheme
schemeRows _ = []

mapRowEquality :: (Ty -> Ty) -> RowEquality -> RowEquality
mapRowEquality f (RowEquality xs ys) = RowEquality (map (mapEffectTypes f) xs) (map (mapEffectTypes f) ys)

rowEqualityType :: RowEquality -> Ty
rowEqualityType (RowEquality xs ys) = TyArrow (TyCon "ℤ") (xs ++ ys) (TyCon "ℤ")

freshForNamesM :: [String] -> [String] -> [String] -> Tc (Map.Map String Ty)
freshForNamesM headVars rowVars vars =
  Map.fromList <$> traverse freshPair vars
  where
    freshPair v
      | v `elem` rowVars = (v,) . TyRowVariable . RowVariable <$> freshNameM "e"
      | v `elem` headVars = (v,) . TyVar <$> freshNameM "h"
      | otherwise = (v,) <$> freshM

freshNameM :: String -> Tc String
freshNameM prefix = (prefix ++) . show <$> freshIdM

replaceVars :: Ty -> Map.Map String Ty -> Ty
replaceVars ty replacements = replaceTypeVariables True replacements ty

replaceTypeVariables :: Bool -> Map.Map String Ty -> Ty -> Ty
replaceTypeVariables includeRows replacements = go
  where
    go ty = case ty of
      TyVar name -> Map.findWithDefault ty name replacements
      TyRowVariable (RowVariable name) | includeRows -> Map.findWithDefault ty name replacements
      TyApplication head@(VariableHead name) args ->
        let arguments = map go args
         in maybe (applyApplicationHead head arguments) (`applyReplacementTy` arguments) (Map.lookup name replacements)
      TyForall parameters body ->
        let active = foldr Map.delete replacements (parameterNames parameters)
            replacementNames = foldr typeVarsInTy [] (Map.elems active)
            (renamed, body2) = renameBoundVariables replacementNames parameters body
         in TyForall renamed (replaceTypeVariables includeRows active body2)
      _ -> mapTyChildren go go ty

renameBoundVariables :: [String] -> [TypeParameter] -> Ty -> ([TypeParameter], Ty)
renameBoundVariables captured parameters body
  | all (`notElem` captured) names = (parameters, body)
  | otherwise =
      let taken = names ++ captured ++ allTypeNames body
          (_, renamed) = foldl' rename (taken, []) names
          replacements = Map.fromList [(old, TyVar new) | (old, new) <- zip names renamed, old /= new]
       in (zip renamed (map snd parameters), replaceVars body replacements)
  where
    names = parameterNames parameters
    rename (taken, names) name =
      let fresh = if name `elem` captured then unused 0 else name
          unused :: Int -> String
          unused index = let candidate = name ++ "$" ++ show index in if candidate `elem` taken then unused (index + 1) else candidate
       in (fresh : taken, names ++ [fresh])

replaceConstraintVars :: Constraint -> Map.Map String Ty -> Constraint
replaceConstraintVars (Constraint args name) repls = Constraint (map (`replaceVars` repls) args) name

typeAnnScheme :: TypeAnn -> TcContext -> Scheme
typeAnnScheme annotation ctx = typeAnnSchemeWith [] ctx annotation

typeAnnSchemeWith :: [String] -> TcContext -> TypeAnn -> Scheme
typeAnnSchemeWith bound ctx (TypeAnn ty (headerRequirements -> constraints)) =
  let (parameters, body) = case ty of TypeForall declarations inner -> (declarations, inner); _ -> ([], ty)
      names = parameterNames parameters
      boundNames = names ++ bound
      scheme = generalizeTyWithConstraints (map (convertClassConstraintWith boundNames ctx) constraints) (convertTypeExprWith boundNames ctx body)
   in if null names then scheme else Specified parameters (addForallVars names scheme)

convertClassConstraint :: ClassConstraint -> TcContext -> Constraint
convertClassConstraint constraint ctx = convertClassConstraintWith [] ctx constraint

convertClassConstraintWith :: [String] -> TcContext -> ClassConstraint -> Constraint
convertClassConstraintWith bound ctx (ClassConstraint args name) =
  Constraint (map (convertTypeExprWith bound ctx) args) (classRefForName name ctx)

classRefForName :: String -> TcContext -> ClassRef
classRefForName name ctx = case findClassInfo name ctx of
  Just info -> ClassRef (classTarget info) name
  Nothing -> ClassRef (SymbolId (tcModule ctx) name) name

canonicalClassName :: String -> TcContext -> String
canonicalClassName name ctx
  | not (isQualifiedName name) = name
  | otherwise = case findClassInfo name ctx of
      Just ClassInfo {classTarget = SymbolId owner targetName}
        | owner == tcModule ctx -> name
        | SourceModule path <- owner -> importNamespace path ++ "~" ++ targetName
        | otherwise -> targetName
      Nothing -> name

convertTypeExpr :: TypeExpr -> TcContext -> Ty
convertTypeExpr expression ctx = convertTypeExprWith [] ctx expression

convertTypeExprWith :: [String] -> TcContext -> TypeExpr -> Ty
convertTypeExprWith bound ctx = \case
  TypeHole ident -> TyVar ("$hole" ++ show ident)
  TypeName name
    | name `elem` bound -> TyVar name
    | otherwise -> atomTypeName name ctx
  TypeApply name args
    | name `elem` bound -> TyApplication (VariableHead name) arguments
    | otherwise -> atomAppliedTypeName name arguments ctx
    where
      arguments = map convert args
  TypeRecord fields -> TyRecord (Map.fromList [(name, convert ty) | (name, ty) <- fields])
  TypeArrow args effects ret ->
    TyFun
      (fmap convert args)
      (map convertEffect effects)
      (convert ret)
  TypeForall parameters body -> TyForall parameters (convertTypeExprWith (parameterNames parameters ++ bound) ctx body)
  where
    convert = convertTypeExprWith bound ctx
    convertEffect = convertEffectExprWith bound ctx

convertEffectExprWith :: [String] -> TcContext -> TypeExpr -> Effect
convertEffectExprWith bound ctx = effectFromTy . convertTypeExprWith bound ctx

typeExprFromAppliedTy :: Ty -> TypeExpr
typeExprFromAppliedTy = \case
  TyMeta ident -> TypeName ("t" ++ show ident)
  TyVar name -> TypeName name
  TyRowVariable (RowVariable name) -> TypeName name
  TyEffectSkolem ident -> TypeName ("$effect" ++ show ident)
  TyCon name -> TypeName name
  TySkolem ident args -> TypeApply (skolemName ident) (map typeExprFromAppliedTy args)
  TyApp name args -> TypeApply name (map typeExprFromAppliedTy args)
  TyNamed TypeRef {typeDisplayName} [] -> TypeName typeDisplayName
  TyNamed TypeRef {typeDisplayName} args -> TypeApply typeDisplayName (map typeExprFromAppliedTy args)
  TyEffect EffectRef {effectDisplayName} args -> TypeApply effectDisplayName (map typeExprFromAppliedTy args)
  TyRecord fields -> TypeRecord [(name, typeExprFromAppliedTy fieldTy) | (name, fieldTy) <- Map.toList fields]
  TyFun args effects ret -> TypeArrow (fmap typeExprFromAppliedTy args) (map (typeExprFromAppliedTy . effectAsTy) effects) (typeExprFromAppliedTy ret)
  TyForall parameters body -> TypeForall parameters (typeExprFromAppliedTy body)

specializeConstraint :: [String] -> [Ty] -> Constraint -> Constraint
specializeConstraint params arguments (Constraint constraintArguments class_) =
  let replacements = Map.fromList (zip params arguments)
   in Constraint (map (replaceClassTyParams replacements) constraintArguments) class_

specializeResolvedTypeAnn :: [String] -> [Ty] -> ResolvedTypeAnn -> ResolvedTypeAnn
specializeResolvedTypeAnn params instanceTypes (ResolvedTypeAnn ty constraints) =
  let replacements = Map.fromList (zip params instanceTypes)
      -- method quantifiers must not capture variables from the instance head.
      (renamedType, renamedConstraints) = case ty of
        TyForall names body ->
          let (fresh, renamed) = renameBoundVariables (foldr typeVarsInTy [] instanceTypes) names body
              renaming = Map.fromList (zip (parameterNames names) (map (TyVar . fst) fresh))
           in (TyForall fresh renamed, map (`replaceConstraintVars` renaming) constraints)
        _ -> (ty, constraints)
   in ResolvedTypeAnn (replaceClassTyParams replacements renamedType) (map (replaceConstraintClassTyParams replacements) renamedConstraints)

replaceConstraintClassTyParams :: Map.Map String Ty -> Constraint -> Constraint
replaceConstraintClassTyParams replacements (Constraint args class_) = Constraint (map (replaceClassTyParams replacements) args) class_

replaceClassTyParams :: Map.Map String Ty -> Ty -> Ty
replaceClassTyParams = replaceTypeVariables True

applyReplacementTy :: Ty -> [Ty] -> Ty
applyReplacementTy replacement args = case replacement of
  TyCon name -> applyApplicationHead (ConstructorHead (StructuralConstructor name)) args
  TyVar name -> applyApplicationHead (VariableHead name) args
  TyApplication head existing -> applyApplicationHead head (existing ++ args)
  _ -> replacement

atomTypeName :: String -> TcContext -> Ty
atomTypeName name ctx
  | isPrimitiveTypeName name = TyCon name
  | Just ([], target) <- aliasInfo name ctx = target
  | Just ref <- typeRefForName name ctx = TyNamed ref []
  | Just ref <- effectRefForName name ctx = TyEffect ref []
  | isQualifiedName name = TyCon name
  | otherwise = TyVar name

atomAppliedTypeName :: String -> [Ty] -> TcContext -> Ty
atomAppliedTypeName "→" args _ = tyAppOrArrow "→" args
atomAppliedTypeName name args ctx = case aliasInfo name ctx of
  Just ([], target) | isScopedTypeVariable target -> applyReplacementTy target args
  Just (params, target)
    | length params == length args -> replaceVars target (Map.fromList (zip params args))
  _
    | Just ref <- typeRefForName name ctx -> TyNamed ref args
    | Just ref <- effectRefForName name ctx -> TyEffect ref args
    | otherwise -> TyApp name args

isScopedTypeVariable :: Ty -> Bool
isScopedTypeVariable TyVar {} = True
isScopedTypeVariable TySkolem {} = True
isScopedTypeVariable _ = False

aliasInfo :: String -> TcContext -> Maybe ([String], Ty)
aliasInfo name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (Alias _ params target) -> Just (params, target)
  _ -> Nothing

typeRefForName :: String -> TcContext -> Maybe TypeRef
typeRefForName name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (DataInfo ref _ _) -> Just ref {typeDisplayName = name}
  _ -> Nothing

canonicalTypeName :: String -> TcContext -> Maybe String
canonicalTypeName name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (Primitive canonical) -> Just canonical
  Just (Alias canonical _ _) -> Just canonical
  Just (DataInfo TypeRef {typeDisplayName} _ _) -> Just typeDisplayName
  Just EffectInfo {} -> Nothing
  Just (ShownType _) -> Nothing
  Nothing -> Nothing

effectRefForName :: String -> TcContext -> Maybe EffectRef
effectRefForName name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (EffectInfo effect _ _) -> Just effect {effectDisplayName = name}
  _
    | name `elem` hostEffectNames -> Just (runtimeEffectRef name)
    | otherwise -> Nothing

runtimeEffectRef :: String -> EffectRef
runtimeEffectRef name = EffectRef (SymbolId RuntimeModule name) name

findTypeInfoForTypeSystem :: String -> TcContext -> Maybe TypeInfo
findTypeInfoForTypeSystem name TcContext {tcTypes} = Map.lookup name tcTypes

typeVarsInTy :: Ty -> [String] -> [String]
typeVarsInTy = foldFreeNamesWith collect collect
  where
    collect (TyVar name) acc = addUnique name acc
    collect (TyRowVariable (RowVariable name)) acc = addUnique name acc
    collect (TyApplication (VariableHead name) _) acc = addUnique name acc
    collect _ acc = acc

typeVarsInConstraints :: [Constraint] -> [String] -> [String]
typeVarsInConstraints = foldConstraintTypes typeVarsInTy

typeHeadVarsInTy :: Ty -> [String] -> [String]
typeHeadVarsInTy = foldFreeNamesWith collect collectEffect
  where
    collect (TyApplication (VariableHead name) _) acc = addUnique name acc
    collect _ acc = acc
    collectEffect effect acc
      | isEffectVarTy effect = acc
      | otherwise = collect effect acc

typeHeadVarsInConstraints :: [Constraint] -> [String] -> [String]
typeHeadVarsInConstraints = foldConstraintTypes typeHeadVarsInTy

effectRowVarsInTy :: Ty -> [String] -> [String]
effectRowVarsInTy = foldFreeNamesWith keep collectEffect
  where
    keep _ acc = acc
    collectEffect (TyRowVariable (RowVariable name)) acc = addUnique name acc
    collectEffect _ acc = acc

effectRowVarsInConstraints :: [Constraint] -> [String] -> [String]
effectRowVarsInConstraints = foldConstraintTypes effectRowVarsInTy

addUnique :: (Eq a) => a -> [a] -> [a]
addUnique x xs = if x `elem` xs then xs else x : xs

showTy :: Ty -> String
showTy = \case
  TyMeta ident -> "?" ++ show ident
  TyVar v -> v
  TyRowVariable (RowVariable name) -> name
  TyEffectSkolem ident -> "$effect" ++ show ident
  TyCon c -> c
  TySkolem ident [] -> skolemName ident
  TySkolem ident args -> skolemName ident ++ "<" ++ showTyList args ++ ">"
  TyForall (parameterNames -> names) body -> "forall " ++ unwords names ++ ". " ++ showTy body
  TyApp name args -> name ++ "<" ++ showTyList args ++ ">"
  TyNamed TypeRef {typeDisplayName} [] -> typeDisplayName
  TyNamed TypeRef {typeDisplayName} args -> typeDisplayName ++ "<" ++ showTyList args ++ ">"
  TyEffect EffectRef {effectDisplayName} args -> effectDisplayName ++ "<" ++ showTyList args ++ ">"
  TyRecord fields -> "r{" ++ showRecordFields fields ++ "}"
  TyFun args effects ret ->
    let effectText = if null effects then "" else "{" ++ showEffects effects ++ "} "
     in "(" ++ showTyList (NE.toList args) ++ " \\" ++ effectText ++ showTy ret ++ ")"

showRecordFields :: Map.Map String Ty -> String
showRecordFields fields = intercalate ", " [name ++ ": " ++ showTy ty | (name, ty) <- Map.toList fields]

showTyList :: [Ty] -> String
showTyList = intercalate ", " . map showTy

baseRuntimeNames :: [String]
baseRuntimeNames = nub (map hostName (baseNativeBindings ++ baseEffectBindings))

-- this base contract must agree with evaluator arities and primitive dictionary
-- support; source libraries supply þe public declarations around these names.
baseContext :: TcContext
baseContext =
  TcContext
    { tcModule = RootModule,
      tcEnv = baseEnv,
      tcTypes = Map.empty,
      tcTypeRegistry = Map.empty,
      tcTypeAmbiguities = Map.empty,
      tcClasses = Map.empty,
      tcKinds = Kind.emptyKindContext,
      tcClassRegistry = Map.empty,
      tcInstances = indexInstances primitiveInstances
    }

primitiveInstances :: [InstanceInfo]
primitiveInstances =
  [ InstanceInfo (ClassRef primitiveInstanceClassTarget primitiveInstanceClass) [TyCon primitiveInstanceType] [] (PrimitiveInstanceId primitiveInstanceClass primitiveInstanceType) 0 [] []
  | PrimitiveInstanceSpec {..} <- primitiveInstanceSpecs
  ]

instanceKeyFor :: ModuleId -> String -> [Ty] -> InstanceId
instanceKeyFor owner className types = DeclaredInstanceId sourceOwner className (map instanceType types)
  where
    sourceOwner = case owner of
      SourceModule {} -> Just owner
      _ -> Nothing

instanceType :: Ty -> InstanceType
instanceType = go (0 :: Int)
  where
    go depth ty = case normalizeFun ty of
      TyMeta ident -> InstanceTypeName ("?" ++ show ident)
      TyVar name -> InstanceTypeName name
      TyRowVariable (RowVariable name) -> InstanceTypeName name
      TyEffectSkolem ident -> InstanceTypeName ("$effect" ++ show ident)
      TyCon name -> InstanceTypeName name
      TySkolem ident arguments -> InstanceTypeApply (skolemName ident) (map (go depth) arguments)
      TyApp name arguments -> InstanceTypeApply name (map (go depth) arguments)
      TyNamed ref arguments -> InstanceTypeNominal ref (map (go depth) arguments)
      TyEffect ref arguments -> InstanceTypeEffect ref (map (go depth) arguments)
      TyRecord fields -> InstanceTypeRecord [(name, go depth fieldType) | (name, fieldType) <- Map.toList fields]
      TyFun arguments effects result -> InstanceTypeArrow (map (go depth) (NE.toList arguments)) (map (go depth . effectAsTy) effects) (go depth result)
      TyForall parameters body ->
        let replacements = Map.fromList [(name, TyCon ("$bound" ++ show depth ++ "_" ++ show index)) | (name, index) <- zip (parameterNames parameters) [0 :: Int ..]]
         in InstanceTypeForall (map snd parameters) (go (depth + 1) (replaceVars body replacements))

-- an imported context contributeþ only shown surface names. instance metadata
-- retainþ private inherited targets needed by its checked runtime dictionaries.
namespaceImportContext :: String -> TcContext -> TcContext
namespaceImportContext ns TcContext {..} =
  let preferredNames = projectedDisplayNames ns tcTypes tcClasses
   in TcContext
        { tcModule,
          tcEnv = namespaceImportEnv ns preferredNames tcEnv,
          tcTypes = namespaceImportTypes ns tcTypes,
          tcTypeRegistry,
          tcTypeAmbiguities = Map.empty,
          tcClasses = namespaceImportClasses ns preferredNames tcClasses,
          tcKinds = namespaceImportKinds ns tcTypes tcClasses tcKinds,
          tcClassRegistry,
          tcInstances = relabelInstanceIndex preferredNames (namespaceDisplayName ns) tcInstances
        }

projectedDisplayNames :: String -> Map.Map String TypeInfo -> Map.Map String ClassInfo -> DisplayNames
projectedDisplayNames ns types classes =
  DisplayNames
    { displayTypes = projectedTypes typeInfoNominalIdentity,
      displayEffects = projectedTypes typeInfoEffectIdentity,
      displayClasses =
        Map.fromList
          [ (classTarget, namespaceDisplayName ns name)
          | (name, ClassInfo {classExported = True, classTarget}) <- Map.toList classes,
            not (isQualifiedName name)
          ]
    }
  where
    projectedTypes identityOf =
      Map.fromList
        [ (identity, namespaceDisplayName ns name)
        | (name, info) <- Map.toList types,
          typeInfoIsShown info,
          not (isQualifiedName name),
          Just identity <- [identityOf info]
        ]

typeInfoNominalIdentity :: TypeInfo -> Maybe SymbolId
typeInfoNominalIdentity info = case unshownTypeInfo info of
  DataInfo TypeRef {typeIdentity} _ _ -> Just typeIdentity
  Alias _ params (TyNamed TypeRef {typeIdentity} arguments)
    | arguments == map TyVar params -> Just typeIdentity
  _ -> Nothing

typeInfoEffectIdentity :: TypeInfo -> Maybe SymbolId
typeInfoEffectIdentity info = case unshownTypeInfo info of
  EffectInfo EffectRef {effectIdentity} _ _ -> Just effectIdentity
  _ -> Nothing

aliasImportContext :: String -> String -> TcContext -> TcContext
aliasImportContext canonical alias ctx@TcContext {..}
  | alias == canonical = ctx
  | otherwise =
      ctx
        { tcEnv = mapMaybe aliasBinding surfaceEnv ++ surfaceEnv,
          tcTypes = aliasMap (relabelAliasedTypeInfoDisplays (replaceNamespace canonical alias)) tcTypes,
          tcClasses = aliasMap (relabelClassInfoRefs emptyDisplayNames (replaceNamespace canonical alias)) tcClasses,
          tcKinds = Kind.renameKinds (\name -> name : [replaceNamespace canonical alias name | canonicalPrefix name]) tcKinds,
          tcInstances = relabelInstanceIndex emptyDisplayNames (replaceNamespace canonical alias) tcInstances
        }
  where
    surfaceEnv = map surfaceOrigin tcEnv
    surfaceOrigin binding@EnvBinding {envScheme, envOrigin} =
      binding
        { envScheme = relabelSchemeTypeRefs emptyDisplayNames (replaceNamespace canonical alias) envScheme,
          envOrigin = replaceNamespace canonical alias envOrigin
        }
    aliasBinding binding@EnvBinding {envName} =
      (\rest -> binding {envName = alias ++ "~" ++ rest}) <$> stripPrefix (canonical ++ "~") envName
    aliasMap transform entries = Map.union (Map.map transform aliased) entries
      where
        aliased = Map.mapKeys (replaceNamespace canonical alias) (Map.filterWithKey (\name _ -> canonicalPrefix name) entries)
    canonicalPrefix = isJust . stripPrefix (canonical ++ "~")

namespaceImportEnv :: String -> DisplayNames -> [EnvBinding] -> [EnvBinding]
namespaceImportEnv ns preferredNames env = concatMap one env
  where
    one binding@EnvBinding {envName = name, envScheme = scheme, envRank = rank, envOrigin = origin}
      | isBaseEnvBinding name origin = []
      | rank /= exportEnvRank || not (isShownOrigin origin) = []
      | otherwise =
          let scheme2 = relabelSchemeTypeRefs preferredNames (namespaceDisplayName ns) scheme
              origin2 = ns ++ "~" ++ lastQualifiedSegment origin
              imported visibleName visibleRank =
                binding
                  { envName = visibleName,
                    envScheme = scheme2,
                    envRank = visibleRank,
                    envOrigin = origin2
                  }
           in imported (lastQualifiedSegment name) aliasEnvRank : [imported (ns ++ "~" ++ name) importEnvRank]

isShownOrigin :: String -> Bool
isShownOrigin origin = "show@" `isPrefixOf` origin

namespaceImportTypes :: String -> Map.Map String TypeInfo -> Map.Map String TypeInfo
namespaceImportTypes ns =
  Map.foldlWithKey'
    step
    Map.empty
  where
    step acc name info =
      let info2 = unshownTypeInfo (relabelTypeInfoDisplays (namespaceTypeName ns) info)
          withExact = Map.insert (ns ++ "~" ++ name) info2 acc
       in if typeInfoIsShown info && not (isQualifiedName name)
            then Map.insert (lastQualifiedSegment name) info2 withExact
            else acc

-- kinds follow the same visible names as types and classes. private imported
-- classes must not introduce parameters through an unqualified byzen header.
namespaceImportKinds :: String -> Map.Map String TypeInfo -> Map.Map String ClassInfo -> Kind.KindContext -> Kind.KindContext
namespaceImportKinds ns types classes Kind.KindContext {..} =
  Kind.KindContext
    (Map.fromList [(visible, kind) | (name, info) <- Map.toList types, Just kind <- [Map.lookup name typeKinds], visible <- (ns ++ "~" ++ name) : [lastQualifiedSegment name | typeInfoIsShown info && not (isQualifiedName name)]])
    (Map.fromList [(visible, kinds) | (name, ClassInfo {classExported = True}) <- Map.toList classes, Just kinds <- [Map.lookup name classKinds], visible <- [lastQualifiedSegment name, ns ++ "~" ++ lastQualifiedSegment name]])

relabelTypeInfoDisplays :: (String -> String) -> TypeInfo -> TypeInfo
relabelTypeInfoDisplays rename = \case
  primitive@Primitive {} -> primitive
  Alias name params target -> Alias (rename name) params (relabelTypeRefs emptyDisplayNames rename target)
  DataInfo ref params constructors -> DataInfo (relabelTypeRef emptyDisplayNames rename ref) params constructors
  EffectInfo effect params operations -> EffectInfo (relabelEffectRef emptyDisplayNames rename effect) params operations
  ShownType info -> ShownType (relabelTypeInfoDisplays rename info)

relabelAliasedTypeInfoDisplays :: (String -> String) -> TypeInfo -> TypeInfo
relabelAliasedTypeInfoDisplays rename = \case
  Alias name params target -> Alias name params (relabelTypeRefs emptyDisplayNames rename target)
  EffectInfo effect params operations -> EffectInfo (relabelEffectRef emptyDisplayNames rename effect) params operations
  info -> info

namespaceTypeName :: String -> String -> String
namespaceTypeName ns name
  | isPrimitiveTypeName name || isQualifiedName name = name
  | otherwise = namespaceDisplayName ns name

namespaceDisplayName :: String -> String -> String
namespaceDisplayName ns name
  | isQualifiedName name = name
  | otherwise = ns ++ "~" ++ name

typeInfoIsShown :: TypeInfo -> Bool
typeInfoIsShown = \case
  ShownType _ -> True
  _ -> False

namespaceImportClasses :: String -> DisplayNames -> Map.Map String ClassInfo -> Map.Map String ClassInfo
namespaceImportClasses ns preferredNames =
  Map.foldlWithKey' step Map.empty
  where
    step acc name ClassInfo {..} =
      let info = relabelClassInfoRefs preferredNames (namespaceDisplayName ns) ClassInfo {classExported = False, ..}
       in if classExported
            then Map.insert (lastQualifiedSegment name) info (Map.insert (ns ++ "~" ++ lastQualifiedSegment name) info acc)
            else acc

relabelClassInfoRefs :: DisplayNames -> (String -> String) -> ClassInfo -> ClassInfo
relabelClassInfoRefs names fallback info@ClassInfo {classConstraints, classMethods} =
  info
    { classConstraints = map relabelConstraint classConstraints,
      classMethods = map relabelMethod classMethods
    }
  where
    relabelTy = relabelTypeRefs names fallback
    relabelConstraint = relabelConstraintRefs names fallback
    relabelAnnotation (ResolvedTypeAnn ty constraints) =
      ResolvedTypeAnn (relabelTy ty) (map relabelConstraint constraints)
    relabelMethod method@ClassMethod {classMethodType} =
      method {classMethodType = relabelAnnotation classMethodType}

relabelInstanceIndex :: DisplayNames -> (String -> String) -> Map.Map String [InstanceInfo] -> Map.Map String [InstanceInfo]
relabelInstanceIndex names fallback = Map.map (map relabelInstance)
  where
    relabelInstance info@InstanceInfo {instanceClass, instanceTypes, instanceConstraints, instanceParents} =
      info
        { instanceClass = relabelClassRef names fallback instanceClass,
          instanceTypes = map (relabelTypeRefs names fallback) instanceTypes,
          instanceConstraints = map (relabelConstraintRefs names fallback) instanceConstraints,
          instanceParents = map (relabelConstraintRefs names fallback) instanceParents
        }

mergeContext :: TcContext -> TcContext -> TcContext
mergeContext left right =
  TcContext
    { tcModule = tcModule right,
      tcEnv = tcEnv left ++ tcEnv right,
      tcTypes = Map.union (tcTypes left) (tcTypes right),
      tcTypeRegistry = Map.union (tcTypeRegistry left) (tcTypeRegistry right),
      tcTypeAmbiguities = Map.unionsWith unionNames [tcTypeAmbiguities left, tcTypeAmbiguities right, collisions],
      tcClasses = Map.union (tcClasses left) (tcClasses right),
      tcKinds = Kind.mergeKinds (tcKinds left) (tcKinds right),
      tcClassRegistry = Map.union (tcClassRegistry left) (tcClassRegistry right),
      tcInstances = Map.unionWith (++) (tcInstances left) (tcInstances right)
    }
  where
    collisions =
      Map.fromList
        [ (name, unionNames (typeInfoNames leftInfo) (typeInfoNames rightInfo))
        | (name, leftInfo) <- Map.toList (tcTypes left),
          not (isQualifiedName name),
          Just rightInfo <- [Map.lookup name (tcTypes right)],
          typeInfoNames leftInfo /= typeInfoNames rightInfo
        ]
    unionNames xs ys = nub (xs ++ ys)

termTargets :: TcContext -> Map.Map String [TermExport]
termTargets TcContext {tcEnv} = foldr add Map.empty tcEnv
  where
    add EnvBinding {envName, envTarget = Just target} = Map.insertWith (++) envName [target]
    add _ = id

-- project runtime term exports from the same context that passed static
-- checking. types and private class/instance closure remain in 'TcContext'.
moduleInterface :: TcContext -> ModuleInterface
moduleInterface TcContext {tcModule, tcEnv} = ModuleInterface tcModule terms
  where
    terms = foldr addShownTerm Map.empty tcEnv
    addShownTerm EnvBinding {envName, envRank, envOrigin, envTarget = Just target}
      | envRank == exportEnvRank,
        isShownOrigin envOrigin =
          Map.insert (SymbolId tcModule (lastQualifiedSegment envName)) target
    addShownTerm _ = id

typeInfoNames :: TypeInfo -> [String]
typeInfoNames = \case
  Primitive name -> [name]
  Alias name _ _ -> [name]
  DataInfo TypeRef {typeDisplayName} _ _ -> [typeDisplayName]
  EffectInfo EffectRef {effectDisplayName} _ _ -> [effectDisplayName]
  ShownType info -> typeInfoNames info

isBaseEnvBinding :: String -> String -> Bool
isBaseEnvBinding name origin = name `elem` baseRuntimeNames && origin == "base@" ++ name

isPrimitiveTypeName :: String -> Bool
isPrimitiveTypeName name = name `elem` primitiveTypeNames

isConcreteSchemaType :: TcContext -> String -> Bool
isConcreteSchemaType ctx name =
  isPrimitiveTypeName name || isQualifiedName name || isJust (findTypeInfoForTypeSystem name ctx)

-- nominal headers make declaration identity independent of source order;
-- transparent aliases are rebuilt as data and effect ABIs settle.
resolveTypeHeadersM :: [Decl] -> TcContext -> Tc TcContext
resolveTypeHeadersM declarations importedCtx = do
  let aliasHeaders = mapMaybe aliasHeader declarations
      aliasNames = Set.fromList (map aliasHeaderName aliasHeaders)
      dataHeaders = mapMaybe (hostDataHeader (tcModule importedCtx)) declarations
      localNames = Set.fromList (mapMaybe declaredTypeName declarations)
      isConcrete name = Set.member name localNames || isConcreteSchemaType importedCtx name
      nominalCtx = foldl' (flip (collectNominalTypeHeader isConcrete)) importedCtx declarations
      aliasComponents = stronglyConnComp [aliasNode aliasNames header | header <- aliasHeaders]
      addAliases = foldM addAlias
  aliasCtx <- addAliases nominalCtx aliasComponents
  let dataCtx = resolveHostDataHeaders dataHeaders aliasCtx
  dataAliasCtx <- addAliases dataCtx aliasComponents
  let effectCtx = foldl' (flip collectResolvedEffectHeader) dataAliasCtx declarations
  addAliases effectCtx aliasComponents
  where
    addAlias ctx (AcyclicSCC AliasHeader {..}) =
      pure (addTypeAliasInfo aliasHeaderParams aliasHeaderName aliasHeaderTarget ctx)
    addAlias _ (CyclicSCC headers) =
      failTc ("recursive type aliases: " ++ intercalate ", " (map aliasHeaderName headers))

aliasHeader :: Decl -> Maybe AliasHeader
aliasHeader = \case
  Export nested -> aliasHeader nested
  TypeAlias (parameterNames -> params) name target -> Just (AliasHeader name params target)
  _ -> Nothing

hostDataHeader :: ModuleId -> Decl -> Maybe DataHeader
hostDataHeader owner = \case
  Export nested -> hostDataHeader owner nested
  DataDecl (parameterNames -> params) name constructors -> do
    expected <- hostTypeId name
    if typeIdentity (dataDeclarationRef True owner params name constructors) == expected
      then Just (DataHeader name params constructors)
      else Nothing
  _ -> Nothing

declaredTypeName :: Decl -> Maybe String
declaredTypeName = \case
  Export nested -> declaredTypeName nested
  TypeAlias _ name _ -> Just name
  DataDecl _ name _ -> Just name
  EffectDecl _ name _ -> Just name
  _ -> Nothing

collectNominalTypeHeader :: (String -> Bool) -> Decl -> TcContext -> TcContext
collectNominalTypeHeader isConcrete declaration ctx = case declaration of
  Export nested -> collectNominalTypeHeader isConcrete nested ctx
  DataDecl (parameterNames -> params) name constructors ->
    addDataTypeHeader name (dataDeclarationRef True (tcModule ctx) params name constructors) params ctx
  EffectDecl (parameterNames -> params) name operations ->
    let target = effectId isConcrete (tcModule ctx) params name operations
        effect = EffectRef target (renderEffectId target)
     in addEffectTypeInfo name effect params operations ctx
  _ -> ctx

collectResolvedEffectHeader :: Decl -> TcContext -> TcContext
collectResolvedEffectHeader declaration ctx = case declaration of
  Export nested -> collectResolvedEffectHeader nested ctx
  EffectDecl (parameterNames -> params) name operations ->
    addEffectTypeInfo name (resolvedEffectRef params name operations ctx) params operations ctx
  _ -> ctx

resolvedEffectRef :: [String] -> String -> [EffectOp] -> TcContext -> EffectRef
resolvedEffectRef params name operations ctx =
  EffectRef target (renderEffectId target)
  where
    candidate = effectId (isConcreteSchemaType ctx) (tcModule ctx) params name operations
    target
      | SymbolId RuntimeModule _ <- candidate,
        hostNominalsMatchResolvedTypes params [operationType | EffectOp _ operationType <- operations] ctx =
          candidate
      | otherwise = SymbolId (tcModule ctx) name

resolveHostDataHeaders :: [DataHeader] -> TcContext -> TcContext
resolveHostDataHeaders headers ctx = foldl' refine ctx components
  where
    names = Set.fromList (map dataHeaderName headers)
    components = stronglyConnComp [dataHeaderNode names header | header <- headers]
    refine current component
      | all (`hostDataHeaderResolves` current) group = current
      | otherwise = foldl' (flip nominalize) current group
      where
        group = case component of
          AcyclicSCC header -> [header]
          CyclicSCC cycleHeaders -> cycleHeaders
    nominalize DataHeader {..} current =
      addDataTypeHeader dataHeaderName ref dataHeaderParams current
      where
        ref = dataDeclarationRef False (tcModule current) dataHeaderParams dataHeaderName dataHeaderConstructors

hostDataHeaderResolves :: DataHeader -> TcContext -> Bool
hostDataHeaderResolves DataHeader {..} =
  hostNominalsMatchResolvedTypes dataHeaderParams (concatMap constructorTypeExprs dataHeaderConstructors)

constructorTypeExprs :: Ctor -> [TypeExpr]
constructorTypeExprs (Ctor _ _ fields result) = fields ++ maybeToList result

dataHeaderNode :: Set.Set String -> DataHeader -> (DataHeader, String, [String])
dataHeaderNode names header@DataHeader {..} =
  ( header,
    dataHeaderName,
    Set.toList (Set.intersection names (Set.difference heads excluded))
  )
  where
    heads = Set.fromList [name | ty <- concatMap constructorTypeExprs dataHeaderConstructors, name <- typeExprHeads ty]
    excluded = Set.fromList (dataHeaderName : dataHeaderParams)

-- raw host schemas are deliberately order-insensitive, but every known
-- reserved spelling must resolve to its type/effect ABI rather than a lookalike.
hostNominalsMatchResolvedTypes :: [String] -> [TypeExpr] -> TcContext -> Bool
hostNominalsMatchResolvedTypes bound expressions ctx = all typeMatches expressions
  where
    typeMatches expression = case expression of
      TypeHole _ -> True
      TypeName name -> headMatches name [] expression
      TypeApply name arguments -> headMatches name arguments expression && all typeMatches arguments
      TypeRecord fields -> all (typeMatches . snd) fields
      TypeArrow arguments effects result -> all typeMatches (NE.toList arguments ++ [result]) && all effectMatches effects
      TypeForall (parameterNames -> names) body -> hostNominalsMatchResolvedTypes (names ++ bound) [body] ctx
    effectMatches expression = case expression of
      TypeName name -> effectHeadMatches name
      TypeApply name arguments -> effectHeadMatches name && all typeMatches arguments
      other -> typeMatches other
    headMatches name arguments expression
      | name `elem` bound = True
      | Just identity <- hostTypeId name,
        isJust (findTypeInfoForTypeSystem name ctx) =
          convertTypeExprWith bound ctx expression
            == TyNamed (TypeRef identity name) (map (convertTypeExprWith bound ctx) arguments)
      | otherwise = True
    effectHeadMatches name
      | name `elem` bound = True
      | name `elem` hostEffectNames = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
          Nothing -> True
          Just (EffectInfo EffectRef {effectIdentity} _ _) -> effectIdentity == SymbolId RuntimeModule name
          Just _ -> False
      | otherwise = True

aliasNode :: Set.Set String -> AliasHeader -> (AliasHeader, String, [String])
aliasNode aliasNames header@AliasHeader {..} =
  ( header,
    aliasHeaderName,
    Set.toList (Set.intersection aliasNames (Set.difference (Set.fromList (typeExprHeads aliasHeaderTarget)) (Set.fromList aliasHeaderParams)))
  )

typeExprHeads :: TypeExpr -> [String]
typeExprHeads = \case
  TypeHole _ -> []
  TypeName name -> [name]
  TypeApply name arguments -> name : concatMap typeExprHeads arguments
  TypeRecord fields -> concatMap (typeExprHeads . snd) fields
  TypeArrow arguments effects result -> concatMap typeExprHeads (NE.toList arguments ++ effects ++ [result])
  TypeForall (parameterNames -> names) body -> filter (`notElem` names) (typeExprHeads body)

-- classes form þe nominal index used by instances. collecting þeir headers first
-- makeþ a forward instance equivalent to þe same declarations in source order.
collectClassContexts :: [Decl] -> TcContext -> TcContext
collectClassContexts ds ctx = foldl' (flip collectClassContext) ctx ds

collectClassContext :: Decl -> TcContext -> TcContext
collectClassContext declaration ctx = case declaration of
  Export nested -> collectClassContext nested ctx
  ClassDecl (typeHeaderParts -> (params, constraints)) name members -> addClassInfo name params constraints members ctx
  _ -> ctx

collectDeclContext :: Decl -> TcContext -> TcContext
collectDeclContext decl ctx = case decl of
  Import _ _ -> ctx
  Export declaration -> exportDeclContext declaration (collectDeclContext declaration ctx)
  ReExport _ -> ctx
  ReExportType _ -> ctx
  DataDecl params name ctors -> case declaredDataRef name ctx of
    Just ref -> collectDataDeclaration name ref params ctors ctx
    Nothing -> ctx
  TypeAlias {} -> ctx
  EffectDecl (parameterNames -> params) name ops -> case effectRefForName name ctx of
    Just effect -> collectEffectOps params name effect ops ctx
    Nothing -> ctx
  ElaboratedEffect {} -> ctx
  ClassDecl (typeHeaderParts -> (params, constraints)) name members -> addClassMembers name params constraints members ctx
  ElaboratedClass {} -> ctx
  Let name (Just ann) _ -> addGlobalEnv name name (typeAnnScheme ann ctx) OrdinaryTerm ctx
  Let _ Nothing _ -> ctx
  InstanceDecl (typeHeaderParts -> (names, constraints)) tyArgs className members -> addInstanceInfo names className tyArgs constraints members ctx
  ElaboratedInstance _ _ tyArgs className constraints members -> addInstanceInfo [] className tyArgs constraints members ctx

exportDeclContext :: Decl -> TcContext -> TcContext
exportDeclContext declaration ctx = case declaration of
  Import _ _ -> ctx
  Export nested -> exportDeclContext nested ctx
  ReExport name -> fromRight ctx (reExportName name ctx)
  ReExportType name -> fromRight ctx (reExportTypeName name ctx)
  Let name _ _ -> addShownEnv name ctx
  TypeAlias _ name _ -> addShownType name ctx
  DataDecl _ name constructors -> foldl' (flip addShownEnv) (addShownType name ctx) [ctorName | Ctor ctorName _ _ _ <- constructors]
  EffectDecl _ name operations -> foldl' (flip addShownEnv) (addShownType name ctx) [opName | EffectOp opName _ <- operations]
  ElaboratedEffect {} -> ctx
  ClassDecl _ name members -> foldl' (flip addShownEnv) (markClassExported name ctx) (classMemberNames members)
  ElaboratedClass {} -> ctx
  InstanceDecl {} -> ctx
  ElaboratedInstance {} -> ctx

markClassExported :: String -> TcContext -> TcContext
markClassExported name ctx@TcContext {tcClasses, tcClassRegistry} =
  case findClassEntry name ctx of
    Just (_, info) ->
      let exported = info {classExported = True}
       in ctx
            { tcClasses = Map.insert (lastQualifiedSegment name) exported tcClasses,
              tcClassRegistry = Map.insert (classTarget info) exported tcClassRegistry
            }
    Nothing -> ctx

reExportName :: String -> TcContext -> Either String TcContext
reExportName name ctx = reExportTerm name ctx >>= maybe (Left ("unknown name '" ++ name ++ "'")) Right

reExportTypeName :: String -> TcContext -> Either String TcContext
reExportTypeName name ctx@TcContext {tcTypes, tcTypeAmbiguities}
  | not (isQualifiedName name) && isPrimitiveTypeName name && Map.notMember name tcTypes =
      Right
        ctx
          { tcTypes = Map.insert name (ShownType (Primitive name)) tcTypes,
            tcTypeAmbiguities = Map.delete name tcTypeAmbiguities
          }
  | otherwise = do
      typeInfo <- reExportTypeInfo name ctx
      let classInfo = findClassEntry name ctx
          withType = maybe ctx (const (addShownType name ctx)) typeInfo
          withClass = maybe withType (const (markClassExported name withType)) classInfo
      if isNothing typeInfo && isNothing classInfo
        then Left ("unknown type or flock '" ++ name ++ "'")
        else Right withClass

reExportTypeInfo :: String -> TcContext -> Either String (Maybe TypeInfo)
reExportTypeInfo name ctx@TcContext {tcTypeAmbiguities}
  | isQualifiedName name = Right (shownTypeInfo name ctx)
  | Just choices <- Map.lookup name tcTypeAmbiguities =
      Left ("ambiguous type '" ++ name ++ "'; qualify it as one of: " ++ intercalate ", " choices)
  | otherwise = Right (shownTypeInfo name ctx)

reExportTerm :: String -> TcContext -> Either String (Maybe TcContext)
reExportTerm name ctx@TcContext {tcEnv}
  | isQualifiedName name = Right (addShownEnv name ctx <$ lookupEnvExact name tcEnv)
  | otherwise = case shownBareEnvBindings name tcEnv of
      Right _ -> Right (Just (addShownEnv name ctx))
      Left message
        | null (collectEnvCandidates name tcEnv) -> Right Nothing
        | otherwise -> Left message

findClassEntry :: String -> TcContext -> Maybe (String, ClassInfo)
findClassEntry name TcContext {tcClasses} = (name,) <$> Map.lookup name tcClasses

declaredDataRef :: String -> TcContext -> Maybe TypeRef
declaredDataRef name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (DataInfo ref _ _) -> Just ref
  _ -> Nothing

collectDataDeclaration :: String -> TypeRef -> [TypeParameter] -> [Ctor] -> TcContext -> TcContext
collectDataDeclaration dataName ref@TypeRef {typeIdentity = SymbolId owner typeName} parameters ctors ctx =
  foldl' addConstructor withType constructors
  where
    constructors =
      [ (name, ConstructorInfo (SymbolId owner (typeName ++ "@" ++ name)) fieldTypes result, constructorParameters)
      | Ctor name declarations fields explicitResult <- ctors,
        let fieldTypes = map (convertTypeExprWith constructorVars ctx) fields
            result = maybe defaultResult (convertTypeExprWith constructorVars ctx) explicitResult
            -- field-only variables become existential when the constructor is matched.
            constructorParameters = declarations ++ parameters
            constructorVars = parameterNames constructorParameters
      ]
    params = parameterNames parameters
    withType = addTypeInfo dataName (DataInfo ref params [info | (_, info, _) <- constructors]) ctx
    defaultResult = TyNamed ref (map TyVar params)
    addConstructor current (name, ConstructorInfo target fields result, declarations) =
      let qualifiedName = dataName ++ "@" ++ name
          scheme = Specified declarations (Forall (parameterNames declarations) [] (curriedFunction fields [] result))
          kind = ConstructorTerm (length fields)
       in addGlobalEnvTarget name qualifiedName scheme target kind (addGlobalEnvTarget qualifiedName qualifiedName scheme target kind current)

collectEffectOps :: [String] -> String -> EffectRef -> [EffectOp] -> TcContext -> TcContext
collectEffectOps params effectName effectRef@EffectRef {effectIdentity} ops ctx = foldl' step ctx ops
  where
    step current (EffectOp opName t) =
      let effect = TyEffect effectRef (map TyVar params)
          annotation = typeAnnSchemeWith params current (TypeAnn t [])
          (vars, constraints, rawTy) = schemeParts annotation
          opTy = addLatentEffect effect rawTy
          scheme = markEffectOpScheme effectRef (Specified (specifiedParameters annotation) (Forall (nub (params ++ vars)) constraints opTy))
          target = effectName ++ "@" ++ opName
          identity = effectOperationId effectIdentity opName
          kind = EffectOperationTerm effectIdentity (effectOperationArity params current t)
       in addGlobalEnvTarget opName target scheme identity kind (addGlobalEnvTarget target target scheme identity kind current)

effectOperationArity :: [String] -> TcContext -> TypeExpr -> Int
effectOperationArity params ctx operationType = case normalizeFun (let (_, _, ty) = schemeParts (typeAnnSchemeWith params ctx (TypeAnn operationType [])) in ty) of
  TyFun arguments _ _ -> NE.length arguments
  _ -> 0

addLatentEffect :: Ty -> Ty -> Ty
addLatentEffect effect ty = case normalizeFun ty of
  TyFun args effects ret -> TyFun args (addEffect (effectFromTy effect) effects) ret
  other -> other

addTypeAliasInfo :: [String] -> String -> TypeExpr -> TcContext -> TcContext
addTypeAliasInfo params name target ctx =
  addTypeInfo name (Alias name params (convertTypeExprWith params ctx target)) ctx

-- provisional data headers participate in name resolution, not coverage.
-- delaying registry insertion preventeþ a rejected host candidate from replacing
-- an imported ABI entry under þe same stable identity.
addDataTypeHeader :: String -> TypeRef -> [String] -> TcContext -> TcContext
addDataTypeHeader name ref params ctx@TcContext {tcTypes, tcTypeAmbiguities} =
  ctx
    { tcTypes = Map.insert name (DataInfo ref params []) tcTypes,
      tcTypeAmbiguities = Map.delete name tcTypeAmbiguities
    }

addEffectTypeInfo :: String -> EffectRef -> [String] -> [EffectOp] -> TcContext -> TcContext
addEffectTypeInfo name effect params operations =
  addTypeInfo name (EffectInfo effect params [effectOperationId (effectIdentity effect) operationName | EffectOp operationName _ <- operations])

addTypeInfo :: String -> TypeInfo -> TcContext -> TcContext
addTypeInfo name info ctx@TcContext {tcTypes, tcTypeRegistry, tcTypeAmbiguities} =
  ctx
    { tcTypes = Map.insert name info tcTypes,
      tcTypeRegistry = case unshownTypeInfo info of
        dataInfo@(DataInfo TypeRef {typeIdentity} _ _) -> Map.insert typeIdentity dataInfo tcTypeRegistry
        _ -> tcTypeRegistry,
      tcTypeAmbiguities = Map.delete name tcTypeAmbiguities
    }

addInstanceInfo :: [String] -> String -> [TypeExpr] -> [ClassConstraint] -> [Decl] -> TcContext -> TcContext
addInstanceInfo names className tyArgs constraints members ctx@TcContext {tcInstances} =
  let actualClass = canonicalClassName className ctx
      actualClassRef = classRefForName className ctx
      actualTypes = map (convertTypeExprWith names ctx) tyArgs
      actualConstraints = map (convertClassConstraintWith names ctx) constraints
      key = instanceKeyFor (tcModule ctx) actualClass actualTypes
      provided = [name | Let name _ _ <- members]
      newInstances = instanceClosure key actualClassRef actualTypes actualConstraints provided ctx [] 0
   in ctx {tcInstances = Map.unionWith (++) (indexInstances newInstances) tcInstances}

indexInstances :: [InstanceInfo] -> Map.Map String [InstanceInfo]
indexInstances = foldr insertInstance Map.empty
  where
    insertInstance info = Map.insertWith (++) (lastQualifiedSegment (classDisplayName (instanceClass info))) [info]

instancesForClass :: String -> TcContext -> [InstanceInfo]
instancesForClass name = Map.findWithDefault [] (lastQualifiedSegment name) . tcInstances

instancesForClassRef :: ClassRef -> TcContext -> [InstanceInfo]
instancesForClassRef class_ = filter ((== class_) . instanceClass) . instancesForClass (classDisplayName class_)

-- duplicate imports may share one InstanceId; nested diagnostic names still follow
-- þe wanted class's alias without changing semantic candidate order.
projectInstanceConstraints :: ClassRef -> InstanceInfo -> [Constraint] -> [Constraint]
projectInstanceConstraints wanted InstanceInfo {instanceClass} = map (relabelConstraintRefs emptyDisplayNames project)
  where
    sourceNamespace = displayNamespace (classDisplayName instanceClass)
    wantedNamespace = displayNamespace (classDisplayName wanted)
    project name = case sourceNamespace >>= (\source -> stripPrefix (source ++ "~") name) of
      Just rest -> maybe rest (\target -> target ++ "~" ++ rest) wantedNamespace
      Nothing -> name
    displayNamespace = fmap fst . splitQualifiedName

instanceParentConstraints :: String -> [TypeExpr] -> TcContext -> [Constraint]
instanceParentConstraints className types ctx = case findClassInfo className ctx of
  Nothing -> []
  Just ClassInfo {classParams, classConstraints} -> map (specializeConstraint classParams (map (`convertTypeExpr` ctx) types)) classConstraints

instanceClosure :: InstanceId -> ClassRef -> [Ty] -> [Constraint] -> [String] -> TcContext -> [ClassRef] -> Int -> [InstanceInfo]
instanceClosure key class_ tyArgs constraints provided ctx seen depth
  | class_ `elem` seen = []
  | otherwise = InstanceInfo class_ tyArgs constraints key depth parents provided : inherited
  where
    parents = case findClassInfoByRef class_ ctx of
      Nothing -> []
      Just ClassInfo {classParams, classConstraints} -> map (specializeConstraint classParams tyArgs) classConstraints
    inherited = concatMap closeConstraint parents
    closeConstraint (Constraint neededTypes neededClass) =
      instanceClosure key neededClass neededTypes constraints provided ctx (class_ : seen) (depth + 1)

addEnv :: String -> Scheme -> TcContext -> TcContext
addEnv name scheme = addEnvWith name scheme localEnvRank name Nothing

addEnvWith :: String -> Scheme -> Int -> String -> Maybe TermExport -> TcContext -> TcContext
addEnvWith name scheme rank origin target ctx@TcContext {tcEnv} =
  ctx {tcEnv = EnvBinding name name scheme rank origin target : tcEnv}

addGlobalEnv :: String -> String -> Scheme -> TermKind -> TcContext -> TcContext
addGlobalEnv name targetName scheme kind ctx@TcContext {tcModule} =
  addGlobalEnvTarget name targetName scheme (SymbolId tcModule targetName) kind ctx

addGlobalEnvTarget :: String -> String -> Scheme -> SymbolId -> TermKind -> TcContext -> TcContext
addGlobalEnvTarget name origin scheme target kind =
  addEnvWith name scheme localEnvRank origin (Just (TermExport target kind))

addShownEnv :: String -> TcContext -> TcContext
addShownEnv name ctx = foldl' addOne ctx (shownEnvBindings name ctx)
  where
    exportName = lastQualifiedSegment name
    addOne current binding =
      addEnvWith exportName (envScheme binding) exportEnvRank ("show@" ++ exportName) (envTarget binding) current

shownEnvBindings :: String -> TcContext -> [EnvBinding]
shownEnvBindings name TcContext {tcEnv}
  | isQualifiedName name = maybeToList (lookupEnvExact name tcEnv)
  | otherwise = case shownBareEnvBindings name tcEnv of
      Right schemes -> schemes
      Left _ -> []

shownBareEnvBindings :: String -> [EnvBinding] -> Either String [EnvBinding]
shownBareEnvBindings name env = rankEnvCandidates name (collectEnvCandidates name env)

addShownType :: String -> TcContext -> TcContext
addShownType name ctx@TcContext {tcTypes, tcTypeAmbiguities}
  | not (isQualifiedName name) && Map.member name tcTypeAmbiguities = ctx
  | otherwise = case shownTypeInfo name ctx of
      Just info -> ctx {tcTypes = Map.insert (lastQualifiedSegment name) (ShownType info) tcTypes}
      Nothing -> ctx

shownTypeInfo :: String -> TcContext -> Maybe TypeInfo
shownTypeInfo name TcContext {tcTypes} = Map.lookup name tcTypes

localEnvRank, aliasEnvRank, importEnvRank, baseEnvRank, exportEnvRank :: Int
localEnvRank = 0
aliasEnvRank = 1
importEnvRank = 1
baseEnvRank = 2
exportEnvRank = 4

lookupEnv :: String -> TcContext -> EnvLookup
lookupEnv name ctx = case lookupEnvChoices name ctx of
  ChoicesMissing -> EnvMissing
  ChoicesAmbiguous msg -> EnvAmbiguous msg
  EnvChoices [] -> EnvMissing
  EnvChoices (binding : _) -> EnvFound (envScheme binding)

lookupEnvBinding :: String -> TcContext -> Either String (Maybe EnvBinding)
lookupEnvBinding name ctx = case lookupEnvChoices name ctx of
  ChoicesMissing -> Right Nothing
  ChoicesAmbiguous message -> Left message
  EnvChoices [] -> Right Nothing
  EnvChoices (binding : _) -> Right (Just binding)

lookupEnvChoices :: String -> TcContext -> EnvChoices
lookupEnvChoices name TcContext {tcEnv}
  | isQualifiedName name = maybe ChoicesMissing (EnvChoices . (: [])) (lookupEnvExact name tcEnv)
  | otherwise = selectEnvChoices name (collectEnvCandidates name tcEnv)

lookupEnvExact :: String -> [EnvBinding] -> Maybe EnvBinding
lookupEnvExact name = find ((== name) . envName)

collectEnvCandidates :: String -> [EnvBinding] -> [EnvBinding]
collectEnvCandidates name = filter ((== name) . envName)

selectEnvChoices :: String -> [EnvBinding] -> EnvChoices
selectEnvChoices _ [] = ChoicesMissing
selectEnvChoices name candidates = case rankEnvCandidates name candidates of
  Left message -> ChoicesAmbiguous message
  Right best -> EnvChoices best

rankEnvCandidates :: String -> [EnvBinding] -> Either String [EnvBinding]
rankEnvCandidates name [] = Left ("unknown name '" ++ name ++ "'")
rankEnvCandidates name candidates =
  let bestRank = minimum (map envRank candidates)
      best = filter ((== bestRank) . envRank) candidates
      origins = nub (map envOrigin best)
   in case origins of
        -- host signatures overload by type; distinct term kinds may share a name.
        -- repeated bindings of one kind follow lexical shadowing.
        [_] -> Right (if bestRank == baseEnvRank then best else nubBy sameKind best)
        _ -> Left ("ambiguous name '" ++ name ++ "'; qualify it as one of: " ++ intercalate ", " origins)
  where
    sameKind left right = bindingKind left == bindingKind right
    bindingKind EnvBinding {envTarget = Just TermExport {termExportKind}} = termExportKind
    bindingKind _ = OrdinaryTerm

baseEnv :: [EnvBinding]
baseEnv =
  map (uncurry baseBinding) baseNativeEnv ++ map baseEffectOpBinding baseEffectOpEnv

baseNativeEnv :: [(String, Scheme)]
baseNativeEnv = [(hostName binding, hostScheme (hostSignature binding)) | binding <- baseNativeBindings]

baseEffectOpEnv :: [(String, String, Int, Scheme)]
baseEffectOpEnv =
  [ (effectName, hostName binding, hostArity binding, hostScheme (hostSignature binding))
  | binding@HostBinding {hostRole = BaseEffect effectName} <- baseEffectBindings
  ]

baseBinding :: String -> Scheme -> EnvBinding
baseBinding name scheme =
  EnvBinding name name scheme baseEnvRank ("base@" ++ name) (Just (TermExport (SymbolId RuntimeModule name) OrdinaryTerm))

baseEffectOpBinding :: (String, String, Int, Scheme) -> EnvBinding
baseEffectOpBinding (effectName, opName, arity, scheme) =
  EnvBinding
    opName
    opName
    (markEffectOpScheme (runtimeEffectRef effectName) scheme)
    baseEnvRank
    ("base@" ++ opName)
    (Just (TermExport (SymbolId RuntimeModule opName) (EffectOperationTerm (SymbolId RuntimeModule effectName) arity)))

hostScheme :: HostSignature -> Scheme
hostScheme HostSignature {..} =
  Forall hostVariables [] (curriedFunction (map hostTy hostArguments) (map hostEffectTy hostEffects) (hostTy hostResult))

hostEffectTy :: HostType -> Effect
hostEffectTy = \case
  HostVar name -> EffectVariable (RowVariable name)
  HostCon name -> EffectLabel (NominalEffect (runtimeEffectRef name)) []
  HostApp name arguments -> EffectLabel (NominalEffect (runtimeEffectRef name)) (map hostTy arguments)

hostTy :: HostType -> Ty
hostTy = \case
  HostVar name -> TyVar name
  HostCon name -> maybe (TyCon name) (\identity -> TyNamed (TypeRef identity name) []) (hostTypeId name)
  HostApp name arguments ->
    let converted = map hostTy arguments
     in maybe (TyApp name converted) (\identity -> TyNamed (TypeRef identity name) converted) (hostTypeId name)

addClassMembers :: String -> [String] -> [ClassConstraint] -> [ClassMember] -> TcContext -> TcContext
addClassMembers className params constraints members ctx = addClassMembersToEnv className members (addClassInfo className params constraints members ctx)

addClassInfo :: String -> [String] -> [ClassConstraint] -> [ClassMember] -> TcContext -> TcContext
addClassInfo className params constraints members ctx@TcContext {tcModule, tcClasses, tcClassRegistry} =
  let signatures = [signature | ClassSignature _ (TypeAnn signature _) <- members]
      schemaResolves = hostNominalsMatchResolvedTypes params signatures ctx
      target = primitiveClassId schemaResolves tcModule params className members
      methods = mapMaybe resolveMethod members
      resolveMethod = \case
        ClassSignature name annotation -> Just (ClassMethod name (resolve annotation))
        _ -> Nothing
      resolve (TypeAnn ty (headerRequirements -> memberConstraints)) =
        ResolvedTypeAnn (convertTypeExprWith params ctx ty) (map (convertClassConstraintWith (annotationParameters ty ++ params) ctx) memberConstraints)
      info =
        ClassInfo
          { classParams = params,
            classConstraints = map (convertClassConstraintWith params ctx) constraints,
            classMethods = methods,
            classExported = False,
            classTarget = target
          }
   in ctx
        { tcClasses = Map.insert className info tcClasses,
          tcClassRegistry = Map.insert target info tcClassRegistry
        }

addClassMembersToEnv :: String -> [ClassMember] -> TcContext -> TcContext
addClassMembersToEnv className members ctx = foldl' step ctx (mapMaybe classMemberSignature members)
  where
    step current (name, annotation) =
      let scheme = classMemberScheme className annotation current
          qualifiedName = className ++ "@" ++ name
          class_ = maybe (SymbolId (tcModule current) className) classTarget (findClassInfo className current)
          kind = ClassMemberTerm class_
       in addGlobalEnv name qualifiedName scheme kind (addGlobalEnv qualifiedName qualifiedName scheme kind current)

classMemberScheme :: String -> TypeAnn -> TcContext -> Scheme
classMemberScheme className ann ctx = case findClassInfo className ctx of
  Just ClassInfo {classParams} ->
    addForallVars classParams (typeAnnSchemeWith classParams ctx (typeAnnWithOwnClassConstraint className ann ctx))
  Nothing -> typeAnnScheme (typeAnnWithOwnClassConstraint className ann ctx) ctx

addForallVars :: [String] -> Scheme -> Scheme
addForallVars extra = \case
  Specified names scheme -> Specified names (addForallVars extra scheme)
  Constrained rows scheme -> Constrained rows (addForallVars extra scheme)
  Forall vars constraints ty -> Forall (nub (extra ++ vars)) constraints ty
  EffectOpForall owner vars constraints ty -> EffectOpForall owner (nub (extra ++ vars)) constraints ty

typeAnnWithOwnClassConstraint :: String -> TypeAnn -> TcContext -> TypeAnn
typeAnnWithOwnClassConstraint className ann ctx = typeAnnWithInternalConstraints (ownClassConstraint className ctx) ann

typeAnnWithInternalConstraints :: [ClassConstraint] -> TypeAnn -> TypeAnn
typeAnnWithInternalConstraints internal (TypeAnn ty (headerRequirements -> constraints)) = TypeAnn ty (map Requirement (internal ++ constraints))

ownClassConstraint :: String -> TcContext -> [ClassConstraint]
ownClassConstraint className ctx = case findClassInfo className ctx of
  Just ClassInfo {classParams} -> [ClassConstraint (map TypeName classParams) className]
  Nothing -> []

findClassInfo :: String -> TcContext -> Maybe ClassInfo
findClassInfo className TcContext {tcClasses} = Map.lookup className tcClasses

findClassInfoByRef :: ClassRef -> TcContext -> Maybe ClassInfo
findClassInfoByRef ClassRef {classIdentity, classDisplayName} TcContext {tcClasses, tcClassRegistry} =
  case Map.lookup classDisplayName tcClasses of
    Just info | classTarget info == classIdentity -> Just info
    _ -> Map.lookup classIdentity tcClassRegistry

resolveClassTargetM :: String -> TcContext -> Tc SymbolId
resolveClassTargetM name ctx =
  maybe (failTc ("internal missing checked flock '" ++ name ++ "'")) (pure . classTarget) (findClassInfo name ctx)

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
bindPatternLinearM (PVar name) expected ctx bound = case lookupConstructorBinding name ctx of
  Left message -> failTc message
  Right Nothing -> bindPatternVariableM name expected ctx bound
  Right (Just binding) -> case constructorBindingArity binding of
    Just 0 -> instantiateConstructorPatternM binding >>= (`unifyPatternResultM` expected) >> pure (ctx, bound)
    Just _ -> failTc ("constructor pattern '" ++ name ++ "' needeþ arguments")
    Nothing -> failTc ("internal missing constructor arity for '" ++ name ++ "'")
bindPatternLinearM (PInteger _) expected ctx bound = unifyM (TyCon "ℤ") expected >> pure (ctx, bound)
bindPatternLinearM (PText _) expected ctx bound = unifyM (TyCon "text") expected >> pure (ctx, bound)
bindPatternLinearM (PCon name args) expected ctx bound = do
  (constructor, fieldPatterns, binding) <- constructorPatternM name args ctx
  conTy <- instantiateConstructorPatternM binding
  case normalizeFun conTy of
    TyFun fieldTypes _ result -> do
      unifyPatternResultM result expected
      bindPatternListM fieldPatterns (NE.toList fieldTypes) ctx bound
    _ -> case fieldPatterns of
      [] -> unifyPatternResultM conTy expected >> pure (ctx, bound)
      _ -> failTc ("constructor '" ++ constructor ++ "' is not a function")
bindPatternLinearM PConstructor {} _ _ _ = failTc "internal resolved constructor appeareþ before elaboration"

-- a GADT pattern may refine an annotation skolem inside this branch. ordinary
-- unification remaineth rigid; only constructor results introduce equalities.
unifyPatternResultM :: Ty -> Ty -> Tc ()
unifyPatternResultM actual expected = do
  st <- getTc
  case (normalizeFun (applyState actual st), normalizeFun (applyState expected st)) of
    (left@(TyMeta _), right) -> unifyM left right
    (left, right@(TyMeta _)) -> unifyM left right
    (TySkolem ident@UniversalSkolem {} [], ty) | TySkolem ident [] /= ty -> refine ident ty
    (ty, TySkolem ident@UniversalSkolem {} []) | TySkolem ident [] /= ty -> refine ident ty
    (applicationView -> Just (left, xs), applicationView -> Just (right, ys))
      | left == right && length xs == length ys -> zipWithM_ unifyPatternResultM xs ys
    (left, right) -> unifyM left right
  where
    refine ident ty = do
      st <- getTc
      let resolved = applyState ty st
      if ident `elem` [key | TySkolem key _ <- foldTyWith (:) (:) resolved []]
        then failTc "recursive GADT index"
        else putTc st {tcGadtRefinements = Map.insert ident resolved (tcGadtRefinements st)}

-- each branch keepeþ ordinary unification, but its index equalities must not
-- constrain a sibling branch.
withScopedGadtRefinements :: Tc a -> Tc a
withScopedGadtRefinements action = do
  before <- tcGadtRefinements <$> getTc
  value <- action
  st <- getTc
  putTc st {tcGadtRefinements = before}
  pure value

-- bracket headers accept both constructor-first and ordinary second-position
-- application. only a known constructor can choose the latter interpretation.
constructorPatternM :: String -> [Pattern] -> TcContext -> Tc (String, [Pattern], EnvBinding)
constructorPatternM name arguments ctx = do
  first <- either failTc pure (lookupConstructorBinding name ctx)
  case first of
    Just binding -> pure (name, arguments, binding)
    Nothing -> case arguments of
      PVar constructor : rest -> do
        second <- either failTc pure (lookupConstructorBinding constructor ctx)
        case second of
          Just binding -> pure (constructor, PVar name : rest, binding)
          Nothing -> failTc ("unknown constructor pattern '" ++ name ++ "'")
      _ -> failTc ("unknown constructor pattern '" ++ name ++ "'")

bindPatternListM :: [Pattern] -> [Ty] -> TcContext -> [String] -> Tc (TcContext, [String])
bindPatternListM patterns tys ctx bound
  | length patterns == length tys = foldM step (ctx, bound) (zip patterns tys)
  | otherwise = failTc "constructor pattern arity mismatch"
  where
    step (currentCtx, currentBound) (pat, ty) = bindPatternLinearM pat ty currentCtx currentBound

bindPatternVariableM :: String -> Ty -> TcContext -> [String] -> Tc (TcContext, [String])
bindPatternVariableM name expected ctx bound
  | name `elem` bound = failTc ("pattern bindeþ '" ++ name ++ "' more than once")
  | otherwise = pure (addEnv name (valueScheme expected) ctx, name : bound)

valueScheme :: Ty -> Scheme
valueScheme (TyForall parameters body) = Specified parameters (Forall (parameterNames parameters) [] body)
valueScheme ty = Forall [] [] ty

knownConstructorArity :: String -> TcContext -> Maybe Int
knownConstructorArity name ctx = either (const Nothing) (>>= constructorBindingArity) (lookupConstructorBinding name ctx)

lookupConstructorBinding :: String -> TcContext -> Either String (Maybe EnvBinding)
lookupConstructorBinding name TcContext {tcEnv}
  | isQualifiedName name = Right (find isConstructor candidates)
  | otherwise = case selectEnvChoices name (filter isConstructor candidates) of
      ChoicesMissing -> Right Nothing
      ChoicesAmbiguous message -> Left message
      EnvChoices [] -> Right Nothing
      EnvChoices (binding : _) -> Right (Just binding)
  where
    candidates = collectEnvCandidates name tcEnv
    isConstructor binding = isJust (constructorBindingArity binding)

constructorBindingArity :: EnvBinding -> Maybe Int
constructorBindingArity EnvBinding {envTarget = Just TermExport {termExportKind = ConstructorTerm arity}} = Just arity
constructorBindingArity _ = Nothing

instantiateConstructorPatternM :: EnvBinding -> Tc Ty
instantiateConstructorPatternM EnvBinding {envScheme} = do
  let (vars, _, ty) = schemeParts envScheme
      result = constructorResultType ty
      resultVars = typeVarsInTy result []
      headVars = typeHeadVarsInTy ty []
      rowVars = effectRowVarsInTy ty []
      (universal, existential) = partition (`elem` resultVars) vars
  universalBindings <- freshForNamesM headVars rowVars universal
  existentialBindings <- traverse (freshExistential rowVars) existential
  pure (replaceVars ty (Map.union universalBindings (Map.fromList existentialBindings)))
  where
    freshExistential rowVars variable = do
      ident <- freshIdM
      let ty = if variable `elem` rowVars then TyEffectSkolem ident else TySkolem (ExistentialSkolem ident) []
      pure (variable, ty)

constructorResultType :: Ty -> Ty
constructorResultType ty = case normalizeFun ty of
  TyFun _ _ result -> result
  result -> result

-- coverage followeþ the constructor-specialisation matrix algorithm and returneþ
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

resolvePatternM :: TcContext -> Pattern -> Tc Pattern
resolvePatternM ctx = \case
  PVar name
    | isJust (knownConstructorArity name ctx) -> PConstructor <$> constructorTargetM name ctx <*> pure []
    | otherwise -> pure (PVar name)
  PInteger value -> pure (PInteger value)
  PText value -> pure (PText value)
  PCon name arguments -> do
    (constructor, fields, _) <- constructorPatternM name arguments ctx
    PConstructor <$> constructorTargetM constructor ctx <*> traverse (resolvePatternM ctx) fields
  PConstructor {} -> failTc "internal resolved constructor appeareþ before elaboration"

constructorTargetM :: String -> TcContext -> Tc SymbolId
constructorTargetM name ctx = case lookupConstructorBinding name ctx of
  Left message -> failTc message
  Right (Just EnvBinding {envTarget = Just TermExport {termExportTarget}}) -> pure termExportTarget
  _ -> failTc ("internal missing constructor identity for '" ++ name ++ "'")

constructorsForType :: Ty -> TcContext -> TcState -> Maybe [(SymbolId, [Ty])]
constructorsForType ty TcContext {tcTypeRegistry} st = case normalizeFun (applyState ty st) of
  TyNamed TypeRef {typeIdentity} args -> case Map.lookup typeIdentity tcTypeRegistry of
    Just (DataInfo _ params ctors) -> Just (specializeCtorInfos params args ctors)
    _ -> Nothing
  _ -> Nothing

unshownTypeInfo :: TypeInfo -> TypeInfo
unshownTypeInfo = \case
  ShownType info -> unshownTypeInfo info
  info -> info

specializeCtorInfos :: [String] -> [Ty] -> [ConstructorInfo] -> [(SymbolId, [Ty])]
specializeCtorInfos _ args ctors =
  [ (constructorInfoTarget, map (replaceTyVarsForCoverage bindings) constructorInfoFields)
  | ConstructorInfo {constructorInfoTarget, constructorInfoFields, constructorInfoResult} <- ctors,
    TyNamed _ resultArgs <- [normalizeFun constructorInfoResult],
    length resultArgs == length args,
    and (zipWith gadtIndicesMayAgree resultArgs args),
    let bindings = fromMaybe Map.empty (zipWithExact typePatternBindings resultArgs args >>= mergeBindingMaps)
  ]

gadtIndicesMayAgree :: Ty -> Ty -> Bool
gadtIndicesMayAgree x y = case (normalizeFun x, normalizeFun y) of
  (TyVar _, _) -> True
  (_, TyVar _) -> True
  (TyMeta _, _) -> True
  (_, TyMeta _) -> True
  (TySkolem {}, _) -> True
  (_, TySkolem {}) -> True
  (TyApplication (VariableHead _) _, _) -> True
  (_, TyApplication (VariableHead _) _) -> True
  (TyCon left, TyCon right) -> left == right
  (applicationView -> Just (left, xs), applicationView -> Just (right, ys)) ->
    left == right && length xs == length ys && and (zipWith gadtIndicesMayAgree xs ys)
  _ -> False

replaceTyVarsForCoverage :: Map.Map String Ty -> Ty -> Ty
replaceTyVarsForCoverage repls ty = case ty of
  TyVar name -> Map.findWithDefault ty name repls
  _ -> mapTyChildren (replaceTyVarsForCoverage repls) (skipEffectVar (replaceTyVarsForCoverage repls)) ty

-- expression inference produceþ an elaborated expression skeleton alongside
-- its value type, immediate effects, and dictionary requirements.
inferExprM :: Expr -> TcContext -> Tc Inferred
inferExprM expr ctx = do
  inferred <- inferExprUncheckedM expr ctx
  checkInferredEffectRowsM inferred
  pure inferred

inferExprUncheckedM :: Expr -> TcContext -> Tc Inferred
inferExprUncheckedM expr ctx = case expr of
  ELocated span inner -> withTcSpan span do
    inferred <- inferExprM inner ctx
    pure inferred {inferredExpr = ELocated span (inferredExpr inferred)}
  EInteger _ -> pure (Inferred (TyCon "ℤ") [] [] expr)
  EFloat _ -> pure (Inferred (TyCon "float") [] [] expr)
  EUnicode _ -> pure (Inferred (TyCon "unicode") [] [] expr)
  EText _ -> pure (Inferred (TyCon "text") [] [] expr)
  EForeign {} -> failTc "fremmed is only allowed as the direct body of an annotated top-level let"
  EVar name -> inferVarM name ctx
  EGlobal {} -> failTc "internal resolved global appeareþ before elaboration"
  EEvidence {} -> failTc "internal evidence appeareþ before elaboration"
  EEvidenceLambda {} -> failTc "internal evidence abstraction appeareþ before elaboration"
  EAscribe expression annotation -> inferAscribedM expression annotation ctx
  EApply f args -> inferApplyM f (NE.toList args) ctx
  ETypeApply f arguments -> inferExplicitApplyM f (NE.toList arguments) [] ctx
  ERecord fields -> inferRecordFieldsM fields ctx
  EField base field -> inferRecordFieldAccessM base field ctx
  EUpdate base updates -> inferRecordUpdateM base updates ctx
  ETry body returnCase cases -> inferTryHandleM body returnCase cases ctx
  EMatch scrutinees cases -> inferMatchM scrutinees cases ctx
  EBlock ds body -> inferBlockM ds body ctx
  EWithEvidence {} -> failTc "internal evidence appeareþ before elaboration"
  EDictionary {} -> failTc "internal dictionary appeareþ before elaboration"

-- effect rows denote sets: occurrences of the same effect must agree on their
-- type arguments, even when inference hath not equated the row with another.
checkInferredEffectRowsM :: Inferred -> Tc ()
checkInferredEffectRowsM Inferred {inferredTy, inferredEffects, inferredConstraints} = do
  checkEffectRowArgumentsM inferredEffects
  checkTypeEffectRowsM inferredTy
  mapM_ (\(Constraint args _) -> mapM_ checkTypeEffectRowsM args) inferredConstraints

checkTypeEffectRowsM :: Ty -> Tc ()
checkTypeEffectRowsM ty = do
  st <- getTc
  case applyState ty st of
    TyApplication _ args -> mapM_ checkTypeEffectRowsM args
    TyEffect _ args -> mapM_ checkTypeEffectRowsM args
    TyRecord fields -> mapM_ checkTypeEffectRowsM (Map.elems fields)
    TyArrow arg effects ret -> do
      checkTypeEffectRowsM arg
      checkEffectRowArgumentsM effects
      mapM_ (checkTypeEffectRowsM . effectAsTy) effects
      checkTypeEffectRowsM ret
    TyForall _ body -> checkTypeEffectRowsM body
    _ -> pure ()

checkEffectRowArgumentsM :: EffectRow -> Tc ()
checkEffectRowArgumentsM effects = do
  st <- getTc
  let concrete = concreteEffects (applyStateEffects effects st)
  when (any (\case EffectLabel (UnresolvedEffect _) _ -> True; _ -> False) concrete) (failTc "an effect row requireþ a declared effect or a row variable")
  unifyCommonEffectsM concrete concrete

inferAscribedM :: Expr -> TypeExpr -> TcContext -> Tc Inferred
inferAscribedM expression annotation ctx = do
  checkTypeExprNamesM "type ascription" annotation ctx
  (expected, _) <- instantiateM (generalizeTy (convertTypeExpr annotation ctx))
  if containsForall expected
    then do
      (ty, effects, constraints, core) <- checkExprAgainstM "type ascription" expected [] [] expression ctx
      pure (Inferred ty effects constraints (EAscribe core annotation))
    else do
      Inferred actual effects constraints core <- inferExprM expression ctx
      instantiated <- instantiateTypeM actual
      mapTcError
        (unifyM instantiated expected)
        (\msg -> "in type ascription: " ++ msg ++ "; actual " ++ showTy actual ++ ", expected " ++ showTy expected)
      st <- getTc
      pure (Inferred (applyState expected st) (applyStateEffects effects st) (applyStateConstraints constraints st) (EAscribe core annotation))

inferRecordFieldsM :: [(String, Expr)] -> TcContext -> Tc Inferred
inferRecordFieldsM fields ctx = do
  inferred <- traverse (traverse (`inferExprM` ctx)) fields
  let recordTy = Map.fromList [(name, inferredTy field) | (name, field) <- inferred]
      effects = foldl' unionEffects [] (map (inferredEffects . snd) inferred)
      constraints = foldl' unionConstraints [] (map (inferredConstraints . snd) inferred)
      fields2 = [(name, inferredExpr field) | (name, field) <- inferred]
  pure (Inferred (TyRecord recordTy) effects constraints (ERecord fields2))

inferRecordUpdateM :: Expr -> [RecordUpdate] -> TcContext -> Tc Inferred
inferRecordUpdateM base updates ctx = do
  Inferred baseTy baseEffects baseConstraints base2 <- inferExprM base ctx
  st <- getTc
  case normalizeFun (applyState baseTy st) of
    TyRecord fields -> inferRecordUpdatesM base2 updates fields ctx baseEffects baseConstraints
    other -> failTc ("record update expected record, found " ++ showTy other)

inferRecordUpdatesM :: Expr -> [RecordUpdate] -> Map.Map String Ty -> TcContext -> EffectRow -> [Constraint] -> Tc Inferred
inferRecordUpdatesM base updates fields ctx effects0 constraints0 = do
  ((updatedFields, effects, constraints), updates2) <- mapAccumM step (fields, effects0, constraints0) updates
  pure (Inferred (TyRecord updatedFields) effects constraints (EUpdate base updates2))
  where
    step (currentFields, effects, constraints) = \case
      RecordRemove name ->
        if Map.member name currentFields
          then pure ((Map.delete name currentFields, effects, constraints), RecordRemove name)
          else failTc ("unknown record field '" ++ name ++ "'")
      RecordSet name expr -> do
        Inferred fieldTy fieldEffects fieldConstraints expr2 <- inferExprM expr ctx
        pure
          ( (Map.insert name fieldTy currentFields, unionEffects effects fieldEffects, unionConstraints constraints fieldConstraints),
            RecordSet name expr2
          )

inferBlockM :: [Decl] -> Expr -> TcContext -> Tc Inferred
inferBlockM ds body ctx = do
  (ds2, ctx2, declEffects) <- inferLocalDeclsM ds ctx
  Inferred bodyTy bodyEffects bodyConstraints body2 <- inferExprM body ctx2
  pure (Inferred bodyTy (unionEffects declEffects bodyEffects) bodyConstraints (EBlock ds2 body2))

inferVarM :: String -> TcContext -> Tc Inferred
inferVarM name ctx = case lookupEnvBinding name ctx of
  Right Nothing -> failTc ("unknown name '" ++ name ++ "'")
  Left message -> failTc message
  Right (Just binding) -> inferBindingM binding

inferBindingM :: EnvBinding -> Tc Inferred
inferBindingM = inferBindingWithTypesM []

inferBindingWithTypesM :: [TypeArgument] -> EnvBinding -> Tc Inferred
inferBindingWithTypesM typeArguments binding = do
  (schemeTy, constraints) <- instantiateWithTypesM typeArguments (exposeSchemeQuantifiers (envScheme binding))
  ty <- instantiateTypeM schemeTy
  holes <- traverse freshEvidenceHoleM constraints
  when (isNothing (envTarget binding) && envName binding /= envLocalName binding) do
    st <- getTc
    putTc st {tcRecursiveUses = Set.insert (envLocalName binding) (tcRecursiveUses st)}
  let variable = maybe (EVar (envLocalName binding)) EGlobal (envTarget binding)
      core = if null holes then variable else EWithEvidence variable holes
  pure (Inferred ty [] constraints core)

freshEvidenceHoleM :: Constraint -> Tc Int
freshEvidenceHoleM constraint = do
  st@TcState {tcEvidenceConstraints} <- getTc
  let ident = maybe 0 ((+ 1) . fst) (IntMap.lookupMax tcEvidenceConstraints)
  putTc st {tcEvidenceConstraints = IntMap.insert ident constraint tcEvidenceConstraints}
  pure ident

inferRecordFieldAccessM :: Expr -> String -> TcContext -> Tc Inferred
inferRecordFieldAccessM baseExpr fieldName ctx = do
  Inferred baseTy effects constraints base <- inferExprM baseExpr ctx
  st <- getTc
  case normalizeFun (applyState baseTy st) of
    TyRecord fields -> case Map.lookup fieldName fields of
      Just fieldTy -> pure (Inferred fieldTy effects constraints (EField base fieldName))
      Nothing -> failTc ("unknown record field '" ++ fieldName ++ "'")
    other -> failTc ("record field access expected record, found " ++ showTy other)

inferApplyM :: Expr -> [Expr] -> TcContext -> Tc Inferred
inferApplyM f args ctx = case flatApply f args of
  (ETypeApply function types, allArgs) -> inferExplicitApplyM function (NE.toList types) allArgs ctx
  (EVar name, allArgs) -> inferNamedApplyM name allArgs ctx
  (other, allArgs) -> inferExprM other ctx >>= \inferred -> inferCurriedApplyM inferred allArgs ctx

inferExplicitApplyM :: Expr -> [TypeExpr] -> [Expr] -> TcContext -> Tc Inferred
inferExplicitApplyM function annotations arguments ctx = do
  mapM_ (\annotation -> checkTypeExprNamesM "type argument" annotation ctx) annotations
  kinds <- either failTc pure (Kind.inferKinds (tcKinds ctx) annotations)
  let types = zip (map (`convertTypeExpr` ctx) annotations) kinds
  case unlocated function of
    EVar name -> inferNamedApplyWithTypesM types name arguments ctx
    other -> do
      Inferred ty effects constraints core <- inferExprM other ctx
      st <- getTc
      case applyState ty st of
        TyForall parameters body -> do
          (instantiated, _) <- instantiateWithTypesM types (Specified parameters (Forall (parameterNames parameters) [] body))
          inferCurriedApplyM (Inferred instantiated effects constraints core) arguments ctx
        _ -> failTc "explicit type application requireþ a polymorphic value"
  where
    unlocated (ELocated _ body) = unlocated body
    unlocated body = body

flatApply :: Expr -> [Expr] -> (Expr, [Expr])
flatApply (ELocated _ inner) args = flatApply inner args
flatApply (EApply inner innerArgs) args =
  let (base, baseArgs) = flatApply inner (NE.toList innerArgs)
   in (base, baseArgs ++ args)
flatApply f args = (f, args)

inferNamedApplyM :: String -> [Expr] -> TcContext -> Tc Inferred
inferNamedApplyM = inferNamedApplyWithTypesM []

inferNamedApplyWithTypesM :: [TypeArgument] -> String -> [Expr] -> TcContext -> Tc Inferred
inferNamedApplyWithTypesM types name args ctx = case lookupEnvChoices name ctx of
  ChoicesMissing -> failTc ("unknown name '" ++ name ++ "'")
  ChoicesAmbiguous msg -> failTc msg
  EnvChoices bindings -> tryApplyBindingsM types name args bindings ctx Nothing

tryApplyBindingsM :: [TypeArgument] -> String -> [Expr] -> [EnvBinding] -> TcContext -> Maybe String -> Tc Inferred
tryApplyBindingsM types name args bindings ctx = foldr tryBinding noMore bindings
  where
    noMore err = failTc (fromMaybe ("unknown name '" ++ name ++ "'") err)
    tryBinding binding fallback err =
      recoverTc
        (inferBindingWithTypesM types binding >>= \inferred -> inferCurriedApplyM inferred args ctx)
        (fallback . keepFirstError err)

keepFirstError :: Maybe String -> String -> Maybe String
keepFirstError Nothing msg = Just msg
keepFirstError some@(Just _) _ = some

inferCurriedApplyM :: Inferred -> [Expr] -> TcContext -> Tc Inferred
inferCurriedApplyM fn args ctx = foldM step fn args
  where
    step (Inferred fTy fEffs fConstraints f) arg = do
      st0 <- getTc
      functionTy <- instantiateTypeM (normalizeFun (applyState fTy st0))
      (retTy, latentEffects, Inferred _ argEffs argConstraints arg2) <- case functionTy of
        TyArrow param latent ret -> do
          argument <- checkArgumentM param arg ctx
          pure (ret, latent, argument)
        _ -> do
          argument <- inferExprM arg ctx
          ret <- freshM
          latent <- unifyApplyFunctionM functionTy (inferredTy argument) ret
          pure (ret, latent, argument)
      let effects = unionEffects fEffs (unionEffects argEffs latentEffects)
          constraints = unionConstraints fConstraints argConstraints
      st <- getTc
      pure (Inferred (applyState retTy st) effects (applyStateConstraints constraints st) (EApply f (arg2 :| [])))

checkArgumentM :: Ty -> Expr -> TcContext -> Tc Inferred
checkArgumentM expected expression ctx = do
  st <- getTc
  if containsForall (applyState expected st)
    then do
      (ty, effects, constraints, core) <- checkExprAgainstM "polymorphic argument" expected [] [] expression ctx
      pure (Inferred ty effects constraints core)
    else do
      inferred <- inferExprM expression ctx
      actual <- instantiateTypeM (inferredTy inferred)
      unifyM actual expected
      pure inferred

containsForall :: Ty -> Bool
containsForall ty = foldTyWith collect collect ty False
  where
    collect TyForall {} _ = True
    collect _ found = found

unifyApplyFunctionM :: Ty -> Ty -> Ty -> Tc EffectRow
unifyApplyFunctionM fTy argTy retTy = do
  st <- getTc
  case normalizeFun (applyState fTy st) of
    TyArrow param effects ret -> unifyM param argTy >> unifyM ret retTy >> pure effects
    _ -> unifyM fTy (TyArrow argTy [] retTy) >> pure []

inferExprListM :: [Expr] -> TcContext -> Tc [Inferred]
inferExprListM exprs ctx = traverse (`inferExprM` ctx) exprs

inferTryHandleM :: Expr -> Maybe ReturnCase -> [HandlerCase] -> TcContext -> Tc Inferred
inferTryHandleM body returnCase cases ctx = do
  Inferred bodyTy bodyEffects bodyConstraints body2 <- inferExprM body ctx
  st <- getTc
  Inferred answerTy returnEffects returnConstraints returnBody <- inferReturnCaseM returnCase (applyState bodyTy st) ctx
  let returnCase2 = case returnCase of
        Nothing -> Nothing
        Just (ReturnCase pat _) -> Just (ReturnCase pat returnBody)
  inferHandlerCasesM body2 returnCase2 cases answerTy bodyEffects (unionConstraints bodyConstraints returnConstraints) returnEffects ctx

inferReturnCaseM :: Maybe ReturnCase -> Ty -> TcContext -> Tc Inferred
inferReturnCaseM Nothing bodyTy _ = pure (Inferred bodyTy [] [] (EVar "$return"))
inferReturnCaseM (Just (ReturnCase pat body)) bodyTy ctx = do
  ensureIrrefutablePatternM "handler yield" pat ctx
  ctx2 <- bindPatternM pat bodyTy ctx
  inferExprM body ctx2

ensureIrrefutablePatternM :: String -> Pattern -> TcContext -> Tc ()
ensureIrrefutablePatternM owner pat ctx = case pat of
  PVar "_" -> pure ()
  PVar name
    | isJust (knownConstructorArity name ctx) -> refutable
    | otherwise -> pure ()
  PInteger {} -> refutable
  PText {} -> refutable
  PCon {} -> refutable
  PConstructor {} -> refutable
  where
    refutable = failTc (owner ++ " pattern must be a binder or '_'")

data HandlerCoverage
  = WholeEffect EffectRef
  | OneOperation EffectRef SymbolId
  deriving (Eq, Show)

-- these effects are executed directly by the standard runner. a source handler
-- cannot intercept them, so it must not erase them from the static row.
rejectRunnerEffectHandlerM :: HandlerCoverage -> Tc ()
rejectRunnerEffectHandlerM coverage = case effectIdentity effect of
  SymbolId RuntimeModule name
    | name `elem` runnerEffectNames ->
        failTc ("runner effect '" ++ effectDisplayName effect ++ "' cannot have a source handler")
  _ -> pure ()
  where
    effect = case coverage of
      WholeEffect owner -> owner
      OneOperation owner _ -> owner

inferHandlerCasesM :: Expr -> Maybe ReturnCase -> [HandlerCase] -> Ty -> EffectRow -> [Constraint] -> EffectRow -> TcContext -> Tc Inferred
inferHandlerCasesM body returnCase cases answerTy effects bodyConstraints returnEffects ctx = do
  coverage <- traverse (handlerCoverageM effects ctx) cases
  mapM_ rejectRunnerEffectHandlerM coverage
  let fullyHandled = completeHandledEffects coverage ctx
  ((finalAnswerTy, accumulatedEffects, accumulatedConstraints), cases2) <- mapAccumM (step fullyHandled) (answerTy, returnEffects, bodyConstraints) (zip coverage cases)
  st <- getTc
  let remainingEffects = removeEffects fullyHandled (applyStateEffects effects st)
  pure (Inferred (applyState finalAnswerTy st) (unionEffects remainingEffects accumulatedEffects) (applyStateConstraints accumulatedConstraints st) (ETry body returnCase cases2))
  where
    step fullyHandled (currentAnswerTy, accumulatedEffects, accumulatedConstraints) (target, HandlerCase name patterns handlerBody) = do
      handlerCtx <- handlerCaseContextM fullyHandled target name patterns currentAnswerTy effects ctx
      Inferred handlerTy caseEffects caseConstraints handlerBody2 <- inferExprM handlerBody handlerCtx
      mapTcError (unifyM currentAnswerTy handlerTy) (\msg -> "in handler for '" ++ name ++ "': " ++ msg)
      st <- getTc
      pure
        ( ( applyState currentAnswerTy st,
            unionEffects accumulatedEffects (applyStateEffects caseEffects st),
            unionConstraints accumulatedConstraints (applyStateConstraints caseConstraints st)
          ),
          ResolvedHandlerCase (handlerTarget target) patterns handlerBody2
        )
    step _ _ (_, ResolvedHandlerCase {}) = failTc "internal resolved handler appeareþ before elaboration"

    handlerTarget (WholeEffect EffectRef {effectIdentity}) = EffectTarget effectIdentity
    handlerTarget (OneOperation _ target) = OperationTarget target

handlerCoverageM :: EffectRow -> TcContext -> HandlerCase -> Tc HandlerCoverage
handlerCoverageM effects ctx (HandlerCase name patterns _) = do
  actualEffects <- applyStateEffects effects <$> getTc
  let whole = do
        effect <- effectRefForName name ctx
        _ <- find (effectHasRef effect) actualEffects
        pure (WholeEffect effect)
  case lookupEnvBinding name ctx of
    Left message -> failTc message
    Right Nothing -> maybe (failTc ("handler for absent effect '" ++ name ++ "'")) pure whole
    Right (Just binding) -> case schemeEffectOwner (envScheme binding) of
      Just owner
        | null patterns,
          Just wholeEffect@(WholeEffect effect) <- whole,
          owner == effect ->
            pure wholeEffect
        | isJust (find (effectHasRef owner) actualEffects),
          Just TermExport {termExportTarget} <- envTarget binding ->
            pure (OneOperation owner termExportTarget)
      _ -> maybe (failTc ("handler case '" ++ name ++ "' is not an effect operation")) pure whole
handlerCoverageM _ _ ResolvedHandlerCase {} = failTc "internal resolved handler appeareþ before elaboration"

completeHandledEffects :: [HandlerCoverage] -> TcContext -> [EffectRef]
completeHandledEffects coverage ctx =
  [ effect
  | effect <- nub [owner | item <- coverage, owner <- coverageEffect item],
    any (wholeEffectMatches effect) coverage
      || maybe False (all (`elem` handledOperations effect)) (effectOperationTargets effect ctx)
  ]
  where
    coverageEffect = \case
      WholeEffect effect -> [effect]
      OneOperation effect _ -> [effect]
    wholeEffectMatches effect (WholeEffect actual) = actual == effect
    wholeEffectMatches _ _ = False
    handledOperations effect = [operation | OneOperation owner operation <- coverage, owner == effect]

effectOperationTargets :: EffectRef -> TcContext -> Maybe [SymbolId]
effectOperationTargets EffectRef {effectIdentity} TcContext {tcTypes} =
  listToMaybe
    [ operations
    | info <- Map.elems tcTypes,
      EffectInfo EffectRef {effectIdentity = declaredIdentity} _ operations <- [unshownTypeInfo info],
      declaredIdentity == effectIdentity
    ]

handlerCaseContextM :: [EffectRef] -> HandlerCoverage -> String -> [Pattern] -> Ty -> EffectRow -> TcContext -> Tc TcContext
handlerCaseContextM fullyHandled target name patterns answerTy effects ctx = case target of
  WholeEffect {} -> effectHandlerCaseContextM name patterns effects ctx
  OneOperation {} -> case lookupEnv name ctx of
    EnvFound scheme -> operationHandlerCaseContextM fullyHandled name patterns answerTy effects scheme ctx
    EnvMissing -> failTc ("handler case '" ++ name ++ "' is not an effect operation")
    EnvAmbiguous msg -> failTc msg

effectHandlerCaseContextM :: String -> [Pattern] -> EffectRow -> TcContext -> Tc TcContext
effectHandlerCaseContextM effectName patterns effects ctx = case patterns of
  [] -> do
    st <- getTc
    if maybe False (\effect -> any (effectHasRef effect) (applyStateEffects effects st)) (effectRefForName effectName ctx)
      then pure ctx
      else failTc ("handler for absent effect '" ++ effectName ++ "'")
  _ -> failTc ("effect-level handler '" ++ effectName ++ "' cannot bind operation arguments")

operationHandlerCaseContextM :: [EffectRef] -> String -> [Pattern] -> Ty -> EffectRow -> Scheme -> TcContext -> Tc TcContext
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
      mapM_ (\pat -> ensureIrrefutablePatternM ("handler for '" ++ name ++ "'") pat ctx) patterns
      ctx2 <- mapTcError (bindHandlerPatternsM patterns argTys ctx) (\msg -> "in handler for '" ++ name ++ "': " ++ msg)
      st <- getTc
      let resumeEffects = removeEffects fullyHandled (applyStateEffects effects st)
          resumeTy = curriedFunction [applyState retTy st] resumeEffects (applyState answerTy st)
      pure (addEnv "eftgin" (Forall [] [] resumeTy) ctx2)

bindHandlerPatternsM :: [Pattern] -> [Ty] -> TcContext -> Tc TcContext
bindHandlerPatternsM [] _ ctx = pure ctx
bindHandlerPatternsM patterns argTys ctx = bindPatternsM patterns argTys ctx

findHandledOperationEffectM :: String -> EffectRef -> EffectRow -> EffectRow -> Tc Effect
findHandledOperationEffectM name ownerEffect opEffects effects = do
  ownEffect <- maybe notFound pure (find (effectHasRef ownerEffect) opEffects)
  st <- getTc
  let actualEffects = applyStateEffects effects st
      hasOpenRow = any (\case EffectVariable {} -> True; EffectLabel RigidEffect {} _ -> True; _ -> False) actualEffects
      parameterized = maybe False (not . null . snd) (effectHeadAndArgs ownEffect)
  when (parameterized && hasOpenRow) $
    failTc ("cannot handle parameterized effect '" ++ effectDisplayName ownerEffect ++ "' across an open effect row")
  case findEffectByHead ownEffect actualEffects of
    Nothing -> failTc ("handler for absent effect '" ++ effectDisplayName ownerEffect ++ "' in operation '" ++ name ++ "'")
    Just actualEffect -> unifyEffectM ownEffect actualEffect >> ((\st -> mapEffectTypes (`applyState` st) actualEffect) <$> getTc)
  where
    notFound = failTc ("handler case '" ++ name ++ "' is not an effect operation")

removeEffects :: [EffectRef] -> EffectRow -> EffectRow
removeEffects handled effects = foldl' (flip removeEffect) effects handled

inferMatchM :: [Expr] -> [MatchCase] -> TcContext -> Tc Inferred
inferMatchM [] cases ctx = do
  arity <- maybe (failTc "empty anonymous match") pure (matchCaseArity cases)
  scrutTys <- replicateM arity freshM
  (branchTy, branchEffects, branchConstraints, cases2) <- inferMatchCasesM cases scrutTys ctx
  st <- getTc
  pure (Inferred (curriedFunction (applyStateAll scrutTys st) branchEffects branchTy) [] (applyStateConstraints branchConstraints st) (EMatch [] cases2))
inferMatchM scrutinees cases ctx = do
  inferred <- inferExprListM scrutinees ctx
  let scrutTys = [t | Inferred t _ _ _ <- inferred]
      scrutEffects = foldl' unionEffects [] [effects | Inferred _ effects _ _ <- inferred]
      scrutConstraints = foldl' unionConstraints [] [constraints | Inferred _ _ constraints _ <- inferred]
      scrutinees2 = map inferredExpr inferred
  (branchTy, branchEffects, branchConstraints, cases2) <- inferMatchCasesM cases scrutTys ctx
  st <- getTc
  pure (Inferred branchTy (unionEffects (applyStateEffects scrutEffects st) branchEffects) (unionConstraints (applyStateConstraints scrutConstraints st) branchConstraints) (EMatch scrutinees2 cases2))

inferMatchCasesM :: [MatchCase] -> [Ty] -> TcContext -> Tc (Ty, EffectRow, [Constraint], [MatchCase])
inferMatchCasesM cases scrutTys ctx = do
  ((branchTy, _, effects, constraints), cases2) <- mapAccumM step (Nothing, scrutTys, [], []) cases
  st <- getTc
  checkExhaustiveM (applyStateAll scrutTys st) cases2 ctx
  t <- maybe freshM pure branchTy
  pure (applyState t st, effects, applyStateConstraints constraints st, cases2)
  where
    step (branchTy, currentScrutTys, effects, constraints) (MatchCase ps e) = do
      (t, caseEffects, caseConstraints, ps2, e2) <- withScopedGadtRefinements do
        scopeStart <- tcNextMeta <$> getTc
        ctx2 <- bindPatternsM (NE.toList ps) currentScrutTys ctx
        Inferred t caseEffects caseConstraints e2 <- inferExprM e ctx2
        ps2 <- traverse (resolvePatternM ctx) ps
        mapM_ (`unifyM` t) branchTy
        st <- getTc
        rejectEscapingExistentials scopeStart (applyState t st)
        pure (applyState t st, applyStateEffects caseEffects st, applyStateConstraints caseConstraints st, ps2, e2)
      st <- getTc
      pure
        ( ( Just (applyState t st),
            applyStateAll currentScrutTys st,
            unionEffects effects caseEffects,
            unionConstraints constraints caseConstraints
          ),
          MatchCase ps2 e2
        )

-- global declarations are checked and elaborated in one sequential pass.
data DeclarationScope = GlobalDeclarations | LocalDeclarations deriving (Eq)

checkAndElaborateDeclsM :: Bool -> [Decl] -> TcContext -> Tc ([Decl], TcContext)
checkAndElaborateDeclsM allowImmediate declarations ctx = do
  (ctx2, elaborated) <- mapAccumM step ctx declarations
  pure (elaborated, ctx2)
  where
    step current declaration = do
      (declaration2, current2, effects) <- elaborateSequentialDeclM GlobalDeclarations declaration current
      unless allowImmediate $
        maybe (pure ()) (\name -> requirePureImmediateEffectsM ("let '" ++ name ++ "'") effects) (declaredLetName declaration)
      pure (current2, declaration2)

declaredLetName :: Decl -> Maybe String
declaredLetName = \case
  Let name _ _ -> Just name
  Export declaration -> declaredLetName declaration
  _ -> Nothing

elaborateSequentialDeclM :: DeclarationScope -> Decl -> TcContext -> Tc (Decl, TcContext, EffectRow)
elaborateSequentialDeclM scope (Export nested) ctx = do
  (nested2, ctx2, effects) <- elaborateSequentialDeclM scope nested ctx
  pure (Export nested2, exportDeclContext nested ctx2, effects)
elaborateSequentialDeclM LocalDeclarations (Let name _ EForeign {}) _ =
  failTc ("fremmed let '" ++ name ++ "' is only allowed at file level")
elaborateSequentialDeclM GlobalDeclarations (Let name Nothing EForeign {}) _ =
  failTc ("fremmed let '" ++ name ++ "' requireþ a type annotation")
elaborateSequentialDeclM GlobalDeclarations declaration@(Let name (Just ann) (EForeign hostKey)) ctx = do
  checkForeignLetM name hostKey ann ctx
  pure (declaration, addElaboratedBinding GlobalDeclarations name (typeAnnScheme ann ctx) ctx, [])
elaborateSequentialDeclM scope (Let name annotation expr) ctx = do
  case annotation of
    Just ann -> do
      checkTypeAnnConstraintsM ("let '" ++ name ++ "'") ann ctx
      let ctx2 = addElaboratedBinding scope name (typeAnnScheme ann ctx) ctx
          checkingCtx = if isAnonymousMatchExpr expr then ctx2 else ctx
      (_, effects, checkedConstraints, core) <- checkAnnotatedExprM name ann expr checkingCtx
      expr2 <- resolveInferredBindingM ctx2 checkedConstraints core
      pure (Let name annotation expr2, ctx2, effects)
    Nothing | isAnonymousMatchExpr expr -> do
      (alias, Inferred ty effects constraints core) <- inferRecursiveBindingM name expr ctx
      normalizedConstraints <- normalizeConstraintsM ctx constraints
      st <- getTc
      let scheme = bindingScheme ctx ty normalizedConstraints effects st
          inferredCtx = addElaboratedBinding scope name scheme ctx
          target = case scope of
            GlobalDeclarations -> EGlobal (TermExport (SymbolId (tcModule ctx) name) OrdinaryTerm)
            LocalDeclarations -> EVar name
          recursiveCore = if Set.member alias (tcRecursiveUses st) then bindRecursiveAlias alias target normalizedConstraints core else core
      expr2 <- resolveInferredBindingM inferredCtx normalizedConstraints recursiveCore
      pure (Let name annotation expr2, inferredCtx, effects)
    Nothing -> do
      Inferred ty effects constraints core <- inferNamedM name expr ctx
      normalizedConstraints <- normalizeConstraintsM ctx constraints
      st <- getTc
      let scheme = bindingScheme ctx ty normalizedConstraints effects st
          inferredCtx = addElaboratedBinding scope name scheme ctx
      expr2 <- resolveInferredBindingM inferredCtx normalizedConstraints core
      pure (Let name annotation expr2, inferredCtx, effects)
elaborateSequentialDeclM GlobalDeclarations (ReExport name) ctx =
  (ReExport name,,[]) <$> either failTc pure (reExportName name ctx)
elaborateSequentialDeclM GlobalDeclarations (ReExportType name) ctx =
  (ReExportType name,,[]) <$> either failTc pure (reExportTypeName name ctx)
elaborateSequentialDeclM GlobalDeclarations declaration ctx = do
  mapTcError (checkDeclM declaration ctx) (\message -> "while checking " ++ declLabel declaration ++ ": " ++ message)
  (,ctx,[]) <$> elaborateDeclM declaration ctx
elaborateSequentialDeclM LocalDeclarations declaration ctx = (,ctx,[]) <$> elaborateDeclM declaration ctx

addElaboratedBinding :: DeclarationScope -> String -> Scheme -> TcContext -> TcContext
addElaboratedBinding GlobalDeclarations name scheme = addGlobalEnv name name scheme OrdinaryTerm
addElaboratedBinding LocalDeclarations name scheme = addEnv name scheme

elaborateDeclM :: Decl -> TcContext -> Tc Decl
elaborateDeclM declaration ctx = case declaration of
  Let _ (Just _) EForeign {} -> pure declaration
  EffectDecl (parameterNames -> parameters) effectName operations -> do
    effect <- maybe (failTc ("internal missing checked effect '" ++ effectName ++ "'")) pure (effectRefForName effectName ctx)
    let target = effectIdentity effect
        resolvedOperationType ty = typeExprFromAppliedTy (normalizeFun (let (_, _, body) = schemeParts (typeAnnSchemeWith parameters ctx (TypeAnn ty [])) in body))
        resolveOperation (EffectOp name operationType) =
          ( TermExport
              (effectOperationId target name)
              (EffectOperationTerm target (effectOperationArity parameters ctx operationType)),
            EffectOp name (resolvedOperationType operationType)
          )
    pure (ElaboratedEffect target (map resolveOperation operations))
  ElaboratedEffect {} -> pure declaration
  ClassDecl (typeHeaderParts -> (_, constraints)) className members -> do
    target <- resolveClassTargetM className ctx
    parents <- traverse (\(ClassConstraint _ name) -> resolveClassTargetM name ctx) constraints
    members2 <- catMaybes <$> traverse (elaborateClassMemberEntryM target className ctx) members
    pure (ElaboratedClass target parents members2)
  ElaboratedClass {} -> pure declaration
  InstanceDecl entries types className members -> do
    let parameters = headerParameters entries
        constraints = headerRequirements entries
        scoped = scopeTypeVariables (Map.fromList [(name, TyVar name) | name <- parameterNames parameters]) (scopeParameterKinds parameters ctx)
    members2 <- elaborateInstanceMembersM types className constraints members scoped
    let key = instanceKeyFor (tcModule ctx) (canonicalClassName className ctx) (map (`convertTypeExpr` scoped) types)
    class_ <- resolveClassTargetM className ctx
    pure (ElaboratedInstance key class_ types className constraints members2)
  ElaboratedInstance {} -> pure declaration
  other -> pure other

resolveInferredBindingM :: TcContext -> [Constraint] -> Expr -> Tc Expr
resolveInferredBindingM ctx constraints core =
  let bindings = evidenceBindings constraints
   in wrapEvidence bindings <$> resolveExprEvidenceM ctx bindings core

-- laws have no runtime term; executable class members retain their nominal
-- selector identities for Core.
elaborateClassMemberEntryM :: SymbolId -> String -> TcContext -> ClassMember -> Tc (Maybe (TermExport, ClassMember))
elaborateClassMemberEntryM _ _ _ ClassLaw {} = pure Nothing
elaborateClassMemberEntryM class_ className ctx member@(ClassSignature name _) = do
  let qualifiedName = className ++ "@" ++ name
  target <- case listToMaybe [found | EnvBinding {envName, envTarget = Just found} <- tcEnv ctx, envName == qualifiedName, termExportKind found == ClassMemberTerm class_] of
    Just found -> pure found
    Nothing -> failTc ("internal missing flock member '" ++ qualifiedName ++ "' during elaboration")
  pure (Just (target, member))

elaborateInstanceMembersM :: [TypeExpr] -> String -> [ClassConstraint] -> [Decl] -> TcContext -> Tc [Decl]
elaborateInstanceMembersM types className instanceConstraints members ctx = case instanceSpecsForClass className types ctx [] of
  Nothing -> failTc ("internal missing bizen instance '" ++ className ++ "' during elaboration")
  Just specs -> traverse (elaborateMember specs) members
  where
    -- inherited members receive the dictionary for the declared instance. member
    -- names themselves are not recursive term bindings.
    internalConstraints = instanceMemberInternalConstraints types className instanceConstraints
    elaborateMember specs (Let name annotation expr) = do
      InstanceSpec {instanceSpecType} <- requireInstanceSpecM name specs
      let memberAnn = resolvedTypeAnnWithInternalConstraints ctx internalConstraints instanceSpecType
      (variables, expected, constraints) <- instantiateResolvedAnnotationM memberAnn
      (_, _, checkedConstraints, core) <- checkInstanceExprM name annotation expected constraints expr (scopeInstanceVariables variables ctx)
      let bindings = evidenceBindings checkedConstraints
      resolved <- resolveExprEvidenceM ctx bindings core
      pure (Let name annotation (wrapExternalEvidence internalConstraints bindings resolved))
    elaborateMember _ _ = failTc "bizen member must be a let"

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
    (mainTy, constraints) <- instantiateM scheme
    st <- getTc
    let actual = normalizeFun (applyState mainTy st)
        unitTy = hostTy (HostCon (fromMaybe "𝟙" (canonicalHostTypeName "𝟙")))
    case actual of
      TyFun arguments effects result
        | [argument] <- NE.toList arguments -> do
            recoverTc
              (unifyM argument unitTy >> unifyM result unitTy)
              (\_ -> invalidMainType actual)
            checkRunnableEffectsM effects
            requireNoConstraintsM ctx constraints
      _ -> invalidMainType actual

invalidMainType :: Ty -> Tc a
invalidMainType actual =
  failTc
    ( "main must have type [𝟙, 𝟙], optionally with console, random, async, file, system, clock, process, or web effects; found "
        ++ showTy actual
    )

declLabel :: Decl -> String
declLabel = \case
  Import path _ -> "use '" ++ path ++ "'"
  Let name _ _ -> "let '" ++ name ++ "'"
  TypeAlias _ name _ -> "type alias '" ++ name ++ "'"
  DataDecl _ name _ -> "ilk '" ++ name ++ "'"
  EffectDecl _ name _ -> "deed '" ++ name ++ "'"
  ElaboratedEffect target _ -> "deed '" ++ symbolName target ++ "'"
  ClassDecl _ name _ -> "flock '" ++ name ++ "'"
  ElaboratedClass target _ _ -> "flock '" ++ symbolName target ++ "'"
  InstanceDecl _ tyArgs className _ -> "bizen '" ++ showClassHeader tyArgs className ++ "'"
  ElaboratedInstance _ _ tyArgs className _ _ -> "bizen '" ++ showClassHeader tyArgs className ++ "'"
  Export declaration -> "show " ++ declLabel declaration
  ReExport name -> "show '" ++ name ++ "'"
  ReExportType name -> "show-ilk '" ++ name ++ "'"

showClassHeader :: [TypeExpr] -> String -> String
showClassHeader arguments name = case map showTypeExprLabel arguments of
  [] -> name
  first : rest -> unwords (first : name : rest)

showTypeExprLabel :: TypeExpr -> String
showTypeExprLabel = \case
  TypeHole _ -> "inferred"
  TypeName name -> name
  TypeApply name _ -> name
  TypeRecord {} -> "record"
  TypeArrow {} -> "function"
  TypeForall _ body -> showTypeExprLabel body

checkRunnableEffectsM :: EffectRow -> Tc ()
checkRunnableEffectsM effects = do
  st <- getTc
  let actualEffects = applyStateEffects effects st
  if all isRunnableEffect actualEffects
    then pure ()
    else failTc ("main hath unsupported effects {" ++ showEffects actualEffects ++ "}; only console, random, async, file, system, clock, process, and web are allowed")

isRunnableEffect :: Effect -> Bool
isRunnableEffect effect = case effectHeadAndArgs effect of
  Just (NominalEffect EffectRef {effectIdentity = SymbolId RuntimeModule name}, []) -> name `elem` runnerEffectNames
  _ -> False

checkDeclM :: Decl -> TcContext -> Tc ()
checkDeclM d ctx = case d of
  TypeAlias (parameterNames -> params) _ target -> checkTypeExprNamesWithM params "type alias" target ctx
  DataDecl (parameterNames -> params) dataName constructors -> mapM_ (checkCtor params dataName) constructors
  EffectDecl (parameterNames -> params) effectName ops -> checkEffectDeclM params effectName ops ctx
  ClassDecl entries className members -> checkClassDeclM (parameterNames (headerParameters entries)) className (headerRequirements entries) members (scopeParameterKinds (headerParameters entries) ctx)
  InstanceDecl entries tyArgs className members -> checkInstanceM (parameterNames (headerParameters entries)) tyArgs className (headerRequirements entries) members (scopeParameterKinds (headerParameters entries) ctx)
  _ -> pure ()
  where
    checkCtor params dataName (Ctor _ (parameterNames -> names) fields result) = do
      checkNewTypeParametersM params names
      mapM_ (\field -> checkTypeExprNamesWithM (names ++ params) ("data '" ++ dataName ++ "'") field ctx) fields
      mapM_ checkResult result
      where
        checkResult ty = do
          checkTypeExprNamesWithM (names ++ params) ("data '" ++ dataName ++ "'") ty ctx
          let resultArgs = case ty of
                TypeName name | name == dataName -> Just []
                TypeApply name args | name == dataName -> Just args
                _ -> Nothing
          case resultArgs of
            Just args | length args == length params -> pure ()
            _ -> failTc ("constructor of data '" ++ dataName ++ "' must return '" ++ dataName ++ "' with " ++ show (length params) ++ " type arguments")

checkEffectDeclM :: [String] -> String -> [EffectOp] -> TcContext -> Tc ()
checkEffectDeclM params effectName ops ctx = mapM_ checkOp ops
  where
    checkOp (EffectOp opName t) = do
      checkNewTypeParametersM params (annotationParameters t)
      checkTypeExprNamesWithM params ("effect operation '" ++ effectName ++ "~" ++ opName ++ "'") t ctx
      case normalizeFun (let (_, _, ty) = schemeParts (typeAnnSchemeWith params ctx (TypeAnn t [])) in ty) of
        TyFun {} -> pure ()
        _ -> failTc ("effect operation '" ++ effectName ++ "~" ++ opName ++ "' must have a function type")

checkClassDeclM :: [String] -> String -> [ClassConstraint] -> [ClassMember] -> TcContext -> Tc ()
checkClassDeclM classParams className constraints members ctx = do
  checkClassConstraintsWithM classParams ("flock '" ++ className ++ "'") constraints ctx
  checkClassMemberConstraintsM classParams className members ctx
  checkClassLawsM classParams className constraints members ctx

checkClassConstraintsWithM :: [String] -> String -> [ClassConstraint] -> TcContext -> Tc ()
checkClassConstraintsWithM bound owner constraints ctx = mapM_ checkConstraint constraints
  where
    checkConstraint (ClassConstraint args neededName) = do
      mapM_ (\argument -> checkTypeExprNamesWithM bound owner argument ctx) args
      case findClassInfo neededName ctx of
        Nothing -> failTc (owner ++ " hath unknown constraint flock '" ++ neededName ++ "'")
        Just ClassInfo {classParams} -> do
          let expected = length classParams
              actual = length args
          unless (actual == expected) $
            failTc (owner ++ " hath constraint '" ++ neededName ++ "' with " ++ show actual ++ " arguments, but '" ++ neededName ++ "' hath " ++ show expected ++ " parameters")

checkClassMemberConstraintsM :: [String] -> String -> [ClassMember] -> TcContext -> Tc ()
checkClassMemberConstraintsM bound className members ctx = mapM_ checkMember (mapMaybe classMemberSignature members)
  where
    checkMember (name, annotation) = checkTypeAnnConstraintsWithM bound ("flock member '" ++ className ++ "~" ++ name ++ "'") annotation ctx

checkTypeAnnConstraintsM :: String -> TypeAnn -> TcContext -> Tc ()
checkTypeAnnConstraintsM = checkTypeAnnConstraintsWithM []

checkTypeAnnConstraintsWithM :: [String] -> String -> TypeAnn -> TcContext -> Tc ()
checkTypeAnnConstraintsWithM bound owner (TypeAnn value (headerRequirements -> constraints)) ctx = do
  checkNewTypeParametersM bound (annotationParameters value)
  checkTypeExprNamesWithM bound owner value ctx
  checkClassConstraintsWithM (annotationParameters value ++ bound) owner constraints ctx

checkNewTypeParametersM :: [String] -> [String] -> Tc ()
checkNewTypeParametersM bound =
  mapM_ (\name -> when (name `elem` bound) (failTc ("type parameter '" ++ name ++ "' is already declared by its owner")))

annotationParameters :: TypeExpr -> [String]
annotationParameters (TypeForall (parameterNames -> names) _) = names
annotationParameters _ = []

checkTypeExprNamesM :: String -> TypeExpr -> TcContext -> Tc ()
checkTypeExprNamesM = checkTypeExprNamesWithM []

checkTypeExprNamesWithM :: [String] -> String -> TypeExpr -> TcContext -> Tc ()
checkTypeExprNamesWithM bound owner expr ctx = case expr of
  TypeHole _ -> pure ()
  TypeName name
    | name `elem` bound -> pure ()
    | otherwise -> checkTypeNameM owner name ctx >> checkAliasArityM owner name 0 ctx
  TypeApply name arguments -> do
    unless (name `elem` bound) do
      checkTypeNameM owner name ctx
      checkAliasArityM owner name (length arguments) ctx
    mapM_ (\argument -> checkTypeExprNamesWithM bound owner argument ctx) arguments
  TypeRecord fields -> mapM_ (\(_, fieldType) -> checkTypeExprNamesWithM bound owner fieldType ctx) fields
  TypeArrow arguments effects result -> do
    mapM_ (\argument -> checkTypeExprNamesWithM bound owner argument ctx) arguments
    mapM_ (\effect -> checkTypeExprNamesWithM bound owner effect ctx) effects
    checkEffectRowArgumentsM (map (convertEffectExprWith bound ctx) effects)
    checkTypeExprNamesWithM bound owner result ctx
  TypeForall (parameterNames -> names) body -> checkTypeExprNamesWithM (names ++ bound) owner body ctx

checkAliasArityM :: String -> String -> Int -> TcContext -> Tc ()
checkAliasArityM owner name actual ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (Alias canonical params _) -> do
    let expected = length params
    unless (actual == expected || maybe False isScopedTypeVariable (snd <$> aliasInfo name ctx)) $
      failTc (owner ++ " useþ type alias '" ++ canonical ++ "' with " ++ show actual ++ " arguments, but it hath " ++ show expected ++ " parameters")
  _ -> pure ()

checkTypeNameM :: String -> String -> TcContext -> Tc ()
checkTypeNameM owner name ctx@TcContext {tcTypeAmbiguities}
  | isQualifiedName name =
      when (isNothing (findTypeInfoForTypeSystem name ctx)) $
        failTc (owner ++ " useþ unknown type '" ++ name ++ "'")
  | Just choices <- Map.lookup name tcTypeAmbiguities =
      failTc (owner ++ " useþ ambiguous type '" ++ name ++ "'; qualify it as one of: " ++ intercalate ", " choices)
  | isPrimitiveTypeName name || isJust (findTypeInfoForTypeSystem name ctx) = pure ()
  | otherwise = failTc (owner ++ " useþ undeclared type '" ++ name ++ "'; declare @" ++ name ++ " with its kind")

checkClassLawsM :: [String] -> String -> [ClassConstraint] -> [ClassMember] -> TcContext -> Tc ()
checkClassLawsM classParams className classConstraints members ctx = zipWithM_ checkLaw [(1 :: Int) ..] laws
  where
    laws = [(declarations, parameters, left, right) | ClassLaw declarations parameters left right <- members]
    availableConstraints = map (convertClassConstraintWith classParams ctx) (ownClassConstraint className ctx ++ classConstraints)

    checkLaw number (declarations, parameters, left, right) = mapTcError check (\message -> "in law " ++ show number ++ " of flock '" ++ className ++ "': " ++ message)
      where
        names = parameterNames declarations
        check = do
          checkNewTypeParametersM classParams names
          mapM_ (\(_, ty) -> checkTypeExprNamesWithM (names ++ classParams) ("law of flock '" ++ className ++ "'") ty ctx) parameters
          let rawParameterTypes = map (convertTypeExprWith (names ++ classParams) ctx . snd) parameters
              variables = nub (names ++ classParams)
              rowVariables = nub (foldl' (flip effectRowVarsInTy) [] rawParameterTypes)
          replacements <- Map.fromList <$> traverse (freshLawVariable rowVariables) variables
          let parameterTypes = map (`replaceVars` replacements) rawParameterTypes
          lawCtx <- bindPatternsM (map fst parameters) parameterTypes (scopeTypeVariables replacements (scopeParameterKinds declarations ctx))
          Inferred leftType leftEffects leftConstraints _ <- inferNamedM "law left side" left lawCtx
          Inferred rightType rightEffects rightConstraints _ <- inferNamedM "law right side" right lawCtx
          mapTcError (unifyM leftType rightType) ("law sides have different types: " ++)
          mapTcError (unifyEffectsM leftEffects rightEffects) ("law sides have different effects: " ++)
          checkConstraintsAgainstM "law left side" ctx leftConstraints availableConstraints
          checkConstraintsAgainstM "law right side" ctx rightConstraints availableConstraints
    freshLawVariable rowVariables variable
      | variable `elem` rowVariables = (variable,) . TyVar <$> freshNameM "e"
      | otherwise = freshSkolem [] variable

checkForeignLetM :: String -> String -> TypeAnn -> TcContext -> Tc ()
checkForeignLetM name hostKey ann ctx = do
  checkTypeAnnConstraintsM ("fremmed let '" ++ name ++ "'") ann ctx
  let actual = typeAnnScheme ann ctx
  case findForeignBinding hostKey of
    Nothing -> failTc ("fremmed let '" ++ name ++ "' referreþ to unknown host binding '" ++ hostKey ++ "'")
    Just binding ->
      let expected = hostScheme (hostSignature binding)
       in unless (actual == expected) $
            failTc
              ( "fremmed let '"
                  ++ name
                  ++ "' hath type "
                  ++ schemeType actual
                  ++ ", but host binding '"
                  ++ hostKey
                  ++ "' hath type "
                  ++ schemeType expected
              )
  where
    schemeType scheme = showTy (let (_, _, ty) = schemeParts scheme in ty)

requirePureImmediateEffectsM :: String -> EffectRow -> Tc ()
requirePureImmediateEffectsM owner effects = do
  st <- getTc
  let actualEffects = applyStateEffects effects st
  unless (null actualEffects) $
    failTc (owner ++ " hath immediate effects {" ++ showEffects actualEffects ++ "}; use a function type or run it from a runnable file")

checkInstanceM :: [String] -> [TypeExpr] -> String -> [ClassConstraint] -> [Decl] -> TcContext -> Tc ()
checkInstanceM names tyArgs className constraints members outer = do
  mapM_ (\tyArg -> checkTypeExprNamesWithM names ("bizen '" ++ className ++ "'") tyArg outer) tyArgs
  checkClassConstraintsWithM names ("bizen '" ++ className ++ "'") constraints outer
  let actualClass = canonicalClassName className ctx
      key = instanceKeyFor (tcModule ctx) actualClass (map (`convertTypeExpr` ctx) tyArgs)
      duplicateCount =
        length
          [ ()
          | InstanceInfo {instanceKey, instanceDepth} <- instancesForClass actualClass ctx,
            instanceDepth == 0,
            instanceKey == key
          ]
  unless (duplicateCount == 1) (failTc ("duplicate bizen '" ++ showClassHeader tyArgs className ++ "'"))
  case findClassInfo className ctx of
    Nothing -> failTc ("unknown flock '" ++ className ++ "' in bizen")
    Just ClassInfo {classParams} -> do
      checkInstanceArityM className classParams tyArgs
      case instanceSpecsForClass className tyArgs ctx [] of
        Nothing -> failTc ("bizen '" ++ className ++ "' hath an unknown required flock")
        Just specs -> checkInstanceMembersM className tyArgs constraints members specs ctx
  where
    ctx = scopeTypeVariables (Map.fromList [(name, TyVar name) | name <- names]) outer

checkInstanceArityM :: String -> [String] -> [TypeExpr] -> Tc ()
checkInstanceArityM className classParams tyArgs =
  unless (length tyArgs == length classParams) $
    failTc ("bizen '" ++ className ++ "' hath " ++ show (length tyArgs) ++ " type arguments, but '" ++ className ++ "' hath " ++ show (length classParams) ++ " parameters")

data InstanceSpec = InstanceSpec
  { instanceSpecName :: String,
    instanceSpecClass :: ClassRef,
    instanceSpecTypes :: [Ty],
    instanceSpecType :: ResolvedTypeAnn
  }
  deriving (Eq, Show)

instanceSpecsForClass :: String -> [TypeExpr] -> TcContext -> [ClassRef] -> Maybe [InstanceSpec]
instanceSpecsForClass className instanceTypes ctx =
  instanceSpecsForClassRef (classRefForName className ctx) (map (`convertTypeExpr` ctx) instanceTypes) ctx

instanceSpecsForClassRef :: ClassRef -> [Ty] -> TcContext -> [ClassRef] -> Maybe [InstanceSpec]
instanceSpecsForClassRef class_ instanceTypes ctx seen
  | class_ `elem` seen = Just []
  | otherwise = case findClassInfoByRef class_ ctx of
      Nothing -> Nothing
      Just ClassInfo {..}
        | length instanceTypes /= length classParams -> Nothing
        | otherwise -> do
            inherited <- instanceSpecsForConstraints classConstraints classParams instanceTypes ctx (class_ : seen)
            pure (instanceMemberSpecs class_ classParams instanceTypes classMethods ++ inherited)

instanceSpecsForConstraints :: [Constraint] -> [String] -> [Ty] -> TcContext -> [ClassRef] -> Maybe [InstanceSpec]
instanceSpecsForConstraints constraints currentParams currentInstanceTypes ctx seen =
  concat <$> traverse specsForConstraint constraints
  where
    specsForConstraint constraint = do
      let Constraint neededInstanceTypes neededClass = specializeConstraint currentParams currentInstanceTypes constraint
      instanceSpecsForClassRef neededClass neededInstanceTypes ctx seen

instanceMemberSpecs :: ClassRef -> [String] -> [Ty] -> [ClassMethod] -> [InstanceSpec]
instanceMemberSpecs class_ classParams instanceTypes = map makeSpec
  where
    makeSpec ClassMethod {..} =
      InstanceSpec classMethodName class_ instanceTypes (specializeResolvedTypeAnn classParams instanceTypes classMethodType)

requireInstanceSpecM :: String -> [InstanceSpec] -> Tc InstanceSpec
requireInstanceSpecM name = maybe (failTc ("bizen defineþ unknown member '" ++ name ++ "'")) pure . find ((== name) . instanceSpecName)

checkInstanceMembersM :: String -> [TypeExpr] -> [ClassConstraint] -> [Decl] -> [InstanceSpec] -> TcContext -> Tc ()
checkInstanceMembersM className tyArgs constraints members specs ctx = do
  requireInstanceMembersM className tyArgs members specs ctx
  mapM_ (checkInstanceMemberM tyArgs className constraints specs ctx) members
  checkRedundantInstanceConstraintsM className tyArgs constraints members specs ctx

checkInstanceMemberM :: [TypeExpr] -> String -> [ClassConstraint] -> [InstanceSpec] -> TcContext -> Decl -> Tc ()
checkInstanceMemberM tyArgs className constraints specs ctx = \case
  Let name annotation expr -> do
    mapM_ (\ann -> checkTypeAnnConstraintsM ("bizen member '" ++ name ++ "'") ann ctx) annotation
    InstanceSpec {instanceSpecClass, instanceSpecType} <- requireInstanceSpecM name specs
    let memberAnn = resolvedTypeAnnWithInternalConstraints ctx (instanceMemberInternalConstraints tyArgs className constraints) instanceSpecType
    mapTcError
      ( do
          (variables, expected, expectedConstraints) <- instantiateResolvedAnnotationM memberAnn
          void (checkInstanceExprM name annotation expected expectedConstraints expr (scopeInstanceVariables variables ctx))
      )
      (\msg -> "in bizen member '" ++ classDisplayName instanceSpecClass ++ "~" ++ name ++ "': " ++ msg)
  _ -> failTc "bizen member must be a let"

checkInstanceExprM :: String -> Maybe TypeAnn -> Ty -> [Constraint] -> Expr -> TcContext -> Tc (Ty, EffectRow, [Constraint], Expr)
checkInstanceExprM name annotation expected constraints expr ctx = do
  scoped <- case annotation of
    Nothing -> pure ctx
    Just ann -> do
      -- the written annotation must hold universally, and its instantiation
      -- must satisfy the class interface. only the latter check elaborateþ.
      before <- getTc
      void (checkAnnotatedExprM name ann expr ctx)
      putTc before
      let scheme = typeAnnScheme ann ctx
      (variables, actual, required) <- instantiateWithBindingsM [] scheme
      unifyM actual expected
      checkConstraintsAgainstM name ctx required constraints
      pure (scopeTypeVariables (Map.restrictKeys variables (Set.fromList (specifiedVars scheme))) (scopeParameterKinds (specifiedParameters scheme) ctx))
  checkExprAgainstM name expected [] constraints expr scoped

checkRedundantInstanceConstraintsM :: String -> [TypeExpr] -> [ClassConstraint] -> [Decl] -> [InstanceSpec] -> TcContext -> Tc ()
checkRedundantInstanceConstraintsM className tyArgs constraints members specs ctx = do
  redundant <- findRedundant (zip [(0 :: Int) ..] constraints)
  case redundant of
    Just constraint -> failTc (owner ++ " hath redundant constraint " ++ showSourceConstraint (convertClassConstraint constraint ctx))
    Nothing -> pure ()
  where
    owner = "bizen '" ++ showClassHeader tyArgs className ++ "'"
    ownKey = instanceKeyFor (tcModule ctx) (canonicalClassName className ctx) (map (`convertTypeExpr` ctx) tyArgs)
    parentConstraints = instanceParentConstraints className tyArgs ctx
    checkWithout index = do
      let remaining = [constraint | (otherIndex, constraint) <- zip [(0 :: Int) ..] constraints, otherIndex /= index]
      mapM_ (checkInstanceMemberM tyArgs className remaining specs ctx) members
      checkParentEvidenceM remaining
    checkParentEvidenceM available = do
      let external = map (`convertClassConstraint` ctx) available
          scheme = generalizeTyWithConstraints (external ++ parentConstraints) (TyCon "ℤ")
      (_, instantiated) <- skolemizeM scheme
      let (external2, parents2) = splitAt (length external) instantiated
          bindings = evidenceBindings external2
      mapM_ (resolveConstraintEvidenceSeenM ctx bindings [ownKey]) parents2
    findRedundant [] = pure Nothing
    findRedundant ((index, constraint) : rest) = do
      redundant <- succeedsTc (checkWithout index)
      if redundant then pure (Just constraint) else findRedundant rest

requireInstanceMembersM :: String -> [TypeExpr] -> [Decl] -> [InstanceSpec] -> TcContext -> Tc ()
requireInstanceMembersM currentClass currentTypes members specs ctx = case missing of
  spec : _ -> failTc ("bizen '" ++ currentClass ++ "' is missing required member '" ++ instanceSpecName spec ++ "'")
  [] -> pure ()
  where
    provided = [name | Let name _ _ <- members]
    missing =
      [ spec
      | spec@InstanceSpec {..} <- specs,
        instanceSpecName `notElem` provided,
        instanceSpecName `notElem` inheritedProvided,
        instanceSpecClass == currentClassRef || not (hasDirectInstance instanceSpecClass instanceSpecTypes ctx)
      ]
    currentClassRef = classRefForName currentClass ctx
    currentTypeValues = map (`convertTypeExpr` ctx) currentTypes
    inheritedProvided = case findClassInfo currentClass ctx of
      Nothing -> []
      Just ClassInfo {classParams, classConstraints} -> concatMap providedByConstraint classConstraints
        where
          providedByConstraint constraint =
            let Constraint neededTypes neededClass = specializeConstraint classParams currentTypeValues constraint
             in if hasDirectInstance neededClass neededTypes ctx
                  then maybe [] (map instanceSpecName) (instanceSpecsForClassRef neededClass neededTypes ctx [])
                  else []

hasDirectInstance :: ClassRef -> [Ty] -> TcContext -> Bool
hasDirectInstance wantedClass wantedTypes ctx = any matches (instancesForClassRef wantedClass ctx)
  where
    matches InstanceInfo {..} =
      instanceDepth == 0
        && isJust (zipWithExact typePatternBindings instanceTypes wantedTypes >>= mergeBindingMaps)

instanceMemberInternalConstraints :: [TypeExpr] -> String -> [ClassConstraint] -> [ClassConstraint]
instanceMemberInternalConstraints types className = (ClassConstraint types className :)

-- local declarations share the global sequential inference and elaboration.
inferLocalDeclsM :: [Decl] -> TcContext -> Tc ([Decl], TcContext, EffectRow)
inferLocalDeclsM declarations ctx = do
  ((ctx2, effects), declarations2) <- mapAccumM step (ctx, []) declarations
  pure (declarations2, ctx2, effects)
  where
    step (currentCtx, currentEffects) declaration = do
      (declaration2, ctx2, declarationEffects) <- elaborateSequentialDeclM LocalDeclarations declaration currentCtx
      pure ((ctx2, unionEffects currentEffects declarationEffects), declaration2)

-- infer recursive calls monomorphically once. after solving, a branch-local
-- alias supplieþ the final dictionary parameters without re-inferring the body.
-- placing it inside the match delays self application until after saturation.
inferRecursiveBindingM :: String -> Expr -> TcContext -> Tc (String, Inferred)
inferRecursiveBindingM name expr ctx = do
  selfTy <- freshM
  ident <- tcNextMeta <$> getTc
  let alias = "$self" ++ show ident
      ctxSelf = ctx {tcEnv = EnvBinding name alias (Forall [] [] selfTy) localEnvRank name Nothing : tcEnv ctx}
  inferred <- inferNamedM name expr ctxSelf
  recursiveType <- instantiateTypeM (inferredTy inferred)
  mapTcError (unifyM selfTy recursiveType) (\msg -> "in '" ++ name ++ "': " ++ msg)
  pure (alias, inferred)

bindRecursiveAlias :: String -> Expr -> [Constraint] -> Expr -> Expr
bindRecursiveAlias alias target constraints = go
  where
    self = case map (EEvidence . snd) (evidenceBindings constraints) of
      [] -> target
      first : rest -> EApply target (first :| rest)
    go (ELocated span body) = ELocated span (go body)
    go (EAscribe body annotation) = EAscribe (go body) annotation
    go (ETypeApply body arguments) = ETypeApply (go body) arguments
    go (EMatch [] cases) = EMatch [] [MatchCase patterns (EBlock [Let alias Nothing self] body) | MatchCase patterns body <- cases]
    go body = body

bindingScheme :: TcContext -> Ty -> [Constraint] -> EffectRow -> TcState -> Scheme
bindingScheme ctx ty constraints effects st
  | null (applyStateEffects effects st) = generalizeApplied ctx ty constraints st
  | otherwise = Forall [] (applyStateConstraints constraints st) (applyState ty st)

inferNamedM :: String -> Expr -> TcContext -> Tc Inferred
inferNamedM name expr ctx = mapTcError (inferExprM expr ctx) (\msg -> "in '" ++ name ++ "': " ++ msg)

resolvedTypeAnnWithInternalConstraints :: TcContext -> [ClassConstraint] -> ResolvedTypeAnn -> ResolvedTypeAnn
resolvedTypeAnnWithInternalConstraints ctx internal (ResolvedTypeAnn ty constraints) =
  ResolvedTypeAnn ty (map (`convertClassConstraint` ctx) internal ++ constraints)

instantiateResolvedAnnotationM :: ResolvedTypeAnn -> Tc (Map.Map String Ty, Ty, [Constraint])
instantiateResolvedAnnotationM (ResolvedTypeAnn ty constraints) =
  let (parameters, body) = case ty of TyForall declarations inner -> (declarations, inner); _ -> ([], ty)
      scheme = Specified parameters (addForallVars (parameterNames parameters) (generalizeTyWithConstraints constraints body))
   in skolemizeWithBindingsM scheme

scopeInstanceVariables :: Map.Map String Ty -> TcContext -> TcContext
scopeInstanceVariables variables ctx = scopeTypeVariables (Map.filterWithKey inScope variables) ctx
  where
    inScope name _ = maybe False (isScopedTypeVariable . snd) (aliasInfo name ctx)

skolemizeM :: Scheme -> Tc (Ty, [Constraint])
skolemizeM scheme = do
  (_, ty, constraints) <- skolemizeWithBindingsM scheme
  pure (ty, constraints)

skolemizeWithBindingsM :: Scheme -> Tc (Map.Map String Ty, Ty, [Constraint])
skolemizeWithBindingsM scheme = do
  -- annotation variables are rigid here; only parser-generated holes remain flexible.
  let (vars, constraints, ty) = schemeParts scheme
      rowVars = effectRowVarsInConstraints constraints (effectRowVarsInTy ty [])
  replacements <- Map.fromList <$> traverse (\name -> if name `elem` specifiedVars scheme then rigidSkolem rowVars name else freshSkolem rowVars name) vars
  pure (replacements, replaceVars ty replacements, map (`replaceConstraintVars` replacements) constraints)

rigidSkolem :: [String] -> String -> Tc (String, Ty)
rigidSkolem rowVars name = do
  ident <- freshIdM
  pure (name, if name `elem` rowVars then TyEffectSkolem ident else TySkolem (UniversalSkolem ident) [])

scopeTypeVariables :: Map.Map String Ty -> TcContext -> TcContext
scopeTypeVariables variables ctx = Map.foldlWithKey' (\current name ty -> addTypeInfo name (Alias name [] ty) current) ctx variables

scopeParameterKinds :: [TypeParameter] -> TcContext -> TcContext
scopeParameterKinds parameters ctx@TcContext {tcKinds = kinds@Kind.KindContext {typeKinds}} =
  ctx {tcKinds = kinds {Kind.typeKinds = Map.union (Map.fromList parameters) typeKinds}}

freshSkolem :: [String] -> String -> Tc (String, Ty)
freshSkolem rowVars variable
  | isImplicitAnnotationVariable variable = (variable,) <$> freshM
  | otherwise = rigidSkolem rowVars variable

isImplicitAnnotationVariable :: String -> Bool
isImplicitAnnotationVariable name = "$hole" `isPrefixOf` name

-- skolems may solve fresh holes inside a check, but must never determine a
-- captured type outside it. inspect earlier metas and free constructor/row
-- variables after substitution, including indirect links to fresh holes.
withPolymorphicScopeM :: [TypeParameter] -> Ty -> [Ty] -> (Map.Map String Ty -> Ty -> Tc a) -> Tc a
withPolymorphicScopeM parameters body surrounding action = do
  before <- getTc
  replacements <- Map.fromList <$> traverse (rigidSkolem rowNames) names
  value <- action replacements (replaceVars body replacements)
  rejectEscapingSkolemsM before replacements (TyForall parameters body : surrounding)
  pure value
  where
    rowNames = effectRowVarsInTy body []
    names = parameterNames parameters

rejectEscapingSkolemsM :: TcState -> Map.Map String Ty -> [Ty] -> Tc ()
rejectEscapingSkolemsM before replacements external = do
  let boundary = tcNextMeta before
      heads = nub (Map.keys (tcTypeHeads before) ++ foldr typeHeadVarsInTy [] external)
      rows = nub (map (\(RowVariable name) -> name) (Map.keys (tcEffectRows before)) ++ foldr effectRowVarsInTy [] external)
  after <- getTc
  let externalTypes = [TyMeta ident | ident <- IntMap.keys (tcMetaSubst after), ident < boundary] ++ map TyVar heads ++ [rowEqualityType (RowEquality [EffectVariable (RowVariable row)] []) | row <- rows]
      escapes = any (any (isSkolem replacements) . (\ty -> foldTyWith (:) (:) (applyState ty after) [])) externalTypes
  when escapes (failTc "polymorphic type variable escapes its scope")
  where
    isSkolem replacements ty =
      ty `elem` Map.elems replacements || case ty of
        TySkolem ident _ -> TySkolem ident [] `elem` Map.elems replacements
        _ -> False

contextTypes :: TcContext -> [Ty]
contextTypes = map (schemeType . envScheme) . tcEnv
  where
    schemeType scheme =
      let (names, constraints, ty) = schemeParts scheme
       in TyForall (unknownParameters names) (TyRecord (Map.fromList (zip (map show [0 :: Int ..]) (ty : map rowEqualityType (schemeRows scheme) ++ [arg | Constraint args _ <- constraints, arg <- args]))))

checkAnnotatedExprM :: String -> TypeAnn -> Expr -> TcContext -> Tc (Ty, EffectRow, [Constraint], Expr)
checkAnnotatedExprM name ann expr ctx = do
  before <- getTc
  let scheme = typeAnnScheme ann ctx
  (variables, expectedValue, expectedConstraints) <- skolemizeWithBindingsM scheme
  let scoped = Map.restrictKeys variables (Set.fromList (specifiedVars scheme))
  checked <- checkExprAgainstM name expectedValue [] expectedConstraints expr (scopeTypeVariables scoped (scopeParameterKinds (specifiedParameters scheme) ctx))
  unless (Map.null scoped) (rejectEscapingSkolemsM before scoped (contextTypes ctx))
  pure checked

checkExprAgainstM :: String -> Ty -> EffectRow -> [Constraint] -> Expr -> TcContext -> Tc (Ty, EffectRow, [Constraint], Expr)
checkExprAgainstM name expectedValue expectedEffects expectedConstraints (ELocated span expression) ctx = withTcSpan span do
  (value, effects, constraints, core) <- checkExprAgainstM name expectedValue expectedEffects expectedConstraints expression ctx
  pure (value, effects, constraints, ELocated span core)
checkExprAgainstM name (TyForall names body) expectedEffects expectedConstraints expression ctx =
  withPolymorphicScopeM names body (contextTypes ctx ++ [rowEqualityType (RowEquality expectedEffects [])]) $ \variables rigid -> do
    (_, effects, constraints, core) <- checkExprAgainstM name rigid expectedEffects expectedConstraints expression (scopeTypeVariables variables (scopeParameterKinds names ctx))
    st <- getTc
    pure (applyState (TyForall names body) st, effects, constraints, core)
checkExprAgainstM name expectedValue expectedEffects expectedConstraints (EBlock declarations body) ctx = do
  (declarations2, ctx2, declarationEffects) <- inferLocalDeclsM declarations ctx
  (ty, effects, constraints, core) <- checkExprAgainstM name expectedValue expectedEffects expectedConstraints body ctx2
  (_, combined) <- checkEffectsAgainstM name expectedValue (unionEffects declarationEffects effects) expectedEffects
  pure (ty, combined, constraints, EBlock declarations2 core)
checkExprAgainstM name expectedValue@(TyRecord fieldTypes) expectedEffects expectedConstraints (ERecord fields) ctx = do
  unless (Map.keys fieldTypes == Map.keys (Map.fromList fields)) (failTc "record field mismatch")
  checked <- traverse (\(field, expression) -> (field,) <$> checkExprAgainstM name (fieldTypes Map.! field) expectedEffects expectedConstraints expression ctx) fields
  let effects = foldl' unionEffects [] [row | (_, (_, row, _, _)) <- checked]
  st <- getTc
  pure (applyState expectedValue st, effects, applyStateConstraints expectedConstraints st, ERecord [(field, core) | (field, (_, _, _, core)) <- checked])
checkExprAgainstM name expectedValue expectedEffects expectedConstraints (EMatch [] cases) ctx
  | TyFun args latentEffects ret <- normalizeFun expectedValue,
    anonymousCasesFitFunction (NE.length args) cases = do
      (checkedConstraints, core) <- checkMatchFunctionAgainstM name args latentEffects ret expectedConstraints cases ctx
      st <- getTc
      pure (applyState expectedValue st, applyStateEffects expectedEffects st, checkedConstraints, core)
checkExprAgainstM name expectedValue expectedEffects expectedConstraints (EMatch scrutinees cases) ctx
  | not (null scrutinees) =
      checkMatchAgainstM name expectedValue expectedEffects expectedConstraints scrutinees cases ctx
checkExprAgainstM name expectedValue expectedEffects expectedConstraints expr ctx = do
  Inferred actual effects constraints core <- inferNamedM name expr ctx
  instantiated <- instantiateTypeM actual
  mapTcError (unifyM instantiated expectedValue) (\msg -> "in '" ++ name ++ "': " ++ msg ++ "; actual " ++ showTy actual ++ ", expected " ++ showTy expectedValue)
  (checkedValue, checkedEffects) <- checkEffectsAgainstM name expectedValue effects expectedEffects
  checkConstraintsAgainstM name ctx constraints expectedConstraints
  st <- getTc
  pure (checkedValue, checkedEffects, applyStateConstraints expectedConstraints st, core)

checkMatchFunctionAgainstM :: String -> NonEmpty Ty -> EffectRow -> Ty -> [Constraint] -> [MatchCase] -> TcContext -> Tc ([Constraint], Expr)
checkMatchFunctionAgainstM name args latentEffects ret expectedConstraints cases ctx = do
  (actualConstraints, cases2) <- mapAccumM step [] cases
  st <- getTc
  checkExhaustiveM (applyStateAll patternArgs st) cases2 ctx
  checkConstraintsAgainstM name ctx actualConstraints expectedConstraints
  checked <- applyStateConstraints expectedConstraints <$> getTc
  pure (checked, EMatch [] cases2)
  where
    arity = functionCaseArity (NE.length args) cases
    (patternArgs, remainingArgs) = splitAt arity (NE.toList args)
    step constraints (MatchCase ps body) = withScopedGadtRefinements do
      scopeStart <- tcNextMeta <$> getTc
      ctx2 <- bindPatternsM (NE.toList ps) patternArgs ctx
      ps2 <- traverse (resolvePatternM ctx) ps
      (bodyConstraints, body2) <- case NE.nonEmpty remainingArgs of
        Nothing -> do
          (checked, _, inferredConstraints, core) <- checkExprAgainstM name ret latentEffects expectedConstraints body ctx2
          rejectEscapingExistentials scopeStart checked
          pure (inferredConstraints, core)
        Just rest -> do
          (_, _, inferredConstraints, core) <- checkExprAgainstM name (TyFun rest latentEffects ret) [] expectedConstraints body ctx2
          pure (inferredConstraints, core)
      pure (unionConstraints constraints bodyConstraints, MatchCase ps2 body2)

checkMatchAgainstM :: String -> Ty -> EffectRow -> [Constraint] -> [Expr] -> [MatchCase] -> TcContext -> Tc (Ty, EffectRow, [Constraint], Expr)
checkMatchAgainstM name expected expectedEffects expectedConstraints scrutinees cases ctx = do
  inferred <- inferExprListM scrutinees ctx
  let scrutTys = map inferredTy inferred
      scrutEffects = foldl' unionEffects [] (map inferredEffects inferred)
      scrutConstraints = foldl' unionConstraints [] (map inferredConstraints inferred)
  cases2 <- traverse (checkCase scrutTys) cases
  st <- getTc
  checkExhaustiveM (applyStateAll scrutTys st) cases2 ctx
  _ <- checkEffectsAgainstM name expected scrutEffects expectedEffects
  checkConstraintsAgainstM name ctx scrutConstraints expectedConstraints
  st2 <- getTc
  pure (applyState expected st2, applyStateEffects expectedEffects st2, applyStateConstraints expectedConstraints st2, EMatch (map inferredExpr inferred) cases2)
  where
    checkCase scrutTys (MatchCase ps body) = withScopedGadtRefinements do
      scopeStart <- tcNextMeta <$> getTc
      ctx2 <- bindPatternsM (NE.toList ps) scrutTys ctx
      ps2 <- traverse (resolvePatternM ctx) ps
      (checked, _, _, body2) <- checkExprAgainstM name expected expectedEffects expectedConstraints body ctx2
      rejectEscapingExistentials scopeStart checked
      pure (MatchCase ps2 body2)

rejectEscapingExistentials :: Int -> Ty -> Tc ()
rejectEscapingExistentials scopeStart ty =
  when (any newlyBound (foldTyWith (:) (:) ty [])) $
    failTc "existential constructor type escapes its match branch"
  where
    newlyBound (TySkolem (ExistentialSkolem ident) _) = ident >= scopeStart
    newlyBound (TyEffectSkolem ident) = ident >= scopeStart
    newlyBound _ = False

anonymousCasesFitFunction :: Int -> [MatchCase] -> Bool
anonymousCasesFitFunction arity cases = case cases of
  [] -> True
  _ -> let caseArity = functionCaseArity arity cases in caseArity <= arity && all (\(MatchCase ps _) -> NE.length ps == caseArity) cases

functionCaseArity :: Int -> [MatchCase] -> Int
functionCaseArity fallback = fromMaybe fallback . matchCaseArity

checkEffectsAgainstM :: String -> Ty -> EffectRow -> EffectRow -> Tc (Ty, EffectRow)
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

checkEffectSubsetM :: EffectRow -> EffectRow -> Tc ()
checkEffectSubsetM actual expected = unifyEffectsM (unionEffects actual expected) expected

checkConstraintsAgainstM :: String -> TcContext -> [Constraint] -> [Constraint] -> Tc ()
checkConstraintsAgainstM name ctx actualConstraints0 expectedConstraints0 = do
  st <- getTc
  let actualConstraints = applyStateConstraints actualConstraints0 st
      expectedConstraints = applyStateConstraints expectedConstraints0 st
  unexpected <- filterM (fmap not . constraintResolvedBy expectedConstraints ctx) actualConstraints
  unless (null unexpected) $
    failTc ("in '" ++ name ++ "': missing constraint " ++ showConstraints unexpected ++ "; available constraints " ++ showConstraints expectedConstraints)

normalizeConstraintsM :: TcContext -> [Constraint] -> Tc [Constraint]
normalizeConstraintsM ctx constraints0 = do
  st <- getTc
  foldM step [] (applyStateConstraints constraints0 st)
  where
    step acc constraint =
      do
        normalized <- normalizeConstraintM ctx [] constraint
        pure (unionConstraints acc normalized)

normalizeConstraintM :: TcContext -> [Constraint] -> Constraint -> Tc [Constraint]
normalizeConstraintM ctx seen constraint
  | constraint `elem` seen = if constraintMayRemainOpen constraint then pure [constraint] else failTc ("missing constraint " ++ showConstraint constraint)
  | otherwise = do
      selected <- either failTc pure (selectInstanceCandidate ctx constraint)
      case selected of
        Just (_, constraints, _) -> foldM step [] constraints
        Nothing
          | constraintMayRemainOpen constraint -> pure [constraint]
          | otherwise -> failTc ("missing constraint " ++ showConstraint constraint)
  where
    step acc nested = do
      normalized <- normalizeConstraintM ctx (constraint : seen) nested
      pure (unionConstraints acc normalized)

requireNoConstraintsM :: TcContext -> [Constraint] -> Tc ()
requireNoConstraintsM ctx constraints = do
  remaining <- normalizeConstraintsM ctx constraints
  if null remaining
    then pure ()
    else failTc ("runnable file hath unresolved constraints " ++ showConstraints remaining)

constraintResolvedBy :: [Constraint] -> TcContext -> Constraint -> Tc Bool
constraintResolvedBy expected ctx = go []
  where
    go seen constraint
      | any (\givenConstraint -> constraintCovers ctx [] givenConstraint constraint) expected = pure True
      | constraint `elem` seen = pure False
      | otherwise = do
          selected <- either failTc pure (selectInstanceCandidate ctx constraint)
          case selected of
            Just (_, constraints, _) -> foldM (\resolved nested -> if resolved then go (constraint : seen) nested else pure False) True constraints
            Nothing -> pure False

constraintSubsumes :: Constraint -> Constraint -> Bool
constraintSubsumes (Constraint expectedArgs expectedClass) (Constraint actualArgs actualClass) =
  expectedClass == actualClass
    && length expectedArgs == length actualArgs
    && hasConsistentTypePatternBindings expectedArgs actualArgs

constraintCovers :: TcContext -> [ClassRef] -> Constraint -> Constraint -> Bool
constraintCovers ctx seen givenConstraint@(Constraint constraintArgs constraintClass) wanted
  | constraintSubsumes givenConstraint wanted = True
  | constraintClass `elem` seen = False
  | otherwise = case findClassInfoByRef constraintClass ctx of
      Nothing -> False
      Just ClassInfo {classParams, classConstraints} ->
        any (\constraint -> constraintCovers ctx (constraintClass : seen) (specializeConstraint classParams constraintArgs constraint) wanted) classConstraints

-- evidence holes keep inference independent of instance selection. resolution turns
-- them into hidden local parameters or a statically chosen dictionary tree.
type EvidenceBinding = (Constraint, Int)

-- evidence parameters are ordinary hidden pure arguments after elaboration.
evidenceBindings :: [Constraint] -> [EvidenceBinding]
evidenceBindings constraints = zip constraints [(0 :: Int) ..]

wrapEvidence :: [EvidenceBinding] -> Expr -> Expr
wrapEvidence [] expr = expr
wrapEvidence ((_, ident) : bindings) expr = EEvidenceLambda (ident :| map snd bindings) expr

wrapExternalEvidence :: [ClassConstraint] -> [EvidenceBinding] -> Expr -> Expr
wrapExternalEvidence internal = wrapEvidence . drop (length internal)

resolveEvidenceHoleM :: TcContext -> [EvidenceBinding] -> Int -> Tc Expr
resolveEvidenceHoleM ctx available ident = do
  st@TcState {tcEvidenceConstraints} <- getTc
  constraint <- maybe (failTc "internal missing evidence hole") pure (IntMap.lookup ident tcEvidenceConstraints)
  resolveConstraintEvidenceM ctx available (applyStateConstraint constraint st)

resolveConstraintEvidenceM :: TcContext -> [EvidenceBinding] -> Constraint -> Tc Expr
resolveConstraintEvidenceM ctx available = resolveConstraintEvidenceSeenM ctx available []

resolveConstraintEvidenceSeenM :: TcContext -> [EvidenceBinding] -> [InstanceId] -> Constraint -> Tc Expr
resolveConstraintEvidenceSeenM ctx available seen wanted = case bestLocalEvidence ctx available wanted of
  Left message -> failTc message
  Right (Just ident) -> pure (EEvidence ident)
  Right Nothing -> do
    (info, nestedConstraints, parentConstraints) <- selectInstanceEvidenceM ctx wanted
    buildInstanceEvidenceM ctx available seen info nestedConstraints parentConstraints

buildInstanceEvidenceM :: TcContext -> [EvidenceBinding] -> [InstanceId] -> InstanceInfo -> [Constraint] -> [Constraint] -> Tc Expr
buildInstanceEvidenceM ctx available seen info nestedConstraints parentConstraints = do
  let key = instanceKey info
  when (key `elem` seen) (failTc ("recursive instance evidence for " ++ renderInstanceId (instanceKey info)))
  nested <- traverse (resolveConstraintEvidenceSeenM ctx available (key : seen)) nestedConstraints
  parents <- resolveParents key [] parentConstraints
  pure (EDictionary key (classIdentity (instanceClass info)) nested parents)
  where
    resolveParents ownKey skipped = fmap concat . traverse (resolveParent ownKey skipped)
    resolveParent ownKey skipped parentConstraint
      | parentConstraint `elem` skipped = failTc ("recursive parent instance evidence for " ++ showConstraint parentConstraint)
      | otherwise = case bestLocalEvidence ctx available parentConstraint of
          Left message -> failTc message
          Right (Just ident) -> pure [EEvidence ident]
          Right Nothing -> do
            (parentInfo, nested, parents) <- selectInstanceEvidenceM ctx parentConstraint
            if instanceKey parentInfo == ownKey
              then resolveParents ownKey (parentConstraint : skipped) parents
              else (: []) <$> buildInstanceEvidenceM ctx available (ownKey : seen) parentInfo nested parents

bestLocalEvidence :: TcContext -> [EvidenceBinding] -> Constraint -> Either String (Maybe Int)
bestLocalEvidence ctx available wanted = choose (filter (covers . fst) available)
  where
    covers given = constraintCovers ctx [] given wanted
    choose [] = Right Nothing
    choose candidates =
      let bestRank = minimum (map (localEvidenceRank . fst) candidates)
          identifiers = [ident | (constraint, ident) <- candidates, localEvidenceRank constraint == bestRank]
       in case (bestRank, identifiers) of
            (_, []) -> Right Nothing
            (0, name : _) -> Right (Just name)
            (_, [name]) -> Right (Just name)
            _ -> Left ("ambiguous instance evidence for " ++ showConstraint wanted)
    localEvidenceRank given = if constraintSubsumes given wanted then (0 :: Int) else 1

selectInstanceEvidenceM :: TcContext -> Constraint -> Tc (InstanceInfo, [Constraint], [Constraint])
selectInstanceEvidenceM ctx wanted = do
  selected <- either failTc pure (selectInstanceCandidate ctx wanted)
  maybe (failTc ("missing instance " ++ showConstraint wanted)) pure selected

type InstanceCandidate = (InstanceInfo, [Constraint], [Constraint])

selectInstanceCandidate :: TcContext -> Constraint -> Either String (Maybe InstanceCandidate)
selectInstanceCandidate ctx wanted@(Constraint actualTypes class_)
  | not (all hasConcreteHead actualTypes) = Right Nothing
  | otherwise = case bestCandidates of
      [] -> Right Nothing
      [candidate] -> Right (Just candidate)
      choices -> Left ("ambiguous instances for " ++ showConstraint wanted ++ ": " ++ intercalate ", " (nub [renderInstanceId (instanceKey info) | (info, _, _) <- choices]))
  where
    -- selection is static: inheritance depth rankeþ instances first, then a
    -- structured type pattern outrankeþ a blanket pattern it refines.
    candidates = mapMaybe match (instancesForClassRef class_ ctx)
    match info@InstanceInfo {..}
      | not (instanceSuppliesClass ctx info) = Nothing
      | otherwise = do
          bindings <- zipWithExact typePatternBindings instanceTypes actualTypes >>= mergeBindingMaps
          let specialize = projectInstanceConstraints class_ info . map (constraintWithBindings bindings)
          pure (info, specialize instanceConstraints, specialize instanceParents)
    candidateRank (InstanceInfo {instanceKey, instanceDepth}, _, _)
      | isPrimitiveInstanceId instanceKey = maxBound :: Int
      | otherwise = instanceDepth
    ranked = case candidates of
      [] -> []
      _ -> let rank = minimum (map candidateRank candidates) in filter ((== rank) . candidateRank) candidates
    uniqueRanked = nubByInstanceKey ranked
    bestCandidates = filter (\candidate -> not (any (`instanceMoreSpecificThan` candidate) uniqueRanked)) uniqueRanked

    instanceMoreSpecificThan (left, _, _) (right, _, _) =
      let leftTypes = instanceTypes left
          rightTypes = instanceTypes right
       in hasConsistentTypePatternBindings rightTypes leftTypes
            && not (hasConsistentTypePatternBindings leftTypes rightTypes)

instanceSuppliesClass :: TcContext -> InstanceInfo -> Bool
instanceSuppliesClass _ InstanceInfo {instanceDepth = 0} = True
instanceSuppliesClass ctx InstanceInfo {instanceClass, instanceMembers} = case findClassInfoByRef instanceClass ctx of
  Nothing -> False
  Just ClassInfo {classMethods} ->
    let methods = map classMethodName classMethods
     in null methods || any (`elem` instanceMembers) methods

nubByInstanceKey :: [(InstanceInfo, [Constraint], [Constraint])] -> [(InstanceInfo, [Constraint], [Constraint])]
nubByInstanceKey = Map.elems . foldl' add Map.empty
  where
    add rest candidate@(info, _, _) = Map.insertWith (\_ old -> old) (instanceKey info) candidate rest

resolveExprEvidenceM :: TcContext -> [EvidenceBinding] -> Expr -> Tc Expr
resolveExprEvidenceM ctx available = go
  where
    go = \case
      EWithEvidence function evidence -> do
        function2 <- go function
        dictionaries <- traverse resolveDictionary evidence
        pure $ case dictionaries of
          [] -> function2
          first : rest -> EApply function2 (first :| rest)
      -- nested declarations have already resolved their own evidence scopes.
      other -> traverseExprChildren pure pure go other
    resolveDictionary = resolveEvidenceHoleM ctx available

constraintWithBindings :: Map.Map String Ty -> Constraint -> Constraint
constraintWithBindings bindings (Constraint args class_) =
  Constraint (map (replaceVarsWithBindings bindings) args) class_

replaceVarsWithBindings :: Map.Map String Ty -> Ty -> Ty
replaceVarsWithBindings = replaceTypeVariables False

typePatternBindings :: Ty -> Ty -> Maybe (Map.Map String Ty)
typePatternBindings patternTy actualTy = case (normalizeFun patternTy, normalizeFun actualTy) of
  (TyVar name, ty) -> Just (Map.singleton name ty)
  (TyMeta ident, ty) -> Just (Map.singleton ("?" ++ show ident) ty)
  (TyCon x, TyCon y) | x == y -> Just Map.empty
  (applicationView -> Just (x, xs), applicationView -> Just (y, ys))
    | x == y && length xs == length ys -> zipWithExact typePatternBindings xs ys >>= mergeBindingMaps
    | VariableHead name <- x,
      length xs <= length ys -> do
        let (captured, supplied) = splitAt (length ys - length xs) ys
        bindings <- zipWithExact typePatternBindings xs supplied >>= mergeBindingMaps
        mergeBindingMaps [Map.singleton name (applyApplicationHead y captured), bindings]
  (TyRecord xs, TyRecord ys)
    | Map.keys xs == Map.keys ys -> traverse recordField (Map.toList xs) >>= mergeBindingMaps
    where
      recordField (name, x) = Map.lookup name ys >>= typePatternBindings x
  (TyFun xs xEffs xRet, TyFun ys yEffs yRet)
    | length xs == length ys && length xEffs == length yEffs ->
        zipWithExact typePatternBindings (NE.toList xs ++ map effectAsTy xEffs ++ [xRet]) (NE.toList ys ++ map effectAsTy yEffs ++ [yRet]) >>= mergeBindingMaps
  _ -> Nothing

zipWithExact :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
zipWithExact _ [] [] = Just []
zipWithExact f (x : xs) (y : ys) = (:) <$> f x y <*> zipWithExact f xs ys
zipWithExact _ _ _ = Nothing

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
  TyCon _ -> True
  TyApplication (ConstructorHead (StructuralConstructor _)) _ -> True
  TyNamed {} -> True
  TyFun {} -> True
  _ -> False

constraintMayRemainOpen :: Constraint -> Bool
constraintMayRemainOpen (Constraint args _) = not (all hasConcreteHead args)

showConstraints :: [Constraint] -> String
showConstraints = intercalate ", " . map showConstraint

showConstraint :: Constraint -> String
showConstraint (Constraint args ClassRef {classDisplayName}) = showTyList args ++ " " ++ classDisplayName

-- purity permitteþ generalisation only of variables not owned by the enclosing
-- environment. captured variables retain their identity across local aliases.
generalizeApplied :: TcContext -> Ty -> [Constraint] -> TcState -> Scheme
generalizeApplied TcContext {tcEnv} ty constraints st =
  let appliedTy = applyState ty st
      appliedConstraints = applyStateConstraints constraints st
      rows = connectedRowEqualities (appliedTy : [arg | Constraint args _ <- appliedConstraints, arg <- args]) (map (applyRowEquality st) (tcRowEqualities st))
      rowTypes = map rowEqualityType rows
      bindingTypes = TyRecord (Map.fromList (zip (map show [0 :: Int ..]) (appliedTy : rowTypes)))
      freeInBinding binding =
        let (bound, constraints, ty) = schemeParts (envScheme binding)
            freeState =
              st
                { tcTypeHeads = foldr Map.delete (tcTypeHeads st) bound,
                  tcEffectRows = foldr (Map.delete . RowVariable) (tcEffectRows st) bound
                }
            constraints2 = applyStateConstraints constraints freeState
            ty2 = applyState (TyRecord (Map.fromList (zip (map show [0 :: Int ..]) (ty : map rowEqualityType (schemeRows (envScheme binding)))))) freeState
         in (metaIdsInConstraints constraints2 (metaIdsInTy ty2 []), filter (`notElem` bound) (typeVarsInConstraints constraints2 (typeVarsInTy ty2 [])))
      (environmentMetas, environmentVars) = unzip (map freeInBinding tcEnv)
      used = nub (concat environmentVars ++ typeVarsInConstraints appliedConstraints (allTypeNames bindingTypes))
      metas = filter (`notElem` concat environmentMetas) (metaIdsInConstraints appliedConstraints (metaIdsInTy bindingTypes []))
      replacements = IntMap.fromList (zip metas (metaVarNames used metas))
      ty2 = replaceTyMetas replacements appliedTy
      constraints2 = map (replaceConstraintMetas replacements) appliedConstraints
      rows2 = map (mapRowEquality (replaceTyMetas replacements)) rows
      variables = filter (`notElem` concat environmentVars) (typeVarsInConstraints constraints2 (typeVarsInTy (replaceTyMetas replacements bindingTypes) []))
      scheme = Forall variables constraints2 ty2
   in if null rows2 then scheme else Constrained rows2 scheme

connectedRowEqualities :: [Ty] -> [RowEquality] -> [RowEquality]
connectedRowEqualities types equations = go [] types
  where
    go selected known =
      let vars = foldr typeVarsInTy [] known
          metas = foldr metaIdsInTy [] known
          touches equation = let ty = rowEqualityType equation in any (`elem` vars) (typeVarsInTy ty []) || any (`elem` metas) (metaIdsInTy ty [])
          added = filter (\row -> row `notElem` selected && touches row) equations
       in if null added then selected else go (selected ++ added) (known ++ map rowEqualityType added)

generalizeTy :: Ty -> Scheme
generalizeTy = generalizeTyWithConstraints []

generalizeTyWithConstraints :: [Constraint] -> Ty -> Scheme
generalizeTyWithConstraints constraints ty =
  let (constraints2, ty2) = replaceMetasForGeneralization constraints ty
   in Forall (typeVarsInConstraints constraints2 (typeVarsInTy ty2 [])) constraints2 ty2

replaceMetasForGeneralization :: [Constraint] -> Ty -> ([Constraint], Ty)
replaceMetasForGeneralization constraints ty =
  let used = typeVarsInConstraints constraints (allTypeNames ty)
      metaIds = metaIdsInConstraints constraints (metaIdsInTy ty [])
      names = metaVarNames used metaIds
      repls = IntMap.fromList (zip metaIds names)
   in (map (replaceConstraintMetas repls) constraints, replaceTyMetas repls ty)

allTypeNames :: Ty -> [String]
allTypeNames ty = foldTyWith collect collect ty []
  where
    collect (TyForall (parameterNames -> names) _) acc = foldr addUnique acc names
    collect (TyVar name) acc = addUnique name acc
    collect (TyApplication (VariableHead name) _) acc = addUnique name acc
    collect (TyRowVariable (RowVariable name)) acc = addUnique name acc
    collect _ acc = acc

metaIdsInConstraints :: [Constraint] -> [Int] -> [Int]
metaIdsInConstraints = foldConstraintTypes metaIdsInTy

metaIdsInTy :: Ty -> [Int] -> [Int]
metaIdsInTy = foldTyWith collect collect
  where
    collect (TyMeta ident) acc = addUnique ident acc
    collect _ acc = acc

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

replaceConstraintMetas :: IntMap.IntMap String -> Constraint -> Constraint
replaceConstraintMetas repls (Constraint args name) = Constraint (map (replaceTyMetas repls) args) name

replaceTyMetas :: IntMap.IntMap String -> Ty -> Ty
replaceTyMetas repls ty = case ty of
  TyMeta ident -> maybe ty TyVar (IntMap.lookup ident repls)
  _ -> mapTyChildren (replaceTyMetas repls) (replaceTyMetas repls) ty

-- source-checking entry points share this pipeline and differ only in their
-- top-level mode. context inspection intentionally stoppeþ before Core lowering.
check :: String -> String
check source = checkWithImports source Map.empty

checkWithImports :: String -> Map.Map String String -> String
checkWithImports source imports = checkPrepared source imports False

checkEditorProgramPrepared :: Program -> Map.Map String SourceUnit -> Maybe TypeFailure
checkEditorProgramPrepared program imports
  | looksRunnable program = case checkMode Runnable of
      Right _ -> Nothing
      Left runnableFailure -> case checkMode ModuleMode of
        Right _ -> Nothing
        Left _ -> Just runnableFailure
  | otherwise = either Just (const Nothing) (checkMode ModuleMode)
  where
    checkMode mode = inferProgramWithModeDetailed program imports mode

checkProgramPrepared :: Program -> Map.Map String SourceUnit -> Bool -> Maybe TypeFailure
checkProgramPrepared program imports runnable =
  either Just (const Nothing) (inferProgramWithModeDetailed program imports (if runnable then Runnable else ModuleMode))

checkRunnableWithImports :: String -> Map.Map String String -> String
checkRunnableWithImports source imports = checkPrepared source imports True

typeOfWithImports :: String -> Map.Map String String -> String -> Either String String
typeOfWithImports source imports = typeOfBundle (prepareBundle source imports)

typeOfBundle :: ParsedBundle -> String -> Either String String
typeOfBundle ParsedBundle {bundleRoot, bundleImports} name = do
  program <- either (Left . failureMessage) (Right . parsedProgram) (unitParsed bundleRoot)
  case runTc (inferProgramContextFromM [] program bundleImports baseContext) initialTcState of
    TcErr message -> Left message
    TcOk context state -> case lookupEnv name context of
      EnvFound scheme -> Right (showScheme state scheme)
      EnvMissing -> Left ("unknown name '" ++ name ++ "'")
      EnvAmbiguous message -> Left message

showScheme :: TcState -> Scheme -> String
showScheme state scheme =
  let (_, constraints, ty) = schemeParts scheme
      typeText = showSourceTy (applyState ty state)
      constraintText = if null constraints then "" else intercalate ", " (map (("byzen " ++) . showSourceConstraint) (applyStateConstraints constraints state)) ++ "; "
      rows = map (applyRowEquality state) (schemeRows scheme)
      rowText = if null rows then "" else " where " ++ intercalate ", " ["{" ++ showEffects xs ++ "} = {" ++ showEffects ys ++ "}" | RowEquality xs ys <- rows]
   in constraintText ++ typeText ++ rowText

showSourceTy :: Ty -> String
showSourceTy = \case
  TyMeta ident -> "?" ++ show ident
  TyVar name -> name
  TyRowVariable (RowVariable name) -> name
  TyEffectSkolem ident -> "$effect" ++ show ident
  TyCon name -> name
  TySkolem ident arguments -> unwords (skolemName ident : map showSourceTy arguments)
  TyForall (parameterNames -> names) body -> case body of
    TyFun {} -> "[" ++ intercalate ", " (map ('@' :) names) ++ ", " ++ drop 1 (showSourceTy body)
    _ -> "forall " ++ unwords names ++ ". " ++ showSourceTy body
  TyApp name arguments -> unwords (map showSourceTy arguments ++ [name])
  TyNamed TypeRef {typeDisplayName} arguments -> unwords (map showSourceTy arguments ++ [typeDisplayName])
  TyEffect EffectRef {effectDisplayName} arguments -> unwords (map showSourceTy arguments ++ [effectDisplayName])
  TyRecord fields -> "r{" ++ intercalate ", " [name ++ ": " ++ showSourceTy ty | (name, ty) <- Map.toList fields] ++ "}"
  TyFun arguments effects result ->
    "["
      ++ intercalate ", " (map showSourceTy (NE.toList arguments) ++ [showSourceTy result])
      ++ (if null effects then "" else "; " ++ intercalate ", " (map (showSourceTy . effectAsTy) effects))
      ++ "]"

showSourceConstraint :: Constraint -> String
showSourceConstraint (Constraint arguments ClassRef {classDisplayName}) = case map showSourceTy arguments of
  [] -> classDisplayName
  first : rest -> unwords (first : classDisplayName : rest)

checkPrepared :: String -> Map.Map String String -> Bool -> String
checkPrepared source imports runnable =
  let ParsedBundle {bundleRoot, bundleImports} = prepareBundle source imports
   in case unitParsed bundleRoot of
        Left failure -> "parse error: " ++ failureMessage failure
        Right ParsedSource {parsedProgram} -> maybe "type ok" (("type error: " ++) . typeFailureMessage) (checkProgramPrepared parsedProgram bundleImports runnable)

looksRunnable :: Program -> Bool
looksRunnable (Program ds) = any declaresMain ds

inferProgramWithModeDetailed :: Program -> Map.Map String SourceUnit -> ProgramMode -> Either TypeFailure CoreProgram
inferProgramWithModeDetailed program imports mode =
  fst <$> runStateT (unTc (inferProgramWithModeM program imports mode)) initialTcState

elaboratePrepared :: Program -> Map.Map String SourceUnit -> Bool -> Either String CoreProgram
elaboratePrepared program imports runnable =
  either (Left . typeFailureMessage) Right (inferProgramWithModeDetailed program imports (if runnable then Runnable else Interactive))

elaborateProgramWithImports :: Program -> Map.Map String String -> Bool -> Either String CoreProgram
elaborateProgramWithImports program imports runnable = elaborateWithMode program imports (if runnable then Runnable else ModuleMode)

elaborateInteractiveProgramWithImports :: Program -> Map.Map String String -> Either String CoreProgram
elaborateInteractiveProgramWithImports program imports = elaborateWithMode program imports Interactive

elaborateWithMode :: Program -> Map.Map String String -> ProgramMode -> Either String CoreProgram
elaborateWithMode program imports mode =
  either (Left . typeFailureMessage) Right (inferProgramWithModeDetailed program (prepareSource <$> imports) mode)

inferProgramWithModeM :: Program -> Map.Map String SourceUnit -> ProgramMode -> Tc CoreProgram
inferProgramWithModeM program imports mode = do
  (elaboratedDecls, checkedCtx) <- checkProgramWithModeM mode [] program imports baseContext
  lowerProgramM elaboratedDecls checkedCtx

checkProgramWithModeM :: ProgramMode -> ImportStack -> Program -> Map.Map String SourceUnit -> TcContext -> Tc ([Decl], TcContext)
checkProgramWithModeM mode importStack (Program ds) imports baseCtx = do
  (typedDs, preparedCtx) <- prepareDeclsM importStack ds imports baseCtx
  when (mode == Runnable && not (any declaresMain typedDs)) (failTc "runnable file must declare main")
  let initialCtx = collectElaborationProgramContext typedDs preparedCtx
  result@(_, checkedCtx) <- checkAndElaborateDeclsM (mode == Interactive) typedDs initialCtx
  when (mode == Runnable) (checkMainM checkedCtx)
  pure result

lowerProgramM :: [Decl] -> TcContext -> Tc CoreProgram
lowerProgramM elaboratedDecls checkedCtx = do
  importedCores <- tcImportCoreCache <$> getTc
  let interface = moduleInterface checkedCtx
      elaborated = Program elaboratedDecls
  -- core construction is þe final assertion þat no inference placeholder leaked.
  either failTc pure (makeCoreProgramWithImports interface (termTargets checkedCtx) elaborated importedCores)

collectElaborationContext :: Decl -> TcContext -> TcContext
collectElaborationContext declaration ctx = case declaration of
  Let {} -> ctx
  Export nested -> collectElaborationContext nested ctx
  other -> collectDeclContext other ctx

collectElaborationProgramContext :: [Decl] -> TcContext -> TcContext
collectElaborationProgramContext declarations ctx =
  foldl' (flip collectElaborationContext) (collectClassContexts declarations ctx) declarations

inferProgramContext :: Program -> Map.Map String String -> TcContext -> TcResult TcContext
inferProgramContext p imports baseCtx = runTc (inferProgramContextFromM [] p (prepareSource <$> imports) baseCtx) initialTcState

inferProgramContextFromM :: ImportStack -> Program -> Map.Map String SourceUnit -> TcContext -> Tc TcContext
inferProgramContextFromM importStack program imports baseCtx =
  snd <$> checkProgramWithModeM ModuleMode importStack program imports baseCtx

prepareDeclsM :: ImportStack -> [Decl] -> Map.Map String SourceUnit -> TcContext -> Tc ([Decl], TcContext)
prepareDeclsM importStack ds imports baseCtx = do
  either failTc pure (validateProgram (Program ds))
  importCtx <- collectImportContextsM importStack ds imports baseCtx
  (Program normalised, kinds) <- either failTc pure (Kind.checkKinds (tcKinds importCtx) (Program ds))
  preparedCtx <- resolveTypeHeadersM normalised (importCtx {tcKinds = kinds})
  pure (normalised, preparedCtx)

-- imports are checked in isolated states. þeir static contexts and lowered
-- core modules are cached by public path and cross back as checked artifacts.
collectImportContextsM :: ImportStack -> [Decl] -> Map.Map String SourceUnit -> TcContext -> Tc TcContext
collectImportContextsM importStack ds imports ctx = foldM step ctx ds
  where
    step currentCtx (Import path alias) = importContext currentCtx path alias
    step currentCtx _ = pure currentCtx

    importContext currentCtx path alias = do
      nextStack <- either failTc pure (enterImport importStack path)
      cached <- Map.lookup path . tcImportCache <$> getTc
      importedCtx <- case cached of
        Just context -> pure context
        Nothing -> do
          source <- maybe (failTc ("missing use '" ++ path ++ "'")) pure (Map.lookup path imports)
          p <- either (failImportTc path) (pure . parsedProgram) (unitParsed source)
          importedContextM nextStack path p imports
      let canonical = importNamespace path
          namespaced = aliasImportContext canonical (importAlias path alias) (namespaceImportContext canonical importedCtx)
      pure (mergeContext namespaced currentCtx)

importedContextM :: ImportStack -> String -> Program -> Map.Map String SourceUnit -> Tc TcContext
importedContextM importStack path p imports = Tc $ StateT $ \st ->
  let importedState =
        initialTcState
          { tcImportCache = tcImportCache st,
            tcImportCoreCache = tcImportCoreCache st
          }
   in case runStateT (unTc imported) importedState of
        Left failure ->
          Left
            failure
              { typeFailureMessage = "in use '" ++ path ++ "': " ++ typeFailureMessage failure,
                typeFailurePath = typeFailurePath failure <|> Just path
              }
        Right ((importedCtx, importedCore), importedSt) ->
          let contextCache = Map.insert path importedCtx (tcImportCache importedSt)
              coreCache = Map.insert path importedCore (tcImportCoreCache importedSt)
           in Right
                ( importedCtx,
                  st
                    { tcImportCache = contextCache,
                      tcImportCoreCache = coreCache
                    }
                )
  where
    imported = inferImportedProgramM importStack path p imports

inferImportedProgramM :: ImportStack -> String -> Program -> Map.Map String SourceUnit -> Tc (TcContext, CoreProgram)
inferImportedProgramM importStack path program imports = do
  let moduleContext = baseContext {tcModule = SourceModule path}
  (elaboratedDecls, checkedCtx) <- checkProgramWithModeM ModuleMode importStack program imports moduleContext
  core <- lowerProgramM elaboratedDecls checkedCtx
  pure (checkedCtx, core)
