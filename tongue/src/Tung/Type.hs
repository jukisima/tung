{- | static semantics: imports and visibility, Hindley-Milner inference,
effect rows, coverage, frames and fills, and elaboration to dictionary passing.
-}
module Tung.Type (
  Ty (..),
  Scheme (..),
  EnvLookup (..),
  EnvBinding (..),
  TcContext (..),
  TcState (..),
  TcResult (..),
  TypeFailure (..),
  ShapeInfo (..),
  check,
  checkWithImports,
  checkProgramWithImportsDetailed,
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
)
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, replicateM, unless, void, when, zipWithM, zipWithM_)
import Control.Monad.Trans.State.Strict (StateT (..), get, put, runStateT)
import Data.Char (isAscii, isDigit, isLower)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.IntMap.Strict qualified as IntMap
import Data.List (find, intercalate, isPrefixOf, nub, partition, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Traversable (mapAccumM)
import Tung.Core (CoreProgram, ModuleInterface (..), makeCoreProgramWithImports)
import Tung.Coverage qualified as Coverage
import Tung.Identity (EffectRef (..), FillId (..), FillType (..), ModuleId (..), ShapeRef (..), SymbolId (..), TermExport (..), TermKind (..), TypeRef (..), isPrimitiveFillId, renderFillId)
import Tung.Import (ImportStack, enterImport)
import Tung.Name (importAlias, importNamespace, isQualifiedName, lastQualifiedSegment, replaceNamespace, splitFieldAccessName, splitQualifiedName)
import Tung.Parse (ParsedSource (..), parse, parseLocated)
import Tung.Primitive
import Tung.Syntax
import Tung.Token (SourceSpan)
import Tung.Validate (validateProgram)

-- function effects are latent on 'TyFun'; 'Inferred' carrieþ only effects caused
-- while evaluating the current expression, plus unresolved frame requirements.
data Ty
  = TyMeta Int
  | TyVar String
  | TyCon String
  | TyApp String [Ty]
  | TyNamed TypeRef [Ty]
  | TyEffect EffectRef [Ty]
  | TyRecord (Map.Map String Ty)
  | TyFun (NonEmpty Ty) [Ty] Ty
  deriving (Eq, Show)

data Need = Need [Ty] ShapeRef deriving (Eq, Show)

-- resolved member annotations preserve graiþ identities; surface syntax stayeþ available for Core.
data ResolvedTypeAnn = ResolvedTypeAnn Ty [Need] deriving (Eq, Show)

data Inferred = Inferred
  { inferredTy :: Ty
  , inferredEffects :: [Ty]
  , inferredNeeds :: [Need]
  , inferredExpr :: Expr
  }
  deriving (Eq, Show)

data Scheme
  = Forall [String] [Need] Ty
  | EffectOpForall EffectRef [String] [Need] Ty
  deriving (Eq, Show)

data EnvLookup = EnvFound Scheme | EnvMissing | EnvAmbiguous String deriving (Eq, Show)

data EnvBinding = EnvBinding
  { envName :: String
  , envScheme :: Scheme
  , envRank :: Int
  , envOrigin :: String
  , envTarget :: Maybe TermExport
  }
  deriving (Eq, Show)

data EnvChoices = EnvChoices [EnvBinding] | ChoicesMissing | ChoicesAmbiguous String deriving (Eq, Show)

data TcResult a = TcOk a TcState | TcErr String deriving (Eq, Show)

data TypeFailure = TypeFailure
  { typeFailureMessage :: String
  , typeFailureSpan :: Maybe SourceSpan
  , typeFailurePath :: Maybe FilePath
  }
  deriving (Eq, Show)

-- these modes share inference but impose different top-level computation bounds.
data ProgramMode = Bookhoard | Interactive | Runnable deriving (Eq)

data TypeConstructor = StructuralConstructor String | NamedConstructor TypeRef deriving (Eq, Show)

data TyHead = TyHead TypeConstructor [Ty] deriving (Eq, Show)

data AliasHeader = AliasHeader
  { aliasHeaderName :: String
  , aliasHeaderParams :: [String]
  , aliasHeaderTarget :: TypeExpr
  }

data DataHeader = DataHeader
  { dataHeaderName :: String
  , dataHeaderParams :: [String]
  , dataHeaderConstructors :: [Ctor]
  }

data DisplayNames = DisplayNames
  { displayTypes :: Map.Map SymbolId String
  , displayEffects :: Map.Map SymbolId String
  , displayShapes :: Map.Map SymbolId String
  }

emptyDisplayNames :: DisplayNames
emptyDisplayNames = DisplayNames Map.empty Map.empty Map.empty

-- rigid effect skolems are structural inference artifacts; every source-level
-- effect head is an 'EffectRef' and therefore compareþ by 'SymbolId'.
data EffectHead = NominalEffect EffectRef | RigidEffect String deriving (Eq, Show)

-- 'TcContext' holds static names, types, and resolved runtime identities;
-- 'TcState' holds substitutions, import results, and evidence holes for one check.
data TcState = TcState
  { tcNextMeta :: !Int
  , tcMetaSubst :: !(IntMap.IntMap Ty)
  , tcTypeHeads :: !(Map.Map String TyHead)
  , tcEffectRows :: !(Map.Map String [Ty])
  , tcImportCache :: !(Map.Map String TcContext)
  , tcImportCoreCache :: !(Map.Map String CoreProgram)
  , tcEvidenceNeeds :: !(IntMap.IntMap Need)
  , tcCurrentSpan :: !(Maybe SourceSpan)
  , tcLocateImports :: !Bool
  }
  deriving (Eq, Show)

-- primitive entries exist only when a module explicitly showeþ a built-in type;
-- unlike aliases, they remain opaque during unification.
data TypeInfo
  = Primitive String
  | Alias String [String] Ty
  | DataInfo TypeRef [String] [ConstructorInfo]
  | EffectInfo EffectRef [String]
  | ShownType TypeInfo
  deriving (Eq, Show)

data ConstructorInfo = ConstructorInfo
  { constructorInfoTarget :: SymbolId
  , constructorInfoFields :: [Ty]
  }
  deriving (Eq, Show)

data ShapeMethod = ShapeMethod
  { shapeMethodName :: String
  , shapeMethodType :: ResolvedTypeAnn
  }
  deriving (Eq, Show)

data ShapeInfo = ShapeInfo
  { shapeParams :: [String]
  , shapeNeeds :: [Need]
  , shapeMethods :: [ShapeMethod]
  , shapeExported :: Bool
  , shapeTarget :: SymbolId
  }
  deriving (Eq, Show)

data FillInfo = FillInfo
  { fillShape :: ShapeRef
  , fillTypes :: [Ty]
  , fillNeeds :: [Need]
  , fillKey :: FillId
  , fillDepth :: Int
  , fillParents :: [Need]
  , fillMembers :: [String]
  }
  deriving (Eq, Show)

data TcContext = TcContext
  { tcModule :: ModuleId
  , tcEnv :: [EnvBinding]
  , tcTypes :: Map.Map String TypeInfo
  , tcTypeRegistry :: Map.Map SymbolId TypeInfo
  , tcTypeAmbiguities :: Map.Map String [String]
  , tcShapes :: Map.Map String ShapeInfo
  , tcShapeRegistry :: Map.Map SymbolId ShapeInfo
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
    , tcImportCoreCache = Map.empty
    , tcEvidenceNeeds = IntMap.empty
    , tcCurrentSpan = Nothing
    , tcLocateImports = False
    }

runTc :: Tc a -> TcState -> TcResult a
runTc computation st = case runStateT (unTc computation) st of
  Left failure -> TcErr (typeFailureMessage failure)
  Right (value, st2) -> TcOk value st2

failTc :: String -> Tc a
failTc message = Tc $ StateT $ \state -> Left (TypeFailure message (tcCurrentSpan state) Nothing)

failImportTc :: FilePath -> String -> Tc a
failImportTc path message = Tc $ StateT $ \state -> Left (TypeFailure message (tcCurrentSpan state) (Just path))

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
  TyNamed ref args -> TyNamed ref (map mapTy args)
  TyEffect effect args -> TyEffect effect (map mapTy args)
  TyRecord fields -> TyRecord (Map.map mapTy fields)
  TyFun args effects ret -> TyFun (fmap mapTy args) (map mapEffect effects) (mapTy ret)
  ty -> ty

-- visit a type before its children. þe second visitor applieþ only to direct
-- effect-row entries, so row variables can retain þeir special semantics.
foldTyWith :: (Ty -> a -> a) -> (Ty -> a -> a) -> Ty -> a -> a
foldTyWith visitType visitEffect = go
 where
  go ty acc = descend ty (visitType ty acc)
  goEffect ty acc = descend ty (visitEffect ty acc)
  descend ty acc = case ty of
    TyApp _ args -> foldl' (flip go) acc args
    TyNamed _ args -> foldl' (flip go) acc args
    TyEffect _ args -> foldl' (flip go) acc args
    TyRecord fields -> foldl' (flip go) acc (Map.elems fields)
    TyFun args effects ret ->
      go ret (foldl' (flip goEffect) (foldl' (flip go) acc (NE.toList args)) effects)
    _ -> acc

foldNeedTypes :: (Ty -> a -> a) -> [Need] -> a -> a
foldNeedTypes visit needs acc = foldl' visitNeed acc needs
 where
  visitNeed current (Need args _) = foldl' (flip visit) current args

-- import projection changeþ only diagnostic spellings; nominal equality and
-- all runtime identities remain untouched.
relabelTypeRefs :: DisplayNames -> (String -> String) -> Ty -> Ty
relabelTypeRefs names fallback = go
 where
  go (TyNamed ref args) = TyNamed (relabelTypeRef names fallback ref) (map go args)
  go (TyEffect effect args) = TyEffect (relabelEffectRef names fallback effect) (map go args)
  go ty = mapTyChildren go go ty

relabelNeedRefs :: DisplayNames -> (String -> String) -> Need -> Need
relabelNeedRefs names fallback (Need args frame) =
  Need (map (relabelTypeRefs names fallback) args) (relabelShapeRef names fallback frame)

relabelTypeRef :: DisplayNames -> (String -> String) -> TypeRef -> TypeRef
relabelTypeRef DisplayNames{displayTypes} fallback ref@TypeRef{typeIdentity, typeDisplayName} =
  ref{typeDisplayName = Map.findWithDefault (fallback typeDisplayName) typeIdentity displayTypes}

relabelEffectRef :: DisplayNames -> (String -> String) -> EffectRef -> EffectRef
relabelEffectRef DisplayNames{displayEffects} fallback ref@EffectRef{effectIdentity, effectDisplayName} =
  ref{effectDisplayName = Map.findWithDefault (fallback effectDisplayName) effectIdentity displayEffects}

relabelShapeRef :: DisplayNames -> (String -> String) -> ShapeRef -> ShapeRef
relabelShapeRef DisplayNames{displayShapes} fallback ref@ShapeRef{shapeIdentity, shapeDisplayName} =
  ref{shapeDisplayName = Map.findWithDefault (fallback shapeDisplayName) shapeIdentity displayShapes}

relabelSchemeTypeRefs :: DisplayNames -> (String -> String) -> Scheme -> Scheme
relabelSchemeTypeRefs names fallback = \case
  Forall vars needs ty -> Forall vars (map relabelNeed needs) (relabelTy ty)
  EffectOpForall owner vars needs ty ->
    EffectOpForall (relabelEffectRef names fallback owner) vars (map relabelNeed needs) (relabelTy ty)
 where
  relabelTy = relabelTypeRefs names fallback
  relabelNeed = relabelNeedRefs names fallback

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

removeEffect :: EffectRef -> [Ty] -> [Ty]
removeEffect effect = filter (not . effectHasRef effect)

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
effectVarOccurs name ty = foldTyWith found found ty False
 where
  found (TyVar actual) acc = actual == name || acc
  found _ acc = acc

isEffectVarTy :: Ty -> Bool
isEffectVarTy (TyVar name) = isEffectVarName name
isEffectVarTy _ = False

isEffectVarName :: String -> Bool
isEffectVarName ('e' : rest) = all isTypeVariableIndex rest
isEffectVarName _ = False

isTypeVariableIndex :: Char -> Bool
isTypeVariableIndex c = isDigit c || c `elem` ("₀₁₂₃₄₅₆₇₈₉" :: String)

unifyCommonEffectsM :: [Ty] -> [Ty] -> Tc ()
unifyCommonEffectsM xs ys =
  mapM_ (\x -> maybe (pure ()) (unifyEffectM x) (findEffectByHead x ys)) xs

unifyEffectM :: Ty -> Ty -> Tc ()
unifyEffectM x y = case (effectHeadAndArgs x, effectHeadAndArgs y) of
  (Just (xn, xs), Just (yn, ys)) | xn == yn -> unifyListsM xs ys
  _ -> failTc ("cannot unify effect " ++ showEffect x ++ " with " ++ showEffect y)

findEffectByHead :: Ty -> [Ty] -> Maybe Ty
findEffectByHead x = find (sameEffectHead x)

containsEffectHead :: Ty -> [Ty] -> Bool
containsEffectHead x = any (sameEffectHead x)

sameEffectHead :: Ty -> Ty -> Bool
sameEffectHead x y = case (effectHeadAndArgs x, effectHeadAndArgs y) of
  (Just (xn, _), Just (yn, _)) -> xn == yn
  _ -> False

effectHasRef :: EffectRef -> Ty -> Bool
effectHasRef expected effect = case effectHeadAndArgs effect of
  Just (NominalEffect actual, _) -> expected == actual
  Nothing -> False
  Just (RigidEffect _, _) -> False

effectHeadAndArgs :: Ty -> Maybe (EffectHead, [Ty])
effectHeadAndArgs (TyEffect effect args) = Just (NominalEffect effect, args)
effectHeadAndArgs (TyApp name args)
  | "$effect" `isPrefixOf` name = Just (RigidEffect name, args)
effectHeadAndArgs (TyCon name)
  | "$effect" `isPrefixOf` name = Just (RigidEffect name, [])
effectHeadAndArgs _ = Nothing

showEffects :: [Ty] -> String
showEffects = intercalate ", " . map showEffect

showEffect :: Ty -> String
showEffect (TyVar name) = name
showEffect (TyEffect EffectRef{effectDisplayName} []) = effectDisplayName
showEffect (TyEffect EffectRef{effectDisplayName} args) = showTyList args ++ " " ++ effectDisplayName
showEffect (TyApp name []) | "$effect" `isPrefixOf` name = name
showEffect (TyApp name args) | "$effect" `isPrefixOf` name = showTyList args ++ " " ++ name
showEffect other = showTy other

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
    (TyApp x xs, TyApp y ys) | x == y -> unifyListsM xs ys
    (TyNamed x xs, TyNamed y ys) | x == y -> unifyListsM xs ys
    (TyEffect x xs, TyEffect y ys) | x == y -> unifyListsM xs ys
    (TyApp x xs, TyApp y ys) | isTypeVarName x -> unifyTypeHeadM x (StructuralConstructor y) xs ys
    (TyApp x xs, TyApp y ys) | isTypeVarName y -> unifyTypeHeadM y (StructuralConstructor x) ys xs
    (TyApp x xs, TyNamed y ys) | isTypeVarName x -> unifyTypeHeadM x (NamedConstructor y) xs ys
    (TyNamed x xs, TyApp y ys) | isTypeVarName y -> unifyTypeHeadM y (NamedConstructor x) ys xs
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
normalizeFun (TyNamed ref args) = TyNamed ref (map normalizeFun args)
normalizeFun (TyEffect effect args) = TyEffect effect (map normalizeFun args)
normalizeFun (TyRecord fields) = TyRecord (Map.map normalizeFun fields)
normalizeFun ty = ty

unifyListsM :: [Ty] -> [Ty] -> Tc ()
unifyListsM xs ys
  | length xs == length ys = zipWithM_ unifyM xs ys
  | otherwise = failTc ("arity mismatch: [" ++ showTyList xs ++ "] vs [" ++ showTyList ys ++ "]")

unifyTypeHeadM :: String -> TypeConstructor -> [Ty] -> [Ty] -> Tc ()
unifyTypeHeadM varName actualHead varArgs actualArgs = do
  st <- getTc
  case lookupTypeHead varName st of
    Just boundHead -> unifyM (applyTypeHead boundHead varArgs) (applyTypeConstructor actualHead actualArgs)
    Nothing | StructuralConstructor actualName <- actualHead, isTypeVarName actualName -> unifyListsM varArgs actualArgs
    Nothing
      | length actualArgs >= length varArgs -> do
          let (captured, supplied) = splitAt (length actualArgs - length varArgs) actualArgs
          bindTypeHeadM varName (TyHead actualHead captured)
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
            [onlyResult] -> bindTypeHeadM varName (TyHead (StructuralConstructor "func") [arg]) >> unifyM onlyResult result
            [fromArg, toArg] -> bindTypeHeadM varName (TyHead (StructuralConstructor "func") []) >> unifyM fromArg arg >> unifyM toArg result
            _ -> failTc "func expecteþ one or two type arguments in function unification"

lookupTypeHead :: String -> TcState -> Maybe TyHead
lookupTypeHead name TcState{tcTypeHeads} = Map.lookup name tcTypeHeads

bindTypeHeadM :: String -> TyHead -> Tc ()
bindTypeHeadM name value = do
  st@TcState{tcTypeHeads} <- getTc
  putTc st{tcTypeHeads = Map.insert name value tcTypeHeads}

applyTypeHead :: TyHead -> [Ty] -> Ty
applyTypeHead (TyHead constructor prefixArgs) args = applyTypeConstructor constructor (prefixArgs ++ args)

applyTypeConstructor :: TypeConstructor -> [Ty] -> Ty
applyTypeConstructor (StructuralConstructor name) = tyAppOrFunc name
applyTypeConstructor (NamedConstructor ref) = TyNamed ref

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
occursMeta ident ty st = ident `elem` metaIdsInTy (applyState ty st) []

applyState :: Ty -> TcState -> Ty
applyState ty st@TcState{tcMetaSubst, tcTypeHeads} = go ty
 where
  go = \case
    TyMeta ident -> maybe (TyMeta ident) go (IntMap.lookup ident tcMetaSubst)
    TyVar name -> maybe (TyVar name) (`applyTypeHead` []) (Map.lookup name tcTypeHeads)
    TyCon name -> TyCon name
    TyApp name args -> maybe (tyAppOrFunc name (map go args)) (\headSubst -> go (applyTypeHead headSubst (map go args))) (Map.lookup name tcTypeHeads)
    TyNamed ref args -> TyNamed ref (map go args)
    TyEffect effect args -> TyEffect effect (map go args)
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

schemeEffectOwner :: Scheme -> Maybe EffectRef
schemeEffectOwner = \case
  EffectOpForall owner _ _ _ -> Just owner
  Forall{} -> Nothing

schemeParts :: Scheme -> ([String], [Need], Ty)
schemeParts = \case
  Forall vars needs ty -> (vars, needs, ty)
  EffectOpForall _ vars needs ty -> (vars, needs, ty)

markEffectOpScheme :: EffectRef -> Scheme -> Scheme
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
    | Just (TyNamed ref prefix) <- Map.lookup name repls -> TyNamed ref (prefix ++ map (`replaceVars` repls) args)
    | otherwise -> TyApp name (map (`replaceVars` repls) args)
  _ -> mapTyChildren (`replaceVars` repls) (`replaceVars` repls) ty

replaceNeedVars :: Need -> Map.Map String Ty -> Need
replaceNeedVars (Need args name) repls = Need (map (`replaceVars` repls) args) name

typeAnnScheme :: TypeAnn -> TcContext -> Scheme
typeAnnScheme annotation ctx = typeAnnSchemeWith [] ctx annotation

typeAnnSchemeWith :: [String] -> TcContext -> TypeAnn -> Scheme
typeAnnSchemeWith bound ctx (TypeAnn ty needs) =
  generalizeTyWithNeeds (map (convertShapeNeedWith bound ctx) needs) (convertTypeExprWith bound ctx ty)

convertShapeNeed :: ShapeNeed -> TcContext -> Need
convertShapeNeed need ctx = convertShapeNeedWith [] ctx need

convertShapeNeedWith :: [String] -> TcContext -> ShapeNeed -> Need
convertShapeNeedWith bound ctx (ShapeNeed args name) =
  Need (map (convertTypeExprWith bound ctx) args) (shapeRefForName name ctx)

shapeRefForName :: String -> TcContext -> ShapeRef
shapeRefForName name ctx = case findShapeInfo name ctx of
  Just info -> ShapeRef (shapeTarget info) name
  Nothing -> ShapeRef (SymbolId (tcModule ctx) name) name

canonicalShapeName :: String -> TcContext -> String
canonicalShapeName name ctx
  | not (isQualifiedName name) = name
  | otherwise = case findShapeInfo name ctx of
      Just ShapeInfo{shapeTarget = SymbolId owner targetName}
        | owner == tcModule ctx -> name
        | SourceModule path <- owner -> importNamespace path ++ "." ++ targetName
        | otherwise -> targetName
      Nothing -> name

convertTypeExpr :: TypeExpr -> TcContext -> Ty
convertTypeExpr expression ctx = convertTypeExprWith [] ctx expression

convertTypeExprWith :: [String] -> TcContext -> TypeExpr -> Ty
convertTypeExprWith bound ctx = \case
  TypeName name
    | name `elem` bound -> TyVar name
    | otherwise -> atomTypeName name ctx
  TypeApply name args
    | name `elem` bound -> TyApp name arguments
    | otherwise -> atomAppliedTypeName name arguments ctx
   where
    arguments = map convert args
  TypeRecord fields -> TyRecord (Map.fromList [(name, convert ty) | (name, ty) <- fields])
  TypeArrow args effects ret ->
    TyFun
      (fmap convert args)
      (map convertEffect effects)
      (convert ret)
 where
  convert = convertTypeExprWith bound ctx
  convertEffect = convertEffectExprWith bound ctx

convertEffectExprWith :: [String] -> TcContext -> TypeExpr -> Ty
convertEffectExprWith bound ctx = \case
  TypeName name | name `elem` bound || isEffectVarName name -> TyVar name
  TypeName name -> maybe (TyApp name []) (`TyEffect` []) (effectRefForName name ctx)
  TypeApply name args
    | name `elem` bound -> TyApp name arguments
    | otherwise -> maybe (TyApp name arguments) (`TyEffect` arguments) (effectRefForName name ctx)
   where
    arguments = map convert args
  expression -> convertTypeExprWith bound ctx expression
 where
  convert = convertTypeExprWith bound ctx

typeExprFromAppliedTy :: Ty -> TypeExpr
typeExprFromAppliedTy = \case
  TyMeta ident -> TypeName ("t" ++ show ident)
  TyVar name -> TypeName name
  TyCon name -> TypeName name
  TyApp name args -> TypeApply name (map typeExprFromAppliedTy args)
  TyNamed TypeRef{typeDisplayName} [] -> TypeName typeDisplayName
  TyNamed TypeRef{typeDisplayName} args -> TypeApply typeDisplayName (map typeExprFromAppliedTy args)
  TyEffect EffectRef{effectDisplayName} args -> TypeApply effectDisplayName (map typeExprFromAppliedTy args)
  TyRecord fields -> TypeRecord [(name, typeExprFromAppliedTy fieldTy) | (name, fieldTy) <- Map.toList fields]
  TyFun args effects ret -> TypeArrow (fmap typeExprFromAppliedTy args) (map typeExprFromAppliedTy effects) (typeExprFromAppliedTy ret)

specializeNeed :: [String] -> [Ty] -> Need -> Need
specializeNeed params arguments (Need needArguments frame) =
  let replacements = Map.fromList (zip params arguments)
   in Need (map (replaceShapeTyParams replacements) needArguments) frame

specializeResolvedTypeAnn :: [String] -> [Ty] -> ResolvedTypeAnn -> ResolvedTypeAnn
specializeResolvedTypeAnn params fillTypes (ResolvedTypeAnn ty needs) =
  let replacements = Map.fromList (zip params fillTypes)
   in ResolvedTypeAnn (replaceShapeTyParams replacements ty) (map (replaceNeedShapeTyParams replacements) needs)

replaceNeedShapeTyParams :: Map.Map String Ty -> Need -> Need
replaceNeedShapeTyParams replacements (Need args frame) = Need (map (replaceShapeTyParams replacements) args) frame

replaceShapeTyParams :: Map.Map String Ty -> Ty -> Ty
replaceShapeTyParams replacements ty = case ty of
  TyVar name -> Map.findWithDefault ty name replacements
  TyApp name args ->
    let args2 = map (replaceShapeTyParams replacements) args
     in maybe (TyApp name args2) (`applyReplacementTy` args2) (Map.lookup name replacements)
  _ -> mapTyChildren (replaceShapeTyParams replacements) (replaceShapeTyParams replacements) ty

applyReplacementTy :: Ty -> [Ty] -> Ty
applyReplacementTy replacement args = case replacement of
  TyCon name -> TyApp name args
  TyVar name -> TyApp name args
  TyApp name existing -> TyApp name (existing ++ args)
  TyNamed ref existing -> TyNamed ref (existing ++ args)
  _ -> replacement

atomTypeName :: String -> TcContext -> Ty
atomTypeName name ctx
  | name == "func" = TyCon "func"
  | isPrimitiveTypeName name = TyCon name
  | Just ([], target) <- aliasInfo name ctx = target
  | Just ref <- typeRefForName name ctx = TyNamed ref []
  | isQualifiedName name = TyCon name
  | otherwise = TyVar name

atomAppliedTypeName :: String -> [Ty] -> TcContext -> Ty
atomAppliedTypeName "func" args _ = tyAppOrFunc "func" args
atomAppliedTypeName name args ctx = case aliasInfo name ctx of
  Just (params, target)
    | length params == length args -> replaceVars target (Map.fromList (zip params args))
  _ -> maybe (TyApp name args) (`TyNamed` args) (typeRefForName name ctx)

aliasInfo :: String -> TcContext -> Maybe ([String], Ty)
aliasInfo name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (Alias _ params target) -> Just (params, target)
  _ -> Nothing

typeRefForName :: String -> TcContext -> Maybe TypeRef
typeRefForName name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (DataInfo ref _ _) -> Just ref{typeDisplayName = name}
  _ -> Nothing

canonicalTypeName :: String -> TcContext -> Maybe String
canonicalTypeName name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (Primitive canonical) -> Just canonical
  Just (Alias canonical _ _) -> Just canonical
  Just (DataInfo TypeRef{typeDisplayName} _ _) -> Just typeDisplayName
  Just EffectInfo{} -> Nothing
  Just (ShownType _) -> Nothing
  Nothing -> Nothing

effectRefForName :: String -> TcContext -> Maybe EffectRef
effectRefForName name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (EffectInfo effect _) -> Just effect{effectDisplayName = name}
  _
    | name `elem` hostEffectNames -> Just (runtimeEffectRef name)
    | otherwise -> Nothing

runtimeEffectRef :: String -> EffectRef
runtimeEffectRef name = EffectRef (SymbolId RuntimeModule name) name

findTypeInfoForTypeSystem :: String -> TcContext -> Maybe TypeInfo
findTypeInfoForTypeSystem name TcContext{tcTypes} = Map.lookup name tcTypes

typeVarsInTy :: Ty -> [String] -> [String]
typeVarsInTy = foldTyWith collect collect
 where
  collect (TyVar name) acc = addUnique name acc
  collect (TyApp name _) acc | isTypeVarName name = addUnique name acc
  collect _ acc = acc

typeVarsInNeeds :: [Need] -> [String] -> [String]
typeVarsInNeeds = foldNeedTypes typeVarsInTy

typeHeadVarsInTy :: Ty -> [String] -> [String]
typeHeadVarsInTy = foldTyWith collect collectEffect
 where
  collect (TyApp name _) acc | isTypeVarName name = addUnique name acc
  collect _ acc = acc
  collectEffect effect acc
    | isEffectVarTy effect = acc
    | otherwise = collect effect acc

typeHeadVarsInNeeds :: [Need] -> [String] -> [String]
typeHeadVarsInNeeds = foldNeedTypes typeHeadVarsInTy

typeAppHeadsInTy :: Ty -> [String] -> [String]
typeAppHeadsInTy = foldTyWith collect collect
 where
  collect (TyApp name _) acc = addUnique name acc
  collect _ acc = acc

typeAppHeadsInNeeds :: [Need] -> [String] -> [String]
typeAppHeadsInNeeds = foldNeedTypes typeAppHeadsInTy

effectRowVarsInTy :: Ty -> [String] -> [String]
effectRowVarsInTy = foldTyWith keep collectEffect
 where
  keep _ acc = acc
  collectEffect (TyVar name) acc | isEffectVarName name = addUnique name acc
  collectEffect _ acc = acc

effectRowVarsInNeeds :: [Need] -> [String] -> [String]
effectRowVarsInNeeds = foldNeedTypes effectRowVarsInTy

addUnique :: (Eq a) => a -> [a] -> [a]
addUnique x xs = if x `elem` xs then xs else x : xs

showTy :: Ty -> String
showTy = \case
  TyMeta ident -> "?" ++ show ident
  TyVar v -> v
  TyCon c -> c
  TyApp name args -> name ++ "<" ++ showTyList args ++ ">"
  TyNamed TypeRef{typeDisplayName} [] -> typeDisplayName
  TyNamed TypeRef{typeDisplayName} args -> typeDisplayName ++ "<" ++ showTyList args ++ ">"
  TyEffect EffectRef{effectDisplayName} args -> effectDisplayName ++ "<" ++ showTyList args ++ ">"
  TyRecord fields -> "r(" ++ showRecordFields fields ++ ")"
  TyFun args effects ret ->
    let effectText = if null effects then "" else "{" ++ showEffects effects ++ "} "
     in "(" ++ showTyList (NE.toList args) ++ " \\" ++ effectText ++ showTy ret ++ ")"

showRecordFields :: Map.Map String Ty -> String
showRecordFields fields = intercalate ", " [name ++ ": " ++ showTy ty | (name, ty) <- Map.toList fields]

showTyList :: [Ty] -> String
showTyList = intercalate ", " . map showTy

baseRuntimeNames, baseEffectNames, runnerEffectNames :: [String]
baseRuntimeNames = nub (map hostName (baseNativeBindings ++ baseEffectBindings))
baseEffectNames = hostEffectNames
runnerEffectNames = filter (`notElem` ["fail", "state"]) baseEffectNames

-- this base contract must agree with evaluator arities and primitive dictionary
-- support; the bookhoard supplieþ the public declarations around these names.
baseContext :: TcContext
baseContext =
  TcContext
    { tcModule = RootModule
    , tcEnv = baseEnv
    , tcTypes = Map.empty
    , tcTypeRegistry = Map.empty
    , tcTypeAmbiguities = Map.empty
    , tcShapes = Map.empty
    , tcShapeRegistry = Map.empty
    , tcFills = indexFills primitiveFills
    }

primitiveFills :: [FillInfo]
primitiveFills =
  [ FillInfo (ShapeRef primitiveFillShapeTarget primitiveFillShape) [TyCon primitiveFillType] [] (PrimitiveFillId primitiveFillShape primitiveFillType) 0 [] []
  | PrimitiveFillSpec{..} <- primitiveFillSpecs
  ]

fillKeyFor :: ModuleId -> String -> [Ty] -> FillId
fillKeyFor owner shapeName types = DeclaredFillId sourceOwner shapeName (map fillType types)
 where
  sourceOwner = case owner of
    SourceModule{} -> Just owner
    _ -> Nothing

fillType :: Ty -> FillType
fillType ty = case normalizeFun ty of
  TyMeta ident -> FillTypeName ("?" ++ show ident)
  TyVar name -> FillTypeName name
  TyCon name -> FillTypeName name
  TyApp name arguments -> FillTypeApply name (map fillType arguments)
  TyNamed ref arguments -> FillTypeNominal ref (map fillType arguments)
  TyEffect ref arguments -> FillTypeEffect ref (map fillType arguments)
  TyRecord fields -> FillTypeRecord [(name, fillType fieldType) | (name, fieldType) <- Map.toList fields]
  TyFun arguments effects result -> FillTypeArrow (map fillType (NE.toList arguments)) (map fillType effects) (fillType result)

-- an imported context contributeþ only shown surface names. fill metadata
-- retainþ private inherited targets needed by its checked runtime dictionaries.
namespaceImportContext :: String -> TcContext -> TcContext
namespaceImportContext ns TcContext{..} =
  let preferredNames = projectedDisplayNames ns tcTypes tcShapes
   in TcContext
        { tcModule
        , tcEnv = namespaceImportEnv ns preferredNames tcEnv
        , tcTypes = namespaceImportTypes ns tcTypes
        , tcTypeRegistry
        , tcTypeAmbiguities = Map.empty
        , tcShapes = namespaceImportShapes ns preferredNames tcShapes
        , tcShapeRegistry
        , tcFills = relabelFillIndex preferredNames (namespaceDisplayName ns) tcFills
        }

projectedDisplayNames :: String -> Map.Map String TypeInfo -> Map.Map String ShapeInfo -> DisplayNames
projectedDisplayNames ns types frames =
  DisplayNames
    { displayTypes = projectedTypes typeInfoNominalIdentity
    , displayEffects = projectedTypes typeInfoEffectIdentity
    , displayShapes =
        Map.fromList
          [ (shapeTarget, namespaceDisplayName ns name)
          | (name, ShapeInfo{shapeExported = True, shapeTarget}) <- Map.toList frames
          , not (isQualifiedName name)
          ]
    }
 where
  projectedTypes identityOf =
    Map.fromList
      [ (identity, namespaceDisplayName ns name)
      | (name, info) <- Map.toList types
      , typeInfoIsShown info
      , not (isQualifiedName name)
      , Just identity <- [identityOf info]
      ]

typeInfoNominalIdentity :: TypeInfo -> Maybe SymbolId
typeInfoNominalIdentity info = case unshownTypeInfo info of
  DataInfo TypeRef{typeIdentity} _ _ -> Just typeIdentity
  Alias _ params (TyNamed TypeRef{typeIdentity} arguments)
    | arguments == map TyVar params -> Just typeIdentity
  _ -> Nothing

typeInfoEffectIdentity :: TypeInfo -> Maybe SymbolId
typeInfoEffectIdentity info = case unshownTypeInfo info of
  EffectInfo EffectRef{effectIdentity} _ -> Just effectIdentity
  _ -> Nothing

aliasImportContext :: String -> String -> TcContext -> TcContext
aliasImportContext canonical alias ctx@TcContext{..}
  | alias == canonical = ctx
  | otherwise =
      ctx
        { tcEnv = mapMaybe aliasBinding surfaceEnv ++ surfaceEnv
        , tcTypes = aliasMap (relabelAliasedTypeInfoDisplays (replaceNamespace canonical alias)) tcTypes
        , tcShapes = aliasMap (relabelShapeInfoRefs emptyDisplayNames (replaceNamespace canonical alias)) tcShapes
        , tcFills = relabelFillIndex emptyDisplayNames (replaceNamespace canonical alias) tcFills
        }
 where
  surfaceEnv = map surfaceOrigin tcEnv
  surfaceOrigin binding@EnvBinding{envScheme, envOrigin} =
    binding
      { envScheme = relabelSchemeTypeRefs emptyDisplayNames (replaceNamespace canonical alias) envScheme
      , envOrigin = replaceNamespace canonical alias envOrigin
      }
  aliasBinding binding@EnvBinding{envName} =
    (\rest -> binding{envName = alias ++ "." ++ rest}) <$> stripPrefix (canonical ++ ".") envName
  aliasMap transform entries = Map.union (Map.map transform aliased) entries
   where
    aliased = Map.mapKeys (replaceNamespace canonical alias) (Map.filterWithKey (\name _ -> canonicalPrefix name) entries)
  canonicalPrefix = isJust . stripPrefix (canonical ++ ".")

namespaceImportEnv :: String -> DisplayNames -> [EnvBinding] -> [EnvBinding]
namespaceImportEnv ns preferredNames env = concatMap one env
 where
  one binding@EnvBinding{envName = name, envScheme = scheme, envRank = rank, envOrigin = origin}
    | isBaseEnvBinding name origin = []
    | rank /= exportEnvRank || not (isShownOrigin origin) = []
    | otherwise =
        let scheme2 = relabelSchemeTypeRefs preferredNames (namespaceDisplayName ns) scheme
            origin2 = ns ++ "." ++ lastQualifiedSegment origin
            imported visibleName visibleRank =
              binding
                { envName = visibleName
                , envScheme = scheme2
                , envRank = visibleRank
                , envOrigin = origin2
                }
         in imported (lastQualifiedSegment name) aliasEnvRank : [imported (ns ++ "." ++ name) importEnvRank]

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
        withExact = Map.insert (ns ++ "." ++ name) info2 acc
     in if typeInfoIsShown info && not (isQualifiedName name)
          then Map.insert (lastQualifiedSegment name) info2 withExact
          else acc

relabelTypeInfoDisplays :: (String -> String) -> TypeInfo -> TypeInfo
relabelTypeInfoDisplays rename = \case
  primitive@Primitive{} -> primitive
  Alias name params target -> Alias (rename name) params (relabelTypeRefs emptyDisplayNames rename target)
  DataInfo ref params constructors -> DataInfo (relabelTypeRef emptyDisplayNames rename ref) params constructors
  EffectInfo effect params -> EffectInfo (relabelEffectRef emptyDisplayNames rename effect) params
  ShownType info -> ShownType (relabelTypeInfoDisplays rename info)

relabelAliasedTypeInfoDisplays :: (String -> String) -> TypeInfo -> TypeInfo
relabelAliasedTypeInfoDisplays rename = \case
  Alias name params target -> Alias name params (relabelTypeRefs emptyDisplayNames rename target)
  EffectInfo effect params -> EffectInfo (relabelEffectRef emptyDisplayNames rename effect) params
  info -> info

namespaceTypeName :: String -> String -> String
namespaceTypeName ns name
  | isPrimitiveTypeName name || isQualifiedName name = name
  | otherwise = namespaceDisplayName ns name

namespaceDisplayName :: String -> String -> String
namespaceDisplayName ns name
  | isQualifiedName name = name
  | otherwise = ns ++ "." ++ name

typeInfoIsShown :: TypeInfo -> Bool
typeInfoIsShown = \case
  ShownType _ -> True
  _ -> False

namespaceImportShapes :: String -> DisplayNames -> Map.Map String ShapeInfo -> Map.Map String ShapeInfo
namespaceImportShapes ns preferredNames =
  Map.foldlWithKey' step Map.empty
 where
  step acc name ShapeInfo{..} =
    let info = relabelShapeInfoRefs preferredNames (namespaceDisplayName ns) ShapeInfo{shapeExported = False, ..}
     in if shapeExported
          then Map.insert (lastQualifiedSegment name) info (Map.insert (ns ++ "." ++ lastQualifiedSegment name) info acc)
          else acc

relabelShapeInfoRefs :: DisplayNames -> (String -> String) -> ShapeInfo -> ShapeInfo
relabelShapeInfoRefs names fallback info@ShapeInfo{shapeNeeds, shapeMethods} =
  info
    { shapeNeeds = map relabelNeed shapeNeeds
    , shapeMethods = map relabelMethod shapeMethods
    }
 where
  relabelTy = relabelTypeRefs names fallback
  relabelNeed = relabelNeedRefs names fallback
  relabelAnnotation (ResolvedTypeAnn ty needs) =
    ResolvedTypeAnn (relabelTy ty) (map relabelNeed needs)
  relabelMethod method@ShapeMethod{shapeMethodType} =
    method{shapeMethodType = relabelAnnotation shapeMethodType}

relabelFillIndex :: DisplayNames -> (String -> String) -> Map.Map String [FillInfo] -> Map.Map String [FillInfo]
relabelFillIndex names fallback = Map.map (map relabelFill)
 where
  relabelFill info@FillInfo{fillShape, fillTypes, fillNeeds, fillParents} =
    info
      { fillShape = relabelShapeRef names fallback fillShape
      , fillTypes = map (relabelTypeRefs names fallback) fillTypes
      , fillNeeds = map (relabelNeedRefs names fallback) fillNeeds
      , fillParents = map (relabelNeedRefs names fallback) fillParents
      }

typeExprVars :: TypeExpr -> [String]
typeExprVars = \case
  TypeName name | isTypeVarName name -> [name]
  TypeName _ -> []
  TypeApply _ args -> nub (concatMap typeExprVars args)
  TypeRecord fields -> nub (concatMap (typeExprVars . snd) fields)
  TypeArrow args effects ret -> nub (concatMap typeExprVars (NE.toList args ++ effects ++ [ret]))

mergeContext :: TcContext -> TcContext -> TcContext
mergeContext left right =
  TcContext
    { tcModule = tcModule right
    , tcEnv = tcEnv left ++ tcEnv right
    , tcTypes = Map.union (tcTypes left) (tcTypes right)
    , tcTypeRegistry = Map.union (tcTypeRegistry left) (tcTypeRegistry right)
    , tcTypeAmbiguities = Map.unionsWith unionNames [tcTypeAmbiguities left, tcTypeAmbiguities right, collisions]
    , tcShapes = Map.union (tcShapes left) (tcShapes right)
    , tcShapeRegistry = Map.union (tcShapeRegistry left) (tcShapeRegistry right)
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

termTargets :: TcContext -> Map.Map String [TermExport]
termTargets TcContext{tcEnv} = foldr add Map.empty tcEnv
 where
  add EnvBinding{envName, envTarget = Just target} = Map.insertWith (++) envName [target]
  add _ = id

-- project runtime term exports from the same context that passed static
-- checking. types and private frame/fill closure remain in 'TcContext'.
moduleInterface :: TcContext -> ModuleInterface
moduleInterface TcContext{tcModule, tcEnv} = ModuleInterface tcModule terms
 where
  terms = foldr addShownTerm Map.empty tcEnv
  addShownTerm EnvBinding{envName, envRank, envOrigin, envTarget = Just target}
    | envRank == exportEnvRank
    , isShownOrigin envOrigin =
        Map.insert (SymbolId tcModule (lastQualifiedSegment envName)) target
  addShownTerm _ = id

typeInfoNames :: TypeInfo -> [String]
typeInfoNames = \case
  Primitive name -> [name]
  Alias name _ _ -> [name]
  DataInfo TypeRef{typeDisplayName} _ _ -> [typeDisplayName]
  EffectInfo EffectRef{effectDisplayName} _ -> [effectDisplayName]
  ShownType info -> typeInfoNames info

isBaseEnvBinding :: String -> String -> Bool
isBaseEnvBinding name origin = name `elem` baseRuntimeNames && origin == "base@" ++ name

isPrimitiveTypeName :: String -> Bool
isPrimitiveTypeName name = name `elem` primitiveTypeNames

isConcreteSchemaType :: TcContext -> String -> Bool
isConcreteSchemaType ctx name =
  isPrimitiveTypeName name || isQualifiedName name || isJust (findTypeInfoForTypeSystem name ctx)

isTypeVarName :: String -> Bool
isTypeVarName (c : rest) = isAscii c && isLower c && all isTypeVariableIndex rest
isTypeVarName [] = False

-- nominal headers make declaration identity independent of source order;
-- transparent aliases are rebuilt as data and effect ABIs settle.
resolveTypeHeadersM :: [Decl] -> TcContext -> Tc TcContext
resolveTypeHeadersM declarations importedCtx = do
  let aliasHeaders = mapMaybe aliasHeader declarations
      aliasNames = Set.fromList (map aliasHeaderName aliasHeaders)
      dataHeaders = mapMaybe (hostDataHeader (tcModule importedCtx)) declarations
      localNames = mapMaybe declaredTypeName declarations
      nameCounts = Map.fromListWith (+) [(name, 1 :: Int) | name <- localNames]
      isConcrete name = Map.member name nameCounts || isConcreteSchemaType importedCtx name
      nominalCtx = foldl' (flip (collectNominalTypeHeader isConcrete)) importedCtx declarations
      aliasComponents = stronglyConnComp [aliasNode aliasNames header | header <- aliasHeaders]
      addAliases = foldM addAlias
  case listToMaybe (Map.keys (Map.filter (> 1) nameCounts)) of
    Just name -> failTc ("type name '" ++ name ++ "' is declared more than once")
    Nothing -> do
      aliasCtx <- addAliases nominalCtx aliasComponents
      let dataCtx = resolveHostDataHeaders dataHeaders aliasCtx
      dataAliasCtx <- addAliases dataCtx aliasComponents
      let effectCtx = foldl' (flip collectResolvedEffectHeader) dataAliasCtx declarations
      addAliases effectCtx aliasComponents
 where
  addAlias ctx (AcyclicSCC AliasHeader{..}) =
    pure (addTypeAliasInfo aliasHeaderParams aliasHeaderName aliasHeaderTarget ctx)
  addAlias _ (CyclicSCC headers) =
    failTc ("recursive type aliases: " ++ intercalate ", " (map aliasHeaderName headers))

aliasHeader :: Decl -> Maybe AliasHeader
aliasHeader = \case
  Export nested -> aliasHeader nested
  TypeAlias params name target -> Just (AliasHeader name params target)
  _ -> Nothing

hostDataHeader :: ModuleId -> Decl -> Maybe DataHeader
hostDataHeader owner = \case
  Export nested -> hostDataHeader owner nested
  DataDecl params name constructors -> do
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
  DataDecl params name constructors ->
    addDataTypeHeader name (dataDeclarationRef True (tcModule ctx) params name constructors) params ctx
  EffectDecl params name operations ->
    let target = effectId isConcrete (tcModule ctx) params name operations
        effect = EffectRef target (renderEffectId target)
     in addEffectTypeInfo name effect params ctx
  _ -> ctx

collectResolvedEffectHeader :: Decl -> TcContext -> TcContext
collectResolvedEffectHeader declaration ctx = case declaration of
  Export nested -> collectResolvedEffectHeader nested ctx
  EffectDecl params name operations ->
    addEffectTypeInfo name (resolvedEffectRef params name operations ctx) params ctx
  _ -> ctx

resolvedEffectRef :: [String] -> String -> [EffectOp] -> TcContext -> EffectRef
resolvedEffectRef params name operations ctx =
  EffectRef target (renderEffectId target)
 where
  candidate = effectId (isConcreteSchemaType ctx) (tcModule ctx) params name operations
  target
    | SymbolId RuntimeModule _ <- candidate
    , hostNominalsMatchResolvedTypes params [operationType | EffectOp _ operationType <- operations] ctx =
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
  nominalize DataHeader{..} current =
    addDataTypeHeader dataHeaderName ref dataHeaderParams current
   where
    ref = dataDeclarationRef False (tcModule current) dataHeaderParams dataHeaderName dataHeaderConstructors

hostDataHeaderResolves :: DataHeader -> TcContext -> Bool
hostDataHeaderResolves DataHeader{..} =
  hostNominalsMatchResolvedTypes dataHeaderParams [field | Ctor _ fields <- dataHeaderConstructors, field <- fields]

dataHeaderNode :: Set.Set String -> DataHeader -> (DataHeader, String, [String])
dataHeaderNode names header@DataHeader{..} =
  ( header
  , dataHeaderName
  , Set.toList (Set.intersection names (Set.difference heads excluded))
  )
 where
  heads = Set.fromList [name | Ctor _ fields <- dataHeaderConstructors, field <- fields, name <- typeExprHeads field]
  excluded = Set.fromList (dataHeaderName : dataHeaderParams)

-- raw host schemas are deliberately order-insensitive, but every known
-- reserved spelling must resolve to its ilk/effect ABI rather than a lookalike.
hostNominalsMatchResolvedTypes :: [String] -> [TypeExpr] -> TcContext -> Bool
hostNominalsMatchResolvedTypes bound expressions ctx = all typeMatches expressions
 where
  typeMatches expression = case expression of
    TypeName name -> headMatches name [] expression
    TypeApply name arguments -> headMatches name arguments expression && all typeMatches arguments
    TypeRecord fields -> all (typeMatches . snd) fields
    TypeArrow arguments effects result -> all typeMatches (NE.toList arguments ++ [result]) && all effectMatches effects
  effectMatches expression = case expression of
    TypeName name -> effectHeadMatches name
    TypeApply name arguments -> effectHeadMatches name && all typeMatches arguments
    other -> typeMatches other
  headMatches name arguments expression
    | name `elem` bound = True
    | Just identity <- hostTypeId name
    , isJust (findTypeInfoForTypeSystem name ctx) =
        convertTypeExprWith bound ctx expression
          == TyNamed (TypeRef identity name) (map (convertTypeExprWith bound ctx) arguments)
    | otherwise = True
  effectHeadMatches name
    | name `elem` bound = True
    | name `elem` hostEffectNames = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
        Nothing -> True
        Just (EffectInfo EffectRef{effectIdentity} _) -> effectIdentity == SymbolId RuntimeModule name
        Just _ -> False
    | otherwise = True

aliasNode :: Set.Set String -> AliasHeader -> (AliasHeader, String, [String])
aliasNode aliasNames header@AliasHeader{..} =
  ( header
  , aliasHeaderName
  , Set.toList (Set.intersection aliasNames (Set.difference (Set.fromList (typeExprHeads aliasHeaderTarget)) (Set.fromList aliasHeaderParams)))
  )

typeExprHeads :: TypeExpr -> [String]
typeExprHeads = \case
  TypeName name -> [name]
  TypeApply name arguments -> name : concatMap typeExprHeads arguments
  TypeRecord fields -> concatMap (typeExprHeads . snd) fields
  TypeArrow arguments effects result -> concatMap typeExprHeads (NE.toList arguments ++ effects ++ [result])

-- frames form þe nominal index used by fills. collecting þeir headers first
-- makeþ a forward fill equivalent to þe same declarations in source order.
collectShapeContexts :: [Decl] -> TcContext -> TcContext
collectShapeContexts ds ctx = foldl' (flip collectShapeContext) ctx ds

collectShapeContext :: Decl -> TcContext -> TcContext
collectShapeContext declaration ctx = case declaration of
  Export nested -> collectShapeContext nested ctx
  ShapeDecl params name needs members -> addShapeInfo name params needs members ctx
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
  TypeAlias{} -> ctx
  EffectDecl params name ops -> case effectRefForName name ctx of
    Just effect -> collectEffectOps params name effect ops ctx
    Nothing -> ctx
  ElaboratedEffect{} -> ctx
  ShapeDecl params name needs members -> addShapeMembers name params needs members ctx
  ElaboratedShape{} -> ctx
  Let name (Just ann) _ -> addGlobalEnv name name (typeAnnScheme ann ctx) OrdinaryTerm ctx
  Let _ Nothing _ -> ctx
  FillDecl tyArgs shapeName needs members -> addFillInfo shapeName tyArgs needs members ctx
  ElaboratedFill _ _ tyArgs shapeName needs members -> addFillInfo shapeName tyArgs needs members ctx

exportDeclContext :: Decl -> TcContext -> TcContext
exportDeclContext declaration ctx = case declaration of
  Import _ _ -> ctx
  Export nested -> exportDeclContext nested ctx
  ReExport name -> either (const ctx) id (reExportName name ctx)
  ReExportType name -> either (const ctx) id (reExportTypeName name ctx)
  Let name _ _ -> addShownEnv name ctx
  TypeAlias _ name _ -> addShownType name ctx
  DataDecl _ name constructors -> foldl' (flip addShownEnv) (addShownType name ctx) [ctorName | Ctor ctorName _ <- constructors]
  EffectDecl _ name operations -> foldl' (flip addShownEnv) (addShownType name ctx) [opName | EffectOp opName _ <- operations]
  ElaboratedEffect{} -> ctx
  ShapeDecl _ name _ members -> foldl' (flip addShownEnv) (markShapeExported name ctx) (shapeMemberNames members)
  ElaboratedShape{} -> ctx
  FillDecl{} -> ctx
  ElaboratedFill{} -> ctx

markShapeExported :: String -> TcContext -> TcContext
markShapeExported name ctx@TcContext{tcShapes, tcShapeRegistry} =
  case findShapeEntry name ctx of
    Just (_, info) ->
      let exported = info{shapeExported = True}
       in ctx
            { tcShapes = Map.insert (lastQualifiedSegment name) exported tcShapes
            , tcShapeRegistry = Map.insert (shapeTarget info) exported tcShapeRegistry
            }
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
  | otherwise = do
      typeInfo <- reExportTypeInfo name ctx
      let shapeInfo = findShapeEntry name ctx
          withType = maybe ctx (const (addShownType name ctx)) typeInfo
          withShape = maybe withType (const (markShapeExported name withType)) shapeInfo
      if isNothing typeInfo && isNothing shapeInfo
        then Left ("unknown type or frame '" ++ name ++ "'")
        else Right withShape

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
findShapeEntry name TcContext{tcShapes} = (name,) <$> Map.lookup name tcShapes

declaredDataRef :: String -> TcContext -> Maybe TypeRef
declaredDataRef name ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (DataInfo ref _ _) -> Just ref
  _ -> Nothing

collectDataDeclaration :: String -> TypeRef -> [String] -> [Ctor] -> TcContext -> TcContext
collectDataDeclaration dataName ref@TypeRef{typeIdentity = SymbolId owner typeName} params ctors ctx =
  foldl' addConstructor withType constructors
 where
  constructors =
    [ (name, ConstructorInfo (SymbolId owner (typeName ++ "@" ++ name)) (map (convertTypeExprWith params ctx) fields))
    | Ctor name fields <- ctors
    ]
  withType = addTypeInfo dataName (DataInfo ref params (map snd constructors)) ctx
  result = TyNamed ref (map TyVar params)
  addConstructor current (name, ConstructorInfo target fields) =
    let qualifiedName = dataName ++ "@" ++ name
        scheme = Forall params [] (curriedFunction fields [] result)
        kind = ConstructorTerm (length fields)
     in addGlobalEnvTarget name qualifiedName scheme target kind (addGlobalEnvTarget qualifiedName qualifiedName scheme target kind current)

collectEffectOps :: [String] -> String -> EffectRef -> [EffectOp] -> TcContext -> TcContext
collectEffectOps params effectName effectRef@EffectRef{effectIdentity} ops ctx = foldl' step ctx ops
 where
  step current (EffectOp opName t) =
    let effect = TyEffect effectRef (map TyVar params)
        opTy = addLatentEffect effect (convertTypeExprWith params current t)
        scheme = markEffectOpScheme effectRef (addForallVars params (generalizeTy opTy))
        target = effectName ++ "@" ++ opName
        identity = effectOperationId effectIdentity opName
        kind = EffectOperationTerm effectIdentity (effectOperationArity params current t)
     in addGlobalEnvTarget opName target scheme identity kind (addGlobalEnvTarget target target scheme identity kind current)

effectOperationArity :: [String] -> TcContext -> TypeExpr -> Int
effectOperationArity params ctx operationType = case normalizeFun (convertTypeExprWith params ctx operationType) of
  TyFun arguments _ _ -> NE.length arguments
  _ -> 0

addLatentEffect :: Ty -> Ty -> Ty
addLatentEffect effect ty = case normalizeFun ty of
  TyFun args effects ret -> TyFun args (addEffect effect effects) ret
  other -> other

addTypeAliasInfo :: [String] -> String -> TypeExpr -> TcContext -> TcContext
addTypeAliasInfo params name target ctx =
  addTypeInfo name (Alias name params (convertTypeExprWith params ctx target)) ctx

-- provisional data headers participate in name resolution, not coverage.
-- delaying registry insertion preventeþ a rejected host candidate from replacing
-- an imported ABI entry under þe same stable identity.
addDataTypeHeader :: String -> TypeRef -> [String] -> TcContext -> TcContext
addDataTypeHeader name ref params ctx@TcContext{tcTypes, tcTypeAmbiguities} =
  ctx
    { tcTypes = Map.insert name (DataInfo ref params []) tcTypes
    , tcTypeAmbiguities = Map.delete name tcTypeAmbiguities
    }

addEffectTypeInfo :: String -> EffectRef -> [String] -> TcContext -> TcContext
addEffectTypeInfo name effect params = addTypeInfo name (EffectInfo effect params)

addTypeInfo :: String -> TypeInfo -> TcContext -> TcContext
addTypeInfo name info ctx@TcContext{tcTypes, tcTypeRegistry, tcTypeAmbiguities} =
  ctx
    { tcTypes = Map.insert name info tcTypes
    , tcTypeRegistry = case unshownTypeInfo info of
        dataInfo@(DataInfo TypeRef{typeIdentity} _ _) -> Map.insert typeIdentity dataInfo tcTypeRegistry
        _ -> tcTypeRegistry
    , tcTypeAmbiguities = Map.delete name tcTypeAmbiguities
    }

addFillInfo :: String -> [TypeExpr] -> [ShapeNeed] -> [Decl] -> TcContext -> TcContext
addFillInfo shapeName tyArgs needs members ctx@TcContext{tcFills} =
  let actualShape = canonicalShapeName shapeName ctx
      actualShapeRef = shapeRefForName shapeName ctx
      actualTypes = map (`convertTypeExpr` ctx) tyArgs
      actualNeeds = map (`convertShapeNeed` ctx) needs
      key = fillKeyFor (tcModule ctx) actualShape actualTypes
      provided = [name | Let name _ _ <- members]
      newFills = fillClosure key actualShapeRef actualTypes actualNeeds provided ctx [] 0
   in ctx{tcFills = Map.unionWith (++) (indexFills newFills) tcFills}

indexFills :: [FillInfo] -> Map.Map String [FillInfo]
indexFills = foldr insertFill Map.empty
 where
  insertFill info = Map.insertWith (++) (lastQualifiedSegment (shapeDisplayName (fillShape info))) [info]

fillsForShape :: String -> TcContext -> [FillInfo]
fillsForShape name = Map.findWithDefault [] (lastQualifiedSegment name) . tcFills

fillsForShapeRef :: ShapeRef -> TcContext -> [FillInfo]
fillsForShapeRef frame = filter ((== frame) . fillShape) . fillsForShape (shapeDisplayName frame)

-- duplicate imports may share one FillId; nested diagnostic names still follow
-- þe wanted frame's alias without changing semantic candidate order.
projectFillNeeds :: ShapeRef -> FillInfo -> [Need] -> [Need]
projectFillNeeds wanted FillInfo{fillShape} = map (relabelNeedRefs emptyDisplayNames project)
 where
  sourceNamespace = displayNamespace (shapeDisplayName fillShape)
  wantedNamespace = displayNamespace (shapeDisplayName wanted)
  project name = case sourceNamespace >>= (\source -> stripPrefix (source ++ ".") name) of
    Just rest -> maybe rest (\target -> target ++ "." ++ rest) wantedNamespace
    Nothing -> name
  displayNamespace = fmap fst . splitQualifiedName

fillParentNeeds :: String -> [TypeExpr] -> TcContext -> [Need]
fillParentNeeds shapeName types ctx = case findShapeInfo shapeName ctx of
  Nothing -> []
  Just ShapeInfo{shapeParams, shapeNeeds} -> map (specializeNeed shapeParams (map (`convertTypeExpr` ctx) types)) shapeNeeds

fillClosure :: FillId -> ShapeRef -> [Ty] -> [Need] -> [String] -> TcContext -> [ShapeRef] -> Int -> [FillInfo]
fillClosure key frame tyArgs needs provided ctx seen depth
  | frame `elem` seen = []
  | otherwise = FillInfo frame tyArgs needs key depth parents provided : inherited
 where
  parents = case findShapeInfoByRef frame ctx of
    Nothing -> []
    Just ShapeInfo{shapeParams, shapeNeeds} -> map (specializeNeed shapeParams tyArgs) shapeNeeds
  inherited = concatMap closeNeed parents
  closeNeed (Need neededTypes neededShape) =
    fillClosure key neededShape neededTypes needs provided ctx (frame : seen) (depth + 1)

addEnv :: String -> Scheme -> TcContext -> TcContext
addEnv name scheme = addEnvWith name scheme localEnvRank name Nothing

addEnvWith :: String -> Scheme -> Int -> String -> Maybe TermExport -> TcContext -> TcContext
addEnvWith name scheme rank origin target ctx@TcContext{tcEnv} =
  ctx{tcEnv = EnvBinding name scheme rank origin target : tcEnv}

addGlobalEnv :: String -> String -> Scheme -> TermKind -> TcContext -> TcContext
addGlobalEnv name targetName scheme kind ctx@TcContext{tcModule} =
  addGlobalEnvTarget name targetName scheme (SymbolId tcModule targetName) kind ctx

addGlobalEnvTarget :: String -> String -> Scheme -> SymbolId -> TermKind -> TcContext -> TcContext
addGlobalEnvTarget name origin scheme target kind =
  addEnvWith name scheme localEnvRank origin (Just (TermExport target kind))

globalizeBindingM :: String -> TcContext -> Tc TcContext
globalizeBindingM name ctx@TcContext{tcModule, tcEnv} =
  case promote tcEnv of
    Nothing -> failTc ("internal missing inferred binding '" ++ name ++ "'")
    Just env -> pure ctx{tcEnv = env}
 where
  target = Just (TermExport (SymbolId tcModule name) OrdinaryTerm)
  promote [] = Nothing
  promote (binding : rest)
    | envName binding == name = Just (binding{envTarget = target} : rest)
    | otherwise = (binding :) <$> promote rest

addShownEnv :: String -> TcContext -> TcContext
addShownEnv name ctx = foldl' addOne ctx (shownEnvBindings name ctx)
 where
  exportName = lastQualifiedSegment name
  addOne current binding =
    addEnvWith exportName (envScheme binding) exportEnvRank ("show@" ++ exportName) (envTarget binding) current

shownEnvBindings :: String -> TcContext -> [EnvBinding]
shownEnvBindings name TcContext{tcEnv}
  | isQualifiedName name = maybeToList (lookupEnvExact name tcEnv)
  | otherwise = case shownBareEnvBindings name tcEnv of
      Right schemes -> schemes
      Left _ -> []

shownBareEnvBindings :: String -> [EnvBinding] -> Either String [EnvBinding]
shownBareEnvBindings name env = fst <$> rankEnvCandidates name (collectEnvCandidates name env)

addShownType :: String -> TcContext -> TcContext
addShownType name ctx@TcContext{tcTypes, tcTypeAmbiguities}
  | not (isQualifiedName name) && Map.member name tcTypeAmbiguities = ctx
  | otherwise = case shownTypeInfo name ctx of
      Just info -> ctx{tcTypes = Map.insert (lastQualifiedSegment name) (ShownType info) tcTypes}
      Nothing -> ctx

shownTypeInfo :: String -> TcContext -> Maybe TypeInfo
shownTypeInfo name TcContext{tcTypes} = Map.lookup name tcTypes

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
lookupEnvChoices name TcContext{tcEnv}
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
  Right (best, lower) -> EnvChoices (best ++ lower)

rankEnvCandidates :: String -> [EnvBinding] -> Either String ([EnvBinding], [EnvBinding])
rankEnvCandidates name [] = Left ("unknown name '" ++ name ++ "'")
rankEnvCandidates name candidates =
  let bestRank = minimum (map envRank candidates)
      (best, lower) = partition ((== bestRank) . envRank) candidates
      origins = nub (map envOrigin best)
   in case origins of
        [origin] -> Right (filter ((== origin) . envOrigin) best, lower)
        _ -> Left ("ambiguous name '" ++ name ++ "'; qualify it as one of: " ++ intercalate ", " origins)

baseEnv :: [EnvBinding]
baseEnv =
  map (uncurry baseBinding) baseNativeEnv ++ map baseEffectOpBinding baseEffectOpEnv

baseNativeEnv :: [(String, Scheme)]
baseNativeEnv = [(hostName binding, hostScheme (hostSignature binding)) | binding <- baseNativeBindings]

baseEffectOpEnv :: [(String, String, Int, Scheme)]
baseEffectOpEnv =
  [ (effectName, hostName binding, hostArity binding, hostScheme (hostSignature binding))
  | binding@HostBinding{hostRole = BaseEffect effectName} <- baseEffectBindings
  ]

baseBinding :: String -> Scheme -> EnvBinding
baseBinding name scheme =
  EnvBinding name scheme baseEnvRank ("base@" ++ name) (Just (TermExport (SymbolId RuntimeModule name) OrdinaryTerm))

baseEffectOpBinding :: (String, String, Int, Scheme) -> EnvBinding
baseEffectOpBinding (effectName, opName, arity, scheme) =
  EnvBinding
    opName
    (markEffectOpScheme (runtimeEffectRef effectName) scheme)
    baseEnvRank
    ("base@" ++ opName)
    (Just (TermExport (SymbolId RuntimeModule opName) (EffectOperationTerm (SymbolId RuntimeModule effectName) arity)))

hostScheme :: HostSignature -> Scheme
hostScheme HostSignature{..} =
  Forall hostVariables [] (curriedFunction (map hostTy hostArguments) (map hostEffectTy hostEffects) (hostTy hostResult))

hostEffectTy :: HostType -> Ty
hostEffectTy = \case
  HostVar name -> TyVar name
  HostCon name -> TyEffect (runtimeEffectRef name) []
  HostApp name arguments -> TyEffect (runtimeEffectRef name) (map hostTy arguments)

hostTy :: HostType -> Ty
hostTy = \case
  HostVar name -> TyVar name
  HostCon name -> maybe (TyCon name) (\identity -> TyNamed (TypeRef identity name) []) (hostTypeId name)
  HostApp name arguments ->
    let converted = map hostTy arguments
     in maybe (TyApp name converted) (\identity -> TyNamed (TypeRef identity name) converted) (hostTypeId name)

addShapeMembers :: String -> [String] -> [ShapeNeed] -> [ShapeMember] -> TcContext -> TcContext
addShapeMembers shapeName params needs members ctx = addShapeMembersToEnv shapeName members (addShapeInfo shapeName params needs members ctx)

addShapeInfo :: String -> [String] -> [ShapeNeed] -> [ShapeMember] -> TcContext -> TcContext
addShapeInfo shapeName params needs members ctx@TcContext{tcModule, tcShapes, tcShapeRegistry} =
  let signatures = [signature | ShapeSpec _ (TypeAnn signature _) <- members]
      schemaResolves = hostNominalsMatchResolvedTypes params signatures ctx
      target = primitiveShapeId schemaResolves tcModule params shapeName members
      methods = mapMaybe resolveMethod members
      resolveMethod = \case
        ShapeSpec name annotation -> Just (ShapeMethod name (resolve annotation))
        _ -> Nothing
      resolve (TypeAnn ty memberNeeds) =
        ResolvedTypeAnn (convertTypeExprWith params ctx ty) (map (convertShapeNeedWith params ctx) memberNeeds)
      info =
        ShapeInfo
          { shapeParams = params
          , shapeNeeds = map (convertShapeNeedWith params ctx) needs
          , shapeMethods = methods
          , shapeExported = False
          , shapeTarget = target
          }
   in ctx
        { tcShapes = Map.insert shapeName info tcShapes
        , tcShapeRegistry = Map.insert target info tcShapeRegistry
        }

addShapeMembersToEnv :: String -> [ShapeMember] -> TcContext -> TcContext
addShapeMembersToEnv shapeName members ctx = foldl' step ctx (mapMaybe shapeMemberSignature members)
 where
  step current (name, annotation) =
    let scheme = shapeMemberScheme shapeName annotation current
        qualifiedName = shapeName ++ "@" ++ name
        frame = maybe (SymbolId (tcModule current) shapeName) shapeTarget (findShapeInfo shapeName current)
        kind = ShapeMemberTerm frame
     in addGlobalEnv name qualifiedName scheme kind (addGlobalEnv qualifiedName qualifiedName scheme kind current)

shapeMemberScheme :: String -> TypeAnn -> TcContext -> Scheme
shapeMemberScheme shapeName ann ctx = case findShapeInfo shapeName ctx of
  Just ShapeInfo{shapeParams} ->
    addForallVars shapeParams (typeAnnSchemeWith shapeParams ctx (typeAnnWithOwnShapeNeed shapeName ann ctx))
  Nothing -> typeAnnScheme (typeAnnWithOwnShapeNeed shapeName ann ctx) ctx

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
findShapeInfo shapeName TcContext{tcShapes} = Map.lookup shapeName tcShapes

findShapeInfoByRef :: ShapeRef -> TcContext -> Maybe ShapeInfo
findShapeInfoByRef ShapeRef{shapeIdentity, shapeDisplayName} TcContext{tcShapes, tcShapeRegistry} =
  case Map.lookup shapeDisplayName tcShapes of
    Just info | shapeTarget info == shapeIdentity -> Just info
    _ -> Map.lookup shapeIdentity tcShapeRegistry

resolveShapeTargetM :: String -> TcContext -> Tc SymbolId
resolveShapeTargetM name ctx =
  maybe (failTc ("internal missing checked frame '" ++ name ++ "'")) (pure . shapeTarget) (findShapeInfo name ctx)

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
    Just 0 -> instantiateBindingTypeM binding >>= (`unifyM` expected) >> pure (ctx, bound)
    Just _ -> failTc ("constructor pattern '" ++ name ++ "' needeþ arguments")
    Nothing -> failTc ("internal missing constructor arity for '" ++ name ++ "'")
bindPatternLinearM (PInteger _) expected ctx bound = unifyM (TyCon "integer") expected >> pure (ctx, bound)
bindPatternLinearM (PText _) expected ctx bound = unifyM (TyCon "text") expected >> pure (ctx, bound)
bindPatternLinearM (PCon name args) expected ctx bound = do
  binding <- either failTc (maybe (failTc ("unknown constructor pattern '" ++ name ++ "'")) pure) (lookupConstructorBinding name ctx)
  conTy <- instantiateBindingTypeM binding
  case normalizeFun conTy of
    TyFun fields _ result -> do
      unifyM result expected
      bindPatternListM args (NE.toList fields) ctx bound
    _ -> case args of
      [] -> unifyM conTy expected >> pure (ctx, bound)
      _ -> failTc ("constructor '" ++ name ++ "' is not a function")
bindPatternLinearM PConstructor{} _ _ _ = failTc "internal resolved constructor appeareþ before elaboration"

bindPatternListM :: [Pattern] -> [Ty] -> TcContext -> [String] -> Tc (TcContext, [String])
bindPatternListM patterns tys ctx bound
  | length patterns == length tys = foldM step (ctx, bound) (zip patterns tys)
  | otherwise = failTc "constructor pattern arity mismatch"
 where
  step (currentCtx, currentBound) (pat, ty) = bindPatternLinearM pat ty currentCtx currentBound

bindPatternVariableM :: String -> Ty -> TcContext -> [String] -> Tc (TcContext, [String])
bindPatternVariableM name expected ctx bound
  | name `elem` bound = failTc ("pattern bindeþ '" ++ name ++ "' more than once")
  | otherwise = pure (addEnv name (Forall [] [] expected) ctx, name : bound)

knownConstructorArity :: String -> TcContext -> Maybe Int
knownConstructorArity name ctx = either (const Nothing) (>>= constructorBindingArity) (lookupConstructorBinding name ctx)

lookupConstructorBinding :: String -> TcContext -> Either String (Maybe EnvBinding)
lookupConstructorBinding name TcContext{tcEnv}
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
constructorBindingArity EnvBinding{envTarget = Just TermExport{termExportKind = ConstructorTerm arity}} = Just arity
constructorBindingArity _ = Nothing

instantiateBindingTypeM :: EnvBinding -> Tc Ty
instantiateBindingTypeM EnvBinding{envScheme} = fst <$> instantiateM envScheme

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
  PCon name arguments -> PConstructor <$> constructorTargetM name ctx <*> traverse (resolvePatternM ctx) arguments
  PConstructor{} -> failTc "internal resolved constructor appeareþ before elaboration"

constructorTargetM :: String -> TcContext -> Tc SymbolId
constructorTargetM name ctx = case lookupConstructorBinding name ctx of
  Left message -> failTc message
  Right (Just EnvBinding{envTarget = Just TermExport{termExportTarget}}) -> pure termExportTarget
  _ -> failTc ("internal missing constructor identity for '" ++ name ++ "'")

constructorsForType :: Ty -> TcContext -> TcState -> Maybe [(SymbolId, [Ty])]
constructorsForType ty TcContext{tcTypeRegistry} st = case normalizeFun (applyState ty st) of
  TyNamed TypeRef{typeIdentity} args -> case Map.lookup typeIdentity tcTypeRegistry of
    Just (DataInfo _ params ctors) -> Just (specializeCtorInfos params args ctors)
    _ -> Nothing
  _ -> Nothing

unshownTypeInfo :: TypeInfo -> TypeInfo
unshownTypeInfo = \case
  ShownType info -> unshownTypeInfo info
  info -> info

specializeCtorInfos :: [String] -> [Ty] -> [ConstructorInfo] -> [(SymbolId, [Ty])]
specializeCtorInfos params args ctors =
  let repls = Map.fromList (zip params args)
   in [ (constructorInfoTarget, map (replaceTyVarsForCoverage repls) constructorInfoFields)
      | ConstructorInfo{constructorInfoTarget, constructorInfoFields} <- ctors
      ]

replaceTyVarsForCoverage :: Map.Map String Ty -> Ty -> Ty
replaceTyVarsForCoverage repls ty = case ty of
  TyVar name -> Map.findWithDefault ty name repls
  _ -> mapTyChildren (replaceTyVarsForCoverage repls) (skipEffectVar (replaceTyVarsForCoverage repls)) ty

-- expression inference produceþ an elaborated expression skeleton alongside
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
  EForeign{} -> failTc "fremmed is only allowed as the direct body of an annotated top-level let"
  EVar name -> inferVarM name ctx
  EGlobal{} -> failTc "internal resolved global appeareþ before elaboration"
  EEvidence{} -> failTc "internal evidence appeareþ before elaboration"
  EEvidenceLambda{} -> failTc "internal evidence abstraction appeareþ before elaboration"
  EAscribe expression annotation -> inferAscribedM expression annotation ctx
  EApply f args -> inferApplyM f (NE.toList args) ctx
  ERecord fields -> inferRecordFieldsM fields ctx
  EField{} -> failTc "internal field access appeareþ before elaboration"
  EUpdate base updates -> inferRecordUpdateM base updates ctx
  ETry body returnCase cases -> inferTryHandleM body returnCase cases ctx
  EMatch scrutinees cases -> inferMatchM scrutinees cases ctx
  EBlock ds body -> inferBlockM ds body ctx
  EWithEvidence{} -> failTc "internal evidence appeareþ before elaboration"
  EDictionary{} -> failTc "internal dictionary appeareþ before elaboration"

inferAscribedM :: Expr -> TypeExpr -> TcContext -> Tc Inferred
inferAscribedM expression annotation ctx = do
  checkTypeExprNamesM "type ascription" annotation ctx
  (expected, _) <- instantiateM (typeAnnScheme (TypeAnn annotation []) ctx)
  Inferred actual effects needs core <- inferExprM expression ctx
  mapTcError
    (unifyM actual expected)
    (\msg -> "in type ascription: " ++ msg ++ "; actual " ++ showTy actual ++ ", expected " ++ showTy expected)
  st <- getTc
  pure (Inferred (applyState expected st) (applyStateEffects effects st) (applyStateNeeds needs st) (EAscribe core annotation))

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
  (ds2, ctx2, declEffects) <- inferLocalDeclsM ds ctx
  Inferred bodyTy bodyEffects bodyNeeds body2 <- inferExprM body ctx2
  pure (Inferred bodyTy (unionEffects declEffects bodyEffects) bodyNeeds (EBlock ds2 body2))

inferVarM :: String -> TcContext -> Tc Inferred
inferVarM name ctx = case lookupEnvBinding name ctx of
  Right Nothing -> inferFieldAccessNameM name ctx
  Left message -> failTc message
  Right (Just binding) -> do
    (ty, needs) <- instantiateM (envScheme binding)
    holes <- traverse freshEvidenceHoleM needs
    let variable = maybe (EVar name) EGlobal (envTarget binding)
        core = if null holes then variable else EWithEvidence variable holes
    pure (Inferred ty [] needs core)

freshEvidenceHoleM :: Need -> Tc Int
freshEvidenceHoleM need = do
  st@TcState{tcEvidenceNeeds} <- getTc
  let ident = maybe 0 ((+ 1) . fst) (IntMap.lookupMax tcEvidenceNeeds)
  putTc st{tcEvidenceNeeds = IntMap.insert ident need tcEvidenceNeeds}
  pure ident

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
flatApply (ELocated _ inner) args = flatApply inner args
flatApply (EApply inner innerArgs) args =
  let (base, baseArgs) = flatApply inner (NE.toList innerArgs)
   in (base, baseArgs ++ args)
flatApply f args = (f, args)

inferNamedApplyM :: String -> [Expr] -> TcContext -> Tc Inferred
inferNamedApplyM name args ctx = case lookupEnvChoices name ctx of
  ChoicesMissing -> inferFieldAccessNameM name ctx >>= \inferred -> inferCurriedApplyM inferred args ctx
  ChoicesAmbiguous msg -> failTc msg
  EnvChoices bindings -> tryApplyBindingsM name args bindings ctx Nothing

tryApplyBindingsM :: String -> [Expr] -> [EnvBinding] -> TcContext -> Maybe String -> Tc Inferred
tryApplyBindingsM name args bindings ctx = foldr tryBinding noMore bindings
 where
  noMore err = failTc (fromMaybe ("unknown name '" ++ name ++ "'") err)
  tryBinding binding fallback err =
    recoverTc
      ( do
          (t, needs) <- instantiateM (envScheme binding)
          holes <- traverse freshEvidenceHoleM needs
          let variable = maybe (EVar name) EGlobal (envTarget binding)
              core = if null holes then variable else EWithEvidence variable holes
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
  ensureIrrefutablePatternM "handler yield" pat ctx
  ctx2 <- bindPatternM pat bodyTy ctx
  inferExprM body ctx2

ensureIrrefutablePatternM :: String -> Pattern -> TcContext -> Tc ()
ensureIrrefutablePatternM owner pattern ctx = case pattern of
  PVar "_" -> pure ()
  PVar name
    | isJust (knownConstructorArity name ctx) -> refutable
    | otherwise -> pure ()
  PInteger{} -> refutable
  PText{} -> refutable
  PCon{} -> refutable
  PConstructor{} -> refutable
 where
  refutable = failTc (owner ++ " pattern must be a binder or '_'")

data HandlerCoverage
  = WholeEffect EffectRef
  | OneOperation EffectRef SymbolId
  deriving (Eq, Show)

inferHandlerCasesM :: Expr -> Maybe ReturnCase -> [HandlerCase] -> Ty -> [Ty] -> [Need] -> [Ty] -> TcContext -> Tc Inferred
inferHandlerCasesM body returnCase cases answerTy effects bodyNeeds returnEffects ctx = do
  coverage <- traverse (handlerCoverageM effects ctx) cases
  let fullyHandled = completeHandledEffects coverage ctx
  ((finalAnswerTy, accumulatedEffects, accumulatedNeeds), cases2) <- mapAccumM (step fullyHandled) (answerTy, returnEffects, bodyNeeds) (zip coverage cases)
  st <- getTc
  let remainingEffects = removeEffects fullyHandled (applyStateEffects effects st)
  pure (Inferred (applyState finalAnswerTy st) (unionEffects remainingEffects accumulatedEffects) (applyStateNeeds accumulatedNeeds st) (ETry body returnCase cases2))
 where
  step fullyHandled (currentAnswerTy, accumulatedEffects, accumulatedNeeds) (target, HandlerCase name patterns handlerBody) = do
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
      , ResolvedHandlerCase (handlerTarget target) patterns handlerBody2
      )
  step _ _ (_, ResolvedHandlerCase{}) = failTc "internal resolved handler appeareþ before elaboration"

  handlerTarget (WholeEffect EffectRef{effectIdentity}) = EffectTarget effectIdentity
  handlerTarget (OneOperation _ target) = OperationTarget target

handlerCoverageM :: [Ty] -> TcContext -> HandlerCase -> Tc HandlerCoverage
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
        | null patterns
        , Just wholeEffect@(WholeEffect effect) <- whole
        , owner == effect ->
            pure wholeEffect
        | isJust (find (effectHasRef owner) actualEffects)
        , Just TermExport{termExportTarget} <- envTarget binding ->
            pure (OneOperation owner termExportTarget)
      _ -> maybe (failTc ("handler case '" ++ name ++ "' is not an effect operation")) pure whole
handlerCoverageM _ _ ResolvedHandlerCase{} = failTc "internal resolved handler appeareþ before elaboration"

completeHandledEffects :: [HandlerCoverage] -> TcContext -> [EffectRef]
completeHandledEffects coverage ctx =
  [ effect
  | effect <- nub [owner | item <- coverage, owner <- coverageEffect item]
  , any (wholeEffectMatches effect) coverage || all (`elem` handledOperations effect) (effectOperationTargets effect ctx)
  ]
 where
  coverageEffect = \case
    WholeEffect effect -> [effect]
    OneOperation effect _ -> [effect]
  wholeEffectMatches effect (WholeEffect actual) = actual == effect
  wholeEffectMatches _ _ = False
  handledOperations effect = [operation | OneOperation owner operation <- coverage, owner == effect]

effectOperationTargets :: EffectRef -> TcContext -> [SymbolId]
effectOperationTargets effect TcContext{tcEnv} =
  nub
    [ termExportTarget
    | EnvBinding{envScheme, envTarget = Just TermExport{termExportTarget}} <- tcEnv
    , schemeEffectOwner envScheme == Just effect
    ]

handlerCaseContextM :: [EffectRef] -> String -> [Pattern] -> Ty -> [Ty] -> TcContext -> Tc TcContext
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
    if maybe False (\effect -> any (effectHasRef effect) (applyStateEffects effects st)) (effectRefForName effectName ctx)
      then pure ctx
      else failTc ("handler for absent effect '" ++ effectName ++ "'")
  _ -> failTc ("effect-level handler '" ++ effectName ++ "' cannot bind operation arguments")

operationHandlerCaseContextM :: [EffectRef] -> String -> [Pattern] -> Ty -> [Ty] -> Scheme -> TcContext -> Tc TcContext
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
    let resumeEffects = removeEffects fullyHandled (applyStateEffects effects st)
        resumeTy = curriedFunction [applyState retTy st] resumeEffects (applyState answerTy st)
    pure (addEnv "eftgin" (Forall [] [] resumeTy) ctx2)

bindHandlerPatternsM :: [Pattern] -> [Ty] -> TcContext -> Tc TcContext
bindHandlerPatternsM [] _ ctx = pure ctx
bindHandlerPatternsM patterns argTys ctx = bindPatternsM patterns argTys ctx

findHandledOperationEffectM :: String -> EffectRef -> [Ty] -> [Ty] -> Tc Ty
findHandledOperationEffectM name ownerEffect opEffects effects = do
  ownEffect <- maybe notFound pure (find (effectHasRef ownerEffect) opEffects)
  st <- getTc
  case findEffectByHead ownEffect (applyStateEffects effects st) of
    Nothing -> failTc ("handler for absent effect '" ++ effectDisplayName ownerEffect ++ "' in operation '" ++ name ++ "'")
    Just actualEffect -> unifyEffectM ownEffect actualEffect >> (applyState actualEffect <$> getTc)
 where
  notFound = failTc ("handler case '" ++ name ++ "' is not an effect operation")

removeEffects :: [EffectRef] -> [Ty] -> [Ty]
removeEffects handled effects = foldl' (flip removeEffect) effects handled

inferMatchM :: [Expr] -> [MatchCase] -> TcContext -> Tc Inferred
inferMatchM [] cases ctx = do
  arity <- maybe (failTc "empty anonymous match") pure (matchCaseArity cases)
  scrutTys <- freshManyM arity
  Inferred branchTy branchEffects branchNeeds core <- inferMatchCasesM cases scrutTys ctx
  st <- getTc
  pure (Inferred (curriedFunction (applyStateAll scrutTys st) branchEffects branchTy) [] (applyStateNeeds branchNeeds st) core)
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
  pure (Inferred branchTy (unionEffects (applyStateEffects scrutEffects st) branchEffects) (unionNeeds (applyStateNeeds scrutNeeds st) branchNeeds) (EMatch scrutinees2 cases2))

inferMatchCasesM :: [MatchCase] -> [Ty] -> TcContext -> Tc Inferred
inferMatchCasesM cases scrutTys ctx = do
  ((branchTy, _, effects, needs), cases2) <- mapAccumM step (Nothing, scrutTys, [], []) cases
  st <- getTc
  checkExhaustiveM (applyStateAll scrutTys st) cases2 ctx
  case branchTy of
    Just t -> pure (Inferred (applyState t st) effects (applyStateNeeds needs st) (EMatch [] cases2))
    Nothing -> do
      t <- freshM
      pure (Inferred (applyState t st) [] [] (EMatch [] []))
 where
  step (branchTy, currentScrutTys, effects, needs) (MatchCase ps e) = do
    ctx2 <- bindPatternsM (NE.toList ps) currentScrutTys ctx
    Inferred t caseEffects caseNeeds e2 <- inferExprM e ctx2
    ps2 <- traverse (resolvePatternM ctx) ps
    mapM_ (`unifyM` t) branchTy
    st <- getTc
    pure
      (
        ( Just (applyState t st)
        , applyStateAll currentScrutTys st
        , unionEffects effects caseEffects
        , unionNeeds needs caseNeeds
        )
      , MatchCase ps2 e2
      )

-- global declarations are checked and elaborated in one sequential pass. an
-- anonymous recursive let needeþ a second inference after its scheme existeþ.
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

elaborateSequentialDeclM :: DeclarationScope -> Decl -> TcContext -> Tc (Decl, TcContext, [Ty])
elaborateSequentialDeclM scope (Export nested) ctx = do
  (nested2, ctx2, effects) <- elaborateSequentialDeclM scope nested ctx
  pure (Export nested2, exportDeclContext nested ctx2, effects)
elaborateSequentialDeclM LocalDeclarations (Let name _ EForeign{}) _ =
  failTc ("fremmed let '" ++ name ++ "' is only allowed at file level")
elaborateSequentialDeclM GlobalDeclarations (Let name Nothing EForeign{}) _ =
  failTc ("fremmed let '" ++ name ++ "' requireþ a type annotation")
elaborateSequentialDeclM GlobalDeclarations declaration@(Let name (Just ann) (EForeign hostKey)) ctx = do
  checkForeignLetM name hostKey ann ctx
  pure (declaration, addElaboratedBinding GlobalDeclarations name (typeAnnScheme ann ctx) ctx, [])
elaborateSequentialDeclM scope (Let name annotation expr) ctx = do
  case annotation of
    Just ann | scope == GlobalDeclarations -> do
      checkTypeAnnNeedsM ("let '" ++ name ++ "'") ann ctx
      let scheme = typeAnnScheme ann ctx
          ctx2 = addElaboratedBinding scope name scheme ctx
      (_, effects, checkedNeeds, core) <- checkAnnotatedExprM name ann expr ctx2
      expr2 <- resolveInferredBindingM ctx2 checkedNeeds core
      pure (Let name annotation expr2, ctx2, effects)
    Just ann -> do
      checkTypeAnnNeedsM ("let '" ++ name ++ "'") ann ctx
      (_, effects, checkedNeeds, core) <- checkAnnotatedExprM name ann expr ctx
      let ctx2 = addEnv name (typeAnnScheme ann ctx) ctx
      expr2 <- resolveInferredBindingM ctx2 checkedNeeds core
      pure (Let name annotation expr2, ctx2, effects)
    Nothing | isAnonymousMatchExpr expr -> do
      (localCtx, effects) <- inferUnannotatedBindingM name expr ctx
      inferredCtx <- case scope of
        GlobalDeclarations -> globalizeBindingM name localCtx
        LocalDeclarations -> pure localCtx
      Inferred _ _ needs core <- inferNamedM name expr inferredCtx
      normalizedNeeds <- normalizeNeedsM inferredCtx needs
      expr2 <- resolveInferredBindingM inferredCtx normalizedNeeds core
      pure (Let name annotation expr2, inferredCtx, effects)
    Nothing -> do
      Inferred ty effects needs core <- inferNamedM name expr ctx
      normalizedNeeds <- normalizeNeedsM ctx needs
      st <- getTc
      let scheme = bindingScheme ty normalizedNeeds effects st
          inferredCtx = addElaboratedBinding scope name scheme ctx
      expr2 <- resolveInferredBindingM inferredCtx normalizedNeeds core
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
  Let _ (Just _) EForeign{} -> pure declaration
  EffectDecl parameters effectName operations -> do
    effect <- maybe (failTc ("internal missing checked effect '" ++ effectName ++ "'")) pure (effectRefForName effectName ctx)
    let target = effectIdentity effect
        resolvedOperationType = typeExprFromAppliedTy . normalizeFun . convertTypeExprWith parameters ctx
        resolveOperation (EffectOp name operationType) =
          ( TermExport
              (effectOperationId target name)
              (EffectOperationTerm target (effectOperationArity parameters ctx operationType))
          , EffectOp name (resolvedOperationType operationType)
          )
    pure (ElaboratedEffect target (map resolveOperation operations))
  ElaboratedEffect{} -> pure declaration
  ShapeDecl _ shapeName needs members -> do
    target <- resolveShapeTargetM shapeName ctx
    parents <- traverse (\(ShapeNeed _ name) -> resolveShapeTargetM name ctx) needs
    members2 <- mapMaybe id <$> traverse (elaborateShapeMemberEntryM target shapeName ctx) members
    pure (ElaboratedShape target parents members2)
  ElaboratedShape{} -> pure declaration
  FillDecl types shapeName needs members -> do
    members2 <- elaborateFillMembersM types shapeName needs members ctx
    let key = fillKeyFor (tcModule ctx) (canonicalShapeName shapeName ctx) (map (`convertTypeExpr` ctx) types)
    frame <- resolveShapeTargetM shapeName ctx
    pure (ElaboratedFill key frame types shapeName needs members2)
  ElaboratedFill{} -> pure declaration
  other -> pure other

resolveInferredBindingM :: TcContext -> [Need] -> Expr -> Tc Expr
resolveInferredBindingM ctx needs core =
  let bindings = evidenceBindings needs
   in wrapEvidence bindings <$> resolveExprEvidenceM ctx bindings core

-- laws have no runtime term; executable frame members retain their nominal
-- selector identities for Core.
elaborateShapeMemberEntryM :: SymbolId -> String -> TcContext -> ShapeMember -> Tc (Maybe (TermExport, ShapeMember))
elaborateShapeMemberEntryM _ _ _ ShapeLaw{} = pure Nothing
elaborateShapeMemberEntryM frame shapeName ctx member@(ShapeSpec name _) = do
  let qualifiedName = shapeName ++ "@" ++ name
  target <- case listToMaybe [found | EnvBinding{envName, envTarget = Just found} <- tcEnv ctx, envName == qualifiedName, termExportKind found == ShapeMemberTerm frame] of
    Just found -> pure found
    Nothing -> failTc ("internal missing frame member '" ++ qualifiedName ++ "' during elaboration")
  pure (Just (target, member))

elaborateFillMembersM :: [TypeExpr] -> String -> [ShapeNeed] -> [Decl] -> TcContext -> Tc [Decl]
elaborateFillMembersM types shapeName fillNeeds members ctx = case fillSpecsForShape shapeName types ctx [] of
  Nothing -> failTc ("internal missing fill frame '" ++ shapeName ++ "' during elaboration")
  Just specs -> traverse (elaborateMember specs) members
 where
  -- inherited members receive the dictionary for the declared fill. member
  -- names themselves are not recursive term bindings.
  internalNeeds = fillMemberInternalNeeds types shapeName fillNeeds
  elaborateMember specs (Let name annotation expr) = do
    FillSpec{fillSpecType} <- requireFillSpecM name specs
    let memberAnn = resolvedTypeAnnWithInternalNeeds ctx internalNeeds fillSpecType
    (expected, needs) <- instantiateResolvedAnnotationM memberAnn
    (_, _, checkedNeeds, core) <- checkExprAgainstM name expected [] needs expr ctx
    let bindings = evidenceBindings checkedNeeds
    resolved <- resolveExprEvidenceM ctx bindings core
    pure (Let name annotation (wrapExternalEvidence internalNeeds bindings resolved))
  elaborateMember _ _ = failTc "fill member must be a let"

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
        unitTy = hostTy (HostCon (fromMaybe "𝟙" (canonicalHostTypeName "𝟙")))
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
    ( "main must have type 𝟙 → 𝟙, optionally with console, random, async, file, system, clock, process, or web effects; found "
        ++ showTy actual
    )

declLabel :: Decl -> String
declLabel = \case
  Import path _ -> "use '" ++ path ++ "'"
  Let name _ _ -> "let '" ++ name ++ "'"
  TypeAlias _ name _ -> "let-ilk '" ++ name ++ "'"
  DataDecl _ name _ -> "ilk '" ++ name ++ "'"
  EffectDecl _ name _ -> "deed '" ++ name ++ "'"
  ElaboratedEffect target _ -> "deed '" ++ symbolName target ++ "'"
  ShapeDecl _ name _ _ -> "frame '" ++ name ++ "'"
  ElaboratedShape target _ _ -> "frame '" ++ symbolName target ++ "'"
  FillDecl tyArgs shapeName _ _ -> "fill '" ++ showShapeHeader tyArgs shapeName ++ "'"
  ElaboratedFill _ _ tyArgs shapeName _ _ -> "fill '" ++ showShapeHeader tyArgs shapeName ++ "'"
  Export declaration -> "show " ++ declLabel declaration
  ReExport name -> "show '" ++ name ++ "'"
  ReExportType name -> "show-ilk '" ++ name ++ "'"

showShapeHeader :: [TypeExpr] -> String -> String
showShapeHeader arguments name = case map showTypeExprLabel arguments of
  [] -> name
  first : rest -> unwords (first : name : rest)

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
  if all isRunnableEffect actualEffects
    then pure ()
    else failTc ("main hath unsupported effects {" ++ showEffects actualEffects ++ "}; only console, random, async, file, system, clock, process, and web are allowed")

isRunnableEffect :: Ty -> Bool
isRunnableEffect effect = case effectHeadAndArgs effect of
  Just (NominalEffect EffectRef{effectIdentity = SymbolId RuntimeModule name}, []) -> name `elem` runnerEffectNames
  _ -> False

checkDeclM :: Decl -> TcContext -> Tc ()
checkDeclM d ctx = case d of
  TypeAlias params _ target -> checkTypeExprNamesWithM params "type alias" target ctx
  DataDecl params dataName constructors -> mapM_ (checkCtor params dataName) constructors
  EffectDecl params effectName ops -> checkEffectDeclM params effectName ops ctx
  ShapeDecl params shapeName needs members -> checkShapeDeclM params shapeName needs members ctx
  FillDecl tyArgs shapeName needs members -> checkFillM tyArgs shapeName needs members ctx
  _ -> pure ()
 where
  checkCtor params dataName (Ctor _ fields) =
    mapM_ (\field -> checkTypeExprNamesWithM params ("data '" ++ dataName ++ "'") field ctx) fields

checkEffectDeclM :: [String] -> String -> [EffectOp] -> TcContext -> Tc ()
checkEffectDeclM params effectName ops ctx = mapM_ checkOp ops
 where
  checkOp (EffectOp opName t) = do
    checkTypeExprNamesWithM params ("effect operation '" ++ effectName ++ "." ++ opName ++ "'") t ctx
    case normalizeFun (convertTypeExprWith params ctx t) of
      TyFun{} -> pure ()
      _ -> failTc ("effect operation '" ++ effectName ++ "." ++ opName ++ "' must have a function type")

checkShapeDeclM :: [String] -> String -> [ShapeNeed] -> [ShapeMember] -> TcContext -> Tc ()
checkShapeDeclM shapeParams shapeName needs members ctx = do
  checkShapeNeedsWithM shapeParams ("frame '" ++ shapeName ++ "'") needs ctx
  checkShapeMemberNeedsM shapeParams shapeName members ctx
  checkShapeLawsM shapeParams shapeName needs members ctx

checkShapeNeedsM :: String -> [ShapeNeed] -> TcContext -> Tc ()
checkShapeNeedsM = checkShapeNeedsWithM []

checkShapeNeedsWithM :: [String] -> String -> [ShapeNeed] -> TcContext -> Tc ()
checkShapeNeedsWithM bound owner needs ctx = mapM_ checkNeed needs
 where
  checkNeed (ShapeNeed args neededName) = do
    mapM_ (\argument -> checkTypeExprNamesWithM bound owner argument ctx) args
    case findShapeInfo neededName ctx of
      Nothing -> failTc (owner ++ " hath unknown graith frame '" ++ neededName ++ "'")
      Just ShapeInfo{shapeParams} -> do
        let expected = length shapeParams
            actual = length args
        unless (actual == expected) $
          failTc (owner ++ " hath graith '" ++ neededName ++ "' with " ++ show actual ++ " arguments, but '" ++ neededName ++ "' hath " ++ show expected ++ " parameters")

checkShapeMemberNeedsM :: [String] -> String -> [ShapeMember] -> TcContext -> Tc ()
checkShapeMemberNeedsM bound shapeName members ctx = mapM_ checkMember (mapMaybe shapeMemberSignature members)
 where
  checkMember (name, annotation) = checkTypeAnnNeedsWithM bound ("frame member '" ++ shapeName ++ "." ++ name ++ "'") annotation ctx

checkTypeAnnNeedsM :: String -> TypeAnn -> TcContext -> Tc ()
checkTypeAnnNeedsM = checkTypeAnnNeedsWithM []

checkTypeAnnNeedsWithM :: [String] -> String -> TypeAnn -> TcContext -> Tc ()
checkTypeAnnNeedsWithM bound owner (TypeAnn value needs) ctx =
  checkTypeExprNamesWithM bound owner value ctx >> checkShapeNeedsWithM bound owner needs ctx

checkTypeExprNamesM :: String -> TypeExpr -> TcContext -> Tc ()
checkTypeExprNamesM = checkTypeExprNamesWithM []

checkTypeExprNamesWithM :: [String] -> String -> TypeExpr -> TcContext -> Tc ()
checkTypeExprNamesWithM bound owner expr ctx = case expr of
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
    checkTypeExprNamesWithM bound owner result ctx

checkAliasArityM :: String -> String -> Int -> TcContext -> Tc ()
checkAliasArityM owner name actual ctx = case unshownTypeInfo <$> findTypeInfoForTypeSystem name ctx of
  Just (Alias canonical params _) -> do
    let expected = length params
    unless (actual == expected) $
      failTc (owner ++ " useþ type alias '" ++ canonical ++ "' with " ++ show actual ++ " arguments, but it hath " ++ show expected ++ " parameters")
  _ -> pure ()

checkTypeNameM :: String -> String -> TcContext -> Tc ()
checkTypeNameM owner name ctx@TcContext{tcTypeAmbiguities}
  | isQualifiedName name =
      when (isNothing (findTypeInfoForTypeSystem name ctx)) $
        failTc (owner ++ " useþ unknown type '" ++ name ++ "'")
  | Just choices <- Map.lookup name tcTypeAmbiguities =
      failTc (owner ++ " useþ ambiguous type '" ++ name ++ "'; qualify it as one of: " ++ intercalate ", " choices)
  | otherwise = pure ()

checkShapeLawsM :: [String] -> String -> [ShapeNeed] -> [ShapeMember] -> TcContext -> Tc ()
checkShapeLawsM shapeParams shapeName shapeNeeds members ctx = zipWithM_ checkLaw [(1 :: Int) ..] laws
 where
  laws = [(parameters, left, right) | ShapeLaw parameters left right <- members]
  availableNeeds = map (convertShapeNeedWith shapeParams ctx) (ownShapeNeed shapeName ctx ++ shapeNeeds)

  checkLaw number (parameters, left, right) = mapTcError check (\message -> "in law " ++ show number ++ " of frame '" ++ shapeName ++ "': " ++ message)
   where
    check = do
      mapM_ (\(_, ty) -> checkTypeExprNamesWithM shapeParams ("law of frame '" ++ shapeName ++ "'") ty ctx) parameters
      let rawParameterTypes = map (convertTypeExprWith shapeParams ctx . snd) parameters
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

checkForeignLetM :: String -> String -> TypeAnn -> TcContext -> Tc ()
checkForeignLetM name hostKey ann ctx = do
  checkTypeAnnNeedsM ("fremmed let '" ++ name ++ "'") ann ctx
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

requirePureImmediateEffectsM :: String -> [Ty] -> Tc ()
requirePureImmediateEffectsM owner effects = do
  st <- getTc
  let actualEffects = applyStateEffects effects st
  unless (null actualEffects) $
    failTc (owner ++ " hath immediate effects {" ++ showEffects actualEffects ++ "}; use a function type or run it from a runnable file")

checkFillM :: [TypeExpr] -> String -> [ShapeNeed] -> [Decl] -> TcContext -> Tc ()
checkFillM tyArgs shapeName needs members ctx = do
  mapM_ (\tyArg -> checkTypeExprNamesM ("fill '" ++ shapeName ++ "'") tyArg ctx) tyArgs
  checkShapeNeedsM ("fill '" ++ shapeName ++ "'") needs ctx
  let actualShape = canonicalShapeName shapeName ctx
      key = fillKeyFor (tcModule ctx) actualShape (map (`convertTypeExpr` ctx) tyArgs)
      duplicateCount =
        length
          [ ()
          | FillInfo{fillKey, fillDepth} <- fillsForShape actualShape ctx
          , fillDepth == 0
          , fillKey == key
          ]
  unless (duplicateCount == 1) (failTc ("duplicate fill '" ++ showShapeHeader tyArgs shapeName ++ "'"))
  case findShapeInfo shapeName ctx of
    Nothing -> failTc ("unknown frame '" ++ shapeName ++ "' in fill")
    Just ShapeInfo{shapeParams} -> do
      checkFillArityM shapeName shapeParams tyArgs
      case fillSpecsForShape shapeName tyArgs ctx [] of
        Nothing -> failTc ("fill '" ++ shapeName ++ "' hath an unknown required frame")
        Just specs -> checkFillMembersM shapeName tyArgs needs members specs ctx

checkFillArityM :: String -> [String] -> [TypeExpr] -> Tc ()
checkFillArityM shapeName shapeParams tyArgs =
  unless (length tyArgs == length shapeParams) $
    failTc ("fill '" ++ shapeName ++ "' hath " ++ show (length tyArgs) ++ " type arguments, but '" ++ shapeName ++ "' hath " ++ show (length shapeParams) ++ " parameters")

data FillSpec = FillSpec
  { fillSpecName :: String
  , fillSpecShape :: ShapeRef
  , fillSpecTypes :: [Ty]
  , fillSpecType :: ResolvedTypeAnn
  }
  deriving (Eq, Show)

fillSpecsForShape :: String -> [TypeExpr] -> TcContext -> [ShapeRef] -> Maybe [FillSpec]
fillSpecsForShape shapeName fillTypes ctx =
  fillSpecsForShapeRef (shapeRefForName shapeName ctx) (map (`convertTypeExpr` ctx) fillTypes) ctx

fillSpecsForShapeRef :: ShapeRef -> [Ty] -> TcContext -> [ShapeRef] -> Maybe [FillSpec]
fillSpecsForShapeRef frame fillTypes ctx seen
  | frame `elem` seen = Just []
  | otherwise = case findShapeInfoByRef frame ctx of
      Nothing -> Nothing
      Just ShapeInfo{..}
        | length fillTypes /= length shapeParams -> Nothing
        | otherwise -> do
            inherited <- fillSpecsForNeeds shapeNeeds shapeParams fillTypes ctx (frame : seen)
            pure (fillMemberSpecs frame shapeParams fillTypes shapeMethods ++ inherited)

fillSpecsForNeeds :: [Need] -> [String] -> [Ty] -> TcContext -> [ShapeRef] -> Maybe [FillSpec]
fillSpecsForNeeds needs currentParams currentFillTypes ctx seen =
  concat <$> traverse specsForNeed needs
 where
  specsForNeed need = do
    let Need neededFillTypes neededShape = specializeNeed currentParams currentFillTypes need
    fillSpecsForShapeRef neededShape neededFillTypes ctx seen

fillMemberSpecs :: ShapeRef -> [String] -> [Ty] -> [ShapeMethod] -> [FillSpec]
fillMemberSpecs frame shapeParams fillTypes = map makeSpec
 where
  makeSpec ShapeMethod{..} =
    FillSpec shapeMethodName frame fillTypes (specializeResolvedTypeAnn shapeParams fillTypes shapeMethodType)

requireFillSpecM :: String -> [FillSpec] -> Tc FillSpec
requireFillSpecM name = maybe (failTc ("fill defineþ unknown member '" ++ name ++ "'")) pure . find ((== name) . fillSpecName)

checkFillMembersM :: String -> [TypeExpr] -> [ShapeNeed] -> [Decl] -> [FillSpec] -> TcContext -> Tc ()
checkFillMembersM shapeName tyArgs needs members specs ctx = do
  requireFillMembersM shapeName tyArgs members specs ctx
  mapM_ (checkFillMemberM tyArgs shapeName needs specs ctx) members
  checkRedundantFillNeedsM shapeName tyArgs needs members specs ctx

checkFillMemberM :: [TypeExpr] -> String -> [ShapeNeed] -> [FillSpec] -> TcContext -> Decl -> Tc ()
checkFillMemberM tyArgs shapeName needs specs ctx = \case
  Let name _ expr -> do
    FillSpec{fillSpecShape, fillSpecType} <- requireFillSpecM name specs
    let memberAnn = resolvedTypeAnnWithInternalNeeds ctx (fillMemberInternalNeeds tyArgs shapeName needs) fillSpecType
    mapTcError
      ( do
          (expected, expectedNeeds) <- instantiateResolvedAnnotationM memberAnn
          void (checkExprAgainstM name expected [] expectedNeeds expr ctx)
      )
      (\msg -> "in fill member '" ++ shapeDisplayName fillSpecShape ++ "." ++ name ++ "': " ++ msg)
  _ -> failTc "fill member must be a let"

checkRedundantFillNeedsM :: String -> [TypeExpr] -> [ShapeNeed] -> [Decl] -> [FillSpec] -> TcContext -> Tc ()
checkRedundantFillNeedsM shapeName tyArgs needs members specs ctx = do
  redundant <- findRedundant (zip [(0 :: Int) ..] needs)
  case redundant of
    Just need -> failTc (owner ++ " hath redundant graiþ " ++ showSourceNeed (convertShapeNeed need ctx))
    Nothing -> pure ()
 where
  owner = "fill '" ++ showShapeHeader tyArgs shapeName ++ "'"
  ownKey = fillKeyFor (tcModule ctx) (canonicalShapeName shapeName ctx) (map (`convertTypeExpr` ctx) tyArgs)
  parentNeeds = fillParentNeeds shapeName tyArgs ctx
  checkWithout index = do
    let remaining = [need | (otherIndex, need) <- zip [(0 :: Int) ..] needs, otherIndex /= index]
    mapM_ (checkFillMemberM tyArgs shapeName remaining specs ctx) members
    checkParentEvidenceM remaining
  checkParentEvidenceM available = do
    let external = map (`convertShapeNeed` ctx) available
        scheme = generalizeTyWithNeeds (external ++ parentNeeds) (TyCon "integer")
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
    , fillSpecName `notElem` provided
    , fillSpecName `notElem` inheritedProvided
    , fillSpecShape == currentShapeRef || not (hasDirectFill fillSpecShape fillSpecTypes ctx)
    ]
  currentShapeRef = shapeRefForName currentShape ctx
  currentTypeValues = map (`convertTypeExpr` ctx) currentTypes
  inheritedProvided = case findShapeInfo currentShape ctx of
    Nothing -> []
    Just ShapeInfo{shapeParams, shapeNeeds} -> concatMap providedByNeed shapeNeeds
     where
      providedByNeed need =
        let Need neededTypes neededShape = specializeNeed shapeParams currentTypeValues need
         in if hasDirectFill neededShape neededTypes ctx
              then maybe [] (map fillSpecName) (fillSpecsForShapeRef neededShape neededTypes ctx [])
              else []

hasDirectFill :: ShapeRef -> [Ty] -> TcContext -> Bool
hasDirectFill wantedShape wantedTypes ctx = any matches (fillsForShapeRef wantedShape ctx)
 where
  matches FillInfo{..} =
    fillDepth == 0
      && isJust (zipWithExact typePatternBindings fillTypes wantedTypes >>= mergeBindingMaps)

fillMemberInternalNeeds :: [TypeExpr] -> String -> [ShapeNeed] -> [ShapeNeed]
fillMemberInternalNeeds types shapeName = (ShapeNeed types shapeName :)

-- local declarations are checked and elaborated in one sequential walk. an
-- anonymous recursive let alone needeþ a second inference pass so recursive
-- calls receive þe evidence parameters of its finalized scheme.
inferLocalDeclsM :: [Decl] -> TcContext -> Tc ([Decl], TcContext, [Ty])
inferLocalDeclsM declarations ctx = do
  ((ctx2, effects), declarations2) <- mapAccumM step (ctx, []) declarations
  pure (declarations2, ctx2, effects)
 where
  step (currentCtx, currentEffects) declaration = do
    (declaration2, ctx2, declarationEffects) <- elaborateSequentialDeclM LocalDeclarations declaration currentCtx
    pure ((ctx2, unionEffects currentEffects declarationEffects), declaration2)

inferUnannotatedBindingM :: String -> Expr -> TcContext -> Tc (TcContext, [Ty])
inferUnannotatedBindingM name expr ctx | isAnonymousMatchExpr expr = do
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
instantiateAnnotationM = instantiateAnnotationWithM []

instantiateAnnotationWithM :: [String] -> TypeAnn -> TcContext -> Tc (Ty, [Ty], [Need])
instantiateAnnotationWithM bound ann ctx = do
  (ty, needs) <- skolemizeM (typeAnnSchemeWith bound ctx ann)
  pure (ty, [], needs)

resolvedTypeAnnWithInternalNeeds :: TcContext -> [ShapeNeed] -> ResolvedTypeAnn -> ResolvedTypeAnn
resolvedTypeAnnWithInternalNeeds ctx internal (ResolvedTypeAnn ty needs) =
  ResolvedTypeAnn ty (map (`convertShapeNeed` ctx) internal ++ needs)

instantiateResolvedAnnotationM :: ResolvedTypeAnn -> Tc (Ty, [Need])
instantiateResolvedAnnotationM (ResolvedTypeAnn ty needs) = skolemizeM (generalizeTyWithNeeds needs ty)

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
  checkExhaustiveM (applyStateAll patternArgs st) cases2 ctx
  checkNeedsAgainstM name ctx actualNeeds expectedNeeds
  checked <- applyStateNeeds expectedNeeds <$> getTc
  pure (checked, EMatch [] cases2)
 where
  arity = functionCaseArity (NE.length args) cases
  (patternArgs, remainingArgs) = splitAt arity (NE.toList args)
  step needs (MatchCase ps body) = do
    ctx2 <- bindPatternsM (NE.toList ps) patternArgs ctx
    ps2 <- traverse (resolvePatternM ctx) ps
    (bodyNeeds, body2) <- case NE.nonEmpty remainingArgs of
      Nothing -> do
        Inferred bodyTy bodyEffects inferredNeeds core <- inferNamedM name body ctx2
        mapTcError (unifyM bodyTy ret) (\msg -> "in '" ++ name ++ "': " ++ msg ++ "; actual " ++ showTy bodyTy ++ ", expected " ++ showTy ret)
        _ <- checkEffectsAgainstM name ret bodyEffects latentEffects
        pure (inferredNeeds, core)
      Just rest -> do
        (_, _, inferredNeeds, core) <- checkExprAgainstM name (TyFun rest latentEffects ret) [] expectedNeeds body ctx2
        pure (inferredNeeds, core)
    pure (unionNeeds needs bodyNeeds, MatchCase ps2 body2)

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

needSubsumes :: TcContext -> Need -> Need -> Bool
needSubsumes _ (Need expectedArgs expectedShape) (Need actualArgs actualShape) =
  expectedShape == actualShape
    && length expectedArgs == length actualArgs
    && hasConsistentTypePatternBindings expectedArgs actualArgs

needCovers :: TcContext -> [ShapeRef] -> Need -> Need -> Bool
needCovers ctx seen graith@(Need graithArgs graithShape) wanted
  | needSubsumes ctx graith wanted = True
  | graithShape `elem` seen = False
  | otherwise = case findShapeInfoByRef graithShape ctx of
      Nothing -> False
      Just ShapeInfo{shapeParams, shapeNeeds} ->
        any (\need -> needCovers ctx (graithShape : seen) (specializeNeed shapeParams graithArgs need) wanted) shapeNeeds

fillNeedsForNeed :: TcContext -> Need -> Maybe [Need]
fillNeedsForNeed ctx (Need tys frame) =
  if all hasConcreteHead tys then listToMaybe (mapMaybe matches (fillsForShapeRef frame ctx)) else Nothing
 where
  matches info@FillInfo{..} = do
    bindings <- zipWithExact typePatternBindings fillTypes tys >>= mergeBindingMaps
    pure (projectFillNeeds frame info (map (needWithBindings bindings) fillNeeds))

-- evidence holes keep inference independent of fill selection. resolution turns
-- them into hidden local parameters or a statically chosen dictionary tree.
type EvidenceBinding = (Need, Int)

-- evidence parameters are ordinary hidden pure arguments after elaboration.
evidenceBindings :: [Need] -> [EvidenceBinding]
evidenceBindings needs = zip needs [(0 :: Int) ..]

wrapEvidence :: [EvidenceBinding] -> Expr -> Expr
wrapEvidence [] expr = expr
wrapEvidence ((_, ident) : bindings) expr = EEvidenceLambda (ident :| map snd bindings) expr

wrapExternalEvidence :: [ShapeNeed] -> [EvidenceBinding] -> Expr -> Expr
wrapExternalEvidence internal = wrapEvidence . drop (length internal)

resolveEvidenceHoleM :: TcContext -> [EvidenceBinding] -> Int -> Tc Expr
resolveEvidenceHoleM ctx available ident = do
  st@TcState{tcEvidenceNeeds} <- getTc
  need <- maybe (failTc "internal missing evidence hole") pure (IntMap.lookup ident tcEvidenceNeeds)
  resolveNeedEvidenceM ctx available (applyStateNeed need st)

resolveNeedEvidenceM :: TcContext -> [EvidenceBinding] -> Need -> Tc Expr
resolveNeedEvidenceM ctx available = resolveNeedEvidenceSeenM ctx available []

resolveNeedEvidenceSeenM :: TcContext -> [EvidenceBinding] -> [FillId] -> Need -> Tc Expr
resolveNeedEvidenceSeenM ctx available seen wanted = case bestLocalEvidence ctx available wanted of
  Left message -> failTc message
  Right (Just ident) -> pure (EEvidence ident)
  Right Nothing -> do
    (info, nestedNeeds, parentNeeds) <- selectFillEvidenceM ctx wanted
    buildFillEvidenceM ctx available seen info nestedNeeds parentNeeds

buildFillEvidenceM :: TcContext -> [EvidenceBinding] -> [FillId] -> FillInfo -> [Need] -> [Need] -> Tc Expr
buildFillEvidenceM ctx available seen info nestedNeeds parentNeeds = do
  let key = fillKey info
  when (key `elem` seen) (failTc ("recursive fill evidence for " ++ renderFillId (fillKey info)))
  nested <- traverse (resolveNeedEvidenceSeenM ctx available (key : seen)) nestedNeeds
  parents <- resolveParents key [] parentNeeds
  pure (EDictionary key (shapeIdentity (fillShape info)) nested parents)
 where
  resolveParents ownKey skipped = fmap concat . traverse (resolveParent ownKey skipped)
  resolveParent ownKey skipped parentNeed
    | parentNeed `elem` skipped = failTc ("recursive parent fill evidence for " ++ showNeed parentNeed)
    | otherwise = case bestLocalEvidence ctx available parentNeed of
        Left message -> failTc message
        Right (Just ident) -> pure [EEvidence ident]
        Right Nothing -> do
          (parentInfo, nested, parents) <- selectFillEvidenceM ctx parentNeed
          if fillKey parentInfo == ownKey
            then resolveParents ownKey (parentNeed : skipped) parents
            else (: []) <$> buildFillEvidenceM ctx available (ownKey : seen) parentInfo nested parents

bestLocalEvidence :: TcContext -> [EvidenceBinding] -> Need -> Either String (Maybe Int)
bestLocalEvidence ctx available wanted = choose (filter (covers . fst) available)
 where
  covers given = needCovers ctx [] given wanted
  choose [] = Right Nothing
  choose candidates =
    let bestRank = minimum (map (localEvidenceRank . fst) candidates)
        identifiers = [ident | (need, ident) <- candidates, localEvidenceRank need == bestRank]
     in case (bestRank, identifiers) of
          (_, []) -> Right Nothing
          (0, name : _) -> Right (Just name)
          (_, [name]) -> Right (Just name)
          _ -> Left ("ambiguous graith evidence for " ++ showNeed wanted)
  localEvidenceRank given = if needSubsumes ctx given wanted then (0 :: Int) else 1

selectFillEvidenceM :: TcContext -> Need -> Tc (FillInfo, [Need], [Need])
selectFillEvidenceM ctx wanted@(Need actualTypes frame)
  | not (all hasConcreteHead actualTypes) = failTc ("missing graith " ++ showNeed wanted)
  | otherwise = case bestCandidates of
      [] -> failTc ("missing graith " ++ showNeed wanted)
      [(info, needs, parents)] -> pure (info, needs, parents)
      choices -> failTc ("ambiguous fills for " ++ showNeed wanted ++ ": " ++ intercalate ", " (nub [renderFillId (fillKey info) | (info, _, _) <- choices]))
 where
  -- selection is static: inheritance depth rankeþ fills first, then a
  -- structured type pattern outrankeþ a blanket pattern it refines.
  candidates = mapMaybe match (fillsForShapeRef frame ctx)
  match info@FillInfo{..}
    | not (fillSuppliesShape ctx info) = Nothing
    | otherwise = do
        bindings <- zipWithExact typePatternBindings fillTypes actualTypes >>= mergeBindingMaps
        let specialize = projectFillNeeds frame info . map (needWithBindings bindings)
        pure (info, specialize fillNeeds, specialize fillParents)
  candidateRank (FillInfo{fillKey, fillDepth}, _, _)
    | isPrimitiveFillId fillKey = maxBound :: Int
    | otherwise = fillDepth
  ranked = case candidates of
    [] -> []
    _ -> let rank = minimum (map candidateRank candidates) in filter ((== rank) . candidateRank) candidates
  uniqueRanked = nubByFillKey ranked
  bestCandidates = filter (\candidate -> not (any (`fillMoreSpecificThan` candidate) uniqueRanked)) uniqueRanked

  fillMoreSpecificThan (left, _, _) (right, _, _) =
    let leftTypes = fillTypes left
        rightTypes = fillTypes right
     in hasConsistentTypePatternBindings rightTypes leftTypes
          && not (hasConsistentTypePatternBindings leftTypes rightTypes)

fillSuppliesShape :: TcContext -> FillInfo -> Bool
fillSuppliesShape _ FillInfo{fillDepth = 0} = True
fillSuppliesShape ctx FillInfo{fillShape, fillMembers} = case findShapeInfoByRef fillShape ctx of
  Nothing -> False
  Just ShapeInfo{shapeMethods} ->
    let methods = map shapeMethodName shapeMethods
     in null methods || any (`elem` fillMembers) methods

nubByFillKey :: [(FillInfo, [Need], [Need])] -> [(FillInfo, [Need], [Need])]
nubByFillKey = Map.elems . foldl' add Map.empty
 where
  add rest candidate@(info, _, _) = Map.insertWith (\_ old -> old) (fillKey info) candidate rest

resolveExprEvidenceM :: TcContext -> [EvidenceBinding] -> Expr -> Tc Expr
resolveExprEvidenceM ctx available = go
 where
  go = \case
    ELocated span expression -> ELocated span <$> go expression
    literal@EInteger{} -> pure literal
    literal@EFloat{} -> pure literal
    literal@EUnicode{} -> pure literal
    literal@EText{} -> pure literal
    fremmed@EForeign{} -> pure fremmed
    variable@EVar{} -> pure variable
    global@EGlobal{} -> pure global
    evidence@EEvidence{} -> pure evidence
    EEvidenceLambda identifiers body -> EEvidenceLambda identifiers <$> go body
    EAscribe expression annotation -> EAscribe <$> go expression <*> pure annotation
    EApply function arguments -> EApply <$> go function <*> traverse go arguments
    ERecord fields -> ERecord <$> traverse (traverse go) fields
    EField base field -> EField <$> go base <*> pure field
    EUpdate base updates -> EUpdate <$> go base <*> traverse resolveUpdate updates
    ETry body returnCase cases -> ETry <$> go body <*> traverse resolveReturn returnCase <*> traverse resolveHandler cases
    EMatch scrutinees cases -> EMatch <$> traverse go scrutinees <*> traverse resolveCase cases
    EBlock declarations body -> EBlock declarations <$> go body
    EWithEvidence function evidence -> do
      function2 <- go function
      dictionaries <- traverse resolveDictionary evidence
      pure $ case dictionaries of
        [] -> function2
        first : rest -> EApply function2 (first :| rest)
    EDictionary key frame nested parents -> EDictionary key frame <$> traverse go nested <*> traverse go parents
  resolveDictionary = resolveEvidenceHoleM ctx available
  resolveUpdate = \case
    RecordSet name expr -> RecordSet name <$> go expr
    update@RecordRemove{} -> pure update
  resolveReturn (ReturnCase pattern body) = ReturnCase pattern <$> go body
  resolveHandler (HandlerCase name patterns body) = HandlerCase name patterns <$> go body
  resolveHandler (ResolvedHandlerCase target patterns body) = ResolvedHandlerCase target patterns <$> go body
  resolveCase (MatchCase patterns body) = MatchCase patterns <$> go body

needWithBindings :: Map.Map String Ty -> Need -> Need
needWithBindings bindings (Need args frame) =
  Need (map (replaceVarsWithBindings bindings) args) frame

replaceVarsWithBindings :: Map.Map String Ty -> Ty -> Ty
replaceVarsWithBindings bindings ty = case ty of
  TyVar name -> Map.findWithDefault ty name bindings
  TyApp name args
    | Just (TyCon headName) <- Map.lookup name bindings -> TyApp headName (map (replaceVarsWithBindings bindings) args)
    | Just (TyVar headName) <- Map.lookup name bindings -> TyApp headName (map (replaceVarsWithBindings bindings) args)
    | Just (TyNamed ref prefix) <- Map.lookup name bindings -> TyNamed ref (prefix ++ map (replaceVarsWithBindings bindings) args)
    | otherwise -> TyApp name (map (replaceVarsWithBindings bindings) args)
  _ -> mapTyChildren (replaceVarsWithBindings bindings) (skipEffectVar (replaceVarsWithBindings bindings)) ty

typePatternBindings :: Ty -> Ty -> Maybe (Map.Map String Ty)
typePatternBindings patternTy actualTy = case (normalizeFun patternTy, normalizeFun actualTy) of
  (TyVar name, ty) -> Just (Map.singleton name ty)
  (TyMeta ident, ty) -> Just (Map.singleton ("?" ++ show ident) ty)
  (TyCon x, TyCon y) | x == y -> Just Map.empty
  (TyNamed x xs, TyNamed y ys)
    | x == y && length xs == length ys -> zipWithExact typePatternBindings xs ys >>= mergeBindingMaps
  (TyApp x xs, TyApp y ys)
    | x == y && length xs == length ys -> zipWithExact typePatternBindings xs ys >>= mergeBindingMaps
    | isTypeVarName x && length xs == length ys -> do
        bindings <- zipWithExact typePatternBindings xs ys >>= mergeBindingMaps
        mergeBindingMaps [Map.singleton x (TyCon y), bindings]
  (TyApp x xs, TyNamed y ys)
    | isTypeVarName x && length xs == length ys -> do
        bindings <- zipWithExact typePatternBindings xs ys >>= mergeBindingMaps
        mergeBindingMaps [Map.singleton x (TyNamed y []), bindings]
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
  TyNamed{} -> True
  TyFun{} -> True
  _ -> False

needMayRemainOpen :: Need -> Bool
needMayRemainOpen (Need args _) = not (all hasConcreteHead args)

showNeeds :: [Need] -> String
showNeeds = intercalate ", " . map showNeed

showNeed :: Need -> String
showNeed (Need args ShapeRef{shapeDisplayName}) = showTyList args ++ " " ++ shapeDisplayName

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
metaIdsInNeeds = foldNeedTypes metaIdsInTy

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

replaceNeedMetas :: IntMap.IntMap String -> Need -> Need
replaceNeedMetas repls (Need args name) = Need (map (replaceTyMetas repls) args) name

replaceTyMetas :: IntMap.IntMap String -> Ty -> Ty
replaceTyMetas repls ty = case ty of
  TyMeta ident -> TyVar (IntMap.findWithDefault ("m" ++ show ident) ident repls)
  _ -> mapTyChildren (replaceTyMetas repls) (replaceTyMetas repls) ty

-- source-checking entry points share this pipeline and differ only in their
-- top-level mode. context inspection intentionally stoppeþ before Core lowering.
check :: String -> String
check source = checkWithImports source Map.empty

checkWithImports :: String -> Map.Map String String -> String
checkWithImports source imports = checkPrepared source imports False

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

checkProgramWithImportsDetailed :: Program -> Map.Map String String -> Bool -> Maybe TypeFailure
checkProgramWithImportsDetailed program imports runnable =
  either Just (const Nothing) (inferProgramWithModeDetailed program imports (if runnable then Runnable else Bookhoard))

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
  TyNamed TypeRef{typeDisplayName} arguments -> unwords (map showSourceTypeArgument arguments ++ [typeDisplayName])
  TyEffect EffectRef{effectDisplayName} arguments -> unwords (map showSourceTypeArgument arguments ++ [effectDisplayName])
  TyRecord fields -> "r(" ++ intercalate ", " [name ++ ": " ++ showSourceTy ty | (name, ty) <- Map.toList fields] ++ ")"
  TyFun arguments effects result ->
    intercalate " → " (map showSourceTypeArgument (NE.toList arguments) ++ [showSourceTy result])
      ++ if null effects then "" else " ! " ++ intercalate ", " (map showSourceTy effects)

showSourceTypeArgument :: Ty -> String
showSourceTypeArgument ty@TyFun{} = "(" ++ showSourceTy ty ++ ")"
showSourceTypeArgument ty = showSourceTy ty

showSourceNeed :: Need -> String
showSourceNeed (Need arguments ShapeRef{shapeDisplayName}) = case map showSourceTypeArgument arguments of
  [] -> shapeDisplayName
  first : rest -> unwords (first : shapeDisplayName : rest)

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
  fst <$> runStateT (unTc (inferProgramWithModeM program imports mode)) initialTcState{tcLocateImports = True}

elaborateProgramWithImports :: Program -> Map.Map String String -> Bool -> Either String CoreProgram
elaborateProgramWithImports program imports runnable = elaborateWithMode program imports (if runnable then Runnable else Bookhoard)

elaborateInteractiveProgramWithImports :: Program -> Map.Map String String -> Either String CoreProgram
elaborateInteractiveProgramWithImports program imports = elaborateWithMode program imports Interactive

elaborateWithMode :: Program -> Map.Map String String -> ProgramMode -> Either String CoreProgram
elaborateWithMode program imports mode = case inferProgramWithMode program imports mode of
  TcErr message -> Left message
  TcOk elaborated _ -> Right elaborated

inferProgramWithModeM :: Program -> Map.Map String String -> ProgramMode -> Tc CoreProgram
inferProgramWithModeM program imports mode = do
  (elaboratedDecls, checkedCtx) <- checkProgramWithModeM mode [] program imports baseContext
  lowerProgramM elaboratedDecls checkedCtx

checkProgramWithModeM :: ProgramMode -> ImportStack -> Program -> Map.Map String String -> TcContext -> Tc ([Decl], TcContext)
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
  Let{} -> ctx
  Export nested -> collectElaborationContext nested ctx
  other -> collectDeclContext other ctx

collectElaborationProgramContext :: [Decl] -> TcContext -> TcContext
collectElaborationProgramContext declarations ctx =
  foldl' (flip collectElaborationContext) (collectShapeContexts declarations ctx) declarations

inferProgramContext :: Program -> Map.Map String String -> TcContext -> TcResult TcContext
inferProgramContext p imports baseCtx = runTc (inferProgramContextFromM [] p imports baseCtx) initialTcState

inferProgramContextFromM :: ImportStack -> Program -> Map.Map String String -> TcContext -> Tc TcContext
inferProgramContextFromM importStack program imports baseCtx =
  snd <$> checkProgramWithModeM Bookhoard importStack program imports baseCtx

prepareDeclsM :: ImportStack -> [Decl] -> Map.Map String String -> TcContext -> Tc ([Decl], TcContext)
prepareDeclsM importStack ds imports baseCtx = do
  either failTc pure (validateProgram (Program ds))
  importCtx <- collectImportContextsM importStack ds imports baseCtx
  preparedCtx <- resolveTypeHeadersM ds importCtx
  pure (ds, preparedCtx)

-- imports are checked in isolated states. þeir static contexts and lowered
-- core modules are cached by public path and cross back as checked artifacts.
collectImportContextsM :: ImportStack -> [Decl] -> Map.Map String String -> TcContext -> Tc TcContext
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
        let importError message = failImportTc path ("in use '" ++ path ++ "': " ++ message)
        locateImports <- tcLocateImports <$> getTc
        p <- either importError pure (if locateImports then parsedProgram <$> parseLocated source else parse source)
        importedContextM nextStack path p imports
    let canonical = importNamespace path
        namespaced = aliasImportContext canonical (importAlias path alias) (namespaceImportContext canonical importedCtx)
    pure (mergeContext namespaced currentCtx)

importedContextM :: ImportStack -> String -> Program -> Map.Map String String -> Tc TcContext
importedContextM importStack path p imports = Tc $ StateT $ \st ->
  let importedState =
        initialTcState
          { tcImportCache = tcImportCache st
          , tcImportCoreCache = tcImportCoreCache st
          , tcLocateImports = tcLocateImports st
          }
   in case runStateT (unTc imported) importedState of
        Left failure ->
          Left
            failure
              { typeFailureMessage = "in use '" ++ path ++ "': " ++ typeFailureMessage failure
              , typeFailurePath = typeFailurePath failure <|> Just path
              }
        Right ((importedCtx, importedCore), importedSt) ->
          let contextCache = Map.insert path importedCtx (tcImportCache importedSt)
              coreCache = Map.insert path importedCore (tcImportCoreCache importedSt)
           in Right
                ( importedCtx
                , st
                    { tcImportCache = contextCache
                    , tcImportCoreCache = coreCache
                    }
                )
 where
  imported = inferImportedProgramM importStack path p imports

inferImportedProgramM :: ImportStack -> String -> Program -> Map.Map String String -> Tc (TcContext, CoreProgram)
inferImportedProgramM importStack path program imports = do
  let moduleContext = baseContext{tcModule = SourceModule path}
  (elaboratedDecls, checkedCtx) <- checkProgramWithModeM Bookhoard importStack program imports moduleContext
  core <- lowerProgramM elaboratedDecls checkedCtx
  pure (checkedCtx, core)
