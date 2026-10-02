-- | kind checking precedeþ term inference. explicit requirements may bind type
-- parameters; ordinary type expressions never introduce undeclared variables.
module Tung.Kind
  ( KindContext (..),
    emptyKindContext,
    checkKinds,
    inferKinds,
    renameKinds,
    mergeKinds,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless, void, when, zipWithM_, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify')
import Data.Foldable (traverse_)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Tung.Name (isQualifiedName)
import Tung.Primitive (primitiveTypeNames)
import Tung.Syntax

data KindContext = KindContext
  { typeKinds :: Map.Map String KindExpr,
    classKinds :: Map.Map String [KindExpr]
  }
  deriving (Eq, Show)

emptyKindContext :: KindContext
emptyKindContext = KindContext Map.empty Map.empty

renameKinds :: (String -> [String]) -> KindContext -> KindContext
renameKinds names KindContext {..} = KindContext (rename typeKinds) (rename classKinds)
  where
    rename entries = Map.fromList [(alias, value) | (name, value) <- Map.toList entries, alias <- names name]

mergeKinds :: KindContext -> KindContext -> KindContext
mergeKinds left right = KindContext (Map.union (typeKinds left) (typeKinds right)) (Map.union (classKinds left) (classKinds right))

data Kind = Star | Arrow Kind Kind | Meta Int deriving (Eq, Show)

data KindState = KindState
  { nextKind :: Int,
    substitution :: Map.Map Int Kind,
    knownTypes :: Map.Map String Kind,
    classVariables :: Map.Map String [(String, Kind)],
    normalisedClasses :: Map.Map String Decl
  }

type Check = StateT KindState (Either String)

type Scope = Map.Map String Kind

initialState :: KindState
initialState = KindState 0 Map.empty Map.empty Map.empty Map.empty

loadContext :: KindContext -> Check ()
loadContext imported = do
  primitive <- writtenKind (KindArrow KindType (KindArrow KindType KindType))
  imports <- traverse writtenKind (typeKinds imported)
  importedClasses <- traverse (traverse writtenKind) (classKinds imported)
  modify'
    ( \state ->
        state
          { knownTypes = Map.insert "→" primitive (Map.union imports (Map.fromList [(name, Star) | name <- primitiveTypeNames])),
            classVariables = Map.map (zip (repeat "")) importedClasses
          }
    )

-- explicit type inputs use the same kind rules as written declarations.
inferKinds :: KindContext -> [TypeExpr] -> Either String [KindExpr]
inferKinds context expressions = evalStateT check initialState
  where
    check = loadContext context >> traverse (typeKind Map.empty >=> finishedKind) expressions

failure :: String -> Check a
failure = lift . Left

fresh :: Check Kind
fresh = do
  state <- get
  modify' (\current -> current {nextKind = nextKind state + 1})
  pure (Meta (nextKind state))

writtenKind :: KindExpr -> Check Kind
writtenKind KindType = pure Star
writtenKind (KindArrow argument result) = Arrow <$> writtenKind argument <*> writtenKind result
writtenKind KindUnknown = fresh
writtenKind (KindVariable variable) = pure (Meta variable)

resolve :: Kind -> Check Kind
resolve kind = case kind of
  Meta variable -> do
    replacements <- substitution <$> get
    maybe (pure kind) resolve (Map.lookup variable replacements)
  Arrow argument result -> Arrow <$> resolve argument <*> resolve result
  Star -> pure Star

occurs :: Int -> Kind -> Bool
occurs variable = \case
  Meta other -> variable == other
  Arrow argument result -> occurs variable argument || occurs variable result
  Star -> False

unify :: Kind -> Kind -> Check ()
unify left right = do
  first <- resolve left
  second <- resolve right
  case (first, second) of
    _ | first == second -> pure ()
    (Meta variable, kind) -> bind variable kind
    (kind, Meta variable) -> bind variable kind
    (Arrow argument result, Arrow argument2 result2) -> unify argument argument2 >> unify result result2
    _ -> failure "type hath the wrong kind"
  where
    bind variable kind
      | occurs variable kind = failure "recursive kind"
      | otherwise = modify' (\state -> state {substitution = Map.insert variable kind (substitution state)})

finishedKind :: Kind -> Check KindExpr
finishedKind kind =
  resolve kind >>= \case
    Star -> pure KindType
    Arrow argument result -> KindArrow <$> finishedKind argument <*> finishedKind result
    Meta _ -> pure KindType

parameters :: [TypeParameter] -> Check [(String, Kind)]
parameters = traverse (\(name, kind) -> (name,) <$> writtenKind kind)

-- requirements introduce parameters in source order. the type constructor
-- appearþ between the first argument and remaining arguments in tung.
freeNames :: TypeExpr -> [String]
freeNames = \case
  TypeName name -> [name]
  TypeHole _ -> []
  TypeApply name [] -> [name]
  TypeApply name (argument : arguments) -> freeNames argument ++ [name] ++ concatMap freeNames arguments
  TypeRecord fields -> concatMap (freeNames . snd) fields
  TypeArrow arguments effects result -> concatMap freeNames arguments ++ freeNames result ++ concatMap freeNames effects
  TypeForall bound body -> filter (`notElem` parameterNames bound) (freeNames body)

headerBindings :: Scope -> [TypeHeaderEntry] -> Check ([TypeParameter], [TypeParameter])
headerBindings bound entries = do
  types <- knownTypes <$> get
  let explicit = headerParameters entries
      explicitNames = parameterNames explicit
      candidate name =
        Map.notMember name bound
          && not (isQualifiedName name)
          && Map.notMember name types
      introduced (Requirement (ClassConstraint arguments _)) = filter candidate (concatMap freeNames arguments)
      introduced (Parameter _) = []
      requiredNames = nub (concatMap introduced entries)
  traverse_ (\name -> when (Map.member name bound) (failure ("type parameter '" ++ name ++ "' is already declared by its owner"))) explicitNames
  let binding (Parameter (name, kind)) = [(name, if name `elem` requiredNames then KindUnknown else kind)]
      binding entry = [(name, KindUnknown) | name <- introduced entry]
  pure (unique (concatMap binding entries), [parameter | parameter@(name, _) <- explicit, name `elem` requiredNames])
  where
    unique = go []
    go _ [] = []
    go seen (parameter@(name, _) : rest)
      | name `elem` seen = go seen rest
      | otherwise = parameter : go (name : seen) rest

-- infer from requirements before inspecting overlapping annotations. a
-- compound requirement may leave part of a parameter's kind undetermined.
checkParameterKinds :: Scope -> [TypeParameter] -> Check ()
checkParameterKinds scope declarations = do
  pending <- traverse inspect declarations
  traverse_ (\(inferred, written) -> writtenKind written >>= unify inferred) pending
  where
    inspect (name, written) = do
      inferred <- resolve (scope Map.! name)
      when (determined inferred) (failure ("redundant type parameter '" ++ name ++ "'; byzen already determineþ its kind"))
      pure (inferred, written)
    determined Star = True
    determined (Arrow argument result) = determined argument && determined result
    determined (Meta _) = False

classArguments :: String -> Check [Kind]
classArguments name = do
  classes <- classVariables <$> get
  maybe (failure ("unknown required flock '" ++ name ++ "'")) (pure . map snd) (Map.lookup name classes)

checkConstraint :: Scope -> ClassConstraint -> Check ()
checkConstraint scope (ClassConstraint arguments name) = do
  expected <- classArguments name
  unless (length arguments == length expected) (failure ("constraint '" ++ name ++ "' hath the wrong number of arguments"))
  zipWithM_ (\argument kind -> void (checkType scope argument kind)) arguments expected

typeKind :: Scope -> TypeExpr -> Check Kind
typeKind scope = fmap snd . inferType scope

-- each type is checked and normalised in one traversal. quantified binders
-- keep their kinds for later type application and alpha-equivalence.
inferType :: Scope -> TypeExpr -> Check (TypeExpr, Kind)
inferType scope expression = case expression of
  TypeHole _ -> (expression,) <$> fresh
  TypeName name -> (expression,) <$> lookupKind name
  TypeApply name arguments -> do
    function <- lookupKind name
    arguments2 <- traverse (inferType scope) arguments
    kind <- foldM apply function (map snd arguments2)
    pure (TypeApply name (map fst arguments2), kind)
  TypeRecord fields -> (,Star) . TypeRecord <$> traverse (traverse (\field -> checkType scope field Star)) fields
  TypeArrow arguments effects result -> do
    arguments2 <- traverse (\argument -> checkType scope argument Star) arguments
    effects2 <- traverse (\effect -> checkType scope effect Star) effects
    result2 <- checkType scope result Star
    pure (TypeArrow arguments2 effects2 result2, Star)
  TypeForall declarations body -> do
    bound <- parameters declarations
    body2 <- checkType (Map.union (Map.fromList bound) scope) body Star
    declarations2 <- normaliseParameters bound
    pure (TypeForall declarations2 body2, Star)
  where
    lookupKind name = do
      types <- knownTypes <$> get
      maybe (failure ("undeclared type '" ++ name ++ "'; declare @" ++ name ++ " with its kind")) pure (Map.lookup name scope <|> Map.lookup name types)
    apply function input = do
      output <- fresh
      unify function (Arrow input output)
      pure output

checkType :: Scope -> TypeExpr -> Kind -> Check TypeExpr
checkType scope expression expected = do
  (normalised, kind) <- inferType scope expression
  unify expected kind
  pure normalised

normaliseParameters :: [(String, Kind)] -> Check [TypeParameter]
normaliseParameters = traverse (\(name, kind) -> (name,) <$> retained kind)
  where
    retained kind =
      resolve kind >>= \case
        Star -> pure KindType
        Arrow argument result -> KindArrow <$> retained argument <*> retained result
        Meta variable -> pure (KindVariable variable)

normaliseHeader :: Scope -> [TypeHeaderEntry] -> Check ([(String, Kind)], [ClassConstraint])
normaliseHeader bound entries = do
  (declarations, annotations) <- headerBindings bound entries
  variables <- parameters declarations
  let scope = Map.union (Map.fromList variables) bound
      constraints = headerRequirements entries
  traverse_ (checkConstraint scope) constraints
  checkParameterKinds scope annotations
  pure (variables, constraints)

normaliseAnnotation :: Scope -> TypeAnn -> Check TypeAnn
normaliseAnnotation bound (TypeAnn value entries) = do
  (variables, constraints) <- normaliseHeader bound entries
  let scope = Map.union (Map.fromList variables) bound
  value2 <- checkType scope value Star
  declarations <- normaliseParameters variables
  pure (TypeAnn (if null declarations then value2 else TypeForall declarations value2) (map Requirement constraints))

normaliseType :: Scope -> TypeExpr -> Check TypeExpr
normaliseType scope = fmap fst . inferType scope

normaliseExpr :: Scope -> Expr -> Check Expr
normaliseExpr scope = \case
  EAscribe body ty -> do
    EAscribe <$> normaliseExpr scope body <*> checkType scope ty Star
  other -> traverseExprChildren (normaliseDecl scope) (normaliseType scope) (normaliseExpr scope) other

normaliseDecl :: Scope -> Decl -> Check Decl
normaliseDecl bound = \case
  Export declaration -> Export <$> normaliseDecl bound declaration
  Let name annotation body -> do
    annotation2 <- traverse (normaliseAnnotation bound) annotation
    let declarations = case annotation2 of
          Just (TypeAnn (TypeForall variables _) _) -> variables
          _ -> []
    variables <- parameters declarations
    Let name annotation2 <$> normaliseExpr (Map.union (Map.fromList variables) bound) body
  ClassDecl _ name _ -> do
    classes <- normalisedClasses <$> get
    maybe (failure "internal missing checked class kinds") pure (Map.lookup name classes)
  InstanceDecl entries arguments name members -> do
    (variables, constraints) <- normaliseHeader bound entries
    expected <- classArguments name
    unless (length arguments == length expected) (failure ("bizen '" ++ name ++ "' hath the wrong number of type arguments"))
    let scope = Map.union (Map.fromList variables) bound
    arguments2 <- sequence (zipWith (checkType scope) arguments expected)
    members2 <- traverse (normaliseDecl scope) members
    declarations <- normaliseParameters variables
    pure (InstanceDecl (map Parameter declarations ++ map Requirement constraints) arguments2 name members2)
  TypeAlias declarations name target -> do
    scope <- declaredScope name declarations
    target2 <- checkType scope target Star
    declarations2 <- scopedParameters scope declarations
    pure (TypeAlias declarations2 name target2)
  DataDecl declarations name constructors -> do
    scope <- declaredScope name declarations
    declarations2 <- scopedParameters scope declarations
    DataDecl declarations2 name <$> traverse (constructor scope) constructors
  EffectDecl declarations name operations -> do
    scope <- declaredScope name declarations
    operations2 <- traverse (operation scope) operations
    declarations2 <- scopedParameters scope declarations
    pure (EffectDecl declarations2 name operations2)
  other -> pure other
  where
    declaredScope name declarations = do
      kind <- typeKind bound (TypeName name)
      arguments <- takeArguments (length declarations) kind
      pure (Map.union (Map.fromList (zip (parameterNames declarations) arguments)) bound)
    scopedParameters scope declarations = normaliseParameters [(name, scope Map.! name) | name <- parameterNames declarations]
    takeArguments 0 _ = pure []
    takeArguments count kind =
      resolve kind >>= \case
        Arrow input output -> (input :) <$> takeArguments (count - 1) output
        _ -> failure "internal missing declaration parameter kinds"
    constructor scope (Ctor name declarations fields result) = do
      variables <- parameters declarations
      let nested = Map.union (Map.fromList variables) scope
      fields2 <- traverse (\field -> checkType nested field Star) fields
      result2 <- traverse (\ty -> checkType nested ty Star) result
      declarations2 <- normaliseParameters variables
      pure (Ctor name declarations2 fields2 result2)
    operation scope (EffectOp name ty) = do
      -- operation headers may contain their own polymorphic parameters.
      annotation <- normaliseAnnotation scope (TypeAnn ty [])
      let TypeAnn ty2 _ = annotation
      pure (EffectOp name ty2)

normaliseMember :: Scope -> ClassMember -> Check ClassMember
normaliseMember scope = \case
  ClassSignature name annotation -> ClassSignature name <$> normaliseAnnotation scope annotation
  ClassLaw declarations arguments left right -> do
    variables <- parameters declarations
    let nested = Map.union (Map.fromList variables) scope
    arguments2 <- traverse (traverse (\ty -> checkType nested ty Star)) arguments
    left2 <- normaliseExpr nested left
    right2 <- normaliseExpr nested right
    declarations2 <- normaliseParameters variables
    pure (ClassLaw declarations2 arguments2 left2 right2)

checkKinds :: KindContext -> Program -> Either String (Program, KindContext)
checkKinds imported (Program declarations) = evalStateT check initialState
  where
    check = do
      loadContext imported
      traverse_ registerType declarations
      traverse_ registerClass declarations
      let headers = Map.fromList [(name, (entries, members)) | declaration <- declarations, ClassDecl entries name members <- [unexport declaration]]
      _ <- foldM (resolveClassHeader headers []) Set.empty (Map.keys headers)
      declarations2 <- traverse (normaliseDecl Map.empty) declarations
      state <- get
      types <- traverse finishedKind (knownTypes state)
      classes <- traverse (traverse (finishedKind . snd)) (classVariables state)
      declarations3 <- traverse finishDecl declarations2
      pure (Program declarations3, KindContext types classes)
    registerType (Export declaration) = registerType declaration
    registerType (TypeAlias parameters name _) = register name parameters
    registerType (DataDecl parameters name _) = register name parameters
    registerType (EffectDecl parameters name _) = register name parameters
    registerType _ = pure ()
    register name declarations = do
      variables <- parameters declarations
      let kind = foldr (Arrow . snd) Star variables
      modify' (\state -> state {knownTypes = Map.insert name kind (knownTypes state)})
    registerClass (Export declaration) = registerClass declaration
    registerClass (ClassDecl entries name _) = do
      (declarations, _) <- headerBindings Map.empty entries
      variables <- parameters declarations
      modify' (\state -> state {classVariables = Map.insert name variables (classVariables state)})
    registerClass _ = pure ()
    unexport (Export declaration) = unexport declaration
    unexport declaration = declaration
    -- resolve parent signatures first so redundancy checks see complete class
    -- kinds, even when the parent is written later in the file.
    resolveClassHeader headers visiting checked name
      | Set.member name checked || Map.notMember name headers = pure checked
      | name `elem` visiting = failure "cyclic flock requirements"
      | otherwise = do
          let (entries, members) = headers Map.! name
              constraints = headerRequirements entries
              parents = [parent | ClassConstraint _ parent <- constraints]
          checked2 <- foldM (resolveClassHeader headers (name : visiting)) checked parents
          variables <- (Map.! name) . classVariables <$> get
          let scope = Map.fromList variables
          traverse_ (checkConstraint scope) constraints
          (_, annotations) <- headerBindings Map.empty entries
          checkParameterKinds scope annotations
          members2 <- traverse (normaliseMember scope) members
          declarations2 <- normaliseParameters variables
          let checkedClass = ClassDecl (map Parameter declarations2 ++ map Requirement constraints) name members2
          modify' (\state -> state {normalisedClasses = Map.insert name checkedClass (normalisedClasses state)})
          pure (Set.insert name checked2)

-- unresolved kind variables must survive forward references. only after every
-- declaration is checked may unconstrained kinds default to *.
finishParameter :: TypeParameter -> Check TypeParameter
finishParameter (name, kind) = (name,) <$> (writtenKind kind >>= finishedKind)

finishEntry :: TypeHeaderEntry -> Check TypeHeaderEntry
finishEntry (Parameter parameter) = Parameter <$> finishParameter parameter
finishEntry (Requirement (ClassConstraint arguments name)) = Requirement . (`ClassConstraint` name) <$> traverse finishType arguments

finishType :: TypeExpr -> Check TypeExpr
finishType = \case
  TypeForall declarations body -> TypeForall <$> traverse finishParameter declarations <*> finishType body
  TypeApply name arguments -> TypeApply name <$> traverse finishType arguments
  TypeArrow arguments effects result -> TypeArrow <$> traverse finishType arguments <*> traverse finishType effects <*> finishType result
  TypeRecord fields -> TypeRecord <$> traverse (\(name, ty) -> (name,) <$> finishType ty) fields
  other -> pure other

finishAnnotation :: TypeAnn -> Check TypeAnn
finishAnnotation (TypeAnn ty entries) = TypeAnn <$> finishType ty <*> traverse finishEntry entries

finishDecl :: Decl -> Check Decl
finishDecl = \case
  Export declaration -> Export <$> finishDecl declaration
  Let name annotation body -> Let name <$> traverse finishAnnotation annotation <*> finishExpr body
  ClassDecl entries name members -> ClassDecl <$> traverse finishEntry entries <*> pure name <*> traverse member members
  InstanceDecl entries arguments name members -> InstanceDecl <$> traverse finishEntry entries <*> traverse finishType arguments <*> pure name <*> traverse finishDecl members
  TypeAlias declarations name body -> TypeAlias <$> traverse finishParameter declarations <*> pure name <*> finishType body
  DataDecl declarations name constructors -> DataDecl <$> traverse finishParameter declarations <*> pure name <*> traverse constructor constructors
  EffectDecl declarations name operations -> EffectDecl <$> traverse finishParameter declarations <*> pure name <*> traverse operation operations
  other -> pure other
  where
    constructor (Ctor name declarations fields result) = Ctor name <$> traverse finishParameter declarations <*> traverse finishType fields <*> traverse finishType result
    operation (EffectOp name ty) = EffectOp name <$> finishType ty
    member (ClassSignature name annotation) = ClassSignature name <$> finishAnnotation annotation
    member (ClassLaw declarations arguments left right) = ClassLaw <$> traverse finishParameter declarations <*> traverse (\(pattern, ty) -> (pattern,) <$> finishType ty) arguments <*> finishExpr left <*> finishExpr right

finishExpr :: Expr -> Check Expr
finishExpr = traverseExprChildren finishDecl finishType finishExpr
