-- | phase-separated, validated boundary between elaboration and evaluation.
module Tung.Core (
  CoreProgram,
  CoreDecl (..),
  CoreLocalDecl (..),
  CoreName (..),
  LocalId (..),
  localIdText,
  CoreExpr (..),
  CorePattern (..),
  CoreShapeMember (..),
  CoreReturnCase (..),
  CoreHandlerCase (..),
  CoreRecordUpdate (..),
  CoreMatchCase (..),
  ModuleInterface (..),
  makeCoreProgramWithImports,
  coreDeclarations,
  coreImportDeclarations,
  coreInterface,
  coreImportInterface,
)
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Tung.Identity (FillId, ModuleId (..), SymbolId (..), TermExport (..), TermKind (..))
import Tung.Name (lastQualifiedSegment)
import Tung.Primitive (findForeignBinding, hostArity)
import Tung.Syntax

data ModuleInterface = ModuleInterface
  { interfaceModule :: ModuleId
  , interfaceTerms :: Map.Map SymbolId TermExport
  }
  deriving stock (Eq, Show)

data CoreDecl
  = CoreImport FilePath
  | CoreLet TermExport CoreExpr
  | CoreForeignLet TermExport String Int
  | CoreData [TermExport]
  | CoreEffect [TermExport]
  | CoreShape SymbolId [SymbolId] [CoreShapeMember]
  | CoreFill FillId SymbolId [(String, CoreExpr)]
  deriving stock (Eq, Show)

data CoreShapeMember = CoreShapeMember TermExport (Maybe CoreExpr)
  deriving stock (Eq, Show)

data CoreLocalDecl = CoreLocalLet LocalId CoreExpr
  deriving stock (Eq, Show)

data LocalId = BoundLocal Int String | EvidenceLocal Int
  deriving stock (Eq, Ord, Show)

localIdText :: LocalId -> String
localIdText (BoundLocal _ name) = name
localIdText (EvidenceLocal index) = "$evidence" ++ show index

data CoreName
  = CoreLocalName LocalId
  | CoreGlobalName TermExport
  deriving stock (Eq, Show)

data CorePattern
  = CoreWildcard
  | CoreBind LocalId
  | CoreIntegerPattern Integer
  | CoreConstructorPattern SymbolId [CorePattern]
  deriving stock (Eq, Show)

data CoreExpr
  = CoreInteger Integer
  | CoreFloat Double
  | CoreUnicode Char
  | CoreText String
  | CoreVar CoreName
  | CoreApply CoreExpr (NonEmpty CoreExpr)
  | CoreRecord [(String, CoreExpr)]
  | CoreField CoreExpr String
  | CoreUpdate CoreExpr [CoreRecordUpdate]
  | CoreTry CoreExpr (Maybe CoreReturnCase) [CoreHandlerCase]
  | CoreMatch [CoreExpr] [CoreMatchCase]
  | CoreBlock [CoreLocalDecl] CoreExpr
  | CoreDictionary FillId SymbolId [CoreExpr] [CoreExpr]
  deriving stock (Eq, Show)

data CoreReturnCase = CoreReturnCase CorePattern CoreExpr
  deriving stock (Eq, Show)

data CoreHandlerCase = CoreHandlerCase HandlerTarget LocalId [CorePattern] CoreExpr
  deriving stock (Eq, Show)

data CoreRecordUpdate = CoreRecordSet String CoreExpr | CoreRecordRemove String
  deriving stock (Eq, Show)

data CoreMatchCase = CoreMatchCase (NonEmpty CorePattern) CoreExpr
  deriving stock (Eq, Show)

data CoreModule = CoreModule
  { coreModuleDeclarations :: [CoreDecl]
  , coreModuleInterface :: ModuleInterface
  }
  deriving stock (Eq, Show)

-- a CoreProgram hath passed type checking and containeþ only executable forms.
-- keeping its constructor private preventeþ evaluation of surface syntax.
data CoreProgram = CoreProgram CoreModule (Map.Map FilePath CoreModule) deriving stock (Eq, Show)

makeCoreProgramWithImports :: ModuleInterface -> Map.Map String [TermExport] -> Program -> Map.Map FilePath CoreProgram -> Either String CoreProgram
makeCoreProgramWithImports interface terms program imports = do
  let importedModules = rootModule <$> imports
  declarations <- lowerCoreProgram terms program
  let root = CoreModule declarations interface
  pure (CoreProgram root importedModules)
 where
  rootModule (CoreProgram imported _) = imported

coreDeclarations :: CoreProgram -> [CoreDecl]
coreDeclarations (CoreProgram CoreModule{coreModuleDeclarations} _) = coreModuleDeclarations

coreImportDeclarations :: FilePath -> CoreProgram -> Maybe [CoreDecl]
coreImportDeclarations path (CoreProgram _ imports) = coreModuleDeclarations <$> Map.lookup path imports

coreInterface :: CoreProgram -> ModuleInterface
coreInterface (CoreProgram CoreModule{coreModuleInterface} _) = coreModuleInterface

coreImportInterface :: FilePath -> CoreProgram -> Maybe ModuleInterface
coreImportInterface path (CoreProgram _ imports) = coreModuleInterface <$> Map.lookup path imports

type Lower = StateT Int (Either String)

lowerFailure :: String -> Lower a
lowerFailure = lift . Left

freshLocal :: String -> Lower LocalId
freshLocal name = do
  index <- get
  put (index + 1)
  pure (BoundLocal index name)

-- lowering allocateþ unique locals and eraseþ source-only syntax. þe checker
-- hath already disambiguated imports, overloads, constructors, and shapes;
-- core mappeþ declarations, expressions, patterns, and locals to þeir checked
-- identities.
lowerCoreProgram :: Map.Map String [TermExport] -> Program -> Either String [CoreDecl]
lowerCoreProgram terms (Program declarations) = evalStateT (concat <$> traverse lowerDecl declarations) 0
 where
  resolveOwn name matches =
    maybe
      (lowerFailure ("internal missing checked term '" ++ name ++ "'"))
      pure
      (Map.lookup name terms >>= find (matches . termExportKind))

  lowerDecl = \case
    Import path _ -> pure [CoreImport path]
    Export declaration -> lowerDecl declaration
    ReExport{} -> pure []
    ReExportType{} -> pure []
    Let name (Just _) (EForeign hostKey) -> do
      target <- resolveOwn name (== OrdinaryTerm)
      arity <- maybe (lowerFailure ("internal unknown fremmed binding '" ++ hostKey ++ "'")) (pure . hostArity) (findForeignBinding hostKey)
      pure [CoreForeignLet target hostKey arity]
    Let _ _ EForeign{} -> lowerFailure "internal misplaced fremmed marker"
    Let name _ body -> do
      target <- resolveOwn name (== OrdinaryTerm)
      (: []) . CoreLet target <$> lowerExpr [] body
    TypeAlias{} -> pure []
    DataDecl _ name constructors -> do
      constructors2 <- traverse (lowerConstructor name) constructors
      pure [CoreData constructors2]
    EffectDecl{} -> lowerFailure "internal unelaborated effect"
    ElaboratedEffect target operations -> do
      operations2 <- traverse (lowerResolvedOperation target) operations
      pure [CoreEffect operations2]
    ShapeDecl{} -> lowerFailure "internal unelaborated shape"
    ElaboratedShape target needs members -> do
      members2 <- traverse (lowerShapeMember target) members
      pure [CoreShape target needs members2]
    FillDecl{} -> lowerFailure "internal unelaborated fill"
    ElaboratedFill key shape _ _ _ members -> do
      members2 <- traverse lowerFillMember members
      pure [CoreFill key shape members2]

  lowerConstructor owner (Ctor name fields) =
    resolveOwn (owner ++ "@" ++ name) (== ConstructorTerm (length fields))

  lowerResolvedOperation owner (target, EffectOp name _)
    | EffectOperationTerm operationOwner _ <- termExportKind target
    , operationOwner == owner =
        pure target
    | otherwise = lowerFailure ("internal effect operation identity mismatch for '" ++ name ++ "'")

  lowerFillMember = \case
    Let name _ body -> (name,) <$> lowerExpr [] body
    _ -> lowerFailure "internal non-let fill member"

  lowerShapeMember owner (target, member) = case member of
    ShapeSpec name _ -> lowerMember owner target name Nothing
    ShapeDefault name _ body -> lowerMember owner target name (Just body)
    ShapeLaw{} -> lowerFailure "internal elaborated shape law"

  lowerMember owner target name body
    | ShapeMemberTerm memberOwner <- termExportKind target
    , memberOwner == owner
    , lastQualifiedSegment (symbolName (termExportTarget target)) == name =
        CoreShapeMember target <$> traverse (lowerExpr []) body
    | otherwise = lowerFailure ("internal shape member identity mismatch for '" ++ name ++ "'")

  lowerExpr locals = \case
    ELocated _ expression -> lowerExpr locals expression
    EInteger value -> pure (CoreInteger value)
    EFloat value -> pure (CoreFloat value)
    EUnicode value -> pure (CoreUnicode value)
    EText value -> pure (CoreText value)
    EForeign{} -> lowerFailure "internal misplaced fremmed marker"
    EGlobal target -> pure (CoreVar (CoreGlobalName target))
    EVar name -> case lookup name locals of
      Just local -> pure (CoreVar (CoreLocalName local))
      Nothing -> lowerFailure ("internal unresolved local '" ++ name ++ "'")
    EEvidence index -> pure (CoreVar (CoreLocalName (EvidenceLocal index)))
    EEvidenceLambda identifiers body -> do
      body2 <- lowerExpr locals body
      pure (CoreMatch [] [CoreMatchCase (fmap (CoreBind . EvidenceLocal) identifiers) body2])
    EAscribe expression _ -> lowerExpr locals expression
    EApply function arguments -> CoreApply <$> lowerExpr locals function <*> traverse (lowerExpr locals) arguments
    ERecord fields -> CoreRecord <$> traverse (traverse (lowerExpr locals)) fields
    EField base field -> CoreField <$> lowerExpr locals base <*> pure field
    EUpdate base updates -> CoreUpdate <$> lowerExpr locals base <*> traverse (lowerUpdate locals) updates
    ETry body returned cases -> CoreTry <$> lowerExpr locals body <*> traverse (lowerReturn locals) returned <*> traverse (lowerHandler locals) cases
    EMatch scrutinees cases -> CoreMatch <$> traverse (lowerExpr locals) scrutinees <*> traverse (lowerMatch locals) cases
    EBlock nested body -> do
      (locals2, nested2) <- lowerLocalDecls locals nested
      CoreBlock nested2 <$> lowerExpr locals2 body
    EWithEvidence{} -> lowerFailure "internal unresolved type-class evidence application"
    EDictionary key shape required parents -> do
      required2 <- traverse (lowerExpr locals) required
      parents2 <- traverse (lowerExpr locals) parents
      pure (CoreDictionary key shape required2 parents2)

  lowerUpdate locals = \case
    RecordSet name value -> CoreRecordSet name <$> lowerExpr locals value
    RecordRemove name -> pure (CoreRecordRemove name)

  lowerLocalDecls locals [] = pure (locals, [])
  lowerLocalDecls locals (declaration : rest) = case declaration of
    Let name _ EForeign{} -> lowerFailure ("internal fremmed local let '" ++ name ++ "'")
    Let name _ body -> do
      local <- freshLocal name
      let locals2 = (name, local) : locals
          recursiveLocals = if isAnonymousMatchExpr body then (name, local) : locals else locals
      declaration2 <- CoreLocalLet local <$> lowerExpr recursiveLocals body
      (locals3, rest2) <- lowerLocalDecls locals2 rest
      pure (locals3, declaration2 : rest2)
    _ -> lowerFailure "internal non-let local declaration"

  lowerReturn locals (ReturnCase pattern body) = do
    (pattern2, bindings) <- lowerPattern pattern
    CoreReturnCase pattern2 <$> lowerExpr (bindings ++ locals) body
  lowerHandler _ HandlerCase{} = lowerFailure "internal unresolved handler target"
  lowerHandler locals (ResolvedHandlerCase target patterns body) = do
    (patterns2, bindings) <- lowerPatterns patterns
    continuation <- freshLocal "eftgin"
    CoreHandlerCase target continuation patterns2 <$> lowerExpr (("eftgin", continuation) : bindings ++ locals) body
  lowerMatch locals (MatchCase patterns body) = do
    (patterns2, bindings) <- lowerPatterns patterns
    CoreMatchCase patterns2 <$> lowerExpr (bindings ++ locals) body

  lowerPatterns :: (Traversable patterns) => patterns Pattern -> Lower (patterns CorePattern, [(String, LocalId)])
  lowerPatterns patterns = do
    lowered <- traverse lowerPattern patterns
    pure (fmap fst lowered, foldMap snd lowered)

  lowerPattern = \case
    PVar "_" -> pure (CoreWildcard, [])
    PVar name -> do
      local <- freshLocal name
      pure (CoreBind local, [(name, local)])
    PInteger value -> pure (CoreIntegerPattern value, [])
    PCon name _ -> lowerFailure ("internal unresolved constructor pattern '" ++ name ++ "'")
    PConstructor target arguments -> do
      (arguments2, bindings) <- lowerPatterns arguments
      pure (CoreConstructorPattern target arguments2, bindings)
