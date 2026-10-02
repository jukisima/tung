-- | shared surface and elaboration syntax. the parser emitteþ only surface
-- forms; elaborated declarations and resolved expression/pattern constructors are
-- internal forms.
module Tung.Syntax
  ( Program (..),
    Decl (..),
    EffectOp (..),
    Ctor (..),
    ClassMember (..),
    classMemberSignature,
    classMemberNames,
    ClassConstraint (..),
    TypeAnn (..),
    TypeExpr (..),
    KindExpr (..),
    TypeParameter,
    TypeHeaderEntry (..),
    parameterNames,
    unknownParameters,
    headerParameters,
    headerRequirements,
    typeHeaderParts,
    Expr (..),
    traverseExprChildren,
    isAnonymousMatchExpr,
    HandlerTarget (..),
    HandlerCase (..),
    ReturnCase (..),
    RecordUpdate (..),
    Pattern (..),
    MatchCase (..),
    matchCaseArity,
    matchRows,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Tung.Identity (HandlerTarget (..), InstanceId, KindExpr (..), SymbolId, TermExport)
import Tung.Token (SourceSpan)

newtype Program = Program [Decl] deriving (Eq, Show)

data Decl
  = Import String (Maybe String)
  | Export Decl
  | ReExport String
  | ReExportType String
  | Let String (Maybe TypeAnn) Expr
  | TypeAlias [TypeParameter] String TypeExpr
  | DataDecl [TypeParameter] String [Ctor]
  | EffectDecl [TypeParameter] String [EffectOp]
  | ElaboratedEffect SymbolId [(TermExport, EffectOp)]
  | ClassDecl [TypeHeaderEntry] String [ClassMember]
  | ElaboratedClass SymbolId [SymbolId] [(TermExport, ClassMember)]
  | InstanceDecl [TypeHeaderEntry] [TypeExpr] String [Decl]
  | ElaboratedInstance InstanceId SymbolId [TypeExpr] String [ClassConstraint] [Decl]
  deriving (Eq, Show)

data EffectOp = EffectOp String TypeExpr deriving (Eq, Show)

data Ctor = Ctor String [TypeParameter] [TypeExpr] (Maybe TypeExpr) deriving (Eq, Show)

data ClassConstraint = ClassConstraint [TypeExpr] String deriving (Eq, Show)

type TypeParameter = (String, KindExpr)

-- header order determineþ the order of explicit type inputs. requirements
-- retain their position until kind checking introduceþ their parameters.
data TypeHeaderEntry = Parameter TypeParameter | Requirement ClassConstraint deriving (Eq, Show)

parameterNames :: [TypeParameter] -> [String]
parameterNames = map fst

unknownParameters :: [String] -> [TypeParameter]
unknownParameters = map (,KindUnknown)

headerParameters :: [TypeHeaderEntry] -> [TypeParameter]
headerParameters entries = [parameter | Parameter parameter <- entries]

headerRequirements :: [TypeHeaderEntry] -> [ClassConstraint]
headerRequirements entries = [constraint | Requirement constraint <- entries]

typeHeaderParts :: [TypeHeaderEntry] -> ([String], [ClassConstraint])
typeHeaderParts entries = (parameterNames (headerParameters entries), headerRequirements entries)

data TypeAnn = TypeAnn TypeExpr [TypeHeaderEntry] deriving (Eq, Show)

data ClassMember
  = ClassSignature String TypeAnn
  | ClassLaw [TypeParameter] [(Pattern, TypeExpr)] Expr Expr
  deriving (Eq, Show)

classMemberSignature :: ClassMember -> Maybe (String, TypeAnn)
classMemberSignature = \case
  ClassSignature name annotation -> Just (name, annotation)
  ClassLaw {} -> Nothing

classMemberNames :: [ClassMember] -> [String]
classMemberNames = mapMaybe \case
  ClassSignature name _ -> Just name
  ClassLaw {} -> Nothing

data TypeExpr
  = TypeName String
  | TypeHole Int
  | TypeApply String [TypeExpr]
  | TypeRecord [(String, TypeExpr)]
  | TypeArrow (NonEmpty TypeExpr) [TypeExpr] TypeExpr
  | TypeForall [TypeParameter] TypeExpr
  deriving (Eq, Show)

data Expr
  = ELocated SourceSpan Expr
  | EInteger Integer
  | EFloat Double
  | EUnicode Char
  | EText String
  | EForeign String
  | EVar String
  | EGlobal TermExport
  | EEvidence Int
  | EEvidenceLambda (NonEmpty Int) Expr
  | EAscribe Expr TypeExpr
  | EApply Expr (NonEmpty Expr)
  | ETypeApply Expr (NonEmpty TypeExpr)
  | ERecord [(String, Expr)]
  | EField Expr String
  | EUpdate Expr [RecordUpdate]
  | ETry Expr (Maybe ReturnCase) [HandlerCase]
  | EMatch [Expr] [MatchCase]
  | EBlock [Decl] Expr
  | EWithEvidence Expr [Int]
  | EDictionary InstanceId SymbolId [Expr] [Expr]
  deriving (Eq, Show)

-- visit immediate children only. callers own binder scopes and the meaning
-- of internal forms; this traversal never interpreteþ names or annotations.
traverseExprChildren :: (Applicative f) => (Decl -> f Decl) -> (TypeExpr -> f TypeExpr) -> (Expr -> f Expr) -> Expr -> f Expr
traverseExprChildren declaration ty expression = \case
  ELocated span body -> ELocated span <$> expression body
  EEvidenceLambda identifiers body -> EEvidenceLambda identifiers <$> expression body
  EAscribe body annotation -> EAscribe <$> expression body <*> ty annotation
  EApply function arguments -> EApply <$> expression function <*> traverse expression arguments
  ETypeApply function arguments -> ETypeApply <$> expression function <*> traverse ty arguments
  ERecord fields -> ERecord <$> traverse (traverse expression) fields
  EField body name -> (`EField` name) <$> expression body
  EUpdate body updates -> EUpdate <$> expression body <*> traverse update updates
  ETry body returned cases -> ETry <$> expression body <*> traverse returnedCase returned <*> traverse handler cases
  EMatch arguments cases -> EMatch <$> traverse expression arguments <*> traverse matchCase cases
  EBlock declarations body -> EBlock <$> traverse declaration declarations <*> expression body
  EWithEvidence body identifiers -> (`EWithEvidence` identifiers) <$> expression body
  EDictionary key class_ required parents -> EDictionary key class_ <$> traverse expression required <*> traverse expression parents
  other -> pure other
  where
    update (RecordSet name body) = RecordSet name <$> expression body
    update other = pure other
    returnedCase (ReturnCase pattern body) = ReturnCase pattern <$> expression body
    handler (HandlerCase name patterns body) = HandlerCase name patterns <$> expression body
    handler (ResolvedHandlerCase target patterns body) = ResolvedHandlerCase target patterns <$> expression body
    matchCase (MatchCase patterns body) = MatchCase patterns <$> expression body

-- transparent wrappers do not change whether a let body is a recursive
-- anonymous match.
isAnonymousMatchExpr :: Expr -> Bool
isAnonymousMatchExpr = \case
  ELocated _ expression -> isAnonymousMatchExpr expression
  EAscribe expression _ -> isAnonymousMatchExpr expression
  EEvidenceLambda _ expression -> isAnonymousMatchExpr expression
  EMatch [] _ -> True
  _ -> False

data HandlerCase
  = HandlerCase String [Pattern] Expr
  | ResolvedHandlerCase HandlerTarget [Pattern] Expr
  deriving (Eq, Show)

data ReturnCase = ReturnCase Pattern Expr deriving (Eq, Show)

data RecordUpdate = RecordSet String Expr | RecordRemove String deriving (Eq, Show)

data Pattern
  = PVar String
  | PInteger Integer
  | PText String
  | PCon String [Pattern]
  | PConstructor SymbolId [Pattern]
  deriving (Eq, Show)

data MatchCase = MatchCase (NonEmpty Pattern) Expr deriving (Eq, Show)

matchCaseArity :: [MatchCase] -> Maybe Int
matchCaseArity (MatchCase ps _ : _) = Just (NE.length ps)
matchCaseArity [] = Nothing

matchRows :: [MatchCase] -> [[Pattern]]
matchRows = map (\(MatchCase ps _) -> NE.toList ps)
