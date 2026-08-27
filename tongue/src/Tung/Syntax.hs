{- | shared surface and elaboration syntax. the parser emitteþ only surface
forms; elaborated declarations and resolved expression/pattern constructors are
internal forms.
-}
module Tung.Syntax (
  Program (..),
  Decl (..),
  EffectOp (..),
  Ctor (..),
  ShapeMember (..),
  shapeMemberSignature,
  shapeMemberNames,
  ShapeNeed (..),
  TypeAnn (..),
  TypeExpr (..),
  Expr (..),
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
import Tung.Identity (FillId, HandlerTarget (..), SymbolId, TermExport)
import Tung.Token (SourceSpan)

newtype Program = Program [Decl] deriving (Eq, Show)

data Decl
  = Import String (Maybe String)
  | Export Decl
  | ReExport String
  | ReExportType String
  | Let String (Maybe TypeAnn) Expr
  | TypeAlias [String] String TypeExpr
  | DataDecl [String] String [Ctor]
  | EffectDecl [String] String [EffectOp]
  | ElaboratedEffect SymbolId [(TermExport, EffectOp)]
  | ShapeDecl [String] String [ShapeNeed] [ShapeMember]
  | ElaboratedShape SymbolId [SymbolId] [(TermExport, ShapeMember)]
  | FillDecl [TypeExpr] String [ShapeNeed] [Decl]
  | ElaboratedFill FillId SymbolId [TypeExpr] String [ShapeNeed] [Decl]
  deriving (Eq, Show)

data EffectOp = EffectOp String TypeExpr deriving (Eq, Show)

data Ctor = Ctor String [TypeExpr] deriving (Eq, Show)

data ShapeNeed = ShapeNeed [TypeExpr] String deriving (Eq, Show)

data TypeAnn = TypeAnn TypeExpr [ShapeNeed] deriving (Eq, Show)

data ShapeMember
  = ShapeSpec String TypeAnn
  | ShapeDefault String (Maybe TypeAnn) Expr
  | ShapeLaw [(String, TypeExpr)] Expr Expr
  deriving (Eq, Show)

shapeMemberSignature :: ShapeMember -> Maybe (String, TypeAnn)
shapeMemberSignature = \case
  ShapeSpec name annotation -> Just (name, annotation)
  ShapeDefault name annotation _ -> (name,) <$> annotation
  ShapeLaw{} -> Nothing

shapeMemberNames :: [ShapeMember] -> [String]
shapeMemberNames = mapMaybe \case
  ShapeSpec name _ -> Just name
  ShapeDefault name _ _ -> Just name
  ShapeLaw{} -> Nothing

data TypeExpr
  = TypeName String
  | TypeApply String [TypeExpr]
  | TypeRecord [(String, TypeExpr)]
  | TypeArrow (NonEmpty TypeExpr) [TypeExpr] TypeExpr
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
  | ERecord [(String, Expr)]
  | EField Expr String
  | EUpdate Expr [RecordUpdate]
  | ETry Expr (Maybe ReturnCase) [HandlerCase]
  | EMatch [Expr] [MatchCase]
  | EBlock [Decl] Expr
  | EWithEvidence Expr [Int]
  | EDictionary FillId SymbolId [Expr] [Expr]
  deriving (Eq, Show)

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
  | PCon String [Pattern]
  | PConstructor SymbolId [Pattern]
  deriving (Eq, Show)

data MatchCase = MatchCase (NonEmpty Pattern) Expr deriving (Eq, Show)

matchCaseArity :: [MatchCase] -> Maybe Int
matchCaseArity (MatchCase ps _ : _) = Just (NE.length ps)
matchCaseArity [] = Nothing

matchRows :: [MatchCase] -> [[Pattern]]
matchRows = map (\(MatchCase ps _) -> NE.toList ps)
