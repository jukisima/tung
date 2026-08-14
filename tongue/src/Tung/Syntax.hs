module Tung.Syntax (
  Program (..),
  Decl (..),
  EffectOp (..),
  ForeignMember (..),
  Ctor (..),
  ShapeMember (..),
  ShapeNeed (..),
  TypeAnn (..),
  TypeExpr (..),
  Expr (..),
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

newtype Program = Program [Decl] deriving (Eq, Show)

data Decl
  = Import String
  | Export Decl
  | ReExport String
  | ReExportType String
  | Let String (Maybe TypeAnn) Expr
  | TypeAlias String TypeExpr
  | DataDecl [String] String [Ctor]
  | EffectDecl [String] String [EffectOp]
  | ForeignDecl [ForeignMember]
  | ShapeDecl [String] String [ShapeNeed] [ShapeMember]
  | FillDecl [TypeExpr] String [ShapeNeed] [Decl]
  deriving (Eq, Show)

data EffectOp = EffectOp String TypeExpr deriving (Eq, Show)

data ForeignMember = ForeignMember String TypeAnn deriving (Eq, Show)

data Ctor = Ctor String [TypeExpr] deriving (Eq, Show)

data ShapeNeed = ShapeNeed [TypeExpr] String deriving (Eq, Show)

data TypeAnn = TypeAnn TypeExpr [ShapeNeed] deriving (Eq, Show)

data ShapeMember = ShapeSpec String TypeAnn | ShapeDefault String (Maybe TypeAnn) Expr deriving (Eq, Show)

data TypeExpr
  = TypeName String
  | TypeApply String [TypeExpr]
  | TypeRecord [(String, TypeExpr)]
  | TypeArrow (NonEmpty TypeExpr) [TypeExpr] TypeExpr
  deriving (Eq, Show)

data Expr
  = EInteger Int
  | EFloat String
  | EChar String
  | EString String
  | EVar String
  | EApply Expr (NonEmpty Expr)
  | ERecord [(String, Expr)]
  | EUpdate Expr [RecordUpdate]
  | ETry Expr (Maybe ReturnCase) [HandlerCase]
  | EMatch [Expr] [MatchCase]
  | EBlock [Decl] Expr
  deriving (Eq, Show)

data HandlerCase = HandlerCase String [Pattern] Expr deriving (Eq, Show)

data ReturnCase = ReturnCase Pattern Expr deriving (Eq, Show)

data RecordUpdate = RecordSet String Expr | RecordRemove String deriving (Eq, Show)

data Pattern = PVar String | PCon String [Pattern] deriving (Eq, Show)

data MatchCase = MatchCase (NonEmpty Pattern) Expr deriving (Eq, Show)

matchCaseArity :: [MatchCase] -> Maybe Int
matchCaseArity (MatchCase ps _ : _) = Just (NE.length ps)
matchCaseArity [] = Nothing

matchRows :: [MatchCase] -> [[Pattern]]
matchRows = map (\(MatchCase ps _) -> NE.toList ps)
