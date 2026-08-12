module Jbini.Syntax (
  Program (..),
  Decl (..),
  EffectOp (..),
  ForeignMember (..),
  Ctor (..),
  BoneMember (..),
  BoneNeed (..),
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
import qualified Data.List.NonEmpty as NE

data Program = Program [Decl] deriving (Eq, Show)

data Decl
  = Import String
  | ShowDecl String
  | ShowTypeDecl String
  | Let String (Maybe TypeAnn) Expr
  | TypeAlias String TypeExpr
  | DataDecl [String] String [Ctor]
  | EffectDecl [String] String [EffectOp]
  | ForeignDecl [ForeignMember]
  | BoneDecl [String] String [BoneNeed] [BoneMember]
  | FleshDecl [TypeExpr] String [BoneNeed] [Decl]
  deriving (Eq, Show)

data EffectOp = EffectOp String TypeExpr deriving (Eq, Show)

data ForeignMember = ForeignMember String TypeAnn deriving (Eq, Show)

data Ctor = Ctor String [TypeExpr] deriving (Eq, Show)

data BoneNeed = BoneNeed [TypeExpr] String deriving (Eq, Show)

data TypeAnn = TypeAnn TypeExpr [BoneNeed] deriving (Eq, Show)

data BoneMember = BoneSpec String TypeAnn | BoneDefault String (Maybe TypeAnn) Expr deriving (Eq, Show)

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
