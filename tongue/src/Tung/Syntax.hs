{- | shared surface and elaboration syntax. the parser emitteth only surface forms;
'Evidence', 'ElaboratedFill', 'EField', and 'EWithEvidence' are internal forms.
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
  Evidence (..),
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
import Data.Maybe (mapMaybe)
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
  | ShapeDecl [String] String [ShapeNeed] [ShapeMember]
  | FillDecl [TypeExpr] String [ShapeNeed] [Decl]
  | ElaboratedFill String [TypeExpr] String [ShapeNeed] [Decl]
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
shapeMemberNames = mapMaybe (fmap fst . shapeMemberSignature)

data TypeExpr
  = TypeName String
  | TypeApply String [TypeExpr]
  | TypeRecord [(String, TypeExpr)]
  | TypeArrow (NonEmpty TypeExpr) [TypeExpr] TypeExpr
  deriving (Eq, Show)

-- evidence holes exist only during inference and must be resolved before CoreProgram.
data Evidence
  = EvidenceHole Int
  | EvidenceLocal String
  | EvidenceFill String [Evidence] [Evidence]
  deriving (Eq, Show)

data Expr
  = ELocated SourceSpan Expr
  | EInteger Integer
  | EFloat Double
  | EUnicode Char
  | EText String
  | EForeign String
  | EVar String
  | EAscribe Expr TypeExpr
  | EApply Expr (NonEmpty Expr)
  | ERecord [(String, Expr)]
  | EField Expr String
  | EUpdate Expr [RecordUpdate]
  | ETry Expr (Maybe ReturnCase) [HandlerCase]
  | EMatch [Expr] [MatchCase]
  | EBlock [Decl] Expr
  | EWithEvidence Expr [Evidence]
  deriving (Eq, Show)

data HandlerCase = HandlerCase String [Pattern] Expr deriving (Eq, Show)

data ReturnCase = ReturnCase Pattern Expr deriving (Eq, Show)

data RecordUpdate = RecordSet String Expr | RecordRemove String deriving (Eq, Show)

data Pattern = PVar String | PInteger Integer | PCon String [Pattern] deriving (Eq, Show)

data MatchCase = MatchCase (NonEmpty Pattern) Expr deriving (Eq, Show)

matchCaseArity :: [MatchCase] -> Maybe Int
matchCaseArity (MatchCase ps _ : _) = Just (NE.length ps)
matchCaseArity [] = Nothing

matchRows :: [MatchCase] -> [[Pattern]]
matchRows = map (\(MatchCase ps _) -> NE.toList ps)
