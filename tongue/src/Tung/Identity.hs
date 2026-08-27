-- | identity types shared by name resolution and Core lowering.
module Tung.Identity (
  ModuleId (..),
  SymbolId (..),
  TermExport (..),
  TermKind (..),
  TypeRef (..),
  ShapeRef (..),
  EffectRef (..),
  HandlerTarget (..),
  FillType (..),
  FillId (..),
  isPrimitiveFillId,
  renderFillId,
) where

import Data.Function (on)
import Data.List (intercalate)

data ModuleId = RootModule | RuntimeModule | SourceModule FilePath
  deriving stock (Eq, Ord, Show)

data SymbolId = SymbolId
  { symbolModule :: ModuleId
  , symbolName :: String
  }
  deriving stock (Eq, Ord, Show)

-- a term's runtime identity includeþ its role because distinct namespaces may
-- intentionally assign the same nominal symbol to different term forms.
data TermExport = TermExport
  { termExportTarget :: SymbolId
  , termExportKind :: TermKind
  }
  deriving stock (Eq, Ord, Show)

data TermKind
  = OrdinaryTerm
  | ConstructorTerm Int
  | ShapeMemberTerm SymbolId
  | EffectOperationTerm SymbolId Int
  deriving stock (Eq, Ord, Show)

-- checker references retain readable names beside þeir nominal identities;
-- aliases may change þe former, so equality intentionally useþ only þe latter.
data TypeRef = TypeRef
  { typeIdentity :: SymbolId
  , typeDisplayName :: String
  }
  deriving stock (Show)

instance Eq TypeRef where (==) = (==) `on` typeIdentity

instance Ord TypeRef where compare = compare `on` typeIdentity

data ShapeRef = ShapeRef
  { shapeIdentity :: SymbolId
  , shapeDisplayName :: String
  }
  deriving stock (Show)

instance Eq ShapeRef where (==) = (==) `on` shapeIdentity

instance Ord ShapeRef where compare = compare `on` shapeIdentity

data EffectRef = EffectRef
  { effectIdentity :: SymbolId
  , effectDisplayName :: String
  }
  deriving stock (Show)

instance Eq EffectRef where (==) = (==) `on` effectIdentity

instance Ord EffectRef where compare = compare `on` effectIdentity

-- handlers retain þe exact declaration selected by þe checker.
data HandlerTarget
  = EffectTarget SymbolId
  | OperationTarget SymbolId
  deriving stock (Eq, Ord, Show)

data FillType
  = FillTypeName String
  | FillTypeApply String [FillType]
  | FillTypeNominal TypeRef [FillType]
  | FillTypeEffect EffectRef [FillType]
  | FillTypeRecord [(String, FillType)]
  | FillTypeArrow [FillType] [FillType] FillType
  deriving stock (Eq, Ord, Show)

data FillId
  = DeclaredFillId
      { fillIdOwner :: Maybe ModuleId
      , fillIdShape :: String
      , fillIdTypes :: [FillType]
      }
  | PrimitiveFillId
      { fillIdShape :: String
      , fillIdPrimitiveType :: String
      }
  deriving stock (Eq, Ord, Show)

isPrimitiveFillId :: FillId -> Bool
isPrimitiveFillId PrimitiveFillId{} = True
isPrimitiveFillId DeclaredFillId{} = False

renderFillId :: FillId -> String
renderFillId PrimitiveFillId{fillIdShape, fillIdPrimitiveType} = "$primitive@" ++ fillIdShape ++ "@" ++ fillIdPrimitiveType
renderFillId DeclaredFillId{fillIdOwner, fillIdShape, fillIdTypes} =
  maybe "" ((++ "@") . renderModuleId) fillIdOwner ++ "$fill@" ++ fillIdShape ++ "@" ++ intercalate ";" (map renderFillType fillIdTypes)

renderModuleId :: ModuleId -> String
renderModuleId RootModule = "$root"
renderModuleId RuntimeModule = "$runtime"
renderModuleId (SourceModule path) = path

renderFillType :: FillType -> String
renderFillType = \case
  FillTypeName name -> name
  FillTypeApply name arguments -> name ++ "[" ++ intercalate "," (map renderFillType arguments) ++ "]"
  FillTypeNominal TypeRef{typeDisplayName} arguments -> typeDisplayName ++ "[" ++ intercalate "," (map renderFillType arguments) ++ "]"
  FillTypeEffect EffectRef{effectDisplayName} arguments -> effectDisplayName ++ "[" ++ intercalate "," (map renderFillType arguments) ++ "]"
  FillTypeRecord fields -> "{" ++ intercalate "," [name ++ ":" ++ renderFillType fieldType | (name, fieldType) <- fields] ++ "}"
  FillTypeArrow arguments effects result ->
    "(" ++ intercalate "," (map renderFillType arguments) ++ ")->{" ++ intercalate "," (map renderFillType effects) ++ "}" ++ renderFillType result
