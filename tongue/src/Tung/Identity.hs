-- | identity types shared by name resolution and Core lowering.
module Tung.Identity
  ( ModuleId (..),
    SymbolId (..),
    TermExport (..),
    TermKind (..),
    TypeRef (..),
    ClassRef (..),
    EffectRef (..),
    HandlerTarget (..),
    InstanceType (..),
    InstanceId (..),
    isPrimitiveInstanceId,
    renderInstanceId,
  )
where

import Data.Function (on)
import Data.List (intercalate)

data ModuleId = RootModule | RuntimeModule | SourceModule FilePath
  deriving stock (Eq, Ord, Show)

data SymbolId = SymbolId
  { symbolModule :: ModuleId,
    symbolName :: String
  }
  deriving stock (Eq, Ord, Show)

-- a term's runtime identity includeþ its role because distinct namespaces may
-- intentionally assign the same nominal symbol to different term forms.
data TermExport = TermExport
  { termExportTarget :: SymbolId,
    termExportKind :: TermKind
  }
  deriving stock (Eq, Ord, Show)

data TermKind
  = OrdinaryTerm
  | ConstructorTerm Int
  | ClassMemberTerm SymbolId
  | EffectOperationTerm SymbolId Int
  deriving stock (Eq, Ord, Show)

-- checker references retain readable names beside þeir nominal identities;
-- aliases may change þe former, so equality intentionally useþ only þe latter.
data TypeRef = TypeRef
  { typeIdentity :: SymbolId,
    typeDisplayName :: String
  }
  deriving stock (Show)

instance Eq TypeRef where (==) = (==) `on` typeIdentity

instance Ord TypeRef where compare = compare `on` typeIdentity

data ClassRef = ClassRef
  { classIdentity :: SymbolId,
    classDisplayName :: String
  }
  deriving stock (Show)

instance Eq ClassRef where (==) = (==) `on` classIdentity

instance Ord ClassRef where compare = compare `on` classIdentity

data EffectRef = EffectRef
  { effectIdentity :: SymbolId,
    effectDisplayName :: String
  }
  deriving stock (Show)

instance Eq EffectRef where (==) = (==) `on` effectIdentity

instance Ord EffectRef where compare = compare `on` effectIdentity

-- handlers retain þe exact declaration selected by þe checker.
data HandlerTarget
  = EffectTarget SymbolId
  | OperationTarget SymbolId
  deriving stock (Eq, Ord, Show)

data InstanceType
  = InstanceTypeName String
  | InstanceTypeApply String [InstanceType]
  | InstanceTypeNominal TypeRef [InstanceType]
  | InstanceTypeEffect EffectRef [InstanceType]
  | InstanceTypeRecord [(String, InstanceType)]
  | InstanceTypeArrow [InstanceType] [InstanceType] InstanceType
  deriving stock (Eq, Ord, Show)

data InstanceId
  = DeclaredInstanceId
      { instanceIdOwner :: Maybe ModuleId,
        instanceIdClass :: String,
        instanceIdTypes :: [InstanceType]
      }
  | PrimitiveInstanceId
      { instanceIdClass :: String,
        instanceIdPrimitiveType :: String
      }
  deriving stock (Eq, Ord, Show)

isPrimitiveInstanceId :: InstanceId -> Bool
isPrimitiveInstanceId PrimitiveInstanceId {} = True
isPrimitiveInstanceId DeclaredInstanceId {} = False

renderInstanceId :: InstanceId -> String
renderInstanceId PrimitiveInstanceId {instanceIdClass, instanceIdPrimitiveType} = "$primitive@" ++ instanceIdClass ++ "@" ++ instanceIdPrimitiveType
renderInstanceId DeclaredInstanceId {instanceIdOwner, instanceIdClass, instanceIdTypes} =
  maybe "" ((++ "@") . renderModuleId) instanceIdOwner ++ "$instance@" ++ instanceIdClass ++ "@" ++ intercalate ";" (map renderInstanceType instanceIdTypes)

renderModuleId :: ModuleId -> String
renderModuleId RootModule = "$root"
renderModuleId RuntimeModule = "$runtime"
renderModuleId (SourceModule path) = path

renderInstanceType :: InstanceType -> String
renderInstanceType = \case
  InstanceTypeName name -> name
  InstanceTypeApply name arguments -> name ++ "[" ++ intercalate "," (map renderInstanceType arguments) ++ "]"
  InstanceTypeNominal TypeRef {typeDisplayName} arguments -> typeDisplayName ++ "[" ++ intercalate "," (map renderInstanceType arguments) ++ "]"
  InstanceTypeEffect EffectRef {effectDisplayName} arguments -> effectDisplayName ++ "[" ++ intercalate "," (map renderInstanceType arguments) ++ "]"
  InstanceTypeRecord fields -> "{" ++ intercalate "," [name ++ ":" ++ renderInstanceType fieldType | (name, fieldType) <- fields] ++ "}"
  InstanceTypeArrow arguments effects result ->
    "(" ++ intercalate "," (map renderInstanceType arguments) ++ ")->{" ++ intercalate "," (map renderInstanceType effects) ++ "}" ++ renderInstanceType result
