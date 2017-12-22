module JsonTypes where

import GHC.Generics
import Data.Text(Text)

data AST = AST {
    sourceReference :: SourceReference, 
    completePath :: CompletePath,
    canonicalName :: CanonicalName,
    package :: Package,
    enumDefinitions :: [EnumDefinition],
    typeDefinitions :: [TypeDefinition],
    componentDefinitions :: [ComponentDefinition]
} deriving (Generic, Show, Eq)


data SourceReference = SourceReference{line :: Int, column :: Int} deriving (Generic, Show, Eq, Ord)
newtype CompletePath = CompletePath Text deriving (Generic, Show, Eq)
newtype CanonicalName = CanonicalName Text deriving (Generic, Show, Eq)
newtype Package = Package [Text] deriving (Generic, Show, Eq)
newtype Name = Name Text deriving (Generic, Show, Eq)
newtype QualifiedName = QualifiedName [Text] deriving (Generic, Show, Eq)
newtype Id = Id Int deriving (Generic, Show, Eq)

newtype RequestType = RequestType Type deriving (Generic, Show, Eq)
newtype ResponseType = ResponseType Type deriving (Generic, Show, Eq)
newtype DataDefinition = DataDefinition Type deriving (Generic, Show, Eq)


data ComponentDefinition = ComponentDefinition{
    sourceReference :: SourceReference,
    name :: Name,
    qualifiedName :: QualifiedName,
    id :: Id,
    dataDefinition :: Maybe DataDefinition,
    eventDefinitions :: [EventDefinition],
    commandDefinitions :: [CommandDefinition]}
    deriving (Generic, Show, Eq)


data EventDefinition = EventDefinition {
    sourceReference :: SourceReference,
    name :: Name,
    typeDef :: Type
} deriving (Generic, Show, Eq)


data CommandDefinition = CommandDefinition{
    sourceReference :: SourceReference,
    name :: Name,
    requestType :: RequestType,
    responseType :: ResponseType
} deriving (Generic, Show, Eq)


data Type = 
    BuiltIn {sourceReference :: SourceReference, builtin :: BuiltInType} |  
    UserType{sourceReference :: SourceReference, userType :: QualifiedName }
   deriving (Generic, Show, Eq)


data BuiltInType = 
    TBool 
    | Uint32 | Uint64 
    | Int32 | Int64
    | Sint32 | Sint64 
    | Fixed32 | Fixed64 | Sfixed32 | Sfixed64
    | Float | TDouble
    | TString | Bytes
    | EntityId | Coordinates | Vector3d | Vector3f
    deriving (Generic, Show, Eq)


data TypeDefinition = TypeDefinition {
    sourceReference :: SourceReference,
    name :: Name,
    qualifiedName :: QualifiedName,
    enumDefinitions :: [EnumDefinition],
    typeDefinitions :: [TypeDefinition],
    fieldDefinitions :: [FieldDefinition]
} deriving (Generic, Show, Eq)


data EnumDefinition = EnumDefinition {
    sourceReference :: SourceReference,
    name :: Name,
    qualifiedName :: QualifiedName,
    valueDefinitions :: [EnumValue]
} deriving (Generic, Show, Eq)

data EnumValue = EnumValue {
    sourceReference :: SourceReference,
    name :: Name,
    value :: Int
} deriving (Generic, Show, Eq)


data FieldDefinition = FieldDefinition {
    sourceReference :: SourceReference,
    name :: Name,
    number :: Int,
    fieldType :: FieldType
} deriving (Generic, Show, Eq)


data FieldType = 
       Singular {singularType :: Type} 
     | Option {optionType :: ValueType}
     | List {listType :: ValueType}
     | Map {mapType :: MapType} 
     deriving (Generic, Show, Eq)


data SingularType = SingularType {
    sourceReference :: SourceReference,
    builtInType :: BuiltInType
}    deriving (Generic, Show, Eq)

data ValueType = ValueType {valueType :: Type }
    deriving (Generic, Show, Eq)


data MapType = MapType {keyType :: Type, valueType :: Type}
    deriving (Generic, Show, Eq)