module Types where


data AST = AST {
    ref  :: SourceReference, 
    completePath :: CompletePath,
    canonicalName :: CanonicalName,
    package :: Package,
    enums :: [EnumDefinition],
    types :: [TypeDefinition],
    components :: [ComponentDefinition]
}


data SourceReference = SourceReference{line :: Int, column :: Int}
newtype CompletePath = CompletePath String
newtype CanonicalName = CanonicalName String
newtype Package = Package [String]
newtype Name = Name String
newtype QualifiedName = QualifiedName [String]
newtype Id = Id Int

newtype RequestType = RequestType Type
newtype ResponseType = ResponseType Type
newtype DataDefinition = DataDefinition Type


data ComponentDefinition = ComponentDefinition{
    ref :: SourceReference,
    name :: Name,
    qualifiedName :: QualifiedName,
    id :: Id,
    dataDefs :: DataDefinition,
    events :: [EventDefinition],
    commands :: [CommandDefinition]}


data EventDefinition = EventDefinition {
    ref :: SourceReference,
    name :: Name,
    typeDef :: Type
}


data CommandDefinition = CommandDefinition{
    ref :: SourceReference,
    name :: Name,
    request :: RequestType,
    response :: ResponseType
}


data Type = 
    BuiltIn {ref :: SourceReference, builtin :: BuiltInType} |  
    UserType{ref :: SourceReference, userType :: QualifiedName } 


data BuiltInType = 
    TBool 
    | Uint32 | Uint64 
    | Int32 | Int64
    | Sint32 | Sint64 
    | Fixed32 | Fixed64 | Sfixed32 | Sfixed64
    | TFloat | TDouble
    | TString | Bytes
    | EntityId | Coordinates | Vector3d | Vector3f


data TypeDefinition = TypeDefinition {
    ref :: SourceReference,
    name :: Name,
    qualifiedName :: QualifiedName,
    enums :: [EnumDefinition],
    types :: [TypeDefinition],
    fields :: [FieldDefinition]
}


data EnumDefinition = EnumDefinition {
    ref :: SourceReference,
    name :: Name,
    qualifiedName :: QualifiedName
}

data FieldDefinition = FieldDefinition {
    ref :: SourceReference,
    name :: Name,
    number :: Int,
    fieldType :: FieldType
}


data FieldType = 
      SingularType {valueType :: Type} 
    | Option {valueType :: Type}
    | List {valueType :: Type}
    | Map {keyType :: Type, valueType :: Type}


