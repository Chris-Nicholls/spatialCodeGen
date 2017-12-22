module SchemaTypes where

import JsonTypes (Name, QualifiedName, BuiltInType, Id)

import GHC.Generics


data Schema = Schema {
    types :: [UserTypeDef], 
    components :: [Component]
} deriving (Generic, Show, Eq)


data Component = Component {
    name :: Name,
    qualifiedName :: QualifiedName,
    id :: Id,
    fields :: [Field],
    events :: [Event],
    commands :: [Command]
} deriving (Generic, Show, Eq)

data Command = Command {
    name :: Name,
    requestType :: Type,
    responseType :: Type
} deriving (Generic, Show, Eq)

data Event = Event {
    name :: Name,
    typeDef :: Type
} deriving (Generic, Show, Eq)

data Field = Field {
    name :: Name,
    number :: Int,
    fieldType :: FieldType
} deriving (Generic, Show, Eq)


data FieldType = 
       Singular {singularType :: Type} 
     | Option {optionType :: Type}
     | List {listType :: Type}
     | Map {key :: Type, value:: Type} 
     deriving (Generic, Show, Eq)



data Type = 
    BuiltIn {builtin :: BuiltInType} |  
    UserType{userType :: UserTypeDef}
    deriving (Generic, Show, Eq)


data UserTypeDef = UserTypeDef {
    name :: Name,
    qualifiedName :: QualifiedName,
    fields :: [Field]
} | UserEnumDef {
    name :: Name,
    qualifiedName :: QualifiedName,
    values :: [EnumValue]
}
    deriving (Generic, Show, Eq)


data EnumValue = EnumValue {
    name :: Name,
    value :: Int
} deriving (Generic, Show, Eq)
