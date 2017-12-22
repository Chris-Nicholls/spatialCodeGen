module Conversion(convert) where

import qualified JsonTypes as J
import qualified SchemaTypes as S
import qualified Data.Map as M

type SourceMap a = M.Map J.SourceReference a

data JsonMap = JsonMap {
    components :: SourceMap J.ComponentDefinition,
    events :: SourceMap J.EventDefinition,
    enums :: SourceMap J.EnumDefinition,
    types :: SourceMap J.TypeDefinition,
    commands :: SourceMap J.CommandDefinition
}

emptyMap = JsonMap M.empty M.empty M.empty M.empty M.empty

class Mappable a where
    addToMap :: a -> JsonMap -> JsonMap

instance Mappable J.EventDefinition where
    addToMap e@J.EventDefinition{..} json = 
        json{events =  M.insert sourceReference e (events json) }

instance Mappable J.CommandDefinition where
     addToMap e@J.CommandDefinition{..} json = 
        json{commands =  M.insert sourceReference e (commands json) }

instance Mappable J.ComponentDefinition where
    addToMap e@J.ComponentDefinition{..} json = 
        addToMap eventDefinitions .
        addToMap commandDefinitions $   
        json{components =  M.insert sourceReference e (components json)}

instance Mappable J.TypeDefinition where
    addToMap e@J.TypeDefinition{..} json = 
        addToMap enumDefinitions .
        addToMap typeDefinitions $   
        json{types =  M.insert sourceReference e (types json) }

instance Mappable J.EnumDefinition where
    addToMap e@J.EnumDefinition{..} json = 
        json{enums =  M.insert sourceReference e (enums json) }

instance Mappable J.AST where
    addToMap a@J.AST{..}  = 
        addToMap enumDefinitions .
        addToMap typeDefinitions .
        addToMap componentDefinitions 

instance Mappable a => Mappable [a] where
    addToMap xs json = foldr addToMap json xs
  
convert :: [J.AST] -> S.Schema 
convert asts = convertMap $  (addToMap asts) emptyMap

convertMap :: JsonMap -> S.Schema
convertMap map = S.Schema (convertTypes map) (convertComponents map)

convertTypes :: JsonMap -> [S.UserTypeDef]
convertTypes jm = 
    map (convertTypeDef jm) (M.elems $ types jm) ++ 
    map (convertEnum jm) (M.elems $ enums jm) 

convertComponents :: JsonMap -> [S.Component]
convertComponents jm = map (convertComponent jm) (M.elems $ components jm)

convertEnum :: JsonMap -> J.EnumDefinition -> S.UserTypeDef
convertEnum jm J.EnumDefinition{..} = S.UserEnumDef{values = vs, ..}
    where 
        vs = map convertEnumValue valueDefinitions
        convertEnumValue J.EnumValue{..} = S.EnumValue{..}

convertTypeDef :: JsonMap -> J.TypeDefinition -> S.UserTypeDef
convertTypeDef jm J.TypeDefinition{..} = 
    S.UserTypeDef {fields = fields, ..}
    where 
        fields = map (convertField jm) fieldDefinitions


convertField jm f@J.FieldDefinition{..} = S.Field{fieldType = convertFieldType jm fieldType, ..}

convertType jm J.BuiltIn{..} = S.BuiltIn{..}
convertType jm (J.UserType ref name) = S.UserType $ lookupType jm ref

convertFieldType jm (J.Singular t) = S.Singular (convertType jm t)
convertFieldType jm (J.Option (J.ValueType t)) = S.Option (convertType jm t)
convertFieldType jm (J.List (J.ValueType t)) = S.List (convertType jm t)
convertFieldType jm (J.Map (J.MapType k v)) = S.Map (convertType jm k) (convertType jm v)

convertComponent :: JsonMap -> J.ComponentDefinition -> S.Component
convertComponent jm J.ComponentDefinition{..} = S.Component{
    commands = map (convertCommand jm) commandDefinitions,
    fields = extractFields jm dataDefinition,
    events = map (convertEvent jm) eventDefinitions, .. } 


convertEvent :: JsonMap -> J.EventDefinition -> S.Event
convertEvent jm J.EventDefinition{..} = S.Event{typeDef = convertType jm typeDef, ..}

extractFields :: JsonMap -> Maybe J.DataDefinition -> [S.Field]
extractFields jm (Just (J.DataDefinition (J.BuiltIn ref _ )))  = 
    case lookupType jm ref of 
        S.UserTypeDef{..} -> fields
extractFields _ _ = []

convertCommand :: JsonMap -> J.CommandDefinition -> S.Command
convertCommand jm (J.CommandDefinition _ name 
    (J.RequestType request) (J.ResponseType response))
        = S.Command name (convertType jm request) (convertType jm response)


lookupType :: JsonMap -> J.SourceReference -> S.UserTypeDef
lookupType jm@JsonMap{..} ref 
    | Just J.TypeDefinition{..} <- M.lookup ref  types 
        = S.UserTypeDef{fields = map (convertField jm) fieldDefinitions, ..}
    | Just J.EnumDefinition{..} <- M.lookup ref enums 
        = S.UserEnumDef{values = map (extractValue jm) valueDefinitions, ..} 

extractValue :: JsonMap -> J.EnumValue -> S.EnumValue
extractValue jm J.EnumValue{..} =  S.EnumValue{..}