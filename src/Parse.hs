module Parse where

import Data.Aeson
import Data.Aeson.Types(Parser)
import GHC.Generics
import Types
import Data.Maybe(fromMaybe)
import Control.Monad(liftM2, liftM3)

import qualified Data.Text as T

instance FromJSON AST
instance FromJSON SourceReference 
instance FromJSON CompletePath 
instance FromJSON CanonicalName 
instance FromJSON Package where
    parseJSON = parseStringList Package

instance FromJSON Name 
instance FromJSON QualifiedName where
    parseJSON = parseStringList QualifiedName

instance FromJSON Id 
instance FromJSON RequestType 
instance FromJSON ResponseType 
instance FromJSON DataDefinition 
instance FromJSON ComponentDefinition 
instance FromJSON EventDefinition 
instance FromJSON CommandDefinition 
instance FromJSON TypeDefinition 
instance FromJSON EnumDefinition 
instance FromJSON ValueType
instance FromJSON MapType 

instance FromJSON FieldDefinition where
    parseJSON (Object v) = 
        FieldDefinition 
            <$> v .: "sourceReference"
            <*> v .: "name"
            <*> v .: "number"
            <*> parseFieldType v


instance FromJSON SingularType 

instance FromJSON BuiltInType where
    parseJSON s = case s of 
        String "float" -> return Float
        String "double" -> return TDouble
        String "sint32" -> return Sint32
        String "sint64" -> return Sint64
        String "int32" -> return Int32
        String "int64" -> return Int64
        String "uint32" -> return Uint32
        String "uint64" -> return Uint64
        String "bool" -> return TBool
        String "string" -> return TString
        String "fixed32" -> return Fixed32
        String "fixed64" -> return Fixed64
        String "sfixed32" -> return Sfixed32
        String "sfixed64" -> return Sfixed64
        String "EntityId" -> return EntityId
        String "bytes" -> return Bytes
        s -> fail ("Unknown type: " ++ show s)
    

parseStringList t (String s)  = return (t (T.splitOn "." s))
parseStringList _ s = fail ("Cannot pares package: "  ++ show s)


parseFieldType :: Object -> Parser FieldType
parseFieldType object = 
           ((fmap Singular) <$> (object .:? "singularType"))
        |? (fmap Option) <$> (object .:? "optionType")
        |? (fmap List) <$> (object .:? "listType")
        |? (fmap Map) <$>  (object .:? "mapType") 
        |? (fail ("Could not extract field type: " ++ show object))


infixr 1 |? 

(|?) :: Parser (Maybe a) -> Parser a -> Parser a
p |? r = p >>= \p' -> case p' of 
    Just a -> return a
    Nothing -> r

instance FromJSON Type where
    parseJSON = parseType

parseType :: Value -> Parser Type
parseType (Object object) =
  (object .:? "sourceReference") >>= \sr -> (object .:? "builtInType" ) >>= \bi -> 
    (object .:? "userType" ) >>= \user -> case (sr, bi, user) of
        (Just s, Just b, _) -> return (BuiltIn s b)
        (Just s, _ , Just u) -> return (UserType s u)
        _ -> fail ("Could not parse type from: " ++ show object ++ show (bi, user))


