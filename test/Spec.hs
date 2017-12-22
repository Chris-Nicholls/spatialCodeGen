import Test.HUnit
import Data.Monoid
import Control.Monad

import JsonTypes 
import Parse
import Data.Aeson
import Conversion

import qualified Data.ByteString.Lazy as B

jsonFile :: FilePath
jsonFile = "test/sourceReference.json"

main :: IO Counts
main = runTestTT $ TestList 
    [testCase $ (testDecode "test/json/field.json" :: IO FieldDefinition)
    ,testCase $ (testDecode "test/json/Vector3.json" :: IO AST)  
    ,testCase $ (testDecode "test/json/test.json" :: IO AST)
    ,testCase $ (testDecode "test/json/standard_library.json" :: IO AST)
    ,testCase $ ((testDecode "test/json/test2.json" :: IO AST) )
    ]
       

testDecode  :: forall a. (Eq a, FromJSON a, Show a) =>  String -> IO a
testDecode file =  do 
    bs <- B.readFile file
    let js = eitherDecode bs ::  Either String a
    case js of 
        Right a -> return a
        Left e -> assertFailure e
    

testCase a = TestCase $ (a  >> return ())

