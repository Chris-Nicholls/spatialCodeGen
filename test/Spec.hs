import Test.HUnit
import Data.Monoid
import Control.Monad

import Types 
import Parse
import Data.Aeson

import qualified Data.ByteString.Lazy as B

jsonFile :: FilePath
jsonFile = "test/sourceReference.json"

main :: IO Counts
main = runTestTT $ TestList 
    [testCase $ (testDecode "test/field.json" :: IO FieldDefinition)
    ,testCase $ (testDecode "test/Vector3.json" :: IO AST)
    ,testCase $ (testDecode "test/test.json" :: IO AST)
    ,testCase $ (testDecode "test/standard_library.json" :: IO AST)
    ]
       

testFile  :: forall a. (Eq a, FromJSON a, Show a) =>  String -> a -> String -> Test
testFile file test name = TestCase $  do 
    bs <- B.readFile file
    let js = eitherDecode bs ::  Either String a
    assertEqual name (Right test) js



testDecode  :: forall a. (Eq a, FromJSON a, Show a) =>  String -> IO a
testDecode file =  do 
    bs <- B.readFile file
    let js = eitherDecode bs ::  Either String a
    case js of 
        Right a -> return a
        Left e -> assertFailure e
    


testCase a = TestCase $ (a >> return ())