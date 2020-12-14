import GoldenTests (goldenTests)
import ParserProperties (runQuickCheck)
import SchemaTests (schemaTests)
import Test.HUnit (Test (..), runTestTT)
import TypecheckerTests (typecheckerTests)

main :: IO ()
main = do
  runTestTT (TestList [schemaTests, typecheckerTests])
  runQuickCheck
  goldenTests
