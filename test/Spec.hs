import GoldenTests (goldenTests)
import SchemaTests (schemaTests)
import Test.HUnit (Test (..), runTestTT)
import TypecheckerTests (typecheckerTests)

main :: IO ()
main = do
  runTestTT (TestList [schemaTests, typecheckerTests])
  goldenTests
  return ()
