import SchemaTests (schemaTests)
import Test.HUnit (Test (..), runTestTT)
import TypecheckerTests (typecheckerTests)

main :: IO ()
main = do
  _ <- runTestTT (TestList [schemaTests, typecheckerTests])
  return ()
