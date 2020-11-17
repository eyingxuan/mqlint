import SchemaTests (schemaTests)
import Test.HUnit (Test (..), runTestTT)

main :: IO ()
main = do
  _ <- runTestTT (TestList [schemaTests])
  return ()
