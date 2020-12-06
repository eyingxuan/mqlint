module TypecheckerTests (typecheckerTests) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (runReaderT)
import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import Test.HUnit (Test (..), (~:), (~?=))
import Typechecker (typecheck)
import Types (AST (..), BSONType (..), Expression (..), Index (..), SchemaTy (..), Stage (..))

typecheckerTests :: Test
typecheckerTests = TestList [testSimpleLookup, testSimpleProject]

m1 :: SchemaTy
m1 = S (Set.fromList [Map.fromList [("x", TStr), ("y", TStr)]])

m2 :: SchemaTy
m2 = S (Set.fromList [Map.fromList [("x", TStr), ("z", TNumber)]])

m3 :: SchemaTy
m3 = S (Set.fromList [Map.fromList [("x", TStr), ("z", TNumber), ("b", TNumber)], Map.fromList [("x", TStr), ("z", TNumber), ("a", TNumber)]])

db1 :: Map.Map String SchemaTy
db1 = Map.fromList [("col1", m1), ("col2", m2)]

testSimpleLookup :: Test
testSimpleLookup =
  "simple lookup typechecking"
    ~: TestList
      [ runIdentity
          ( runExceptT
              ( runReaderT
                  ( typecheck
                      (Pipeline [Lookup "col1" [ObjectIndex "x"] [ObjectIndex "y"] "m1"])
                      m2
                  )
                  db1
              )
          )
          ~?= Right
            ( S
                ( Set.fromList
                    [ Map.fromList
                        [ ( "m1",
                            TObject
                              (Map.fromList [("x", TStr), ("y", TStr)])
                          ),
                          ("x", TStr),
                          ("z", TNumber)
                        ]
                    ]
                )
            )
      ]

testSimpleProject :: Test
testSimpleProject =
  "simple project typechecking"
    ~: TestList
      [ runIdentity
          ( runExceptT
              ( runReaderT
                  ( typecheck
                      (Pipeline [Project (Map.fromList [("z", Inclusion False)])])
                      m2
                  )
                  db1
              )
          )
          ~?= Right (S (Set.fromList [Map.fromList [("x", TStr)]])),
        runIdentity
          ( runExceptT
              ( runReaderT
                  ( typecheck
                      (Pipeline [Project (Map.fromList [("z", Inclusion False)])])
                      m3
                  )
                  db1
              )
          )
          ~?= Right (S (Set.fromList [Map.fromList [("a", TNumber), ("x", TStr)], Map.fromList [("b", TNumber), ("x", TStr)]]))
      ]