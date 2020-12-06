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

s1 :: Map.Map String BSONType
s1 = Map.fromList [("x", TStr), ("y", TStr)]

s2 :: Map.Map String BSONType
s2 = Map.fromList [("x", TStr), ("z", TNumber)]

s3 :: Map.Map String BSONType
s3 = Map.fromList [("x", TObject s1)]

s4 :: Map.Map String BSONType
s4 = Map.fromList [("x", TObject s2)]

m1 :: SchemaTy
m1 = S (Set.fromList [s1])

m2 :: SchemaTy
m2 = S (Set.fromList [s2])

m3 :: SchemaTy
m3 = S (Set.fromList [s3, s4])

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
                      (Pipeline [Project (Map.fromList [("x", EObject (Map.fromList [("x", Inclusion False)]))])])
                      m3
                  )
                  db1
              )
          )
          ~?= Right
            ( S
                ( Set.fromList
                    [ Map.fromList [("x", TObject (Map.fromList [("y", TStr)]))],
                      Map.fromList [("x", TObject (Map.fromList [("z", TNumber)]))]
                    ]
                )
            )
      ]