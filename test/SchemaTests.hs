module SchemaTests (schemaTests) where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import Schema (accessPossibleTys, insertSchemaPath, narrowDiscUnion, removeSchemaPath, updateSchemaTy)
import Test.HUnit (Test (..), (~:), (~?=))
import Types (BSONType (..), Index (..), SchemaTy (..))
import Utils (withErr)

schemaTests :: Test
schemaTests =
  TestList
    [ testDirectAccess,
      testNestedAccess,
      testSumTypeAccess,
      testTypeDiscrimination,
      testUpdateTy,
      testRemoveSchemaType
    ]

m1 :: Map.Map String BSONType
m1 = Map.fromList [("x", TStr), ("y", TNumber)]

s1 :: SchemaTy
s1 = S (Set.fromList [m1])

m2 :: Map.Map String BSONType
m2 = Map.fromList [("x", TObject m1), ("z", TDate)]

s2 :: SchemaTy
s2 = S (Set.fromList [m2])

s3 :: SchemaTy
s3 = S (Set.fromList [Map.fromList [("x", TObject m2), ("y", TObject m1)]])

testDirectAccess :: Test
testDirectAccess =
  "direct access schema"
    ~: TestList
      [ accessPossibleTys [ObjectIndex "x"] s1 ~?= return [TStr],
        accessPossibleTys [ObjectIndex "y"] s1 ~?= return [TNumber],
        accessPossibleTys [ObjectIndex "x"] s2 ~?= return [TObject m1],
        accessPossibleTys [ObjectIndex "z"] s2 ~?= return [TDate]
      ]

testNestedAccess :: Test
testNestedAccess =
  "nested access schema"
    ~: TestList
      [ accessPossibleTys [ObjectIndex "x", ObjectIndex "z"] s3 ~?= return [TDate],
        accessPossibleTys [ObjectIndex "x", ObjectIndex "x"] s3 ~?= return [TObject m1],
        accessPossibleTys [ObjectIndex "y", ObjectIndex "x"] s3 ~?= return [TStr]
      ]

d1 :: Map.Map String BSONType
d1 = Map.fromList [("v", TConst "version1"), ("x", TNumber), ("z", TNumber)]

d2 :: Map.Map String BSONType
d2 = Map.fromList [("v", TConst "version2"), ("y", TObject m2), ("z", TDate)]

s4 :: SchemaTy
s4 = S (Set.fromList [d1, d2])

testSumTypeAccess :: Test
testSumTypeAccess =
  "basic sum type access schema"
    ~: TestList
      [ accessPossibleTys
          [ ObjectIndex "z"
          ]
          s4
          ~?= return [TNumber, TDate],
        accessPossibleTys
          [ObjectIndex "x"]
          s4
          ~?= throwError "Field name not found in object"
      ]

d3 :: Map.Map String BSONType
d3 = Map.fromList [("v", TConst "version1"), ("x", TArray TNumber)]

d4 :: Map.Map String BSONType
d4 = Map.fromList [("v", TConst "version2"), ("x", TArray TDate)]

s5 :: SchemaTy
s5 = S (Set.fromList [d3, d4])

testUpdateTy :: Test
testUpdateTy =
  "update type schema"
    ~: TestList
      [ updateSchemaTy
          [ObjectIndex "x"]
          ( \b -> case b of
              TArray t -> return t
              _ -> throwError "Cannot unwind non-array type"
          )
          s5
          ~?= return
            ( S
                ( Set.fromList
                    [ Map.fromList [("v", TConst "version2"), ("x", TDate)],
                      Map.fromList [("v", TConst "version1"), ("x", TNumber)]
                    ]
                )
            )
      ]

d5 :: Map.Map String BSONType
d5 = Map.fromList [("t", TConst "short"), ("x", TStr)]

d6 :: Map.Map String BSONType
d6 = Map.fromList [("t", TConst "long"), ("x", TNumber), ("y", TStr)]

s6 :: SchemaTy
s6 = S (Set.fromList [Map.fromList [("person", TStr), ("addr", TSum (Set.fromList [TObject d5, TObject d6]))]])

testTypeDiscrimination :: Test
testTypeDiscrimination =
  "type discrimination schema"
    ~: TestList
      [ narrowDiscUnion [ObjectIndex "v"] (== "version1") s4
          ~?= return (S (Set.fromList [d1])),
        narrowDiscUnion [ObjectIndex "addr", ObjectIndex "t"] (== "long") s6
          ~?= return (S (Set.fromList [Map.fromList [("person", TStr), ("addr", TSum (Set.fromList [TObject d6]))]]))
      ]

testRemoveSchemaType :: Test
testRemoveSchemaType =
  "remove arbitrary nested field from schema"
    ~: TestList
      [ removeSchemaPath [ObjectIndex "v"] s5
          ~?= return
            ( S
                (Set.fromList [Map.fromList [("x", TArray TNumber)], Map.fromList [("x", TArray TDate)]])
            ),
        removeSchemaPath [ObjectIndex "x", ObjectIndex "x"] s3
          ~?= return
            ( S
                ( Set.fromList
                    [ Map.fromList
                        [ ( "x",
                            TObject (Map.fromList [("z", TDate)])
                          ),
                          ("y", TObject (Map.fromList [("x", TStr), ("y", TNumber)]))
                        ]
                    ]
                )
            )
      ]