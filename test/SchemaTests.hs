module SchemaTests (schemaTests) where

import qualified Data.Map.Internal as Map
import Schema (accessPossibleTys, narrowDiscUnion, updateSchemaTy)
import Test.HUnit (Test (..), (~:), (~?=))
import Types (BSONType (..), Index (..), SchemaTy (..))
import Utils (withErr)

schemaTests :: Test
schemaTests = TestList [testDirectAccess, testNestedAccess, testSumTypeAccess, testTypeDiscrimination, testUpdateTy]

m1 :: Map.Map String BSONType
m1 = Map.fromList [("x", TStr), ("y", TIntgr)]

s1 :: SchemaTy
s1 = S [m1]

m2 :: Map.Map String BSONType
m2 = Map.fromList [("x", TObject m1), ("z", TDate)]

s2 :: SchemaTy
s2 = S [m2]

s3 :: SchemaTy
s3 = S [Map.fromList [("x", TObject m2), ("y", TObject m1)]]

testDirectAccess :: Test
testDirectAccess =
  "direct access schema"
    ~: TestList
      [ accessPossibleTys [ObjectIndex "x"] s1 ~?= Right [TStr],
        accessPossibleTys [ObjectIndex "y"] s1 ~?= Right [TIntgr],
        accessPossibleTys [ObjectIndex "x"] s2 ~?= Right [TObject m1],
        accessPossibleTys [ObjectIndex "z"] s2 ~?= Right [TDate]
      ]

testNestedAccess :: Test
testNestedAccess =
  "nested access schema"
    ~: TestList
      [ accessPossibleTys [ObjectIndex "x", ObjectIndex "z"] s3 ~?= Right [TDate],
        accessPossibleTys [ObjectIndex "x", ObjectIndex "x"] s3 ~?= Right [TObject m1],
        accessPossibleTys [ObjectIndex "y", ObjectIndex "x"] s3 ~?= Right [TStr]
      ]

d1 :: Map.Map String BSONType
d1 = Map.fromList [("v", TConst "version1"), ("x", TIntgr), ("z", TIntgr)]

d2 :: Map.Map String BSONType
d2 = Map.fromList [("v", TConst "version2"), ("y", TObject m2), ("z", TDate)]

s4 :: SchemaTy
s4 = S [d1, d2]

testSumTypeAccess :: Test
testSumTypeAccess =
  "basic sum type access schema"
    ~: TestList
      [ accessPossibleTys
          [ ObjectIndex "z"
          ]
          s4
          ~?= Right [TIntgr, TDate],
        accessPossibleTys
          [ObjectIndex "x"]
          s4
          ~?= Left "Field name not found in object"
      ]

testTypeDiscrimination :: Test
testTypeDiscrimination =
  "type discrimination schema"
    ~: TestList
      [ narrowDiscUnion
          ( \m -> do
              ty <- withErr (m Map.!? "v") "Field name not found in object"
              return $ ty == TConst "version1"
          )
          s4
          ~?= Right (S [d1])
      ]

d3 :: Map.Map String BSONType
d3 = Map.fromList [("v", TConst "version1"), ("x", TArray TIntgr)]

d4 :: Map.Map String BSONType
d4 = Map.fromList [("v", TConst "version2"), ("x", TArray TDate)]

s5 :: SchemaTy
s5 = S [d3, d4]

testUpdateTy :: Test
testUpdateTy =
  "update type schema"
    ~: TestList
      [ updateSchemaTy
          [ObjectIndex "x"]
          ( \b -> case b of
              TArray t -> Right t
              _ -> Left "Cannot unwind non-array type"
          )
          s5
          ~?= Right
            ( S
                [ Map.fromList [("v", TConst "version1"), ("x", TIntgr)],
                  Map.fromList [("v", TConst "version2"), ("x", TDate)]
                ]
            )
      ]