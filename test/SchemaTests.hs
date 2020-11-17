module SchemaTests (schemaTests) where

import qualified Data.Map.Internal as Map
import Schema (accessPossibleTys)
import Test.HUnit (Test (..), (~:), (~?=))
import Types (BSONType (..))

schemaTests :: Test
schemaTests = TestList [testDirectAccess, testNestedAccess, testBasicDiscriminationAccess]

ty1 :: BSONType
ty1 = TObject (Map.fromList [("x", TStr), ("y", TIntgr)])

ty2 :: BSONType
ty2 = TObject (Map.fromList [("x", ty1), ("z", TDate)])

ty3 :: BSONType
ty3 = TObject (Map.fromList [("x", ty2), ("y", ty1)])

testDirectAccess :: Test
testDirectAccess =
  "direct access schema"
    ~: TestList
      [ accessPossibleTys [("x", const True)] ty1 ~?= Right [TStr],
        accessPossibleTys [("y", const True)] ty1 ~?= Right [TIntgr],
        accessPossibleTys [("x", const True)] ty2 ~?= Right [ty1],
        accessPossibleTys [("z", const True)] ty2 ~?= Right [TDate]
      ]

testNestedAccess :: Test
testNestedAccess =
  "nested access schema"
    ~: TestList
      [ accessPossibleTys [("x", const True), ("z", const True)] ty3 ~?= Right [TDate],
        accessPossibleTys [("x", const True), ("x", const True)] ty3 ~?= Right [ty1],
        accessPossibleTys [("y", const True), ("x", const True)] ty3 ~?= Right [TStr]
      ]

d1 :: BSONType
d1 = TObject (Map.fromList [("v", TConst "version1"), ("x", TIntgr), ("z", TIntgr)])

d2 :: BSONType
d2 = TObject (Map.fromList [("v", TConst "version2"), ("y", ty2), ("z", TDate)])

ty4 :: BSONType
ty4 = TSum [d1, d2]

testBasicDiscriminationAccess :: Test
testBasicDiscriminationAccess =
  "basic discrimination access schema"
    ~: TestList
      [ accessPossibleTys
          [ ( "x",
              \m -> case m Map.!? "v" of
                Just v -> v == TConst "version1"
                Nothing -> False
            )
          ]
          ty4
          ~?= Right [TIntgr],
        accessPossibleTys
          [ ( "y",
              \m -> case m Map.!? "v" of
                Just v -> v == TConst "version2"
                Nothing -> False
            ),
            ("x", const True),
            ("x", const True)
          ]
          ty4
          ~?= Right [TStr],
        accessPossibleTys
          [ ( "z",
              const True
            )
          ]
          ty4
          ~?= Right [TIntgr, TDate]
      ]
