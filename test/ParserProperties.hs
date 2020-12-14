module ParserProperties where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import Printing (indented, oneLine)
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    Property,
    Testable (..),
    generate,
    classify,
    elements,
    frequency,
    listOf,
    maxSize,
    maxSuccess,
    oneof,
    quickCheckWith,
    resize,
    scale,
    sized,
    stdArgs,
    vectorOf,
    (==>),
    quickCheck,
    listOf1,
  )

import Types
import SchemaParser (parseSchemaTy)
import MqlParser (parsePipeline)

rootTypes = [
    TBool,
    TNumber,
    TStr
  ]

genField :: Gen String
genField = listOf1 (oneof $ pure <$> ['a'..'z']) 

genBSONType :: Int -> Gen BSONType
genBSONType 0 = oneof $ pure <$> rootTypes
genBSONType n = frequency (map (\t -> (1, pure t)) rootTypes ++ [
    (n, TArray <$> genBSONType n')
  , (n, TSum . Set.fromList <$> vectorOf n (genBSONType n'))
  , (n `div` 3, TObject <$> genSchemaMap n')
  , (n, TConst <$> genField)
  ])
  where n' = n `div` 2


-- instance Arbitrary Stage where
  
instance Arbitrary Op where
  arbitrary = elements [
      Add, Abs, Ceil, Floor, Avg, Min, Max, Eq, ConcatArrays, Concat, Cond, Convert, IndexOfArray
    ]

instance Arbitrary Accumulator where
  arbitrary = elements [
      AAvg, First, Last, AMin, AMax, Push, Sum
    ]
instance Arbitrary Index where
  arbitrary = oneof [
      return ArrayIndex,
      ObjectIndex <$> genField
    ]

genBSON :: Int -> Gen BSON
genBSON 0 = oneof [Number <$> arbitrary, Str <$> genField, pure Null, Boolean <$> arbitrary]
genBSON n = frequency [
    (n `div` 3, genBSON 0),
    (n, Object . Map.fromList <$> (zip <$> listOf genField <*> listOf (genBSON n'))),
    (n, Array <$> vectorOf n' (genBSON n'))
  ]
  where n' = n `div` 2

genExp :: Int -> Gen Expression
genExp 0 = oneof [FP <$> genFP, Inclusion <$> arbitrary]
genExp n = frequency [
    (n `div` 3, genExp 0),
    (n, Application <$> arbitrary <*> oneof [
      vectorOf 1 (genExp n'),
      vectorOf 2 (genExp n')
    ]),
    (n, EArray <$> vectorOf n' arbitrary),
    (n, EObject . Map.fromList <$> (zip <$> listOf genField <*> listOf (genExp n')))
  ]
  where
    n' = n `div` 2

instance Arbitrary Expression where
  arbitrary = genExp 2
  shrink (FP fp) = []
  shrink (Inclusion _) = []
  shrink (EObject m) = EObject <$> shrinkMap m
  shrink (EArray arr) = if length arr > 1 then EArray . (:[]) <$> arr else []
  shrink (Application op arr) = if length arr > 1 then Application op . (:[]) <$> arr else []


genFP :: Gen [Index]
genFP = listOf1 arbitrary

instance Arbitrary Stage where
  arbitrary = oneof [
      Match <$> arbitrary,
      Unwind <$> genFP,
      Lookup <$> genField <*> genFP <*> genFP <*> genField,
      Group <$> arbitrary <*> listOf ((,,) <$> genField <*> arbitrary <*> arbitrary),
      Facet . Map.fromList <$> (zip <$> listOf genField <*> listOf (genAST 1)),
      Project . Map.fromList <$> (zip <$> listOf genField <*> listOf arbitrary)
    ]
  shrink (Match e) = Match <$> shrink e
  shrink (Unwind fp) = Unwind . (:[]) <$> fp
  shrink t@(Lookup f1 fp1 fp2 f2) = []
  shrink (Group idexp lst) = do
    exp <- shrink idexp
    (f, acc, exp2) <- lst
    exp2' <- shrink exp2
    return $ Group exp [(f, acc, exp2')]
  shrink (Facet m) = []
  shrink (Project m) = Project <$> shrinkMap m

genAST :: Int -> Gen AST
genAST 0 = return $ Pipeline []
genAST n = Pipeline <$> vectorOf n arbitrary

genSchemaMap :: Int -> Gen SchemaMap
genSchemaMap 0 = return Map.empty
genSchemaMap n = Map.fromList <$> (zip <$> listOf genField <*> listOf (genBSONType n))

genSchemaTy :: Int -> Gen SchemaTy
genSchemaTy 0 = return $ S Set.empty
genSchemaTy n = S . Set.fromList <$> vectorOf n (genSchemaMap n)


shrinkMap :: Arbitrary a => Map String a -> [Map String a]
shrinkMap m = do
  (k, v) <- Map.toList m
  vs <- shrink v
  ks <- shrink k
  return $ Map.singleton ks vs

instance Arbitrary BSONType where
  arbitrary = undefined
  shrink (TObject m) = TObject <$> shrinkMap m
  shrink _ = []

instance Arbitrary SchemaTy where
  arbitrary = genSchemaTy 6
  shrink (S schemas) = if Set.size schemas > 1
                       then S . Set.singleton <$> Set.toList schemas
                       else do
                         schema <- Set.toList schemas
                         shrunken <- shrinkMap schema
                         return $ S $ Set.singleton shrunken
  -- shrink (S x) = S . Set.singleton <$> Set.toList x

instance Arbitrary AST where
  arbitrary = genAST 1
  shrink (Pipeline stages) = do
    shrunk_stages <- shrink <$> stages
    fmap (Pipeline . (:[])) shrunk_stages

prop_schema_roundtrip :: SchemaTy -> Bool
prop_schema_roundtrip t = parseSchemaTy (indented t) == Right t

prop_ast_roundtrip :: AST -> Bool
prop_ast_roundtrip ast = parsePipeline (indented ast) == Right ast

-- >>> oneLine $ Pipeline [Group (Application Max [EArray [],EArray []]) [("k",AMin,Application Cond [EObject (Map.fromList [("w",Inclusion False)])])]]
-- "[ { \"$group\": { \"_id\": { \"$max\": [ [ ], [ ] ] }, \"k\": { \"$min\": { \"$cond\": [ { \"w\": 0 } ] } } } } ]"

-- >>> parsePipeline $ indented $ Pipeline [Lookup "p" [ArrayIndex] [ObjectIndex "c"] "u"]
-- Left "(line 2, column 22):\nunexpected \"u\"\nexpecting JSON value"


-- >>> parsePipeline $ indented $ Pipeline [Group (Application ConcatArrays [EObject (Map.fromList [("dh",FP [ObjectIndex "vhlas",ObjectIndex "g",ArrayIndex,ArrayIndex,ObjectIndex "k"]),("jndg",Inclusion True),("jrfnoj",Inclusion False)])]) [("t",AMin,Application Avg [EObject (Map.fromList [("mnmvpp",Inclusion True)])])]]-- Pipeline [Group (Application ConcatArrays [EObject (fromList [("dh",FP [ObjectIndex "vhlas",ObjectIndex "g",ArrayIndex,ArrayIndex,ObjectIndex "k"]),("jndg",Inclusion True),("jrfnoj",Inclusion False)])]) [("t",AMin,Application Avg [EObject (fromList [("mnmvpp",Inclusion True)])])]]
-- Left "(line 3, column 51):\nunexpected \"$\"\nexpecting JSON value"
