module ParserProperties where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Printing (indented)
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
  )

import Types (BSONType (..), SchemaTy (..), SchemaMap)
import SchemaParser (parseSchemaTy)

rootTypes = [
    TBool,
    TNumber,
    TStr
  ]

genField :: Gen String
genField = listOf (oneof $ pure <$> ['a'..'z']) 

genBSONType :: Int -> Gen BSONType
genBSONType 0 = oneof $ pure <$> rootTypes
genBSONType n = frequency (map (\t -> (1, pure t)) rootTypes ++ [
    (n, TArray <$> genBSONType n')
  , (n, TSum . Set.fromList <$> vectorOf n (genBSONType n'))
  , (n `div` 3, TObject <$> genSchemaMap n')
  , (n, TConst <$> genField)
  ])
  where n' = n `div` 2

genSchemaMap :: Int -> Gen SchemaMap
genSchemaMap 0 = return Map.empty
genSchemaMap n = Map.fromList <$> (zip <$> listOf genField <*> listOf (genBSONType n))

genSchemaTy :: Int -> Gen SchemaTy
genSchemaTy 0 = return $ S Set.empty
genSchemaTy n = S . Set.fromList <$> vectorOf n (genSchemaMap n)

instance Arbitrary SchemaTy where
  arbitrary = sized genSchemaTy
  -- shrink (S x) = S . Set.singleton <$> Set.toList x

prop_roundtrip :: SchemaTy -> Bool
prop_roundtrip t = parseSchemaTy (indented t) == Right t
