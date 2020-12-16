module ParserProperties (runQuickCheck) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Parser.MqlParser (parsePipeline)
import Parser.ParserCommon (runTransformResult)
import Parser.Printing (indented)
import Parser.SchemaParser (parseSchemaTy)
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    choose,
    elements,
    frequency,
    listOf,
    listOf1,
    maxSize,
    maxSuccess,
    oneof,
    quickCheck,
    vectorOf,
  )
import Types

rootTypes =
  [ TBool,
    TNumber,
    TStr
  ]

genField :: Gen String
genField = do
  len <- choose (1, 5)
  vectorOf len (oneof $ pure <$> ['a' .. 'z'])

genConstField :: Gen String
genConstField = do
  s <- genField
  -- exclude keywords
  if s `elem` ["string", "boolean", "number", "date", "id", "sum", "array"] then genConstField else return s

genBSONType :: Int -> Gen BSONType
genBSONType 0 = oneof $ pure <$> rootTypes
genBSONType n =
  frequency
    ( map (\t -> (1, pure t)) rootTypes
        ++ [ (n, TArray <$> genBSONType n'),
             (n, TSum . Set.fromList <$> vectorOf n (genBSONType n')),
             (n `div` 3, TObject <$> genSchemaMap n'),
             (n, TConst <$> genConstField)
           ]
    )
  where
    n' = n `div` 5

instance Arbitrary Op where
  arbitrary =
    elements
      [ Add,
        Abs,
        Ceil,
        Floor,
        Avg,
        Min,
        Max,
        Eq,
        ConcatArrays,
        Concat,
        Cond,
        IndexOfArray
      ]

instance Arbitrary Accumulator where
  arbitrary =
    elements
      [ AAvg,
        First,
        Last,
        AMin,
        AMax,
        Push,
        Sum
      ]

instance Arbitrary Index where
  arbitrary =
    oneof
      [ return ArrayIndex,
        ObjectIndex <$> genField
      ]

genBSON :: Int -> Gen BSON
genBSON 0 = oneof [Number <$> arbitrary, Str <$> genField, pure Null, Boolean <$> arbitrary]
genBSON n =
  frequency
    [ (3, genBSON 0),
      (n, Object . Map.fromList <$> (zip <$> listOf genField <*> listOf (genBSON n'))),
      (n, Array <$> vectorOf n' (genBSON n'))
    ]
  where
    n' = n `div` 5

genExp :: Int -> Gen Expression
genExp 0 = oneof [FP <$> genFP, Inclusion <$> arbitrary]
genExp n =
  frequency
    [ (3, genExp 0),
      ( n,
        Application <$> arbitrary
          <*> oneof
            [ vectorOf 1 (genExp n'),
              vectorOf 2 (genExp n')
            ]
      ),
      (n, EArray <$> vectorOf n' arbitrary),
      ( n,
        do
          len <- choose (1, 3)
          fields <- vectorOf len genField
          exps <- vectorOf len (genExp n')
          return $ EObject (Map.fromList (zip fields exps))
      )
    ]
  where
    n' = n `div` 10


  -- shrink (TObject m) = do
  --   (ks, vs) <- Map.toList m
  --   vs <- shrink vs
  --   ks <- shrink ks
  --   case vs of
  --     TObject m' -> TObject . uncurry Map.singleton <$> Map.toList m'
  --     _ -> return $ TObject $ Map.singleton ks vs

instance Arbitrary Expression where
  arbitrary = genExp 2
  shrink (FP fp) = [FP fp]
  shrink t@(Inclusion _) = [t]
  shrink (EObject m) = do
    (k', v') <- Map.toList m
    ks <- shrink k'
    vs <- shrink v'
    case vs of
      EObject m' -> EObject . uncurry Map.singleton <$> Map.toList m'
      _ -> return $ EObject $ Map.singleton ks vs
  shrink t@(EArray arr) = if length arr > 1 then EArray . (: []) <$> arr else [t]
  shrink t@(Application op arr) = if length arr > 1 then Application op . singleton <$> arr else [t]

genFP :: Gen [Index]
genFP = listOf1 arbitrary

instance Arbitrary Stage where
  arbitrary =
    oneof
      [ Match <$> arbitrary,
        Unwind <$> genFP,
        Lookup <$> genField <*> genFP <*> genFP <*> genField,
        do
          len <- choose (1, 3)
          fields <- vectorOf len ((,) <$> genField <*> ((,) <$> arbitrary <*> arbitrary))
          exp <- arbitrary
          return $ Group exp (Map.fromList fields),
        do
          len <- choose (1, 3)
          fields <- vectorOf len genField
          asts <- vectorOf len (genAST 1)
          return $ Facet (Map.fromList (zip fields asts)),
        do
          len <- choose (1, 3)
          fields <- vectorOf len genField
          exps <- vectorOf len arbitrary
          return $ Project (Map.fromList (zip fields exps))
      ]
  shrink (Match e) = Match <$> shrink e
  shrink (Unwind fp) = if length fp > 1 then Unwind . singleton <$> fp else [Unwind fp]
  shrink t@(Lookup f1 fp1 fp2 f2) = [t]
  shrink (Group idexp s) = do
    id' <- shrink idexp
    (k, v) <- Map.toList s
    return $ Group id' (Map.singleton k v)
  shrink (Facet m) = [Facet m]
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
  shrink (TObject m) = do
    (ks, vs) <- Map.toList m
    vs <- shrink vs
    ks <- shrink ks
    case vs of
      TObject m' -> TObject . uncurry Map.singleton <$> Map.toList m'
      _ -> return $ TObject $ Map.singleton ks vs
  shrink t = [t]

instance Arbitrary SchemaTy where
  arbitrary = genSchemaTy 6
  shrink (S schemas) =
    if Set.size schemas > 1
      then S . Set.singleton <$> Set.toList schemas
      else do
        schema <- Set.toList schemas
        shrunken <- shrinkMap schema
        return $ S $ Set.singleton shrunken

instance Arbitrary AST where
  arbitrary = genAST 5
  shrink (Pipeline stages) = do
    -- shrunk_stages <- shrink <$> stages
    stage <- stages
    case stage of
      Facet m -> snd <$> Map.toList m
      _ -> if length stages > 1 then Pipeline . shrink <$> stages else []

    
singleton :: a -> [a]
singleton = (: [])

prop_schema_roundtrip :: SchemaTy -> Bool
prop_schema_roundtrip t = runTransformResult (parseSchemaTy (indented t)) == Right t

prop_ast_roundtrip :: AST -> Bool
prop_ast_roundtrip ast = runTransformResult (parsePipeline (indented ast)) == Right ast

runQuickCheck :: IO ()
runQuickCheck = do
  quickCheck prop_schema_roundtrip
  quickCheck prop_ast_roundtrip
