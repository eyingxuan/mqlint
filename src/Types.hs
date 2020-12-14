{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types
  ( Context,
    BSON (..),
    BSONType (..),
    AST (..),
    Stage (..),
    FieldPath,
    SchemaTy (..),
    Index (..),
    SchemaMap,
    Accumulator (..),
    Expression (..),
    Op (..),
    TypecheckResult,
    Contextual (..),
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT, ask, withReaderT)
import Control.Monad.Writer (WriterT)
import Data.Bifunctor (second)
import Data.Map.Internal (Map)
import Data.Set (Set)
import Text.PrettyPrint (Doc, nest, ($+$))

type TypecheckTrace = Doc -> Doc

type Context = Map String SchemaTy

type Exception = WriterT [String] (ExceptT String Identity)

type TypecheckResult = ReaderT (Context, TypecheckTrace) Exception

class Contextual m where
  withContext :: m a -> Doc -> m a
  getContext :: m (Doc -> Doc)

instance Contextual TypecheckResult where
  withContext m d = withReaderT (second (\prevDoc nxtDoc -> prevDoc (d $+$ nest 2 nxtDoc))) m
  getContext = do
    (_, errorCtx) <- ask
    return errorCtx

data Index
  = ArrayIndex
  | ObjectIndex String
  deriving (Eq, Ord, Show)

type FieldPath = [Index]

data BSON
  = Number Double
  | Str String
  | Object (Map String BSON)
  | Array [BSON]
  | ObjectId String
  | Null
  | Date Int
  | Boolean Bool
  deriving (Eq, Ord, Show)

type SchemaMap = Map String BSONType

newtype SchemaTy = S (Set SchemaMap) deriving (Eq, Show)

data BSONType
  = TSum (Set BSONType)
  | TConst String
  | TBool
  | TNumber
  | TStr
  | TObject (Map String BSONType)
  | TArray BSONType
  | TObjectId
  | TNull
  | TDate
  deriving (Eq, Ord, Show)

data Op
  = Add
  | Abs
  | Ceil
  | Floor
  | Avg
  | Min
  | Max
  | Eq
  | ObjectToArray
  | ConcatArrays
  | Concat
  | Cond
  | IndexOfArray
  deriving (Eq, Ord, Show)

data Accumulator
  = AAvg
  | First
  | Last
  | AMin
  | AMax
  | Push
  | Sum
  deriving (Eq, Ord, Show)

data Expression
  = FP FieldPath
  | Inclusion Bool
  | Lit BSON
  | EObject (Map String Expression)
  | EArray [Expression]
  | Application Op [Expression]
  deriving (Eq, Ord, Show)

data Stage
  = Match Expression
  | Unwind FieldPath
  | Lookup String FieldPath FieldPath String
  | Group Expression (Map String (Accumulator, Expression))
  | Facet (Map String AST)
  | Project (Map String Expression)
  deriving (Eq, Ord, Show)

newtype AST = Pipeline [Stage]
  deriving (Eq, Ord, Show)
