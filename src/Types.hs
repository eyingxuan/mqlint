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
    Exception,
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.Writer (WriterT)
import Data.Map.Internal (Map)
import Data.Set (Set)

type Context = Map String SchemaTy

type Exception = WriterT [String] (ExceptT String Identity)

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
  deriving (Eq, Ord, Show)

data Accumulator
  = AAvg
  | First
  | Last
  | AMin
  | AMax
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
  | Group Expression [(String, Accumulator, Expression)]
  | Facet (Map String AST)
  | Project (Map String Expression)
  deriving (Eq, Ord, Show)

newtype AST = Pipeline [Stage]
  deriving (Eq, Ord, Show)
