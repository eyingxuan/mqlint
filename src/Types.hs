module Types (BSON (..), BSONType (..), AST (..), Stage (..), FieldPath, SchemaTy (..), Index (..), SchemaMap, ProjectField (..), Accumulator (..), Expression (..), Op (..)) where

import Data.Map.Internal (Map)
import Data.Set (Set)

data Index
  = ArrayIndex
  | ObjectIndex String

type FieldPath = [Index]

data BSON
  = Dbl Double
  | Str String
  | Object (Map String BSON)
  | Array [BSON]
  | ObjectId String
  | Null
  | Intgr Int
  | Date Int
  | Boolean Bool
  deriving (Eq, Show)

type SchemaMap = Map String BSONType

newtype SchemaTy = S (Set SchemaMap) deriving (Eq, Show)

-- Should the Sum type be a set?
data BSONType
  = TSum (Set BSONType)
  | TConst String
  | TBool
  | TDbl
  | TStr
  | TObject (Map String BSONType)
  | TArray BSONType
  | TObjectId
  | TNull
  | TIntgr
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

data Accumulator
  = AAvg
  | First
  | Last
  | AMin
  | AMax

data Expression
  = FP FieldPath
  | Lit BSON
  | Obj [(String, Expression)]
  | Application Op [Expression]

data ProjectField = Inclusion Bool | NewField Expression

data Stage
  = Match Expression
  | Unwind FieldPath
  | Lookup String FieldPath FieldPath String
  | Group Expression [(String, Accumulator, Expression)]
  | Facet (Map String AST)
  | Project (Map String ProjectField)

newtype AST = Pipeline [Stage]
