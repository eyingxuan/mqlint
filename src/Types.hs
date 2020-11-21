module Types (BSON (..), BSONType (..), AST (..), Stage (..), FieldPath, SchemaTy (..), Index (..), SchemaMap) where

import Data.Map.Internal (Map)
import Data.Set (Set)

data Index
  = ArrayIndex
  | ObjectIndex String

type FieldPath = [Index]

data BSON
  = Dbl Double
  | Str String
  | Object [(String, BSON)]
  | Array [BSON]
  | ObjectId String
  | Null
  | Intgr Int
  | Date Int

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
  | Facet [(String, AST)]
  | Lookup String FieldPath FieldPath String
  | Project [(FieldPath, ProjectField)]
  | Unwind FieldPath
  | Group (Expression, [(String, Accumulator, Expression)])

newtype AST = Pipeline [Stage]
