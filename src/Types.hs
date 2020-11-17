module Types (BSONType (..), AST (..), Stage (..)) where

import Data.Map (Map)

type FieldPath = String

data BSON
  = Dbl Double
  | Str String
  | Object [(String, BSON)]
  | Array [BSON]
  | ObjectId String
  | Null
  | Intgr Int
  | Date Int

data BSONType
  = TDbl
  | TStr
  | TObject [(String, BSONType)]
  | TArray BSONType
  | TObjectId
  | TNull
  | TIntgr
  | TDate
  deriving (Eq, Show)

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
  | Project [(String, ProjectField)]
  | Unwind FieldPath
  | Group (Expression, [(String, Accumulator, Expression)])

newtype AST = Pipeline [Stage]

type Context = [(String, Map String BSONType)]

-- nextStage :: Stage -> Context -> Context

{--
  State monad with every field and its type

--}
