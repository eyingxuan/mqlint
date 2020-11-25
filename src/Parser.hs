import Text.ParserCombinators.Parsec
import Types (AST (..), Accumulator (..), BSON (..), Expression (..), Op (..), ProjectField (..), Stage (..))

opP :: GenParser String st Op
opP = undefined

accP :: GenParser String st Accumulator
accP = undefined

bsonP :: GenParser String st BSON
bsonP = undefined

expP :: GenParser String st Expression
expP = undefined

projectP :: GenParser String st ProjectField
projectP = undefined

stageP :: GenParser String st Stage
stageP = undefined

pipelineP :: GenParser String st AST
pipelineP = undefined
