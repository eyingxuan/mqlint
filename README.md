# Typechecker for MongoDB Aggregation Queries

Names: Davis Haupt (dhaupt), Yingxuan Eng (yxeng)

## Library Dependencies

- containers
- parsec
- mtl
- pretty
- dlist
- QuickCheck
- tasty
- tasty-golden
- filepath
- bytestring
- HUnit

## Overview of Files

### Library

This project is split into two components, parsing and typechecking. The parser parses a file into relevant ASTs as defined in `src/Types.hs`, while the typechecker takes in relevant ASTs, verifies the correctness of a query and outputs the expected output schema as well as possible lint errors. Relevant functions required to parse queries and run the typechecker are reexported in `src/Lib.hs`.

The parser (in `src/Parser`) first parses files into a basic JSON AST (`src/Parser/JsonParser.hs`), and the JSON AST is furthered transformed into the MQL (Mongo Query Language) AST or a schema AST based on the input file, with each parser defined in `src/Parser/MqlParser.hs` and `src/Parser/SchemaParser.hs`. A pretty printer was also written in `src/Parser/Printing.hs` for pretty error messages and roundtrip QuickCheck tests.

The typechecker (in `src/Typechecker`) takes in a mapping of collection names to schemas, a MQL query and the collection name and verifies its correctness, preventing issues including invalid field path accesses, applying aggregation operations to incorrect types etc. and supports discriminated unions, which are particularly useful for the Document Versioning Pattern. The typechecker processes each stage independently in the pipeline, and turns an input schema into an output schema based on the stage, as defined in the `processStage` function in `src/Typechecker/Typechecker.hs`. Various primitive operations that are commonly applied to the schema ASTs are implemented in `src/Typechecker/Schema.hs` and expression typechecking is implemented in `src/Typechecker/ExpressionType.hs`.

### Testing

We use a combination of unit tests, golden tests, and QuickCheck properties to verify the correctness of our program. Unit tests are used to verify basic schema manipulation primitives and basic typechecking. QuickCheck, in particular round trip properties, are used to verify the correctness of the parser and pretty printer. Golden tests are used to maintain snapshots of expected output when running more complicated queries, where the first accepted results are manually verified.
