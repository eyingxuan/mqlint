module Printing (PP (..), indented, oneLine) where

import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
import Types (
  BSONType (..),
  SchemaTy (..),
  BSON (..),
  Op (..),
  Accumulator (..),
  Expression (..),
  Stage (..),
  AST (..),
  Index (..)
  )

-- import Data.Sequence (mapWithIndex)

-- data BSONType
--   = TSum (Set BSONType)
--   | TConst String
--   | TBool
--   | TNumber
--   | TStr
--   | TObject (Map String BSONType)
--   | TArray BSONType
--   | TObjectId
--   | TNull
--   | TDate
--   deriving (Eq, Ord, Show)

class PP a where
  pp :: a -> Doc

wrap :: String -> String -> [Doc] -> Doc
wrap o c d =
  ( if length d > 1
      then PP.vcat
      else PP.hsep
  )
    $ [PP.text o]
      ++ map
        ( \(i, doc) ->
            PP.nest
              1
              ( if i == length d - 1
                  then doc
                  else doc <> PP.text ","
              )
        )
        (zip [0 ..] d)
      ++ [PP.text c]

quote :: String -> Doc
quote s = PP.text "\"" <> PP.text s <> PP.text "\""

objectify :: [Doc] -> Doc
objectify = wrap "{" "}"

arrayify :: [Doc] -> Doc
arrayify = wrap "[" "]"

keyvalify :: String -> Doc -> Doc
keyvalify k v = PP.text "\"" <> PP.text k <> (PP.text "\":" PP.<+> v)

ppo :: PP a => Map String a -> Doc
ppo m = objectify $ map (\(k, v) -> keyvalify k (pp v)) (Map.toList m)

ppt :: String -> Doc
ppt s = objectify [keyvalify "type" (quote s)]

instance PP BSONType where
  pp TNull = ppt "null"
  pp TObjectId = ppt "ObjectId"
  pp (TConst s) = ppt s
  pp TBool = ppt "boolean"
  pp TNumber = ppt "number"
  pp TStr = ppt "string"
  pp TDate = ppt "date"
  pp (TSum s) = objectify [keyvalify "type" (quote "sum"), keyvalify "types" (arrayify (pp <$> Set.toList s))]
  pp (TArray tarr) = objectify [keyvalify "type" (quote "array"), keyvalify "items" (pp tarr)]
  pp (TObject props) =
    objectify
      [ keyvalify "type" (quote "object"),
        keyvalify "properties" (ppo props)
      ]

instance PP SchemaTy where
  pp (S l) = case Set.toList l of
    [ty] -> pp $ TObject ty
    l -> arrayify (map (pp . TObject) l)

instance PP BSON where
  pp (Number n) = PP.double n
  pp (Str s) = PP.text s
  pp (ObjectId s) = PP.text s
  pp Null = PP.text "null"
  pp (Date d) = PP.int d
  pp (Boolean True) = PP.text "true"
  pp (Boolean False) = PP.text "false"
  pp (Array arr) = arrayify (pp <$> arr)
  pp (Object obj) = ppo obj

instance PP Op where
  pp Add = PP.text "$add"
  pp Abs = PP.text "$abs"
  pp Ceil = PP.text "$ceil"
  pp Floor = PP.text "$floor"
  pp Avg = PP.text "$avg"
  pp Min = PP.text "$min"
  pp Max = PP.text "$max"
  pp Eq = PP.text "$eq"

instance PP Accumulator where
  pp AAvg = PP.text "$avg"
  pp First = PP.text "$first"
  pp Last = PP.text "$last"
  pp AMin = PP.text "$min"
  pp AMax = PP.text "$max"
  pp Push = PP.text "$push"
  pp Sum = PP.text "$sum"

instance PP Index where
  pp ArrayIndex = PP.text "0"
  pp (ObjectIndex s) = PP.text s

instance PP Expression where
  pp (FP fp) = foldMap id (intersperse (PP.text ".") (pp <$> fp))
  pp (Inclusion True) = PP.text "1"
  pp (Inclusion False) = PP.text "0"
  pp (Lit bson) = pp bson
  pp (EArray arr) = arrayify (pp <$> arr)
  pp (EObject obj) = ppo obj
  pp (Application op exprs) = ppo (Map.fromList [(PP.render (pp op), EArray exprs)])


oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode = PP.OneLineMode}) . pp

indented :: PP a => a -> String
indented = PP.render . pp

ex =
  TObject $
    Map.fromList
      [ ("version", TConst "v1"),
        ("active", TBool),
        ("name", TStr),
        ("addresses", TArray TStr)
      ]

-- >>> pp ex
-- {
--  "type": "object",
--  "properties": {
--                 "active": {"type": "boolean"},
--                 "addresses": {
--                               "type": "sum",
--                               "items": {"type": "string"}
--                              },
--                 "name": {"type": "string"},
--                 "version": {"type": "v1"}
--                }
-- }
