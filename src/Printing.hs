module Printing where

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Types (BSONType (..))
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
wrap o c d = (if length d > 1
              then PP.vcat
              else PP.hsep)
              $ [PP.text o] ++
              map (\(i, doc) -> PP.nest 1 (
                if i == length d - 1
                  then doc
                  else doc <> PP.text ","
              )) (zip [0..] d)
              ++ [PP.text c]

quote :: String -> Doc
quote s = PP.text "\"" <> PP.text s <> PP.text "\""

objectify :: [Doc] -> Doc
objectify = wrap "{" "}"

arrayify :: [Doc] -> Doc
arrayify = wrap "[" "]"

keyvalify :: String -> Doc -> Doc
keyvalify k v = PP.text "\"" <> PP.text k <> (PP.text "\":" PP.<+> v)

ppo :: Map String BSONType -> Doc
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
  pp (TArray tarr) = objectify [keyvalify "type" (quote "sum"), keyvalify "items" (pp tarr)]
  pp (TObject props) = objectify [
      keyvalify "type" (quote "object"),
      keyvalify "properties" (ppo props)
    ]

ex = TObject $ Map.fromList [
    ("version", TConst "v1"),
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
  


