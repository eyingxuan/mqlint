module Utils (withErr) where

withErr :: Maybe a -> String -> Either String a
withErr (Just x) _ = Right x
withErr Nothing msg = Left msg