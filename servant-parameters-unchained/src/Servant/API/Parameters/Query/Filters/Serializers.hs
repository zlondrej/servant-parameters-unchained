{-# LANGUAGE OverloadedStrings #-}

module Servant.API.Parameters.Query.Filters.Serializers where

import Data.List as List
import Data.Text as Text

-- | Given a single item serializer, serializes a list of items.
--
-- Output format: `[item1,item2,...]`
-- Escaping of `,` is supported by using `\` as an escape character.
serializeQueryParamList :: (Foldable f) => (a -> Text) -> f a -> Text
serializeQueryParamList serializeItem input =
  subscript $
    Text.intercalate
      ","
      ( foldMap
          (List.singleton . escapeString "," . serializeItem)
          input
      )

subscript :: Text -> Text
subscript op = "[" <> op <> "]"

-- | Scans a string, escaping selected characters.
--
-- Escape character is backslash and it will be escaped as well.
escapeString :: [Char] -> Text -> Text
escapeString escChars = Text.concatMap escapeChar
 where
  escapeChar c
    | c `List.elem` escChars = "\\" <> Text.singleton c
    | otherwise = Text.singleton c
