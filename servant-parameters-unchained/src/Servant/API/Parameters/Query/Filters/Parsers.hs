{-# LANGUAGE OverloadedStrings #-}

module Servant.API.Parameters.Query.Filters.Parsers where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text qualified as P
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.String.Conversions
import Data.Text as T

-- | Given a single item parser, parses a list of items.
--
-- Expected format: `item1,item2,...`
-- Escaping of `,` is supported by using `\` as an escape character.
--
-- Note: When item parser accepts empty string, seemingly empty list `?param=` will be treated
-- as a list with a single empty item (e.g. `[""]` for string types).
parseQueryParamList :: (Text -> Either Text a) -> Text -> Either Text (NonEmpty a)
parseQueryParamList parseItem input =
  case P.parseOnly (parserList P.<?> "value list") input of
    Left err -> Left $ convertString err
    Right values -> traverse parseItem values
 where
  parserList = do
    values <- escapedString "," `P.sepBy'` P.char ','
    P.endOfInput
    case NonEmpty.nonEmpty values of
      Nothing -> fail "empty list"
      Just values' -> pure values'

matchOp :: Text -> P.Parser ()
matchOp op =
  P.peekChar >>= \case
    Just '_' -> parseUnderscore P.<?> "_operator"
    Just '[' -> parseBracket P.<?> "[operator]"
    _ -> fail "missing operator"
 where
  parseUnderscore :: P.Parser ()
  parseUnderscore = do
    void $ P.char '_'
    void $ P.string op

  parseBracket :: P.Parser ()
  parseBracket = do
    void $ P.char '['
    void $ P.string op
    void $ P.char ']'

parseSubscript :: P.Parser Text
parseSubscript = key P.<?> "subscript"
 where
  key = do
    void $ P.char '['
    value <- escapedString "]"
    void $ P.char ']'
    pure $ convertString value

-- | Scans a string, decoding escape sequences for selected characters.
--
-- Escape character is backslash and it has to be escaped itself.
escapedString :: [Char] -> P.Parser Text
escapedString escChars' = convertString <$> (escaped P.<?> "escaped string")
 where
  escaped =
    P.many' $
      ( ((P.char '\\' *> P.satisfy (`List.elem` escChars)) P.<?> "escaped character")
          <|> (P.satisfy (`notElem` escChars) P.<?> "non-terminating character")
      )

  escChars = '\\' : escChars'

infixr 1 >=|

-- | Similar to `>=>`, but throws away the result of the first computation.
(>=|) :: (Monad m) => (a -> m ()) -> m c -> a -> m c
f >=| g = \x -> f x >> g

parserMatchKeyPrefix :: Text -> P.Parser ()
parserMatchKeyPrefix prefix = void (P.string prefix P.<?> "prefix: " <> convertString prefix)
