{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Provides some commonly used filters for query parameters.
--
-- Feel free to use these or copy and modify them to suit your needs.
module Servant.API.Parameters.Query.Filters.Presets where

import Control.Monad
import Data.ByteString
import Data.List as List
import Data.String.Conversions
import Data.Text
import Servant.API
import Servant.API.Parameters.Query.Filters.Internal
import Servant.API.Parameters.Query.Filters.Parsers
import Servant.API.Parameters.Query.Filters.Serializers

-- | Filter for equality matching.
data EqFilter a
  = Eq a
  | In [a]
  | NotEq a
  deriving stock (Show)

-- | Filter for substring matching.
data ContainsFilter a
  = Contains a
  | ContainsCaseInsentitive a
  deriving stock (Show)

-- | Filter wrapper that extracts an additional `[${subscript}]` from the
-- query parameter key to be used with the underlying filter.
data SubscriptFilter f a = SubscriptFilter Text (f a)
  deriving stock (Show)

-- | Parses a single value from a query parameter.
valueParser :: (FromHttpApiData a) => (a -> b) -> ByteString -> Either Text b
valueParser f = (f <$>) . parseQueryParam . convertString

-- | Parses a list of values from a query parameter.
listParser :: (FromHttpApiData a) => ([a] -> b) -> ByteString -> Either Text b
listParser f = (f <$>) . parseQueryParamList parseQueryParam . convertString

-- | Serializes a list of values to a single query parameter.
toQueryParamList :: (ToHttpApiData a) => [a] -> Text
toQueryParamList = serializeQueryParamList toQueryParam

instance (FromHttpApiData a) => IsServerFilter (EqFilter a) where
  filterParsers =
    [ FilterParser
        { parserMatchKey = parserMatchKeyPrefix >=| matchOp "eq"
        , parserParseValue = const $ valueParser Eq
        }
    , FilterParser
        { parserMatchKey = parserMatchKeyPrefix >=| matchOp "in"
        , parserParseValue = const $ listParser In
        }
    , FilterParser
        { parserMatchKey = parserMatchKeyPrefix >=| matchOp "neq"
        , parserParseValue = const $ valueParser NotEq
        }
    ]

instance (ToHttpApiData a) => IsClientFilter (EqFilter a) where
  filterSerializer prefix = \case
    Eq a -> (prefix <> subscript "eq", toQueryParam a)
    In as -> (prefix <> subscript "in", toQueryParamList as)
    NotEq a -> (prefix <> subscript "neq", toQueryParam a)

instance (FromHttpApiData a) => IsServerFilter (ContainsFilter a) where
  filterParsers =
    [ FilterParser
        { parserMatchKey = parserMatchKeyPrefix >=| matchOp "contains"
        , parserParseValue = const $ valueParser Contains
        }
    , FilterParser
        { parserMatchKey = parserMatchKeyPrefix >=| matchOp "icontains"
        , parserParseValue = const $ valueParser ContainsCaseInsentitive
        }
    ]

instance (ToHttpApiData a) => IsClientFilter (ContainsFilter a) where
  filterSerializer prefix = \case
    Contains a -> (prefix <> subscript "contains", toQueryParam a)
    ContainsCaseInsentitive a -> (prefix <> subscript "icontains", toQueryParam a)

instance (FromHttpApiData a, IsServerFilter (f a)) => IsServerFilter (SubscriptFilter f a) where
  filterParsers =
    List.map
      ( \FilterParser{parserMatchKey, parserParseValue} ->
          FilterParser
            { parserMatchKey =
                parserMatchKey >=> \param ->
                  (param,) <$> parseSubscript
            , parserParseValue = \(param, subscriptValue) text ->
                SubscriptFilter subscriptValue <$> parserParseValue param text
            }
      )
      (filterParsers @(f a))

instance (ToHttpApiData a, IsClientFilter (f a)) => IsClientFilter (SubscriptFilter f a) where
  filterSerializer prefix (SubscriptFilter subscriptValue param) =
    let (key, value) = filterSerializer prefix param
     in (key <> subscript subscriptValue, value)
