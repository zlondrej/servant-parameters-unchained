{- | Provides some commonly used filters for query parameters.

Feel free to use these or copy and modify them to suit your needs.
-}
module Servant.Server.Parameters.Query.Filters.Presets where

-- TODO: Move this to a package so that client instances can be written too.

import Servant.Server.Parameters.Query.Filters.Internal
import Web.HttpApiData

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

{- | Filter wrapper that extracts an additional `[${subscript}]` from the
query parameter key to be used with the underlying filter.
-}
data SubscriptFilter f a = SubscriptFilter Text (f a)
  deriving stock (Show)

-- | Parses a single value from a query parameter.
valueFilter :: (FromHttpApiData a) => (a -> b) -> Text -> Either Text b
valueFilter f = (f <$>) . parseQueryParam

-- | Parses a list of values from a query parameter.
listFilter :: (FromHttpApiData a) => ([a] -> b) -> Text -> Either Text b
listFilter f = (f <$>) . parseQueryParamList parseQueryParam

instance (FromHttpApiData a) => IsFilter (EqFilter a) where
  listFilters =
    [ FilterParser
        { parserMatchKey = parserMatchKeyPrefix >=| matchOp "eq"
        , parserParseValue = const $ valueFilter Eq
        }
    , FilterParser
        { parserMatchKey = parserMatchKeyPrefix >=| matchOp "in"
        , parserParseValue = const $ listFilter In
        }
    , FilterParser
        { parserMatchKey = parserMatchKeyPrefix >=| matchOp "neq"
        , parserParseValue = const $ valueFilter NotEq
        }
    ]

instance (FromHttpApiData a) => IsFilter (ContainsFilter a) where
  listFilters =
    [ FilterParser
        { parserMatchKey = parserMatchKeyPrefix >=| matchOp "contains"
        , parserParseValue = const $ valueFilter Contains
        }
    , FilterParser
        { parserMatchKey = parserMatchKeyPrefix >=| matchOp "icontains"
        , parserParseValue = const $ valueFilter ContainsCaseInsentitive
        }
    ]

instance (FromHttpApiData a, IsFilter (f a)) => IsFilter (SubscriptFilter f a) where
  listFilters =
    map
      ( \FilterParser{parserMatchKey, parserParseValue} ->
          FilterParser
            { parserMatchKey =
                parserMatchKey >=> \param ->
                  (param,) <$> parseSubscript
            , parserParseValue = \(param, subscript) text ->
                SubscriptFilter subscript <$> parserParseValue param text
            }
      )
      (listFilters @(f a))
