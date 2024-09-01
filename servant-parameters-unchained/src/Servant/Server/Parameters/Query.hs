{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Server.Parameters.Query (
  module Servant.API.Parameters,
  module Servant.Server.Parameters.Query,
  module Servant.Server.Parameters.FormatError,
  module Servant.Server.Parameters.Internal.Delayed,
)
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Kind
import Data.List as List
import Data.Proxy
import Data.String.Conversions
import Data.Text
import Data.Typeable
import Network.HTTP.Types
import Network.Wai
import Servant.API
import Servant.API.Parameters
import Servant.Server
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Parameters.FormatError
import Servant.Server.Parameters.Internal.Delayed

class IsQueryServerParameter a where
  -- | The type which is passed to the handler.
  --
  -- By default, this is the same as the type class parameter, but can be overridden if needed.
  type QueryServerType a :: Type

  type QueryServerType a = a

  -- | Parse the query parameter from a list of key-value pairs.
  --
  -- Both the key and the value will be properly URL-decoded.
  parseQueryParameter :: DecodedQuery -> DelayedWithErrorFormatterIO QueryParameter (QueryServerType a)

instance
  ( HasServer api context
  , IsQueryServerParameter a
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  HasServer (QueryParameter a :> api) context
  where
  type ServerT (QueryParameter a :> api) m = QueryServerType a -> ServerT api m

  route Proxy context subserver =
    let errorFormatters = getContextEntry (context .++ (defaultErrorFormatters :. EmptyContext))
        parseServerParameter = withDecodedQuery (parseQueryParameter @a) errorFormatters
     in route (Proxy @api) context (runDelayedCont parseServerParameter subserver)

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt . s

-- | Given a function to parse query parameters, create a delayed handler that parses the query parameters.
--
-- The query parameters are passed as is (without URL-decoding) to the function.
withQuery :: (Query -> DelayedWithErrorFormatterIO QueryParameter a) -> ErrorFormatters -> DelayedCont env a b
withQuery parseParams errorFormatters = do
  DelayedCont
    ( `addParameterCheck`
        withRequest
          ( \req ->
              runReaderT
                (parseParams $ queryString req)
                errorFormatters
          )
    )

-- | Given a function to parse query parameters, create a delayed handler that parses the query parameters.
--
-- The query parameters are URL-decoded before being passed to the function.
withDecodedQuery :: (DecodedQuery -> DelayedWithErrorFormatterIO QueryParameter a) -> ErrorFormatters -> DelayedCont env a b
withDecodedQuery parseParams = withQuery (parseParams . decodeQuery)

-- | Decode a list of key-value pairs to a list of key-value pairs with URL-decoded keys and values, converted to `Text`.
decodeQuery :: Query -> DecodedQuery
decodeQuery = List.map (\(k, mv) -> (convertString $ urlDecode True k, urlDecode True <$> mv))

-- | Helper function to format an error message using the formatter for url parsing errors.
delayedFailFatalQuery :: forall a b. (Typeable a) => String -> DelayedWithErrorFormatterIO QueryParameter b
delayedFailFatalQuery = lift . delayedFailFatal <=< formatError @QueryParameter @a

failQueryParamRequired :: forall a b. (Typeable a) => Text -> DelayedWithErrorFormatterIO QueryParameter b
failQueryParamRequired paramName =
  delayedFailFatalQuery @a
    . convertString
    $ "Query parameter " <> convertString paramName <> " is required"

failQueryParamParsing :: forall a b. (Typeable a) => Text -> Text -> DelayedWithErrorFormatterIO QueryParameter b
failQueryParamParsing paramName err =
  delayedFailFatalQuery @a
    . convertString
    $ "Error parsing query parameter " <> convertString paramName <> ": " <> convertString err

failQueryParamValueRequired :: forall a b. (Typeable a) => Text -> DelayedWithErrorFormatterIO QueryParameter b
failQueryParamValueRequired paramName =
  delayedFailFatalQuery @a
    . convertString
    $ "Query parameter " <> convertString paramName <> " requires a value"

-- | Parse a query parameter and fail if it is not present or doesn't have a value.
--
-- If your type has `FromHttpApiData` instance, you can use `parseQueryParam` as the `parseValue` argument.
queryParamRequired ::
  forall a.
  (Typeable a) =>
  DecodedQuery ->
  Text ->
  (Text -> Either Text a) ->
  DelayedWithErrorFormatterIO QueryParameter a
queryParamRequired query paramName parseValue = case lookup paramName query of
  Nothing -> failQueryParamRequired @a paramName
  Just Nothing -> failQueryParamValueRequired @a paramName
  Just (Just queryParam) ->
    either (failQueryParamParsing @a paramName) pure
      . parseValue
      $ convertString queryParam

-- | Parse a query parameter and return `Nothing` if it is not present
-- or fail when it is present but doesn't have a value.
--
-- If your type has `FromHttpApiData` instance, you can use `parseQueryParam` as the `parseValue` argument.
queryParamOptional ::
  forall a.
  (Typeable a) =>
  DecodedQuery ->
  Text ->
  (Text -> Either Text a) ->
  DelayedWithErrorFormatterIO QueryParameter (Maybe a)
queryParamOptional query paramName parseValue = case lookup paramName query of
  Nothing -> pure Nothing
  Just Nothing -> failQueryParamValueRequired @a paramName
  Just (Just queryParam) ->
    either (failQueryParamParsing @a paramName) (pure . Just)
      . parseValue
      $ convertString queryParam

-- | A typeclass to convert a boolean value to a flag.
--
-- This is used by `queryFlag` to convert a boolean query parameter to type's a value.
class IsServerFlag a where
  boolToFlag :: Bool -> a

-- | Parse a query parameter as a flag.
--
-- If the query parameter is not present, it defaults to `boolToFlag False`.
-- If the query parameter is present but has no value, it defaults to `boolToFlag True`.
-- Otherwise it parses the value as `Bool` using `parseQueryParam` from `Web.HttpApiData`.
queryFlag :: forall a. (Typeable a, IsServerFlag a) => DecodedQuery -> Text -> DelayedWithErrorFormatterIO QueryParameter a
queryFlag query paramName = case lookup paramName query of
  Nothing -> pure $ boolToFlag False
  Just Nothing -> pure $ boolToFlag True
  Just (Just queryParam) ->
    either (failQueryParamParsing @a paramName) (pure . boolToFlag)
      . parseQueryParam
      $ convertString queryParam
