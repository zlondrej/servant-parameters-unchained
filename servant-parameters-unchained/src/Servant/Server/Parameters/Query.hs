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
import Data.ByteString
import Data.Kind
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
import Servant.Server.Internal.ErrorFormatter
import Servant.Server.Parameters.FormatError
import Servant.Server.Parameters.Internal.Delayed

class IsQueryServerParameter a where
  -- | The type which is passed to the handler.
  --
  -- By default, this is the same as the type class parameter, but can be overridden if needed.
  type QueryServerType a :: Type

  type QueryServerType a = a

  parseQueryParameter :: Query -> DelayedWithErrorFormatterIO QueryParameter (QueryServerType a)

instance
  ( HasServer api context
  , IsQueryServerParameter a
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  ) =>
  HasServer (QueryParameter a :> api) context
  where
  type ServerT (QueryParameter a :> api) m = QueryServerType a -> ServerT api m

  route Proxy context subserver =
    let errorFormatters = getContextEntry (mkContextWithErrorFormatter context)
        parseServerParameter = withQuery (parseQueryParameter @a) errorFormatters
     in route (Proxy @api) context (runDelayedCont parseServerParameter subserver)

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt . s

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

-- | Helper function to format an error message using the formatter for url parsing errors.
delayedFailFatalQuery :: forall a b. (Typeable a) => String -> DelayedWithErrorFormatterIO QueryParameter b
delayedFailFatalQuery = lift . delayedFailFatal <=< formatError @QueryParameter @a

failQueryParamRequired :: forall a b. (Typeable a) => ByteString -> DelayedWithErrorFormatterIO QueryParameter b
failQueryParamRequired paramName =
  delayedFailFatalQuery @a
    . convertString
    $ "Query parameter " <> convertString paramName <> " is required"

failQueryParamParsing :: forall a b. (Typeable a) => ByteString -> Text -> DelayedWithErrorFormatterIO QueryParameter b
failQueryParamParsing paramName err =
  delayedFailFatalQuery @a
    . convertString
    $ "Error parsing query parameter " <> convertString paramName <> ": " <> convertString err

failQueryParamValueRequired :: forall a b. (Typeable a) => ByteString -> DelayedWithErrorFormatterIO QueryParameter b
failQueryParamValueRequired paramName =
  delayedFailFatalQuery @a
    . convertString
    $ "Query parameter " <> convertString paramName <> " requires a value"

queryParamRequired :: forall a. (Typeable a, FromHttpApiData a) => Query -> ByteString -> DelayedWithErrorFormatterIO QueryParameter a
queryParamRequired query paramName = case lookup paramName query of
  Nothing -> failQueryParamRequired @a paramName
  Just Nothing -> failQueryParamValueRequired @a paramName
  Just (Just queryParam) ->
    either (failQueryParamParsing @a paramName) pure
      . parseQueryParam
      $ convertString queryParam

queryParamOptional :: forall a. (Typeable a, FromHttpApiData a) => Query -> ByteString -> DelayedWithErrorFormatterIO QueryParameter (Maybe a)
queryParamOptional query paramName = case lookup paramName query of
  Nothing -> pure Nothing
  Just Nothing -> failQueryParamValueRequired @a paramName
  Just (Just queryParam) ->
    either (failQueryParamParsing @a paramName) pure
      . parseQueryParam
      $ convertString queryParam

{- | A typeclass to convert a boolean value to a flag.

This is used by `queryFlag` to convert a boolean query parameter to type's a value.
-}
class IsServerFlag a where
  boolToFlag :: Bool -> a

{- | Parse a query parameter as a flag.

If the query parameter is not present, it defaults to `boolToFlag False`.
If the query parameter is present but has no value, it defaults to `boolToFlag True`.
Otherwise it parses the value as `Bool` using `parseQueryParam` from `Web.HttpApiData`.
-}
queryFlag :: forall a. (Typeable a, IsServerFlag a) => Query -> ByteString -> DelayedWithErrorFormatterIO QueryParameter a
queryFlag query paramName = case lookup paramName query of
  Nothing -> pure $ boolToFlag False
  Just Nothing -> pure $ boolToFlag True
  Just (Just queryParam) ->
    either (failQueryParamParsing @a paramName) (pure . boolToFlag)
      . parseQueryParam
      $ convertString queryParam
