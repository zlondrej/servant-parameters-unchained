module Servant.API.Parameter where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.ByteString
import Data.Proxy
import Data.String.Conversions
import Data.Text
import Data.Typeable
import Network.HTTP.Types
import Network.Wai
import Servant.API
import Servant.Server
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.ErrorFormatter

{- | Lookup a value(s) in the request.

This can be header value, query parameter, or even a part of the request body.
However, one parameter can only perform one type of check (query, header, body, ...).
-}
data Parameter a
    deriving (Typeable)

type FormatError = String -> ServerError

newtype DelayedCont env a b = DelayedCont
    { runDelayedCont :: Delayed env (a -> b) -> Delayed env b
    }

type DelayedWithErrorFormatterIO a = ReaderT ErrorFormatters DelayedIO a

withQuery :: (Query -> DelayedWithErrorFormatterIO a) -> ErrorFormatters -> DelayedCont env a b
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

{- | Helper function to format an error message using the formatters
provided by servant.
-}
formatError ::
    forall a.
    (Typeable a) =>
    (ErrorFormatters -> ErrorFormatter) ->
    String ->
    DelayedWithErrorFormatterIO ServerError
formatError selectFormatter err = do
    errorFormatter <- asks selectFormatter
    lift . withRequest $ \req ->
        pure $ errorFormatter (typeRep (Proxy @(Parameter a))) req err

-- | Helper function to format an error message using the formatter for url parsing errors.
delayedFailFatalQuery :: forall a b. (Typeable a) => String -> DelayedWithErrorFormatterIO b
delayedFailFatalQuery = lift . delayedFailFatal <=< formatError @a urlParseErrorFormatter

failQueryParamRequired :: forall a b. (Typeable a) => ByteString -> DelayedWithErrorFormatterIO b
failQueryParamRequired paramName =
    delayedFailFatalQuery @a
        . convertString
        $ "Query parameter " <> convertString paramName <> " is required"

failQueryParamParsing :: forall a b. (Typeable a) => ByteString -> Text -> DelayedWithErrorFormatterIO b
failQueryParamParsing paramName err =
    delayedFailFatalQuery @a
        . convertString
        $ "Error parsing query parameter " <> convertString paramName <> ": " <> convertString err

failQueryParamValueRequired :: forall a b. (Typeable a) => ByteString -> DelayedWithErrorFormatterIO b
failQueryParamValueRequired paramName =
    delayedFailFatalQuery @a
        . convertString
        $ "Query parameter " <> convertString paramName <> " requires a value"

queryParamRequired :: forall a. (Typeable a, FromHttpApiData a) => Query -> ByteString -> DelayedWithErrorFormatterIO a
queryParamRequired query paramName = case lookup paramName query of
    Nothing -> failQueryParamRequired @a paramName
    Just Nothing -> failQueryParamValueRequired @a paramName
    Just (Just queryParam) ->
        either (failQueryParamParsing @a paramName) pure
            . parseQueryParam
            $ convertString queryParam

queryParamOptional :: forall a. (Typeable a, FromHttpApiData a) => Query -> ByteString -> DelayedWithErrorFormatterIO (Maybe a)
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
class IsFlag a where
    boolToFlag :: Bool -> a

{- | Parse a query parameter as a flag.

If the query parameter is not present, it defaults to `boolToFlag False`.
If the query parameter is present but has no value, it defaults to `boolToFlag True`.
Otherwise it parses the value as `Bool` using `parseQueryParam` from `Web.HttpApiData`.
-}
queryFlag :: forall a. (Typeable a, IsFlag a) => Query -> ByteString -> DelayedWithErrorFormatterIO a
queryFlag query paramName = case lookup paramName query of
    Nothing -> pure $ boolToFlag False
    Just Nothing -> pure $ boolToFlag True
    Just (Just queryParam) ->
        either (failQueryParamParsing @a paramName) (pure . boolToFlag)
            . parseQueryParam
            $ convertString queryParam

class IsServerParameter a where
    getServerParameter :: ErrorFormatters -> DelayedCont env a b

instance
    ( HasServer api context
    , IsServerParameter a
    , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
    ) =>
    HasServer (Parameter a :> api) context
    where
    type ServerT (Parameter a :> api) m = a -> ServerT api m

    route Proxy context subserver =
        let errorFormatters = getContextEntry (mkContextWithErrorFormatter context)
         in route (Proxy @api) context (runDelayedCont (getServerParameter @a errorFormatters) subserver)

    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt . s
