module Servant.API.Parameter where

import Data.ByteString
import Data.Coerce
import Data.Functor
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

{- | Lookup a value in the request.

This can be header value, query parameter, or even a part of the request body.
One parameter can even capture multiple values from multiple sources.
-}
data Parameter a
    deriving (Typeable)

-- Delayed env (a -> b) -> DelayedIO a -> Delayed env b

data DelayedParameterIO env a = DelayedParameterIO
    { runDelayedParameterIO :: Delayed env (a -> b) -> Delayed env b
    }

instance Functor DelayedParameterIO env where
    fmap f (DelayedParameterIO run) = DelayedParameterIO $ \env -> f <$> run env

withQuery :: (Query -> Either Text a) -> DelayedParameterIO env a
withQuery parseParam =
    DelayedParameterIO
        (`addParameterCheck` withRequest (parseParam . queryString))

failQueryParamRequired :: ByteString -> DelayedIO a
failQueryParamRequired paramName =
    delayedFailFatal
        . undefined -- TODONOW: Implement error formatters
        . convertString
        $ "Query parameter " <> paramName <> " is required"

failQueryParamParsing :: ByteString -> Text -> Text
failQueryParamParsing paramName err =
    convertString $
        "Error parsing query parameter " <> paramName <> ": " <> convertString err

queryParamRequired :: (FromHttpApiData a) => ByteString -> DelayedParameterIO env a
queryParamRequired paramName = withQuery $ \query -> case lookup paramName query of
    Nothing -> Left failQueryParamRequired
    Just queryParam -> left failQueryParamParsing $ parseQueryParam queryParam

queryParamOptional :: (FromHttpApiData a) => ByteString -> DelayedParameterIO env (Maybe a)
queryParamOptional paramName = withQuery $ \query -> case lookup paramName query of
    Nothing -> Right Nothing
    Just queryParam -> left failQueryParamParsing $ parseQueryParam queryParam

queryFlag :: (Coercible Bool a) => ByteString -> a -> DelayedParameterIO env (Maybe a)
queryFlag paramName def = maybe def coerce <$> queryParamOptional paramName

class IsParameter a where
    getParameter :: DelayedParameterIO env a

instance
    ( HasServer api context
    , IsParameter a
    -- TODONOW: Incorporate error formatters?
    -- , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
    ) =>
    HasServer (Parameter a :> api) context
    where
    type ServerT (Parameter a :> api) m = a -> ServerT api m

    route Proxy context subserver =
        -- let formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)
        --     rep = typeRep (Proxy :: Proxy Parameter)
        route (Proxy @api) context (runDelayedParameterIO getParameter subserver)

    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt . s
