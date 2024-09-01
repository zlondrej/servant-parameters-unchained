{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Client.Parameters.Query (
  module Servant.Client.Parameters.Query,
)
where

import Data.Foldable as Foldable
import Data.Kind
import Data.Proxy
import Network.HTTP.Types
import Servant.API
import Servant.API.Parameters
import Servant.Client.Core

class IsQueryClientParameter a where
  -- | The type which is passed to the handler.
  --
  -- By default, this is the same as the type class parameter, but can be overridden if needed.
  type QueryClientType a :: Type

  type QueryClientType a = a

  -- | Serialize the parameter to a list of key-value pairs.
  --
  -- Both the key and the value will be properly URL-encoded.
  serializeQueryParameter :: QueryClientType a -> DecodedQuery

instance (HasClient m api, IsQueryClientParameter a) => HasClient m (QueryParameter a :> api) where
  type Client m (QueryParameter a :> api) = QueryClientType a -> Client m api

  clientWithRoute pm Proxy req queryParam =
    clientWithRoute pm (Proxy @api)
      . Foldable.foldl mkQuery req
      $ serializeQueryParameter @a queryParam
   where
    mkQuery :: Request -> DecodedQueryItem -> Request
    mkQuery req' (name, value) =
      appendToQueryString
        name -- Parameter name should be URL-encoded by servant-client (TODO: Write test)
        (urlEncode True <$> value)
        req'

  hoistClientMonad pm _ f cl arg = hoistClientMonad pm (Proxy :: Proxy api) f (cl arg)
