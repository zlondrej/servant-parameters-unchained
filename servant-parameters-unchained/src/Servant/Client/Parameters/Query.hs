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

import Data.ByteString
import Data.Foldable as Foldable
import Data.Kind
import Data.Proxy
import Data.Text
import Servant.API
import Servant.API.Parameters
import Servant.Client.Core

class IsQueryClientParameter a where
  -- | The type which is passed to the handler.
  --
  -- By default, this is the same as the type class parameter, but can be overridden if needed.
  type QueryClientType a :: Type

  type QueryClientType a = a

  serializeQueryParameter :: QueryClientType a -> [(Text, Maybe ByteString)]

instance (HasClient m api, IsQueryClientParameter a) => HasClient m (QueryParameter a :> api) where
  type Client m (QueryParameter a :> api) = QueryClientType a -> Client m api

  clientWithRoute pm Proxy req queryParam =
    clientWithRoute pm (Proxy @api)
      . Foldable.foldl mkQuery req
      $ serializeQueryParameter @a queryParam
   where
    mkQuery :: Request -> (Text, Maybe ByteString) -> Request
    mkQuery req' (name, value) = appendToQueryString name value req'

  hoistClientMonad pm _ f cl = \arg ->
    hoistClientMonad pm (Proxy :: Proxy api) f (cl arg)
