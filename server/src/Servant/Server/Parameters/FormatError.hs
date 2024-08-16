{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Servant.Server.Parameters.FormatError where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Typeable
import Servant.API.Parameters
import Servant.Server
import Servant.Server.Internal.DelayedIO
import Servant.Server.Parameters.Internal.Delayed

class FormatError f a where
    -- | Helper function to format an error message using the formatters provided by servant.
    formatError :: String -> DelayedWithErrorFormatterIO f ServerError

-- | General implementation of `formatError`.
formatErrorImpl ::
    forall a f.
    (Typeable (f a)) =>
    (ErrorFormatters -> ErrorFormatter) ->
    String ->
    DelayedWithErrorFormatterIO f ServerError
formatErrorImpl selectFormatter err = do
    errorFormatter <- asks selectFormatter
    lift . withRequest $ \req ->
        pure $ errorFormatter (typeRep (Proxy @(f a))) req err

-- Instances

instance (Typeable a) => FormatError QueryParameter a where
    formatError = formatErrorImpl @a @QueryParameter urlParseErrorFormatter
