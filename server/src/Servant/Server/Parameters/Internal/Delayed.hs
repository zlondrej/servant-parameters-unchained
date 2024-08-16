{-# LANGUAGE KindSignatures #-}

module Servant.Server.Parameters.Internal.Delayed where

import Control.Monad.Trans.Reader
import Data.Kind
import Servant.Server
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO

-- import Servant.Server.Parameters.ErrorFormatter

newtype DelayedCont env a b = DelayedCont
    { runDelayedCont :: Delayed env (a -> b) -> Delayed env b
    }

type DelayedWithErrorFormatterIO (f :: Type -> Type) a = ReaderT ErrorFormatters DelayedIO a
