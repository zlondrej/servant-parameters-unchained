module Servant.API.Parameters where

import Data.Typeable

-- | Lookup value(s) in the query.
data QueryParameter a
  deriving (Typeable)
