module Servant.API.Parameters where

import Data.ByteString
import Data.Text
import Data.Typeable

-- | Lookup value(s) in the query.
data QueryParameter a
  deriving (Typeable)

-- | A query name-value pair that's not URL-encoded.
type DecodedQueryItem = (Text, Maybe ByteString)

-- | A list of query name-value pairs that are not URL-encoded.
type DecodedQuery = [DecodedQueryItem]
