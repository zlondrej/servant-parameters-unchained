{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Servant.Server.Parameters.Query.Filters (
  module Servant.Server.Parameters.Query.Filters,
  module Servant.Server.Parameters.Query,
)
where

import Servant.Server.Parameters.Query
import Servant.Server.Parameters.Query.Filters.Internal

-- | Function to reduce filters to arbitrary unified type.
--
-- This can be for example some intermediate representation for
-- your DB library, monadic computation or pure SQL.
--
-- The function takes variadic number of functions parameters.
-- This number is equivalent to the number of supported filters
-- in the `SupportedFilters` type family.
applyFilters ::
  forall output filters.
  (ApplyFilter filters output, Monoid output) =>
  [TypedFilter filters] ->
  FoldApplyFn filters output
applyFilters someFilters = applyFilter @filters @output someFilters mempty
