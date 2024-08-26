{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Servant.API.Parameters.Query.Filters (
  module Servant.API.Parameters.Query.Filters,
  module Servant.API.Parameters.Query.Filters.Internal
)
where

import Servant.API.Parameters.Query.Filters.Internal

-- | Function to reduce filters to arbitrary unified type.
--
-- This can be for example some intermediate representation for
-- your DB library, monadic computation or raw SQL expression.
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
