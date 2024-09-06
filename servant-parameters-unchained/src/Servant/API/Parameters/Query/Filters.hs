{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Servant.API.Parameters.Query.Filters (
  module Servant.API.Parameters.Query.Filters,
  module Servant.API.Parameters.Query.Filters.Internal,
)
where

import Servant.API.Parameters.Query.Filters.Internal

-- | Function to reduce list of filters to arbitrary unified `Monoid` type.
--
-- This can be for example some intermediate representation for
-- your DB library, monadic computation or raw SQL expression.
--
-- The function takes variadic number of functions parameters.
-- This number is equivalent to the number of supported filters
-- in the `SupportedFilters` type family.
foldUnifyTypedFilters ::
  forall output filters f.
  ( UnifyTypedFilter filters output
  , Foldable f
  , Monoid (UnifyTypedFilterFn filters output)
  ) =>
  f (TypedFilter filters) ->
  UnifyTypedFilterFn filters output
foldUnifyTypedFilters = foldMap (unifyTypedFilter @filters @output)
