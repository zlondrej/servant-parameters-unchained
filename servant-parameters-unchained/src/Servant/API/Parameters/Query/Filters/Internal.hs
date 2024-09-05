{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.Parameters.Query.Filters.Internal (
  module Servant.API.Parameters.Query.Filters.Internal,
  module Servant.API.Parameters.Internal.TypeLevel,
)
where

import Data.Attoparsec.Text
import Data.ByteString
import Data.Either
import Data.Kind
import Data.List as List
import Data.String.Conversions
import Data.Text
import Data.Typeable
import GHC.TypeError
import Servant.API.Parameters
import Servant.API.Parameters.Internal.TypeLevel
import Unsafe.Coerce

type TypedFilters a = [TypedFilter (SupportedFilterList a)]

data TypedFilter ts where
  TypedFilter :: (Unique ts, OneOf ts a, Typeable a) => a -> TypedFilter ts
  deriving stock (Typeable)

infixr 0 />

-- | Helper constructor for `TypedFilter`.
(/>) :: (Typeable f, Unique fs, OneOf fs f) => (p -> f) -> p -> TypedFilter fs
filterCons /> value = TypedFilter $ filterCons value

instance (Typeable t, Show t) => Show (TypedFilter '[t]) where
  show a = show $ castTypedFilter @t a

instance (Typeable t, Show t, Show (TypedFilter (t1 : ts))) => Show (TypedFilter (t : t1 : ts)) where
  show a = case castTypedFilter @t a of
    Left a' -> show a'
    Right a' -> show a'

instance (Typeable t, Eq t) => Eq (TypedFilter '[t]) where
  a == b = castTypedFilter @t a == castTypedFilter @t b

instance (Typeable t, Eq t, Eq (TypedFilter (t1 : ts))) => Eq (TypedFilter (t : t1 : ts)) where
  a == b = case (castTypedFilter @t a, castTypedFilter @t b) of
    (Left a', Left b') -> a' == b'
    (Right a', Right b') -> a' == b'
    _ -> False

class SupportsFilters t where
  type SupportedFilters t :: [Type -> Type]

type SupportedFilterList t = Apply (SupportedFilters t) t

-- | Type class to reduce filters to arbitrary unified type.
--
-- This can be for example some intermediate representation for
-- your DB library, monadic computation or pure SQL.
class ApplyFilter (filters :: [Type]) output where
  type FoldApplyFn filters output :: Type
  applyFilter :: [TypedFilter filters] -> output -> FoldApplyFn filters output

instance ApplyFilter '[] output where
  type FoldApplyFn '[] output = TypeError ('Text "Empty filter is not supported")
  applyFilter = undefined

instance (Typeable f, Monoid output) => ApplyFilter '[f] output where
  type FoldApplyFn '[f] output = (f -> output) -> output
  applyFilter someFilters acc fn =
    let matching = List.map (castTypedFilter @f) someFilters
     in foldMap fn matching <> acc

instance (Typeable f, Monoid output, ApplyFilter (f1 : fs) output) => ApplyFilter (f : f1 : fs) output where
  type FoldApplyFn (f : f1 : fs) output = (f -> output) -> FoldApplyFn (f1 : fs) output
  applyFilter someFilters acc fn =
    let (matching, remainingFilters) = partitionEithers $ List.map (castTypedFilter @f) someFilters
     in applyFilter @(f1 : fs) @output remainingFilters (foldMap fn matching <> acc)

-- | Helper class to evaluate individual filters.
--
-- This could be included directly in the `ApplyFilter` class, but because
-- of the type-level issues when casting and removing the types from type list,
-- it was decided to keep it separate.
class CastTypedFilter a (ts :: [Type]) where
  type Casted a ts :: Type
  castTypedFilter :: TypedFilter ts -> Casted a ts

instance (Typeable t) => CastTypedFilter t '[t] where
  type Casted t '[t] = t
  castTypedFilter (TypedFilter f) = case cast f of
    Just a -> a
    -- This should never happen as the filter can only be constructed
    -- with a value of the type in the list, and so it should always cast.
    Nothing -> error "Failed to cast filter of single possible type"

-- | Only allows casting to the first type in the list.
--
-- This is sufficient for the `applyFilter` function.
instance (Typeable t) => CastTypedFilter t (t : u : vs) where
  type Casted t (t : u : vs) = Either t (TypedFilter (u : vs))
  castTypedFilter tf@(TypedFilter f) = case cast f of
    Just a -> Left a
    -- `unsafeCoerce` should be safe here as the `TypedFilter` can only be
    -- constructed with a value of one of the types in the list.
    -- Since we are trying to cast the filter to the first type in the list,
    -- then failure to cast must mean that the filter is one of the remaining types.
    --
    -- However as I'm unable to pass the internal type of the `TypedFilter` to the
    -- compiler, the type checker is unable to satisfy the `OneOf` constraint:
    --
    -- Could not deduce (OneOf' (u : vs) (u : vs) a ~ a)
    --     arising from a use of ‘TypedFilter’
    --   from the context: (OneOf (t : u : vs) a, Typeable a, Show a)
    --     bound by a pattern with constructor:
    --                TypedFilter :: forall (ts :: [*]) a.
    --                               (OneOf ts a, Typeable a, Show a) =>
    --                               a -> TypedFilter ts,
    --              in an equation for ‘castTypedFilter’
    --     at backend/api-v3/src/API/V3/Server/User.hs:283:22-39
    --   ‘a’ is a rigid type variable bound by
    --     a pattern with constructor:
    --       TypedFilter :: forall (ts :: [*]) a.
    --                      (OneOf ts a, Typeable a, Show a) =>
    --                      a -> TypedFilter ts,
    --     in an equation for ‘castTypedFilter’
    --     at backend/api-v3/src/API/V3/Server/User.hs:283:22-39
    --
    -- In case you figure out how to do this without using `unsafeCoerce`, please let me know.
    -- I'm guessing it's doable by fully unwrapping the `TypedFilter` and then
    -- reconstructing it, but that's way too much work and probably inneficient too.
    --
    --   @zlondrej
    Nothing -> Right $ unsafeCoerce tf

-- Server-side

class IsServerFilter a where
  filterParsers :: [FilterParser a]

-- | Filter record describing a filter behavior.
data FilterParser a where
  FilterParser ::
    -- TODO: Replace Parse with something that can distinguish between
    -- soft (try another filter) and hard (stop trying and report error immediately) errors.
    -- Currently the failed parser is treated as a soft error.
    { parserMatchKey :: Text -> Parser b
    , parserParseValue :: b -> ByteString -> Either Text a
    } ->
    FilterParser a

data SomeFilterParser ts where
  SomeFilterParser :: (OneOf ts a, Typeable a, Show a) => FilterParser a -> SomeFilterParser ts
  deriving stock (Typeable)

class AllFilterParsers' (filters :: [Type]) (ts :: [Type]) where
  allFilterParsers' :: [SomeFilterParser ts]

instance AllFilterParsers' '[] ts where
  allFilterParsers' = []

instance (IsServerFilter f, Typeable f, Show f, OneOf ts f, AllFilterParsers' fs ts) => AllFilterParsers' (f : fs) ts where
  allFilterParsers' = List.map SomeFilterParser (filterParsers @f) <> allFilterParsers' @fs

type AllFilterParsers ts = AllFilterParsers' ts ts

-- | Get all filter records for a list of types.
allFilterParsers :: forall ts. (AllFilterParsers ts) => [SomeFilterParser ts]
allFilterParsers = allFilterParsers' @ts @ts

-- Client-side

class IsClientFilter a where
  filterSerializer :: Text -> a -> (Text, Text)

-- | A helper class to serialize filters to query client-side parameters.
class SerializeTypedFilter (ts :: [Type]) where
  serializeTypedFilter :: Text -> TypedFilter ts -> DecodedQueryItem

instance (IsClientFilter t, Typeable t) => SerializeTypedFilter '[t] where
  serializeTypedFilter prefix = textTupleToQueryItem . filterSerializer prefix . castTypedFilter @t

instance
  (IsClientFilter t, Typeable t, SerializeTypedFilter (t1 : ts)) =>
  SerializeTypedFilter (t : t1 : ts)
  where
  serializeTypedFilter prefix typedFilter = case castTypedFilter @t typedFilter of
    Left a -> textTupleToQueryItem $ filterSerializer prefix a
    Right a -> serializeTypedFilter prefix a

textTupleToQueryItem :: (Text, Text) -> DecodedQueryItem
textTupleToQueryItem (k, v) = (k, Just $ convertString v)
