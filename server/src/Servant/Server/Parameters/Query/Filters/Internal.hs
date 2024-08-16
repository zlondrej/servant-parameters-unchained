-- {-# LANGUAGE GADTs #-}

module Servant.Server.Parameters.Query.Filters.Internal (
    module Servant.Server.Parameters.Query.Filters,
)
where

import Servant.Server.Parameters.Query
import Servant.Server.Parameters.Query.Filters
import Unsafe.Coerce
import Servant.Server.Parameters.TypeLevel

type TypedFilters a = [TypedFilter (SupportedFilterList a)]

-- |
data TypedFilter ts where
  -- TODO: Remove/move show requirement?
  TypedFilter :: (OneOf ts a, Typeable a) => a -> TypedFilter ts
  deriving stock (Typeable)

instance Show (TypedFilter ts) where
  show (TypedFilter a) = show a

class SupportsFilters f where
  type SupportedFilters f :: [Type -> Type]

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
    let matching = map (castTypedFilter @f) someFilters
    in foldMap fn matching <> acc

instance (Typeable f, Monoid output, ApplyFilter (f1 ': fs) output) => ApplyFilter (f ': f1 ': fs) output where
  type FoldApplyFn (f ': f1 ': fs) output = (f -> output) -> FoldApplyFn (f1 ': fs) output
  applyFilter someFilters acc fn =
    let (matching, remainingFilters) = partitionEithers $ map (castTypedFilter @f) someFilters
    in applyFilter @(f1 ': fs) @output remainingFilters (foldMap fn matching <> acc)

-- | Helper class to evaluate individual filters.
--
-- This could be included directly in the `ApplyFilter` class, but because
-- of the type-level issues when casting and removing the types from type list,
-- it was decided to keep it separate.
class CastTypedFilter a (ts :: [Type]) where
  type Casted a ts :: Type
  castTypedFilter :: TypedFilter ts -> Casted a ts

instance Typeable t => CastTypedFilter t '[t] where
  type Casted t '[t] = t
  castTypedFilter (TypedFilter filter) = case cast filter of
    Just a -> a
    Nothing -> unexpectedError "Failed to cast filter of single possible type"

-- | Only allows casting to the first type in the list.
--
-- This is sufficient for the `applyFilter` function.
instance Typeable t => CastTypedFilter t (t : u : vs) where
  type Casted t (t : u : vs) = Either t (TypedFilter (u : vs))
  castTypedFilter f@(TypedFilter filter) = case cast filter of
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
    Nothing -> Right $ unsafeCoerce f

class AllFilterParsers' (filters :: [Type]) (ts :: [Type]) where
  allFilterRecords' :: [SomeFilterParser ts]

instance AllFilterParsers' '[] ts where
  allFilterRecords' = []

instance (IsFilter f, Typeable f, Show f, OneOf ts f, AllFilterParsers' fs ts) => AllFilterParsers' (f ': fs) ts where
  allFilterRecords' = map SomeFilterParser (listFilters @f) <> allFilterRecords' @fs

type AllFilterParsers ts = AllFilterParsers' ts ts

-- | Get all filter records for a list of types.
allFilterRecords :: forall ts. AllFilterParsers ts => [SomeFilterParser ts]
allFilterRecords = allFilterRecords' @ts @ts

-- | Filter record describing a filter behavior.
data FilterParser a where
  FilterParser
    :: { parserMatchKey :: Text -> P.Parser b
       , parserParseValue :: b -> Either Text a
       }
    -> FilterParser a

data SomeFilterParser ts where
  SomeFilterParser :: (OneOf ts a, Typeable a, Show a) => FilterParser a -> SomeFilterParser ts
  deriving stock (Typeable)

class IsFilter a where
  listFilters :: [FilterParser a]


newtype UserTagF = UserTagF Text
  deriving stock (Show)
  deriving newtype (FromHttpApiData)
instance SupportsFilters UserTagF where
  type SupportedFilters UserTagF = '[SubscriptFilter EqFilter, SubscriptFilter ContainsFilter]

newtype AnyF = AnyF Text
  deriving stock (Show)
  deriving newtype (FromHttpApiData)
instance SupportsFilters AnyF where
  type SupportedFilters AnyF = '[ContainsFilter]



instance IsQueryParameter UserFilters where
  parseQueryParameter =
    fmap fold
      . traverse
        ( \(key, mValue) -> do
            case P.parseOnly parseKey $ convertString key of
              Left _ -> pure mempty -- No supported key found
              Right parse -> parse key mValue
        )
    where
      parseKey :: P.Parser (ByteString -> Maybe ByteString -> DelayedWithErrorFormatterIO QueryParameter UserFilters)
      parseKey =
        asum
          [ parseFilters "tag" (\filter -> mempty {tagFilters = [filter]})
          , parseFilters "any" (\filter -> mempty {anyFilters = [filter]})
          ]

      parseFilters
        :: forall ts b
         . AllFilterParsers ts
        => Text
        -> (TypedFilter ts -> b)
        -> P.Parser (ByteString -> Maybe ByteString -> DelayedWithErrorFormatterIO QueryParameter b)
      parseFilters prefix convert = do
        parserMatchKeyPrefix prefix
        asum . map (guardFilterValue . filterToParser @ts convert) $ allFilterRecords' @ts

      guardFilterValue
        :: P.Parser (ByteString -> ByteString -> DelayedWithErrorFormatterIO QueryParameter b)
        -> P.Parser (ByteString -> Maybe ByteString -> DelayedWithErrorFormatterIO QueryParameter b)
      guardFilterValue parser = do
        parserParseValuer <- parser
        pure $ \key -> \case
          Just value -> parserParseValuer key value
          Nothing -> failQueryParamValueRequired @UserFilters key

      filterToParser :: forall ts b. (TypedFilter ts -> b) -> SomeFilterParser ts -> P.Parser (ByteString -> ByteString -> DelayedWithErrorFormatterIO QueryParameter b)
      filterToParser convert (SomeFilterParser filterRecord) = case filterRecord of
        FilterSimple {parserMatchKeySimple, parserParseValueSimple} -> do
          parserMatchKeySimple <* P.endOfInput
          pure $ \key value -> case parserParseValueSimple $ convertString value of
            Left err -> failQueryParamParsing @UserFilters key err
            Right parsed -> pure . convert $ TypedFilter parsed
        FilterCustom {parserMatchKeyCustom, parserParseValueCustom} -> do
          param <- parserMatchKeyCustom <* P.endOfInput
          pure $ \key value -> case parserParseValueCustom param $ convertString value of
            Left err -> failQueryParamParsing @UserFilters key err
            Right parsed -> pure . convert $ TypedFilter parsed
