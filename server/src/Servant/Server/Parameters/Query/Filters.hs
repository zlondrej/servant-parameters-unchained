module Servant.Server.Parameters.Query.Filters (
    module Servant.Server.Parameters.Query.Filters,
    module Servant.Server.Parameters.Query,
)
where

import Servant.Server.Parameters.Query
import Servant.Server.Parameters.TypeLevel
import Unsafe.Coerce

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

{- | Function to reduce filters to arbitrary unified type.

This can be for example some intermediate representation for
your DB library, monadic computation or pure SQL.

The function takes variadic number of functions parameters.
This number is equivalent to the number of supported filters
in the `SupportedFilters` type family.
-}
applyFilters ::
    forall output filters.
    (ApplyFilter filters output, Monoid output) =>
    [TypedFilter filters] ->
    FoldApplyFn filters output
applyFilters someFilters = applyFilter @filters @output someFilters mempty

-- | Implementation of `AllFilterRecords`.
class AllFilterRecords' (filters :: [Type]) (ts :: [Type]) where
    allFilterRecords' :: [SomeFilterRecord ts]

instance AllFilterRecords' '[] ts where
    allFilterRecords' = []

instance (IsFilter f, Typeable f, Show f, OneOf ts f, AllFilterRecords' fs ts) => AllFilterRecords' (f ': fs) ts where
    allFilterRecords' = map SomeFilterRecord (listFilters @f) <> allFilterRecords' @fs

-- | Type class to collect all filter records.
type AllFilterRecords ts = AllFilterRecords' ts ts

allFilterRecords :: forall ts. (AllFilterRecords ts) => [SomeFilterRecord ts]
allFilterRecords = allFilterRecords' @ts @ts

class IsFilter a where
    listFilters :: [FilterRecord a]

valueFilter :: (FromHttpApiData a) => (a -> b) -> Text -> Either Text b
valueFilter f = (f <$>) . parseQueryParam

listFilter :: (FromHttpApiData a) => ([a] -> b) -> Text -> Either Text b
listFilter f = (f <$>) . parseQueryParamList

parseQueryParamList :: (FromHttpApiData a) => Text -> Either Text [a]
parseQueryParamList input = case P.parseOnly (parserList P.<?> "value list") input of
    Left err -> Left $ convertString err
    Right values -> traverse parseQueryParam values
  where
    -- TODONOW: Verify escaping and error messages
    parserList = do
        void $ P.char '['
        values <- escapedString ",]" `P.sepBy'` P.char ','
        void $ P.char ']'
        P.endOfInput
        pure values

instance (FromHttpApiData a) => IsFilter (EqFilter a) where
    listFilters =
        [ FilterSimple
            { operatorMatchSimple = matchOp "eq"
            , parserParseValueSimple = valueFilter Eq
            }
        , FilterSimple
            { operatorMatchSimple = matchOp "in"
            , parserParseValueSimple = listFilter In
            }
        , FilterSimple
            { operatorMatchSimple = matchOp "neq"
            , parserParseValueSimple = valueFilter NotEq
            }
        ]

instance (FromHttpApiData a) => IsFilter (ContainsFilter a) where
    listFilters =
        [ FilterSimple
            { operatorMatchSimple = matchOp "contains"
            , parserParseValueSimple = valueFilter Contains
            }
        , FilterSimple
            { operatorMatchSimple = matchOp "icontains"
            , parserParseValueSimple = valueFilter ContainsCaseInsentitive
            }
        ]

instance (FromHttpApiData a, IsFilter (f a)) => IsFilter (SubscriptFilter f a) where
    listFilters =
        map
            ( \case
                FilterSimple{operatorMatchSimple, parserParseValueSimple} ->
                    FilterCustom
                        { operatorMatchCustom = operatorMatchSimple *> parseSubscript
                        , parserParseValueCustom = \subscript input -> SubscriptFilter subscript <$> parserParseValueSimple input
                        }
                FilterCustom{operatorMatchCustom, parserParseValueCustom} ->
                    FilterCustom
                        { operatorMatchCustom =
                            operatorMatchCustom
                                >>= \param -> (param,) <$> parseSubscript
                        , parserParseValueCustom = \(param, subscript) text ->
                            SubscriptFilter subscript <$> parserParseValueCustom param text
                        }
            )
            (listFilters @(f a))

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

matchOp :: Text -> P.Parser ()
matchOp op =
    P.peekChar >>= \case
        Just '_' -> parseUnderscore P.<?> "_operator"
        Just '[' -> parseBracket P.<?> "[operator]"
        _ -> fail "missing operator"
  where
    parseUnderscore :: P.Parser ()
    parseUnderscore = do
        void $ P.char '_'
        void $ P.string op

    parseBracket :: P.Parser ()
    parseBracket = do
        void $ P.char '['
        void $ P.string op
        void $ P.char ']'

parseSubscript :: P.Parser Text
parseSubscript = key P.<?> "subscript"
  where
    key = do
        void $ P.char '['
        value <- escapedString "]"
        void $ P.char ']'
        pure $ convertString value

{- | Scans a string, decoding escape sequences for selected characters.

Escape character is backslash and it has to be escaped itself.
-}
escapedString :: [Char] -> P.Parser Text
escapedString escChars' = convertString <$> (escaped P.<?> "escaped string")
  where
    escaped =
        P.many' $
            asum
                [ (P.char '\\' *> P.satisfy (`elem` escChars)) P.<?> "escaped character"
                , P.satisfy (`notElem` escChars) P.<?> "non-terminating character"
                ]

    escChars = '\\' : escChars'

parserMatchKeyPrefix :: Text -> P.Parser ()
parserMatchKeyPrefix prefix = void (P.string prefix P.<?> "prefix: " <> convertString prefix)

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
                [ parseFilters "tag" (\filter -> mempty{tagFilters = [filter]})
                , parseFilters "any" (\filter -> mempty{anyFilters = [filter]})
                ]

        parseFilters ::
            forall ts b.
            (AllFilterRecords ts) =>
            Text ->
            (TypedFilter ts -> b) ->
            P.Parser (ByteString -> Maybe ByteString -> DelayedWithErrorFormatterIO QueryParameter b)
        parseFilters prefix convert = do
            parserMatchKeyPrefix prefix
            asum . map (guardFilterValue . filterToParser @ts convert) $ allFilterRecords' @ts

        guardFilterValue ::
            P.Parser (ByteString -> ByteString -> DelayedWithErrorFormatterIO QueryParameter b) ->
            P.Parser (ByteString -> Maybe ByteString -> DelayedWithErrorFormatterIO QueryParameter b)
        guardFilterValue parser = do
            parserParseValuer <- parser
            pure $ \key -> \case
                Just value -> parserParseValuer key value
                Nothing -> failQueryParamValueRequired @UserFilters key

        filterToParser :: forall ts b. (TypedFilter ts -> b) -> SomeFilterRecord ts -> P.Parser (ByteString -> ByteString -> DelayedWithErrorFormatterIO QueryParameter b)
        filterToParser convert (SomeFilterRecord filterRecord) = case filterRecord of
            FilterSimple{operatorMatchSimple, parserParseValueSimple} -> do
                operatorMatchSimple <* P.endOfInput
                pure $ \key value -> case parserParseValueSimple $ convertString value of
                    Left err -> failQueryParamParsing @UserFilters key err
                    Right parsed -> pure . convert $ TypedFilter parsed
            FilterCustom{operatorMatchCustom, parserParseValueCustom} -> do
                param <- operatorMatchCustom <* P.endOfInput
                pure $ \key value -> case parserParseValueCustom param $ convertString value of
                    Left err -> failQueryParamParsing @UserFilters key err
                    Right parsed -> pure . convert $ TypedFilter parsed
