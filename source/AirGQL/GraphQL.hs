{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Replace case with maybe" #-}

module AirGQL.GraphQL (
  getDerivedSchema,
  queryType,
  sqlDataToGQLValue,
  gqlValueToSQLData,
)
where

import Protolude (
  Applicative (pure),
  Bool (False, True),
  Double,
  Either (Left, Right),
  Eq ((==)),
  IO,
  Int,
  Integer,
  Maybe (Just, Nothing),
  MonadIO (liftIO),
  MonadReader (ask),
  Monoid (mempty),
  ReaderT,
  Semigroup ((<>)),
  Text,
  fromIntegral,
  notElem,
  otherwise,
  show,
  when,
  ($),
  (&),
  (&&),
  (.),
  (<$>),
  (<&>),
  (<=),
  (>),
  (>=),
 )
import Protolude qualified as P

import Control.Exception (throw)
import Control.Monad.Catch (catchAll)
import Data.Aeson (object, (.=))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (nub)
import Data.Ord (Ord (min))
import Data.Text (intercalate, isInfixOf, pack, toUpper)
import Data.Text qualified as T
import Database.SQLite.Simple (
  Connection,
  Query (Query),
  SQLData (SQLBlob, SQLFloat, SQLInteger, SQLNull, SQLText),
  changes,
  execute_,
  query,
  query_,
 )
import Database.SQLite.Simple qualified as SS
import DoubleXEncoding (doubleXDecode, doubleXEncodeGql)
import GHC.IO.Exception (userError)
import Language.GraphQL.AST.Document (Name)
import Language.GraphQL.Error (ResolverException (ResolverException))
import Language.GraphQL.Type as GQL (
  Arguments (Arguments),
  Resolver (EventStreamResolver, ValueResolver),
  Schema,
  Value (Boolean, Enum, Float, Int, List, Null, Object, String),
  schema,
 )
import Language.GraphQL.Type.Out qualified as Out
import Numeric (showFFloat)

import AirGQL.Config (
  maxGraphqlResultCount,
 )

import AirGQL.Introspection qualified as Introspection
import AirGQL.Introspection.NamingConflict (encodeOutsidePKNames)
import AirGQL.Introspection.Resolver qualified as Introspection
import AirGQL.Introspection.Types qualified as Introspection
import AirGQL.Lib (
  AccessMode,
  ColumnEntry (column_name, datatype),
  ObjectType (Table),
  TableEntry (columns, name, object_type),
  canRead,
  canWrite,
  column_name_gql,
 )
import AirGQL.Types.OutObjectType (
  OutObjectType (OutObjectType, descriptionMb, fields, interfaceTypes, name),
  outObjectTypeToObjectType,
 )
import AirGQL.Types.PragmaConf (getSQLitePragmas)
import AirGQL.Types.SchemaConf (
  SchemaConf (accessMode, maxRowsPerTable, pragmaConf),
 )
import AirGQL.Types.Utils (encodeToText)
import AirGQL.Utils (colToFileUrl, collectErrorList, quoteKeyword, quoteText)
import Data.Either.Extra qualified as Either
import Data.List qualified as List
import Language.GraphQL.Class (FromGraphQL (fromGraphQL))


-- | Prevent numbers of being shown with exponents (0.01 instead of 1e-2)
showFullPrecision :: Double -> Text
showFullPrecision x =
  pack $ showFFloat Nothing x ""


showGqlValue :: Value -> Text
showGqlValue = \case
  String str -> str
  Int integer -> show integer
  Float double -> showFullPrecision double
  Boolean bool -> show bool
  Enum text -> text
  List list -> "[" <> T.intercalate ", " (list <&> showGqlValue) <> "]"
  Object obj -> show $ Object obj
  Null -> "null"


gqlValueToSQLText :: Value -> Text
gqlValueToSQLText = \case
  String str -> quoteText str
  Int integer -> show integer
  Float double -> showFullPrecision double
  Boolean bool -> T.toUpper $ show bool
  Enum text -> text
  List list ->
    quoteText $
      "[" <> T.intercalate ", " (list <&> showGqlValue) <> "]"
  Object obj -> quoteText $ show $ Object obj
  Null -> "NULL"


-- TODO: Add Support for GraphQL's type "ID"

-- | Convert any GraphQL value to a nullable String
gqlValueToNullableString :: Value -> Value
gqlValueToNullableString value =
  case value of
    String text -> String text
    Null -> Null
    val -> String $ showGqlValue val


buildSortClause :: [ColumnEntry] -> [(Name, Value)] -> Text
buildSortClause columnEntries orderElems =
  if P.null orderElems
    then
      if "rowid" `P.elem` (columnEntries <&> T.toLower . AirGQL.Lib.column_name)
        then "ORDER BY rowid ASC"
        else ""
    else
      "ORDER BY "
        <> ( orderElems
              <&> ( \(name, value) ->
                      ( name
                      , case value of
                          Enum "ASC" -> "ASC"
                          Enum "asc" -> "ASC"
                          Enum "DESC" -> "DESC"
                          Enum "desc" -> "DESC"
                          _ -> ""
                      )
                  )
              <&> (\(name, order) -> name <> " " <> order)
              & T.intercalate ", "
           )


data Pagination = Pagination
  { limit :: Int
  , offset :: Maybe Int
  }


buildPaginationClause :: Maybe Pagination -> Text
buildPaginationClause = \case
  Nothing -> ""
  Just pagination ->
    P.fold
      [ "LIMIT "
      , show (min pagination.limit maxGraphqlResultCount)
      , case pagination.offset of
          Nothing -> ""
          Just offset -> "\nOFFSET " <> show offset
      ]


getColNamesQuoted :: [ColumnEntry] -> [Text]
getColNamesQuoted columnEntries =
  columnEntries
    <&> ( \col ->
            ( if "BLOB" `T.isPrefixOf` col.datatype
                then
                  "IIF("
                    <> quoteKeyword col.column_name
                    <> " IS NOT NULL, rowid, NULL)"
                    <> " AS "
                    <> quoteKeyword col.column_name
                else quoteKeyword col.column_name
            )
        )


opAndValToSql :: HashMap.HashMap Text Value -> [Text]
opAndValToSql operatorAndValue =
  case HashMap.toList operatorAndValue of
    [("eq", value)] ->
      pure $
        if value == Null
          then " IS NULL"
          else " == " <> gqlValueToSQLText value
    [("neq", value)] ->
      if value == Null
        then pure " IS NOT NULL"
        else
          [ " != " <> gqlValueToSQLText value
          , " IS NULL"
          ]
    [("in", List values)] ->
      let listValues = values <&> gqlValueToSQLText & intercalate ","
      in  [" IN (" <> listValues <> ")"]
    [("nin", List values)] ->
      let listValues = values <&> gqlValueToSQLText & intercalate ","
      in  [" NOT IN (" <> listValues <> ")"]
            <> if P.elem Null values
              then []
              else [" IS NULL"]
    [("gt", value)] -> [" > " <> gqlValueToSQLText value]
    [("gte", value)] -> [" >= " <> gqlValueToSQLText value]
    [("lt", value)] -> [" < " <> gqlValueToSQLText value]
    [("lte", value)] -> [" <= " <> gqlValueToSQLText value]
    [("like", value)] -> [" like " <> gqlValueToSQLText value]
    [("ilike", value)] -> [" like " <> gqlValueToSQLText value]
    filter -> do
      throw $
        userError $
          "Error: Filter "
            <> show filter
            <> " is not yet supported"


getWhereClause :: [(Text, Value)] -> Text
getWhereClause filterElements =
  if P.null filterElements
    then " "
    else
      "WHERE "
        <> ( filterElements
              <&> ( \(colName, x) -> case x of
                      Object operatorAndValue ->
                        let orClauses =
                              opAndValToSql operatorAndValue
                                <&> (colName <>)
                                & intercalate " OR "
                        in  "(" <> orClauses <> ")"
                      _ -> ""
                  )
              & intercalate " AND "
           )


-- The gql lib does not offer a way to know what properties have
-- been requested at the moment, so we always return every column
getReturningClause :: TableEntry -> Text
getReturningClause table =
  "RETURNING "
    <> ( table.columns
          <&> column_name
          <&> quoteKeyword
          & intercalate ", "
       )


-- | Converts an argument map of (pk, value) pairs into a list of filters
getByPKFilterElements :: HashMap Text Value -> [(Text, Value)]
getByPKFilterElements args = do
  (key, value) <- HashMap.toList args
  pure (key, Object $ HashMap.singleton "eq" value)


setCaseInsensitive :: Connection -> [(Text, Value)] -> IO ()
setCaseInsensitive connection filterElements = do
  when
    ( filterElements
        & P.any
          ( \(_, value) -> case value of
              Object operatorAndValue ->
                case HashMap.toList operatorAndValue of
                  [("ilike", _)] -> True
                  _ -> False
              _ -> False
          )
    )
    $ do
      execute_ connection "PRAGMA case_sensitive_like = False"


executeSqlQuery
  :: Connection
  -> Text
  -> [ColumnEntry]
  -> [(Text, Value)]
  -> [(Text, Value)]
  -> Maybe Pagination
  -> IO [[SQLData]]
executeSqlQuery
  connection
  tableName
  colEntries
  filterElems
  orderElems
  paginationMb = do
    let
      sqlQuery :: Query
      sqlQuery =
        Query $
          "SELECT "
            <> intercalate ", " (getColNamesQuoted colEntries)
            <> "\n"
            <> "FROM "
            <> quoteKeyword tableName
            <> "\n"
            <> getWhereClause filterElems
            <> "\n"
            <> buildSortClause colEntries orderElems
            <> "\n"
            <> buildPaginationClause paginationMb

    setCaseInsensitive connection filterElems

    liftIO $ query_ connection sqlQuery


-- | WARNING: Also change duplicate `sqlDataToAesonValue`
sqlDataToGQLValue :: Text -> SQLData -> Either Text Value
sqlDataToGQLValue datatype sqlData = case (datatype, sqlData) of
  (_, SQLInteger int64) ->
    if isInfixOf "BOOL" $ toUpper datatype
      then pure $ case int64 of
        0 -> Boolean False
        _ -> Boolean True
      else
        if int64 >= fromIntegral (P.minBound :: P.Int32)
          && int64 <= fromIntegral (P.maxBound :: P.Int32)
          then pure $ Int $ fromIntegral int64 -- Int32
          else
            Left $
              "Integer "
                <> show int64
                <> " would overflow. "
                <> "This happens because SQLite uses 64-bit ints, "
                <> "but GraphQL uses 32-bit ints. "
                <> "Use a Number (64-bit float) or Text column instead."
  (_, SQLFloat double) -> pure $ Float double
  (_, SQLText text) -> pure $ String text
  (_, SQLBlob _) -> Left "Can't encode BLOB as a GraphQL value"
  (_, SQLNull) -> pure Null


{-| Convert a GraphQL `Value` to a `SQLData`
TODO: ? -> SQLBlob $ string
-}
gqlValueToSQLData :: Value -> SQLData
gqlValueToSQLData = \case
  Int int32 -> SQLInteger $ fromIntegral int32 -- Int64
  Float double -> SQLFloat double
  String text -> SQLText text
  Null -> SQLNull
  Boolean aBool ->
    if aBool
      then SQLInteger 1
      else SQLInteger 0
  Enum name -> SQLText name
  List aList -> SQLText $ show aList
  Object obj -> SQLText $ show obj


rowToGraphQL :: Text -> TableEntry -> [SQLData] -> Either [(Text, Text)] Value
rowToGraphQL dbId table row =
  let
    buildMetadataJson :: Text -> Text -> Text
    buildMetadataJson colName rowid =
      object ["url" .= colToFileUrl dbId table.name colName rowid]
        & encodeToText

    parseSqlData :: (ColumnEntry, SQLData) -> Either (Text, Text) (Text, Value)
    parseSqlData (colEntry, colVal) =
      if "BLOB" `T.isPrefixOf` colEntry.datatype
        then
          pure
            ( colEntry.column_name_gql
            , case colVal of
                SQLNull -> Null
                SQLInteger id ->
                  String $
                    buildMetadataJson colEntry.column_name (show id)
                SQLText id ->
                  String $
                    buildMetadataJson colEntry.column_name id
                _ -> Null
            )
        else case sqlDataToGQLValue colEntry.datatype colVal of
          Left err ->
            Left
              (colEntry.column_name_gql, err)
          Right gqlData ->
            Right
              ( colEntry.column_name_gql
              , case colEntry.datatype of
                  -- Coerce value to nullable String
                  -- if no datatype is set.
                  -- This happens for columns in views.
                  "" -> gqlValueToNullableString gqlData
                  _ -> gqlData
              )
  in
    -- => [(ColumnEntry, SQLData)]
    P.zip table.columns row
      -- => [Either (Text, Text) (Text, Value)]
      <&> parseSqlData
      -- => Either [(Text, Text)] (Text, Value)
      & collectErrorList
      -- => Either [(Text, Text)] (HashMap Text Value)
      <&> HashMap.fromList
      -- => Either [(Text, Text)] Value
      <&> Object


rowsToGraphQL
  :: Text
  -> TableEntry
  -> [[SQLData]]
  -> Either [(Text, Text)] Value
rowsToGraphQL dbId table updatedRows =
  updatedRows
    -- => [Either [(Text, Text)] Value]
    <&> rowToGraphQL dbId table
    -- => Either [[(Text, Text)]] [Value]
    & collectErrorList
    -- => Either [(Text, Text)] [Value]
    & Either.mapLeft P.join
    -- => Either [(Text, Text)] Value
    <&> List


-- | Formats errors from `row(s)ToGraphQL` and throws them.
colErrorsToUserError :: forall m a. (MonadIO m) => Either [(Text, Text)] a -> m a
colErrorsToUserError = \case
  Right v -> pure v
  Left errors ->
    let
      errorLines =
        errors
          <&> \(column, err) -> "On column " <> show column <> ": " <> err
    in
      P.throwIO $
        userError $
          T.unpack $
            "Multiple errors occurred:\n" <> P.unlines errorLines


tryGetArg
  :: forall m a
   . (FromGraphQL a)
  => (MonadIO m)
  => Text
  -> HashMap Text Value
  -> m (Maybe a)
tryGetArg name args = do
  case HashMap.lookup name args of
    Nothing -> pure Nothing
    Just value ->
      case fromGraphQL value of
        Just decoded -> pure $ Just decoded
        Nothing ->
          P.throwIO $
            userError $
              "Argument " <> T.unpack name <> " has invalid format"


getArg
  :: forall m a
   . (FromGraphQL a)
  => (MonadIO m)
  => Text
  -> HashMap Text Value
  -> m a
getArg name args = do
  result <- tryGetArg name args
  case result of
    Just value -> pure value
    Nothing ->
      P.throwIO $
        userError $
          "Argument " <> T.unpack name <> " not found"


getArgWithDefault
  :: forall m a
   . (FromGraphQL a)
  => (MonadIO m)
  => Text
  -> HashMap Text Value
  -> a
  -> m a
getArgWithDefault name args def =
  tryGetArg name args <&> P.fromMaybe def


executeUpdateMutation
  :: Connection
  -> TableEntry
  -> HashMap Text Value
  -> [(Text, Value)]
  -> IO (Int, [[SQLData]])
executeUpdateMutation connection table pairsToSet filterElements = do
  let
    columnsToSet :: [(ColumnEntry, Value)]
    columnsToSet =
      table.columns
        & P.mapMaybe
          ( \col -> case HashMap.lookup col.column_name_gql pairsToSet of
              Just value -> Just (col, value)
              _ -> Nothing
          )

    columnsToSetText :: Text
    columnsToSetText =
      columnsToSet
        <&> (\(col, _) -> quoteKeyword col.column_name <> " = ?")
        & intercalate ", "

  updatedRows :: [[SQLData]] <-
    if P.null columnsToSet
      then pure []
      else liftIO $ do
        let
          sqlQuery =
            Query $
              "UPDATE "
                <> quoteKeyword table.name
                <> "\n"
                <> "SET "
                <> columnsToSetText
                <> "\n"
                <> getWhereClause filterElements
                <> "\n"
                <> getReturningClause table

          valuesToSetNorm =
            columnsToSet
              <&> \(col, gqlValue) -> do
                let sqlValue = gqlValueToSQLData gqlValue
                if (sqlValue == SQLText "{}")
                  P.&& ("BLOB" `T.isPrefixOf` T.toUpper col.datatype)
                  then SQLBlob ""
                  else sqlValue

        setCaseInsensitive connection filterElements
        query connection sqlQuery valuesToSetNorm

  liftIO $
    changes connection
      & P.fmap (,updatedRows)


{-| Ties custom resolver logic to a pre-existing field type. The resolver is
wrapped such that exceptions are caught and converted to the appropriate type
expected by the GQL query executor.
-}
makeResolver
  :: Introspection.Field
  -> Out.Resolve IO
  -> IO (Text, Out.Resolver IO)
makeResolver field resolve = do
  case Introspection.makeField field of
    Left err -> P.throwIO $ userError $ T.unpack err
    Right outField ->
      pure
        ( field.name
        , ValueResolver
            outField
            $ catchAll
              resolve
              (throw . ResolverException)
        )


queryType
  :: Connection
  -> AccessMode
  -> Text
  -> [TableEntry]
  -> IO (Out.ObjectType IO)
queryType connection accessMode dbId tables = do
  let
    documentation :: Text
    documentation =
      "Available queries for database \"" <> dbId <> "\""

    getDbEntries :: TableEntry -> Out.Resolve IO
    getDbEntries table = do
      context <- ask

      rows :: [[SQLData]] <- case context.arguments of
        Arguments args -> do
          filterElements <- case args & HashMap.lookup "filter" of
            Nothing -> pure []
            Just colToFilter -> case colToFilter of
              Object filterObj -> case HashMap.toList filterObj of
                [] -> P.throwIO $ userError "Error: Filter must not be empty"
                filterElements -> pure filterElements
              _ -> pure []

          orderElements :: [(Name, Value)] <-
            case args & HashMap.lookup "order_by" of
              Nothing -> pure []
              Just colToOrder -> case colToOrder of
                List objects ->
                  -- => [Value]
                  objects
                    -- => IO [[(Name, Value)]]
                    & P.traverse
                      ( \case
                          Object orderObject -> case HashMap.toList orderObject of
                            [] -> P.throwIO $ userError "Error: Order must not be empty"
                            orderElements -> pure orderElements
                          _ -> pure [] -- Should not be reachable
                      )
                    -- => IO [(Name, Value)]
                    <&> P.join
                _ -> pure []

          limitElements :: Maybe P.Int32 <-
            case args & HashMap.lookup "limit" of
              Just (Int limit)
                | limit >= 0 ->
                    pure (Just limit)
                | otherwise ->
                    P.throwIO $
                      userError
                        "Error: limit must be positive"
              _ -> pure Nothing

          paginationMb :: Maybe Pagination <-
            case (limitElements, args & HashMap.lookup "offset") of
              (Just limit, Just (Int offset))
                | offset >= 0 ->
                    pure $
                      Just $
                        Pagination
                          (fromIntegral limit)
                          (Just $ fromIntegral offset)
                | otherwise ->
                    P.throwIO $ userError "Error: offset must be positive"
              (Just limit, _) ->
                pure $
                  Just $
                    Pagination
                      (fromIntegral limit)
                      Nothing
              (Nothing, Just (Int _)) ->
                P.throwIO $
                  userError
                    "Error: cannot specify offset \
                    \without also specifying a limit"
              _ -> pure Nothing

          let
            countQuery :: Query
            countQuery =
              Query $
                P.fold
                  [ "SELECT COUNT() FROM"
                  , quoteKeyword table.name
                  , "\n"
                  , getWhereClause filterElements
                  ]

          -- Will be equal `Just numRows` when the number of
          -- returned rows is too large.
          tooManyReturnedRows :: Maybe Int <- case paginationMb of
            -- Limit doesn't seem to affect COUNT(),
            -- so we consider it manually.
            Just pagination
              | pagination.limit <= maxGraphqlResultCount ->
                  pure Nothing
            _ -> do
              results <- liftIO $ SS.query_ connection countQuery

              let numRows = case P.head results of
                    Just numRowsOnly -> SS.fromOnly numRowsOnly
                    Nothing -> 0

              pure $
                if numRows > maxGraphqlResultCount
                  then Just numRows
                  else Nothing

          P.for_ tooManyReturnedRows $ \numRows -> do
            P.throwIO $
              userError $
                P.fold
                  [ "The graphql API cannot return more than "
                  , show maxGraphqlResultCount
                  , " entries at a time. Your query would have returned "
                  , show numRows
                  , " rows. "
                  , "Consider setting the `limit` argument on your query: `{ "
                  , T.unpack table.name
                  , " (limit: 50) { ... } }`"
                  ]

          liftIO $
            executeSqlQuery
              connection
              table.name
              table.columns
              filterElements
              orderElements
              paginationMb

      colErrorsToUserError $ rowsToGraphQL dbId table rows

    getDbEntriesByPK :: TableEntry -> Out.Resolve IO
    getDbEntriesByPK tableEntry = do
      context <- ask
      let Arguments args = context.arguments

      -- This query can return at most one row, so we don't worry checking for
      -- COUNT() and asserting it's within the set limits.
      queryResult <-
        liftIO $
          executeSqlQuery
            connection
            tableEntry.name
            tableEntry.columns
            (getByPKFilterElements args)
            []
            Nothing

      case P.head queryResult of
        Nothing -> pure Null
        Just row ->
          colErrorsToUserError $
            rowToGraphQL
              dbId
              tableEntry
              row

    getResolvers :: IO (HashMap.HashMap Text (Resolver IO))
    getResolvers = do
      let
        getTableTuple :: TableEntry -> IO (Text, Resolver IO)
        getTableTuple table =
          makeResolver
            (Introspection.tableQueryField table)
            (getDbEntries table)

        getTableByPKTuple :: TableEntry -> IO (Maybe (Text, Resolver IO))
        getTableByPKTuple table =
          P.for (Introspection.tableQueryByPKField tables table) $ \field ->
            makeResolver field (getDbEntriesByPK table)

      queryMany <- P.for tables getTableTuple
      queryByPKMbs <- P.for tables getTableByPKTuple
      let queryByPK = P.catMaybes queryByPKMbs
      pure $ HashMap.fromList $ queryMany <> queryByPK

  resolvers <- getResolvers
  schemaResolver <- Introspection.getSchemaResolver accessMode tables

  let
    -- Resolve = ReaderT Context m Value
    wrapResolve resolve = do
      when (P.not $ canRead accessMode) $ do
        throw $
          ResolverException $
            userError "Cannot read field using writeonly access code"
      resolve

    protectResolver = \case
      ValueResolver field resolve ->
        ValueResolver field (wrapResolve resolve)
      EventStreamResolver field resolve subscribe ->
        EventStreamResolver field (wrapResolve resolve) subscribe

  pure $
    outObjectTypeToObjectType $
      OutObjectType
        { name = "Query"
        , descriptionMb = Just documentation
        , interfaceTypes = []
        , fields =
            P.fold
              [ schemaResolver
              , Introspection.typeNameResolver
              , resolvers
              ]
              <&> protectResolver
        }


mutationType
  :: Connection
  -> Integer
  -> AccessMode
  -> Text
  -> [TableEntry]
  -> IO (Maybe (Out.ObjectType IO))
mutationType connection maxRowsPerTable accessMode dbId tables = do
  let
    getColValue :: HashMap.HashMap Text Value -> Text -> Value
    getColValue rowObj columnName =
      HashMap.findWithDefault Null (doubleXEncodeGql columnName) rowObj

    mutationResponse :: TableEntry -> Int -> [[SQLData]] -> IO Value
    mutationResponse table numChanges rows = do
      returning <-
        colErrorsToUserError $
          rowsToGraphQL dbId table rows

      pure $
        Object $
          HashMap.fromList
            [ ("affected_rows", Int $ fromIntegral numChanges)
            , ("returning", returning)
            ]

    mutationByPKResponse :: TableEntry -> Int -> Maybe [SQLData] -> IO Value
    mutationByPKResponse table numChanges mbRow = do
      returning <- case mbRow of
        Nothing -> pure Null
        Just row ->
          colErrorsToUserError $
            rowToGraphQL dbId table row

      pure $
        Object $
          HashMap.fromList
            [ ("affected_rows", Int $ fromIntegral numChanges)
            , ("returning", returning)
            ]

    executeDbInserts :: TableEntry -> ReaderT Out.Context IO Value
    executeDbInserts table = do
      context <- ask
      let
        insertInDb :: Arguments -> ReaderT Out.Context IO (Int, [[SQLData]])
        insertInDb (Arguments argMap) = do
          -- Yields for example:
          --   [ { name: "John", email: "john@example.com" }
          --   , { name: "Eve",  email: "eve@example.com" }
          --   ]
          values :: [HashMap Text Value] <- getArg "objects" argMap
          let
            -- All colums that are contained in the entries
            containedColumns :: [Text]
            containedColumns =
              values
                <&> HashMap.keys
                & P.concat
                & nub
                <&> doubleXDecode

            boundVariableNames :: [Text]
            boundVariableNames =
              containedColumns
                <&> (\name -> ":" <> doubleXEncodeGql name)

          onConflictArg :: [HashMap Text Value] <-
            getArgWithDefault "on_conflict" argMap []

          onConflictClauses <- P.for onConflictArg $ \fields -> do
            let
              getColumnList fieldName =
                getArgWithDefault fieldName fields []
                  <&> P.mapMaybe
                    ( \case
                        Enum columnName -> Just columnName
                        _ -> Nothing
                    )

            constraint <- getColumnList "constraint"
            update <- getColumnList "update_columns"

            updateClauses <- P.for update $ \column -> do
              when (column `notElem` containedColumns) $ do
                P.throwIO $
                  userError $
                    "Column "
                      <> T.unpack column
                      <> " cannot be set on conflicts without \
                         \being explicitly provided"

              pure $
                quoteKeyword column
                  <> " = :"
                  <> doubleXEncodeGql column

            filterElements <- getArgWithDefault "where" fields mempty

            pure $
              "ON CONFLICT ("
                <> ( constraint
                      <&> quoteKeyword
                      & intercalate ", "
                   )
                <> ")\n DO UPDATE SET \n"
                <> intercalate ",\n" updateClauses
                <> "\n"
                <> getWhereClause (HashMap.toList filterElements)

          let
            columnList =
              if P.null containedColumns
                then ""
                else
                  " ("
                    <> ( containedColumns
                          <&> quoteKeyword
                          & intercalate ", "
                       )
                    <> ")"
            insertedValues =
              if P.null boundVariableNames
                then "DEFAULT VALUES"
                else
                  "VALUES ("
                    <> intercalate ", " boundVariableNames
                    <> ")"
            sqlQuery =
              Query $
                P.unlines
                  [ "INSERT INTO "
                      <> quoteKeyword table.name
                      <> columnList
                  , insertedValues
                  , P.unlines onConflictClauses
                  , getReturningClause table
                  ]

            sqlDataRows :: [[SQLData]]
            sqlDataRows =
              values <&> \rowObj ->
                containedColumns
                  <&> getColValue rowObj
                  <&> gqlValueToSQLData

          returnedRows <-
            liftIO $ P.forM sqlDataRows $ \sqlDataRow -> do
              numRowsRes :: [[Integer]] <-
                query_
                  connection
                  $ Query
                  $ "SELECT COUNT() FROM "
                    <> quoteKeyword table.name

              case numRowsRes of
                [[numRows]] -> do
                  when (numRows >= maxRowsPerTable) $
                    P.throwIO $
                      userError $
                        "Please upgrade to a Pro account \
                        \to insert more than "
                          <> show maxRowsPerTable
                          <> " rows into a table"
                _ -> pure ()

              SS.queryNamed connection sqlQuery $
                P.zipWith (SS.:=) boundVariableNames sqlDataRow

          -- FIXME:
          --   This should probably be used, but sqlite-simple
          --   doesn't use only one query to execute the insert
          --   https://github.com/nurpax/sqlite-simple/issues/82
          -- liftIO $ changes connection
          pure (P.length sqlDataRows, returnedRows & P.concat)

      (numOfChanges, returnedRows) <- insertInDb context.arguments
      liftIO $ mutationResponse table numOfChanges returnedRows

    -- Execute SQL query to update selected entries
    executeDbUpdates :: TableEntry -> ReaderT Out.Context IO Value
    executeDbUpdates table = do
      context <- ask
      let Arguments args = context.arguments
      liftIO $ do
        filterObj <- getArg "filter" args
        pairsToSet <- getArg "set" args
        (numOfChanges, updatedRows) <- case HashMap.toList filterObj of
          [] -> P.throwIO $ userError "Error: Filter must not be empty"
          filterElements ->
            executeUpdateMutation
              connection
              table
              pairsToSet
              filterElements

        mutationResponse table numOfChanges updatedRows

    executeDbUpdatesByPK :: TableEntry -> ReaderT Out.Context IO Value
    executeDbUpdatesByPK table = do
      context <- ask
      let Arguments args = context.arguments
      let filterElements =
            args
              & HashMap.delete "set"
              & getByPKFilterElements

      liftIO $ do
        pairsToSet <- getArg (encodeOutsidePKNames table "set") args
        (numOfChanges, updatedRows) <-
          executeUpdateMutation
            connection
            table
            pairsToSet
            filterElements

        mutationByPKResponse table numOfChanges $ P.head updatedRows

    -- Execute SQL query to delete selected entries
    executeDbDeletions :: TableEntry -> ReaderT Out.Context IO Value
    executeDbDeletions table = do
      context <- ask
      let Arguments args = context.arguments

      liftIO $ do
        filterElements <- getArg "filter" args
        let sqlQuery =
              Query $
                P.unlines
                  [ "DELETE FROM " <> quoteKeyword table.name
                  , getWhereClause $ HashMap.toList filterElements
                  , getReturningClause table
                  ]

        deletedRows :: [[SQLData]] <- SS.query_ connection sqlQuery
        numOfChanges <- SS.changes connection
        mutationResponse table numOfChanges deletedRows

    executeDbDeletionsByPK :: TableEntry -> ReaderT Out.Context IO Value
    executeDbDeletionsByPK table = do
      context <- ask
      let Arguments args = context.arguments

      liftIO $ do
        let sqlQuery =
              Query $
                P.unlines
                  [ "DELETE FROM " <> quoteKeyword table.name
                  , getWhereClause $ getByPKFilterElements args
                  , getReturningClause table
                  ]

        deletedRows :: [[SQLData]] <- SS.query_ connection sqlQuery
        numOfChanges <- SS.changes connection
        mutationByPKResponse table numOfChanges $ P.head deletedRows

    getMutationResolvers :: IO (HashMap.HashMap Text (Resolver IO))
    getMutationResolvers = do
      let
        getInsertTableTuple :: TableEntry -> IO (Text, Resolver IO)
        getInsertTableTuple table =
          makeResolver
            (Introspection.tableInsertField accessMode table)
            (executeDbInserts table)

        getUpdateTableTuple :: TableEntry -> IO (Text, Resolver IO)
        getUpdateTableTuple table =
          makeResolver
            (Introspection.tableUpdateField accessMode table)
            (executeDbUpdates table)

        getUpdateByPKTableTuple :: TableEntry -> IO (Maybe (Text, Resolver IO))
        getUpdateByPKTableTuple table =
          P.for (Introspection.tableUpdateFieldByPk accessMode tables table) $
            \field -> makeResolver field (executeDbUpdatesByPK table)

        getDeleteTableTuple :: TableEntry -> IO (Text, Resolver IO)
        getDeleteTableTuple table =
          makeResolver
            (Introspection.tableDeleteField accessMode table)
            (executeDbDeletions table)

        getDeleteByPKTableTuple :: TableEntry -> IO (Maybe (Text, Resolver IO))
        getDeleteByPKTableTuple table =
          P.for (Introspection.tableDeleteFieldByPK accessMode tables table) $
            \field -> makeResolver field (executeDbDeletionsByPK table)

        getTableTuples :: IO [(Text, Resolver IO)]
        getTableTuples =
          let
            tablesWithoutViews =
              List.filter
                (\table -> table.object_type == Table)
                tables
          in
            P.fold
              [ P.for tablesWithoutViews getInsertTableTuple
              , P.for tablesWithoutViews getUpdateTableTuple
              , P.for tablesWithoutViews getDeleteTableTuple
              , P.for tablesWithoutViews getUpdateByPKTableTuple
                  <&> P.catMaybes
              , P.for tablesWithoutViews getDeleteByPKTableTuple
                  <&> P.catMaybes
              ]

      getTableTuples <&> HashMap.fromList

  if canWrite accessMode
    then
      Just
        . Out.ObjectType
          "Mutation"
          Nothing
          []
        <$> getMutationResolvers
    else pure Nothing


-- | Automatically generated schema derived from the SQLite database
getDerivedSchema
  :: SchemaConf
  -> Connection
  -> Text
  -> [TableEntry]
  -> IO (Schema IO)
getDerivedSchema schemaConf connection dbId tables = do
  let sqlitePragmas = getSQLitePragmas schemaConf.pragmaConf
  P.forM_ sqlitePragmas (execute_ connection)

  queries <- queryType connection schemaConf.accessMode dbId tables
  mutations <-
    mutationType
      connection
      schemaConf.maxRowsPerTable
      schemaConf.accessMode
      dbId
      tables

  pure $
    schema
      queries
      mutations
      Nothing -- subscriptions
      mempty
