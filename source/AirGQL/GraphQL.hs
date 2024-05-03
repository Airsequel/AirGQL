{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Replace case with maybe" #-}

module AirGQL.GraphQL (
  getDerivedSchema,
  queryType,
  sqlDataToGQLValue,
  getMutationResponse,
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
  Traversable (sequence),
  fromIntegral,
  fromMaybe,
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
  EnumType (EnumType),
  EnumValue (EnumValue),
  InputField (InputField),
  Resolver (EventStreamResolver, ValueResolver),
  ScalarType,
  Schema,
  Value (Boolean, Enum, Float, Int, List, Null, Object, String),
  boolean,
  float,
  int,
  schema,
  string,
 )
import Language.GraphQL.Type.In (
  InputObjectType (InputObjectType),
  Type (NamedInputObjectType),
 )
import Language.GraphQL.Type.In qualified as In
import Language.GraphQL.Type.Out qualified as Out
import Numeric (showFFloat)

import AirGQL.Config (
  maxGraphqlResultCount,
 )
import AirGQL.GQLWrapper (
  InArgument (InArgument, argDescMb, argType, valueMb),
  OutField (OutField, arguments, descriptionMb, fieldType),
  inArgumentToArgument,
  outFieldToField,
 )
import AirGQL.Introspection (getSchemaResolver, typeNameResolver)
import AirGQL.Lib (
  AccessMode (ReadAndWrite, ReadOnly, WriteOnly),
  ColumnEntry (column_name, datatype, datatype_gql),
  GqlTypeName (root),
  TableEntryRaw (name),
  column_name_gql,
  getColumns,
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


typeNameToScalarType :: Maybe GqlTypeName -> ScalarType
typeNameToScalarType Nothing = string
typeNameToScalarType (Just typeName) =
  case typeName.root of
    "Int" -> int
    "Float" -> float
    "String" -> string
    "Boolean" -> boolean
    _ -> string


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


colNamesWithValResolver :: [ColumnEntry] -> [(Text, Resolver IO)]
colNamesWithValResolver columnEntries =
  columnEntries <&> \colEntry ->
    let
      fieldToResolve =
        Out.Field
          (Just colEntry.column_name_gql)
          ( Out.NamedScalarType $
              typeNameToScalarType
                colEntry.datatype_gql
          )
          mempty

      resolvedValue = do
        context <- ask
        pure $ case context.values of
          Object obj ->
            case obj & HashMap.lookup colEntry.column_name_gql of
              Nothing -> String "Error: Field does not exist"
              Just val ->
                case colEntry.datatype of
                  -- Coerce value to nullable String
                  -- if no datatype is set.
                  -- This happens for columns in views.
                  "" -> gqlValueToNullableString val
                  _ -> val
          _ -> String "Error: Value could not be retrieved"
    in
      ( colEntry.column_name_gql
      , ValueResolver fieldToResolve resolvedValue
      )


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


colNamesWithFilterField :: Text -> [ColumnEntry] -> [(Text, InputField)]
colNamesWithFilterField tableName columnEntries =
  columnEntries <&> \colEntry ->
    let
      inputField =
        InputField
          (Just $ "Filter for " <> colEntry.column_name_gql)
          ( NamedInputObjectType $
              InputObjectType
                (doubleXEncodeGql tableName <> "_filter")
                (Just "Filter object for the column")
                ( let theInputField =
                        InputField
                          (Just "Value to compare to")
                          ( In.NamedScalarType $
                              typeNameToScalarType
                                colEntry.datatype_gql
                          )
                          Nothing -- Default value
                      listInputField =
                        InputField
                          (Just "Values to compare to")
                          ( In.ListType $
                              In.NamedScalarType $
                                typeNameToScalarType
                                  colEntry.datatype_gql
                          )
                          Nothing -- Default value
                  in  HashMap.fromList
                        [ ("eq", theInputField)
                        , ("neq", theInputField)
                        , ("gt", theInputField)
                        , ("gte", theInputField)
                        , ("lt", theInputField)
                        , ("lte", theInputField)
                        , ("like", theInputField)
                        , ("ilike", theInputField)
                        , ("in", listInputField)
                        , ("nin", listInputField)
                        ]
                )
          )
          Nothing -- Default value
    in
      ( colEntry.column_name_gql
      , inputField
      )


queryType
  :: Connection
  -> AccessMode
  -> Text
  -> [TableEntryRaw]
  -> IO (Out.ObjectType IO)
queryType connection accessMode dbId tables = do
  let
    documentation :: Text
    documentation =
      "Available queries for database \"" <> dbId <> "\""

    getOutField :: Text -> IO (Out.Field IO)
    getOutField tableName = do
      columnEntries <- liftIO $ getColumns dbId connection tableName

      let
        colNamesWithOrderingTerm :: [(Text, InputField)]
        colNamesWithOrderingTerm =
          columnEntries <&> \colEntry ->
            ( colEntry.column_name_gql
            , InputField
                (Just $ "Ordering term for " <> colEntry.column_name_gql)
                ( In.NamedEnumType $
                    EnumType
                      "OrderingTerm"
                      (Just "Ordering object for the column")
                      ( HashMap.fromList
                          [ ("ASC", EnumValue (Just "ASC"))
                          , ("asc", EnumValue (Just "ASC"))
                          , ("DESC", EnumValue (Just "DESC"))
                          , ("desc", EnumValue (Just "DESC"))
                          ]
                      )
                )
                Nothing -- Default value
            )

        typeNameField :: Text -> [(Text, Resolver IO)]
        typeNameField nameOfTable =
          let
            typeNameOutField =
              outFieldToField $
                OutField
                  { descriptionMb = Just $ "The type name of " <> nameOfTable
                  , fieldType = Out.NonNullScalarType string
                  , arguments = HashMap.empty
                  }
          in
            [
              ( "__typename"
              , ValueResolver typeNameOutField $
                  pure $
                    String $
                      doubleXEncodeGql nameOfTable <> "_row"
              )
            ]

      pure $
        outFieldToField $
          OutField
            { descriptionMb = Just $ "Provides entries from " <> tableName
            , fieldType =
                Out.ListType $
                  Out.NamedObjectType $
                    Out.ObjectType
                      tableName
                      (Just "short desc")
                      []
                      ( HashMap.fromList $
                          colNamesWithValResolver columnEntries
                            <> typeNameField tableName
                      )
            , arguments =
                HashMap.fromList
                  [
                    ( "filter"
                    , inArgumentToArgument $
                        InArgument
                          { argDescMb = Just "Filter objects"
                          , argType =
                              NamedInputObjectType $
                                InputObjectType
                                  (doubleXEncodeGql tableName <> "_filter")
                                  ( Just
                                      "Filter objects for the specified columns"
                                  )
                                  (HashMap.fromList (colNamesWithFilterField tableName columnEntries))
                          , valueMb = Nothing
                          }
                    )
                  ,
                    ( "order_by"
                    , inArgumentToArgument $
                        InArgument
                          { argDescMb = Just "Order by the specified columns"
                          , argType =
                              In.ListType $
                                In.NamedInputObjectType $
                                  InputObjectType
                                    (doubleXEncodeGql tableName <> "_order_by")
                                    (Just "Options for ordering by columns")
                                    (HashMap.fromList colNamesWithOrderingTerm)
                          , valueMb = Nothing
                          }
                    )
                  ,
                    ( "limit"
                    , inArgumentToArgument $
                        InArgument
                          { argDescMb =
                              Just "Limit the number of returned rows."
                          , argType = In.NamedScalarType int
                          , valueMb = Nothing
                          }
                    )
                  ,
                    ( "offset"
                    , inArgumentToArgument $
                        InArgument
                          { argDescMb =
                              Just
                                "Change the index rows \
                                \start being returned from"
                          , argType = In.NamedScalarType int
                          , valueMb = Nothing
                          }
                    )
                  ]
            }
    -- -- TODO: Use for retrieving record by primary key
    -- , arguments = HashMap.fromList $ columnEntries
    --     <&> (\colEntry ->
    --           ( colEntry.column_name_gql :: Text
    --           , inArgumentToArgument $ InArgument
    --               { argDescMb = Just "Retrieve object by primary key"
    --               , argType = In.NamedScalarType $
    --                   typeNameToScalarType $ colEntry.datatype
    --               , valueMb = Nothing
    --               }
    --           )
    --         )

    getDbEntries :: Text -> Out.Resolve IO
    getDbEntries tableName = do
      context <- ask
      colEntries <- liftIO $ getColumns dbId connection tableName

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
                  , quoteKeyword tableName
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
                  , T.unpack tableName
                  , " (limit: 50) { ... } }`"
                  ]

          liftIO $
            executeSqlQuery
              connection
              tableName
              colEntries
              filterElements
              orderElements
              paginationMb

      rowsToList dbId tableName colEntries rows

    getResolvers :: IO (HashMap.HashMap Text (Resolver IO))
    getResolvers = do
      let
        getTableTuple :: TableEntryRaw -> IO (Text, Resolver IO)
        getTableTuple table = do
          outField <- getOutField table.name
          pure
            ( doubleXEncodeGql table.name
            , ValueResolver
                outField
                ( -- Exceptions must be converted to ResolverExceptions
                  -- to be picked up by GQL query executor
                  catchAll
                    (getDbEntries table.name)
                    (throw . ResolverException)
                )
            )

        getTableTuples :: IO [(Text, Resolver IO)]
        getTableTuples =
          P.for tables getTableTuple

      getTableTuples <&> HashMap.fromList

  -- -- TODO: Add support for retriving record by ID
  -- getResolversPrimaryKey :: IO (HashMap.HashMap Text (Resolver IO))
  -- getResolversPrimaryKey = do
  --   let
  --     getTableTuple table = do
  --       outField <- getOutField $ table.name
  --       pure
  --         ( table.name) <> "_by_pk"
  --         , ValueResolver
  --             outField
  --             (getDbEntries $ table.name)
  --         )

  --     getTableTuples :: IO [(Text, Resolver IO)]
  --     getTableTuples =
  --       sequence $ tables <&> getTableTuple

  --   getTableTuples <&> HashMap.fromList

  resolvers <- getResolvers
  schemaResolver <- getSchemaResolver dbId connection accessMode tables

  -- resolversPrimaryKey <- getResolversPrimaryKey
  let
    -- Resolve = ReaderT Context m Value
    wrapResolve resolve = do
      when (accessMode == WriteOnly) $ do
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
              , typeNameResolver
              , resolvers
              -- , resolversPrimaryKey)
              ]
              <&> protectResolver
        }


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
  (_, SQLBlob byteString) -> pure $ String $ show byteString
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


mutationTypeNameField :: Text -> (Text, Resolver IO)
mutationTypeNameField nameOfTable =
  let
    typeNameOutField =
      outFieldToField $
        OutField
          { descriptionMb = Just $ "The type name of " <> nameOfTable
          , fieldType = Out.NonNullScalarType string
          , arguments = HashMap.empty
          }
  in
    ( "__typename"
    , ValueResolver typeNameOutField $
        pure $
          String $
            doubleXEncodeGql nameOfTable <> "_mutation_response"
    )


getMutationResponse
  :: AccessMode
  -> Text
  -> [ColumnEntry]
  -> Out.Type IO
getMutationResponse accessMode tableName columnEntries =
  Out.NamedObjectType $
    outObjectTypeToObjectType $
      OutObjectType
        { name = doubleXEncodeGql tableName <> "_mutation_response"
        , descriptionMb =
            Just $
              tableName <> " mutation response description"
        , interfaceTypes = []
        , fields =
            HashMap.fromList $
              [
                ( "affected_rows"
                , let
                    field :: Out.Field m
                    field =
                      outFieldToField $
                        OutField
                          { descriptionMb = Just "nonNullInt description"
                          , fieldType = Out.NonNullScalarType int
                          , arguments = HashMap.empty
                          }

                    value :: ReaderT Out.Context IO Value
                    value = do
                      context <- ask
                      case context & Out.values of
                        Object obj ->
                          pure $
                            fromMaybe (Int 0) $
                              HashMap.lookup "affected_rows" obj
                        _ -> pure $ Int 0
                  in
                    ValueResolver field value
                )
              , mutationTypeNameField tableName
              ]
                <> case accessMode of
                  WriteOnly -> []
                  _ ->
                    [
                      ( "returning"
                      , let
                          field :: Out.Field IO
                          field =
                            outFieldToField $
                              OutField
                                { descriptionMb =
                                    Just
                                      "Non null seturning description"
                                , fieldType =
                                    Out.NonNullListType $
                                      Out.NamedObjectType $
                                        Out.ObjectType
                                          "returning"
                                          (Just "short desc")
                                          []
                                          ( HashMap.fromList $
                                              colNamesWithValResolver columnEntries
                                          )
                                , arguments = HashMap.empty
                                }

                          value :: ReaderT Out.Context IO Value
                          value = do
                            context <- ask
                            case context & Out.values of
                              Object obj ->
                                pure $
                                  fromMaybe (Object P.mempty) $
                                    HashMap.lookup "returning" obj
                              _ -> pure $ Object P.mempty
                        in
                          ValueResolver field value
                      )
                    ]
        }


rowsToList :: (MonadIO m) => Text -> Text -> [ColumnEntry] -> [[SQLData]] -> m Value
rowsToList dbId tableName columnEntries updatedRows =
  let
    buildMetadataJson :: Text -> Text -> Text
    buildMetadataJson colName rowid =
      object ["url" .= colToFileUrl dbId tableName colName rowid]
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
              (colEntry.column_name_gql, gqlData)
  in
    updatedRows
      <&> ( \row ->
              -- => [(ColumnEntry, SQLData)]
              P.zip columnEntries row
                -- => [Either (Text, Text) (Text, Value)]
                <&> parseSqlData
                -- => Either [(Text, Text)] (Text, Value)
                & collectErrorList
                -- => Either [(Text, Text)] (HashMap Text Value)
                <&> HashMap.fromList
                -- => Either [(Text, Text)] Value
                <&> Object
          )
      -- => Either [[(Text, Text)]] [Value]
      & collectErrorList
      & \case
        Right values -> pure $ List values
        Left errors ->
          let
            errorLines =
              P.join errors
                <&> \(column, err) -> "On column " <> show column <> ": " <> err
          in
            P.throwIO $
              userError $
                T.unpack $
                  "Multiple errors occurred:\n" <> P.unlines errorLines


executeSqlMutation
  :: Connection
  -> Text
  -> HashMap.HashMap Text Value
  -> [ColumnEntry]
  -> [(Text, Value)]
  -> IO (Int, [[SQLData]])
executeSqlMutation connection tableName args columnEntries filterElements = do
  let
    colNamesToUpdateRaw :: [Text]
    colNamesToUpdateRaw =
      case HashMap.lookup "set" args of
        Just (Object dataObj) -> HashMap.keys dataObj
        _ -> []

    colNamesToUpdate :: [Text]
    colNamesToUpdate =
      columnEntries
        <&> column_name
        <&> ( \columnName ->
                if doubleXEncodeGql columnName `P.elem` colNamesToUpdateRaw
                  then Just columnName
                  else Nothing
            )
        & P.catMaybes

    columnNamesText :: Text
    columnNamesText =
      columnEntries
        <&> column_name
        <&> quoteKeyword
        & intercalate ", "

    setText :: Text
    setText =
      colNamesToUpdate
        <&> (\columnName -> quoteKeyword columnName <> " = ?")
        & intercalate ", "

    valuesToSet :: [SQLData]
    valuesToSet =
      case HashMap.lookup "set" args of
        Just (Object dataObj) ->
          columnEntries
            <&> column_name
            <&> ( \columnName ->
                    HashMap.lookup
                      (doubleXEncodeGql columnName)
                      dataObj
                )
            & P.catMaybes
            <&> gqlValueToSQLData
        _ -> []

  updatedRows :: [[SQLData]] <-
    if setText == ""
      then pure []
      else
        let
          sqlQuery =
            Query $
              "UPDATE "
                <> quoteKeyword tableName
                <> "\n"
                <> "SET "
                <> setText
                <> "\n"
                <> getWhereClause filterElements
                <> "\n"
                <> "RETURNING "
                <> columnNamesText

          colTypesToUpdate :: [Text]
          colTypesToUpdate =
            columnEntries
              <&> ( \colEntry ->
                      if doubleXEncodeGql colEntry.column_name
                        `P.elem` colNamesToUpdateRaw
                        then Just colEntry.datatype
                        else Nothing
                  )
              & P.catMaybes

          valuesToSetNorm =
            P.zip valuesToSet colTypesToUpdate
              <&> \(val, datatype) ->
                if (val == SQLText "{}")
                  P.&& ("BLOB" `T.isPrefixOf` T.toUpper datatype)
                  then SQLBlob ""
                  else val
        in
          catchAll
            ( liftIO $ do
                setCaseInsensitive connection filterElements
                query connection sqlQuery valuesToSetNorm
            )
            (throw . ResolverException)

  liftIO $
    changes connection
      & P.fmap (,updatedRows)


mutationType
  :: Connection
  -> Integer
  -> AccessMode
  -> Text
  -> [TableEntryRaw]
  -> IO (Maybe (Out.ObjectType IO))
mutationType connection maxRowsPerTable accessMode dbId tables = do
  let
    documentation =
      "Available queries for database \"" <> dbId <> "\""

    getTableFilterType :: Text -> [ColumnEntry] -> InputObjectType
    getTableFilterType tableName columnEntries = do
      InputObjectType
        (doubleXEncodeGql tableName <> "_filter")
        ( Just
            "Filter objects for the specified columns"
        )
        (HashMap.fromList (colNamesWithFilterField tableName columnEntries))

    getOutField :: Text -> IO (Out.Field IO)
    getOutField tableName = do
      columnEntries <- liftIO $ getColumns dbId connection tableName

      let
        colNamesWithField :: [(Text, InputField)]
        colNamesWithField =
          columnEntries <&> \colEntry ->
            let
              inputField =
                InputField
                  (Just colEntry.column_name_gql)
                  ( In.NamedScalarType $
                      typeNameToScalarType colEntry.datatype_gql
                  )
                  Nothing -- Default value
            in
              ( colEntry.column_name_gql
              , inputField
              )

      let
        objectsType =
          inArgumentToArgument $
            InArgument
              { argDescMb =
                  Just
                    "Objects to be inserted into the database"
              , argType =
                  In.ListType $
                    NamedInputObjectType $
                      InputObjectType
                        ( doubleXEncodeGql tableName
                            <> "_insert_input"
                        )
                        ( Just
                            "Object to be inserted into the database"
                        )
                        (HashMap.fromList colNamesWithField)
              , valueMb = Nothing
              }

        onConflictDescription =
          "Specifies how to handle brtoken unique constraints" :: Text

        columnEnumVariants =
          columnEntries
            <&> \entry ->
              (entry.column_name_gql, EnumValue Nothing)

        columnEnumType =
          EnumType
            (doubleXEncodeGql tableName <> "_column")
            (Just "This enum contains a variant for each colum in the table")
            (HashMap.fromList columnEnumVariants)

        onConflictType =
          inArgumentToArgument $
            InArgument
              { argDescMb = Just onConflictDescription
              , argType =
                  In.ListType
                    $ In.NonNullInputObjectType
                    $ InputObjectType
                      ( doubleXEncodeGql tableName
                          <> "_upsert_on_conflict"
                      )
                      (Just onConflictDescription)
                    $ HashMap.fromList
                      [
                        ( "constraint"
                        , InputField
                            (Just "columns to handle conflicts of")
                            ( In.NonNullListType $
                                In.NonNullEnumType columnEnumType
                            )
                            Nothing
                        )
                      ,
                        ( "update_columns"
                        , InputField
                            (Just "columns to override on conflict")
                            ( In.NonNullListType $
                                In.NonNullEnumType columnEnumType
                            )
                            Nothing
                        )
                      ,
                        ( "where"
                        , InputField
                            (Just "filter specifying which conflicting columns to update")
                            ( In.NamedInputObjectType $
                                getTableFilterType tableName columnEntries
                            )
                            Nothing
                        )
                      ]
              , valueMb = Nothing
              }

      pure $
        outFieldToField $
          OutField
            { descriptionMb = Just "description"
            , fieldType = getMutationResponse accessMode tableName columnEntries
            , arguments =
                HashMap.fromList
                  [ ("objects", objectsType)
                  , ("on_conflict", onConflictType)
                  ]
            }

    getColValue :: HashMap.HashMap Text Value -> Text -> Value
    getColValue rowObj columnName =
      HashMap.findWithDefault Null (doubleXEncodeGql columnName) rowObj

    executeDbInserts :: Text -> ReaderT Out.Context IO Value
    executeDbInserts tableName = do
      columnEntries <- liftIO $ getColumns dbId connection tableName

      context <- ask

      let
        columnNames :: [Text]
        columnNames =
          columnEntries <&> column_name

        columnNamesText :: Text
        columnNamesText =
          columnNames
            <&> quoteKeyword
            & intercalate ", "

        insertInDb :: Arguments -> ReaderT Out.Context IO (Int, [[SQLData]])
        insertInDb (Arguments argMap) = do
          let
            -- Yields for example:
            --   [ { name: "John", email: "john@example.com" }
            --   , { name: "Eve",  email: "eve@example.com" }
            --   ]
            entries =
              HashMap.findWithDefault
                (List [])
                "objects"
                argMap

            -- All colums that are contained in the entries
            containedColumns :: [Text]
            containedColumns =
              case entries of
                List values ->
                  ( values
                      <&> \case
                        Object rowObj ->
                          HashMap.keys rowObj
                        _ -> []
                  )
                    & P.concat
                    & nub
                    <&> doubleXDecode
                _ -> []

            boundVariableNames :: [Text]
            boundVariableNames =
              containedColumns
                <&> (\name -> ":" <> doubleXEncodeGql name)

            onConflictArg =
              case HashMap.lookup "on_conflict" argMap of
                Just (List values) -> values
                _ -> []

          onConflictClauses <- P.for onConflictArg $ \case
            Object fields -> do
              let
                getColumnList fieldName = do
                  case HashMap.lookup fieldName fields of
                    Just (List elements) -> do
                      element <- elements
                      case element of
                        Enum columnName -> pure columnName
                        _ -> []
                    _ -> []

                constraint = getColumnList "constraint"
                update = getColumnList "update_columns"

              updateClauses <- P.for update $ \column -> do
                when (column `notElem` containedColumns) $ do
                  P.throwIO $
                    userError $
                      "Column "
                        <> T.unpack column
                        <> " cannot be set on conflicts without being explicitly provided"

                pure $
                  quoteKeyword column
                    <> " = :"
                    <> doubleXEncodeGql column

              let
                filterElements = case HashMap.lookup "where" fields of
                  Just (Object filterObj) -> HashMap.toList filterObj
                  _ -> []

              pure $
                "ON CONFLICT ("
                  <> ( constraint
                        <&> quoteKeyword
                        & intercalate "<>"
                     )
                  <> ")\n DO UPDATE SET \n"
                  <> intercalate ",\n" updateClauses
                  <> "\n"
                  <> getWhereClause filterElements
            _ -> pure ""

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
                "INSERT INTO "
                  <> quoteKeyword tableName
                  <> columnList
                  <> insertedValues
                  <> "\n"
                  <> P.unlines onConflictClauses
                  <> "RETURNING "
                  <>
                  -- TODO: Only return the actually requested values
                  columnNamesText

            sqlDataRows :: [[SQLData]]
            sqlDataRows =
              case entries of
                List values ->
                  values <&> \case
                    Object rowObj ->
                      containedColumns
                        <&> getColValue rowObj
                        <&> gqlValueToSQLData
                    _ -> []
                _ -> []

          -- Exception from SQLite must be converted into
          -- ResolverExceptions to be picked up by GQL query executor
          returnedRows <-
            catchAll
              ( liftIO $ P.forM sqlDataRows $ \sqlDataRow -> do
                  numRowsRes :: [[Integer]] <-
                    query_
                      connection
                      $ Query
                      $ "SELECT COUNT() FROM "
                        <> quoteKeyword tableName

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
              )
              (throw . ResolverException)

          -- FIXME:
          --   This should probably be used, but sqlite-simple
          --   doesn't use only one query to execute the insert
          --   https://github.com/nurpax/sqlite-simple/issues/82
          -- liftIO $ changes connection
          pure (P.length sqlDataRows, returnedRows & P.concat)

      (numOfChanges, returnedRows) <- insertInDb context.arguments
      returning <- rowsToList dbId tableName columnEntries returnedRows

      pure $
        Object $
          HashMap.fromList
            [ ("affected_rows", Int $ fromIntegral numOfChanges)
            , ("returning", returning)
            ]

    -- Execute SQL query to update selected entries
    executeDbUpdates :: Text -> ReaderT Out.Context IO Value
    executeDbUpdates tableName = do
      columnEntries <- liftIO $ getColumns dbId connection tableName

      context <- ask

      let Arguments args = context.arguments

      (numOfChanges, updatedRows) <- case HashMap.lookup "filter" args of
        Just (Object filterObj) -> case HashMap.toList filterObj of
          [] -> P.throwIO $ userError "Error: Filter must not be empty"
          filterElements ->
            liftIO $
              executeSqlMutation
                connection
                tableName
                args
                columnEntries
                filterElements
        _ -> pure (0, [])

      returning <- rowsToList dbId tableName columnEntries updatedRows

      pure $
        Object $
          HashMap.fromList
            [ ("affected_rows", Int $ fromIntegral (numOfChanges :: Int))
            , ("returning", returning)
            ]

    -- Execute SQL query to delete selected entries
    executeDbDeletions :: Text -> ReaderT Out.Context IO Value
    executeDbDeletions tableName = do
      columnEntries <- liftIO $ getColumns dbId connection tableName
      context <- ask

      let
        columnNamesText :: Text
        columnNamesText =
          columnEntries
            <&> column_name
            <&> quoteKeyword
            & intercalate ", "

        deleteEntry columnName value = do
          let sqlQuery =
                Query $
                  "DELETE FROM "
                    <> quoteKeyword tableName
                    <> " \
                       \WHERE "
                    <> quoteKeyword columnName
                    <> " = ?\n"
                    <> "RETURNING "
                    <> columnNamesText
          deletedRows :: [[SQLData]] <-
            catchAll
              (liftIO $ query connection sqlQuery [value])
              (throw . ResolverException)
          numChanges <- liftIO $ changes connection

          pure (numChanges, deletedRows)

      (numOfChanges, deletedRows) <- case context.arguments of
        Arguments args -> case HashMap.lookup "filter" args of
          Just colToFilter -> case colToFilter of
            Object filterObj -> case HashMap.toList filterObj of
              [(columnName, Object operatorAndValue)] -> do
                case HashMap.toList operatorAndValue of
                  [("eq", String value)] ->
                    deleteEntry columnName value
                  [("eq", Int value)] ->
                    deleteEntry columnName $ show value
                  _ -> pure (0, [])
              _ -> pure (0, [])
            _ -> pure (0, [])
          Nothing -> pure (0, [])

      returning <- rowsToList dbId tableName columnEntries deletedRows

      pure $
        Object $
          HashMap.fromList
            [ ("affected_rows", Int $ fromIntegral numOfChanges)
            , ("returning", returning)
            ]

    getOutFieldUpdate :: Text -> IO (Out.Field IO)
    getOutFieldUpdate tableName = do
      columnEntries <- liftIO $ getColumns dbId connection tableName

      let
        colNamesWithField :: [(Text, InputField)]
        colNamesWithField =
          columnEntries <&> \colEntry ->
            let
              inputField =
                InputField
                  (Just colEntry.column_name_gql)
                  ( In.NamedScalarType $
                      typeNameToScalarType colEntry.datatype_gql
                  )
                  Nothing -- Default value
            in
              ( colEntry.column_name_gql
              , inputField
              )

      pure $
        outFieldToField $
          OutField
            { descriptionMb = Just $ "Provides entries from " <> tableName
            , fieldType = getMutationResponse accessMode tableName columnEntries
            , arguments =
                HashMap.fromList
                  [
                    ( "filter"
                    , inArgumentToArgument $
                        InArgument
                          { argDescMb = Just "Filter objects"
                          , argType =
                              NamedInputObjectType $
                                getTableFilterType tableName columnEntries
                          , valueMb = Nothing
                          }
                    )
                  ,
                    ( "set"
                    , inArgumentToArgument $
                        InArgument
                          { argDescMb = Just "Map with new values"
                          , argType =
                              NamedInputObjectType $
                                InputObjectType
                                  (doubleXEncodeGql tableName <> "_set_input")
                                  (Just "New values for the specified columns")
                                  (HashMap.fromList colNamesWithField)
                          , valueMb = Nothing
                          }
                    )
                  ]
            }

    getOutFieldDeletion :: Text -> IO (Out.Field IO)
    getOutFieldDeletion tableName = do
      columnEntries <- liftIO $ getColumns dbId connection tableName

      pure $
        outFieldToField $
          OutField
            { descriptionMb = Just $ "Provides entries from " <> tableName
            , fieldType = getMutationResponse accessMode tableName columnEntries
            , arguments =
                HashMap.fromList
                  [
                    ( "filter"
                    , inArgumentToArgument $
                        InArgument
                          { argDescMb = Just "Filter objects"
                          , argType =
                              NamedInputObjectType $
                                InputObjectType
                                  (doubleXEncodeGql tableName <> "_filter")
                                  ( Just
                                      "Filter objects for the specified columns"
                                  )
                                  (HashMap.fromList (colNamesWithFilterField tableName columnEntries))
                          , valueMb = Nothing
                          }
                    )
                  ]
            }
    -- -- TODO: Use for retrieving record by primary key
    -- , arguments = HashMap.fromList $ columnEntries
    --     <&> (\colEntry ->
    --           ( colEntry & column_name_gql :: Text
    --           , inArgumentToArgument $ InArgument
    --               { argDescMb = Just "Retrieve object by primary key"
    --               , argType = In.NamedScalarType $
    --                   typeNameToScalarType $ colEntry & datatype
    --               , valueMb = Nothing
    --               }
    --           )
    --         )

    getMutationResolvers :: IO (HashMap.HashMap Text (Resolver IO))
    getMutationResolvers = do
      let
        getInsertTableTuple :: TableEntryRaw -> IO (Text, Resolver IO)
        getInsertTableTuple table = do
          outFieldInsertion <- getOutField table.name
          pure
            ( "insert_" <> doubleXEncodeGql table.name
            , ValueResolver
                outFieldInsertion
                (executeDbInserts table.name)
            )

        getUpdateTableTuple :: TableEntryRaw -> IO (Text, Resolver IO)
        getUpdateTableTuple table = do
          outFieldUpdate <- getOutFieldUpdate table.name
          pure
            ( "update_" <> doubleXEncodeGql table.name
            , ValueResolver
                outFieldUpdate
                (executeDbUpdates table.name)
            )

        getDeleteTableTuple :: TableEntryRaw -> IO (Text, Resolver IO)
        getDeleteTableTuple table = do
          outFieldDeletion <- getOutFieldDeletion table.name
          pure
            ( "delete_" <> doubleXEncodeGql table.name
            , ValueResolver
                outFieldDeletion
                (executeDbDeletions table.name)
            )

        getTableTuples :: IO [(Text, Resolver IO)]
        getTableTuples =
          sequence $
            (tables <&> getInsertTableTuple)
              <> (tables <&> getUpdateTableTuple)
              <> (tables <&> getDeleteTableTuple)

      getTableTuples <&> HashMap.fromList

  Just
    . Out.ObjectType
      "Mutation"
      (Just documentation)
      []
    <$> getMutationResolvers


-- | Automatically generated schema derived from the SQLite database
getDerivedSchema
  :: SchemaConf
  -> Connection
  -> Text
  -> [TableEntryRaw]
  -> IO (Schema IO)
getDerivedSchema schemaConf connection dbId tables = do
  sqlitePragmas <- getSQLitePragmas schemaConf.pragmaConf
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
      ( case schemaConf.accessMode of
          ReadOnly -> Nothing
          WriteOnly -> mutations
          ReadAndWrite -> mutations
      )
      Nothing -- subscriptions
      mempty
