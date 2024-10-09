module AirGQL.Introspection (
  typeNameResolver,
  getSchemaResolver,
  tableQueryField,
  tableQueryByPKField,
)
where

import Protolude (
  Applicative (pure),
  Either (Left, Right),
  IO,
  Maybe (Just, Nothing),
  Monoid (mempty),
  Semigroup ((<>)),
  Text,
  ($),
  (&),
  (<&>),
 )
import Protolude qualified as P

import Data.HashMap.Strict as HashMap (HashMap, singleton)
import Language.GraphQL.Type.Out as Out (
  Field (Field),
  Resolver (ValueResolver),
  Type (NonNullScalarType),
 )

import AirGQL.Introspection.Resolver (makeType)
import AirGQL.Introspection.Types (IntrospectionType)
import AirGQL.Introspection.Types qualified as Type
import AirGQL.Lib (
  AccessMode,
  ColumnEntry (isRowid, primary_key),
  GqlTypeName (full),
  TableEntry (columns, name),
  canRead,
  canWrite,
  column_name_gql,
  datatype_gql,
  isOmittable,
  notnull,
 )
import Control.Monad (MonadFail (fail))
import Data.List qualified as List
import Data.Text qualified as T
import DoubleXEncoding (doubleXEncodeGql)
import Language.GraphQL.Class (ToGraphQL (toGraphQL))
import Language.GraphQL.Type (string)


typeNameResolver :: HashMap Text (Resolver IO)
typeNameResolver =
  HashMap.singleton
    "__typename"
    $ ValueResolver
      (Out.Field Nothing (Out.NonNullScalarType string) mempty)
    $ pure "Query"


columnTypeName :: ColumnEntry -> Text
columnTypeName entry =
  case entry.datatype_gql of
    Nothing -> "String"
    Just type_ -> type_.full


columnType :: ColumnEntry -> IntrospectionType
columnType entry = case entry.datatype_gql of
  Nothing -> Type.typeString
  Just type_ ->
    Type.scalar type_.full
      & Type.withDescription
        ("Data type for column" <> entry.column_name_gql)


orderingTermType :: Type.IntrospectionType
orderingTermType =
  Type.enum
    "OrderingTerm"
    [ Type.enumValue "ASC" & Type.enumValueWithDescription "In ascending order"
    , Type.enumValue "asc"
        & Type.enumValueWithDescription "In ascending order"
        & Type.deprecatedEnumValue "GraphQL spec recommends all caps for enums"
    , Type.enumValue "DESC" & Type.enumValueWithDescription "In descending order"
    , Type.enumValue "desc"
        & Type.enumValueWithDescription "In descending order"
        & Type.deprecatedEnumValue "GraphQL spec recommends all caps for enums"
    ]


filterType :: TableEntry -> IntrospectionType
filterType table = do
  let
    fieldsWithComparisonExp =
      table.columns <&> \columnEntry -> do
        let colName = columnEntry.column_name_gql
        let typeName = columnTypeName columnEntry
        let type_ = columnType columnEntry
        Type.inputValue colName $
          Type.inputObject
            (typeName <> "Comparison")
            [ Type.inputValue "eq" type_
            , Type.inputValue "neq" type_
            , Type.inputValue "gt" type_
            , Type.inputValue "gte" type_
            , Type.inputValue "lt" type_
            , Type.inputValue "lte" type_
            , Type.inputValue "like" type_
            , Type.inputValue "ilike" type_
            , Type.inputValue "in" $ Type.list type_
            , Type.inputValue "nin" $ Type.list type_
            ]

  Type.inputObject
    (doubleXEncodeGql table.name <> "_filter")
    fieldsWithComparisonExp
    & Type.withDescription "Select rows matching the provided filter object"


tableRowType :: TableEntry -> Type.IntrospectionType
tableRowType table = do
  let fields =
        table.columns <&> \columnEntry -> do
          let colName = columnEntry.column_name_gql
          let base = columnType columnEntry
          let type_ =
                if columnEntry.notnull
                  then Type.nonNull base
                  else base
          Type.field colName type_
  Type.object (doubleXEncodeGql table.name <> "_row") fields


tableQueryField :: TableEntry -> Type.Field
tableQueryField table =
  let
    fieldsWithOrderingTerm =
      table.columns <&> \columnEntry -> do
        let colName = columnEntry.column_name_gql
        Type.inputValue colName orderingTermType

    orderType =
      Type.inputObject
        (doubleXEncodeGql table.name <> "_order_by")
        fieldsWithOrderingTerm
        & Type.withDescription
          ( "Ordering options when selecting data from \""
              <> table.name
              <> "\"."
          )
  in
    Type.field
      (doubleXEncodeGql table.name)
      (Type.nonNull $ Type.list $ Type.nonNull $ tableRowType table)
      & Type.fieldWithDescription ("Rows from the table \"" <> table.name <> "\"")
      & Type.withArguments
        [ Type.inputValue "filter" (filterType table)
            & Type.inputValueWithDescription "Filter to select specific rows"
        , Type.inputValue "order_by" (Type.list orderType)
            & Type.inputValueWithDescription "Columns used to sort the data"
        , Type.inputValue "limit" Type.typeInt
            & Type.inputValueWithDescription "Limit the number of returned rows"
        , Type.inputValue "offset" Type.typeInt
            & Type.inputValueWithDescription "The index to start returning rows from"
        ]


tableQueryByPKField :: TableEntry -> Maybe Type.Field
tableQueryByPKField table = do
  let pks = List.filter (\col -> col.primary_key) table.columns

  -- We filter out the rowid column, unless it is the only one
  withoutRowid <- case pks of
    [] -> Nothing
    [first] | first.isRowid -> Just [first]
    _ -> Just $ List.filter (\col -> P.not col.isRowid) pks

  let pkArguments =
        withoutRowid <&> \column -> do
          let name = doubleXEncodeGql column.column_name_gql
          Type.inputValue name $ Type.nonNull $ columnType column

  pure $
    Type.field
      (doubleXEncodeGql table.name <> "_by_pk")
      (tableRowType table)
      & Type.fieldWithDescription
        ( "Rows from the table \""
            <> table.name
            <> "\", accessible by their primary key"
        )
      & Type.withArguments pkArguments


mutationResponseType :: AccessMode -> TableEntry -> Type.IntrospectionType
mutationResponseType accessMode table = do
  let tableName = doubleXEncodeGql table.name
  let readFields =
        if canRead accessMode
          then
            pure
              $ Type.field
                "returning"
              $ Type.nonNull
              $ Type.list
              $ Type.nonNull
              $ tableRowType table
          else []

  Type.object
    (tableName <> "_mutation_response")
    ( [ Type.field "affected_rows" (Type.nonNull Type.typeInt)
      ]
        <> readFields
    )
    & Type.withDescription ("Mutation response for " <> table.name)


tableInsertField :: AccessMode -> TableEntry -> Type.Field
tableInsertField accessMode table = do
  let
    tableName = doubleXEncodeGql table.name
    fields =
      table.columns <&> \columnEntry -> do
        let colName = columnEntry.column_name_gql
        let base = columnType columnEntry
        let type_ =
              if columnEntry.isOmittable
                then Type.nonNull base
                else base
        Type.inputValue colName type_

    insertRows =
      Type.inputObject
        (tableName <> "_insert_input")
        fields
        & Type.withDescription
          ("Input object for " <> table.name)

    fieldEnumVariants =
      table.columns <&> \columnEntry ->
        Type.enumValue $ column_name_gql columnEntry

    fieldEnumType =
      Type.enum (tableName <> "_column") fieldEnumVariants
        & Type.withDescription
          "This enum contains a variant for each column in the table"

    onConflict =
      Type.inputObject
        (tableName <> "_upsert_on_conflict")
        [ Type.inputValue
            "constraint"
            (Type.nonNull $ Type.list $ Type.nonNull fieldEnumType)
            & Type.inputValueWithDescription
              "columns to handle conflicts of"
        , Type.inputValue
            "update_columns"
            (Type.nonNull $ Type.list $ Type.nonNull fieldEnumType)
            & Type.inputValueWithDescription
              "columns to override on conflict"
        , Type.inputValue "where" (filterType table)
            & Type.inputValueWithDescription
              "filter specifying which conflicting columns to update"
        ]
        & Type.withDescription
          ( "Specifies how broken UNIQUE constraints for "
              <> table.name
              <> " should be handled"
          )

  Type.field
    ("insert_" <> tableName)
    (mutationResponseType accessMode table)
    & Type.fieldWithDescription
      ("Insert new rows in table \"" <> table.name <> "\"")
    & Type.withArguments
      [ Type.inputValue
          "objects"
          (Type.nonNull $ Type.list $ Type.nonNull insertRows)
          & Type.inputValueWithDescription "Rows to be inserted"
      , Type.inputValue
          "on_conflict"
          (Type.list $ Type.nonNull onConflict)
          & Type.inputValueWithDescription
            "Specifies how to handle broken UNIQUE constraints"
      ]


tableUpdateField :: AccessMode -> TableEntry -> Type.Field
tableUpdateField accessMode table = do
  let
    tableName = doubleXEncodeGql table.name

    fields =
      table.columns <&> \columnEntry -> do
        let colName = columnEntry.column_name_gql
        let base = columnType columnEntry
        Type.inputValue colName $ Type.nonNull base

    updateRow =
      Type.inputObject
        (tableName <> "_set_input")
        fields
        & Type.withDescription
          ("Fields to set for " <> table.name)

  Type.field
    ("update_" <> tableName)
    (mutationResponseType accessMode table)
    & Type.fieldWithDescription
      ("Update rows in table \"" <> table.name <> "\"")
    & Type.withArguments
      [ Type.inputValue
          "set"
          (Type.nonNull updateRow)
          & Type.inputValueWithDescription "Fields to be updated"
      , Type.inputValue
          "filter"
          (Type.nonNull $ filterType table)
          & Type.inputValueWithDescription "Filter to select rows to be updated"
      ]


tableDeleteField :: AccessMode -> TableEntry -> Type.Field
tableDeleteField accessMode table = do
  Type.field
    ("delete_" <> doubleXEncodeGql table.name)
    (mutationResponseType accessMode table)
    & Type.fieldWithDescription
      ("Delete rows in table \"" <> table.name <> "\"")
    & Type.withArguments
      [ Type.inputValue
          "filter"
          (Type.nonNull $ filterType table)
          & Type.inputValueWithDescription "Filter to select rows to be deleted"
      ]


getSchema
  :: AccessMode
  -> [TableEntry]
  -> Type.Schema
getSchema accessMode tables = do
  let
    queryType =
      if canRead accessMode
        then
          P.fold
            [ tables <&> tableQueryField
            , tables & P.mapMaybe tableQueryByPKField
            ]
        else []

    mutationType =
      if canWrite accessMode
        then
          P.fold
            [ tables <&> tableInsertField accessMode
            , tables <&> tableUpdateField accessMode
            , tables <&> tableDeleteField accessMode
            ]
        else []

  Type.collectSchemaTypes $
    Type.Schema
      Nothing
      []
      (Type.object "Query" queryType)
      (Just $ Type.object "Mutation" mutationType)


-- We make this toplevel, because putting it inside `getSchemaResolver`
-- means haskell will evaluate it each time, which leads to each execution
-- taking 2-3s
makeSchemaResolver :: Either Text (Type.Schema -> Resolver IO)
makeSchemaResolver = do
  let schemaField = Type.field "__schema" Type.typeSchema
  ty <- makeType schemaField.type_
  let gqlField = Out.Field schemaField.description ty mempty
  pure $ \schema -> Out.ValueResolver gqlField $ pure $ toGraphQL schema


getSchemaResolver
  :: AccessMode
  -> [TableEntry]
  -> IO (HashMap Text (Resolver IO))
getSchemaResolver accessMode tables = do
  case makeSchemaResolver of
    Right make -> do
      let schema = getSchema accessMode tables
      pure $ HashMap.singleton "__schema" $ make schema
    Left err -> fail $ T.unpack err
