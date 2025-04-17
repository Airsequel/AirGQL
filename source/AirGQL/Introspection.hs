module AirGQL.Introspection (
  typeNameResolver,
  getSchemaResolver,
  tableQueryField,
  tableQueryByPKField,
  tableInsertField,
  tableUpdateField,
  tableUpdateFieldByPk,
  tableDeleteField,
  tableDeleteFieldByPK,
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
  (==),
 )
import Protolude qualified as P

import Data.HashMap.Strict as HashMap (HashMap, singleton)
import Language.GraphQL.Type.Out as Out (
  Field (Field),
  Resolver (ValueResolver),
  Type (NonNullScalarType),
 )

import AirGQL.Introspection.NamingConflict (
  encodeOutsidePKNames,
  encodeOutsideTableNames,
 )
import AirGQL.Introspection.Resolver (makeType)
import AirGQL.Introspection.Types (IntrospectionType)
import AirGQL.Introspection.Types qualified as Type
import AirGQL.Lib (
  AccessMode,
  ColumnEntry,
  GqlTypeName (full, root),
  ObjectType (Table),
  TableEntry (columns, name, object_type),
  canInsert,
  canRead,
  canWrite,
  column_name_gql,
  datatype_gql,
  getPKColumns,
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
    Type.scalar type_.root
      -- NOTE: I wonder if the fact we're generating different descriptions
      -- based off the field is ok.
      & Type.withDescription
        ("Data type for column '" <> entry.column_name_gql <> "'")


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
    & Type.withDescription "Ordering options when ordering by a column"


filterType :: TableEntry -> IntrospectionType
filterType table = do
  let
    fieldsWithComparisonExp =
      table.columns <&> \columnEntry -> do
        let colName = columnEntry.column_name_gql
        let typeName = columnTypeName columnEntry
        let type_ = columnType columnEntry

        let comparisonField name ty =
              [ Type.deprecatedInputValue "Unify naming with Hasura" $
                  Type.inputValue name ty
              , Type.inputValue ("_" <> name) ty
              ]

        Type.inputValue colName
          $ Type.withDescription ("Compare to a(n) " <> typeName)
          $ Type.inputObject
            (typeName <> "Comparison")
          $ P.fold
            [ comparisonField "eq" type_
            , comparisonField "neq" type_
            , comparisonField "gt" type_
            , comparisonField "gte" type_
            , comparisonField "lt" type_
            , comparisonField "lte" type_
            , comparisonField "like" type_
            , comparisonField "ilike" type_
            , comparisonField "in" $ Type.list type_
            , comparisonField "nin" $ Type.list type_
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
    & Type.withDescription
      ("Available columns for table \"" <> table.name <> "\"")


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
        ( Type.inputValue "filter" (filterType table)
            & Type.inputValueWithDescription "Filter to select specific rows"
            & Type.renameInputValue "where" "Unify naming with Hasura"
        )
      & Type.withArguments
        [ Type.inputValue "order_by" (Type.list orderType)
            & Type.inputValueWithDescription "Columns used to sort the data"
        , Type.inputValue "limit" Type.typeInt
            & Type.inputValueWithDescription "Limit the number of returned rows"
        , Type.inputValue "offset" Type.typeInt
            & Type.inputValueWithDescription "The index to start returning rows from"
        ]


tablePKArguments :: TableEntry -> Maybe [Type.InputValue]
tablePKArguments table = do
  pks <- getPKColumns table

  pure $
    pks <&> \column -> do
      let name = doubleXEncodeGql column.column_name_gql
      Type.inputValue name $ Type.nonNull $ columnType column


tableQueryByPKField :: [TableEntry] -> TableEntry -> Maybe Type.Field
tableQueryByPKField tables table = do
  pkArguments <- tablePKArguments table
  pure $
    Type.field
      (encodeOutsideTableNames tables $ doubleXEncodeGql table.name <> "_by_pk")
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
  let readonlyFields =
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
        <> readonlyFields
    )
    & Type.withDescription
      ( "Mutation response for table \""
          <> table.name
          <> "\""
      )


mutationByPkResponseType :: AccessMode -> TableEntry -> Type.IntrospectionType
mutationByPkResponseType accessMode table = do
  let readonlyFields =
        if canRead accessMode
          then
            pure $
              Type.field "returning" $
                tableRowType table
          else []

  Type.object
    (doubleXEncodeGql table.name <> "_mutation_by_pk_response")
    ( [ Type.field "affected_rows" (Type.nonNull Type.typeInt)
      ]
        <> readonlyFields
    )
    & Type.withDescription
      ( "Response for a PK-based mutation on table \""
          <> table.name
          <> "\""
      )


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
                then base
                else Type.nonNull base
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
    (Type.nonNull $ mutationResponseType accessMode table)
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


tableSetInput :: TableEntry -> Type.IntrospectionType
tableSetInput table =
  let
    fields =
      table.columns <&> \columnEntry -> do
        let colName = columnEntry.column_name_gql
        let base = columnType columnEntry
        Type.inputValue colName base
  in
    Type.inputObject
      (doubleXEncodeGql table.name <> "_set_input")
      fields
      & Type.withDescription
        ("Fields to set for " <> table.name)


tableUpdateField :: AccessMode -> TableEntry -> Type.Field
tableUpdateField accessMode table = do
  let tableName = doubleXEncodeGql table.name

  Type.field
    ("update_" <> tableName)
    (Type.nonNull $ mutationResponseType accessMode table)
    & Type.fieldWithDescription
      ("Update rows in table \"" <> table.name <> "\"")
    & Type.withArguments
      ( Type.inputValue
          "set"
          (tableSetInput table)
          & Type.inputValueWithDescription "Fields to be updated"
          & Type.renameInputValue "_set" "Unify naming with Hasura"
      )
    & Type.withArguments
      ( Type.inputValue
          "filter"
          (filterType table)
          & Type.inputValueWithDescription "Filter to select rows to be updated"
          & Type.renameInputValue "where" "Unify naming with Hasura"
      )


tableUpdateFieldByPk
  :: AccessMode
  -> [TableEntry]
  -> TableEntry
  -> Maybe Type.Field
tableUpdateFieldByPk accessMode tables table = do
  pkArguments <- tablePKArguments table

  pure $
    Type.field
      ( "update_"
          <> encodeOutsideTableNames
            tables
            ( doubleXEncodeGql table.name
                <> "_by_pk"
            )
      )
      (Type.nonNull $ mutationByPkResponseType accessMode table)
      & Type.fieldWithDescription
        ("Update row in table \"" <> table.name <> "\" by PK")
      & Type.withArguments pkArguments
      & Type.withArguments
        ( Type.inputValue
            (encodeOutsidePKNames table "set")
            (tableSetInput table)
            & Type.inputValueWithDescription "Fields to be updated"
            & Type.renameInputValue
              (encodeOutsidePKNames table "_set")
              "Unify naming with Hasura"
        )


tableDeleteField :: AccessMode -> TableEntry -> Type.Field
tableDeleteField accessMode table = do
  Type.field
    ("delete_" <> doubleXEncodeGql table.name)
    (Type.nonNull $ mutationResponseType accessMode table)
    & Type.fieldWithDescription
      ("Delete rows in table \"" <> table.name <> "\"")
    & Type.withArguments
      ( Type.inputValue
          "filter"
          (filterType table)
          & Type.inputValueWithDescription "Filter to select rows to be deleted"
          & Type.renameInputValue "where" "Unify naming with Hasura"
      )


tableDeleteFieldByPK
  :: AccessMode
  -> [TableEntry]
  -> TableEntry
  -> Maybe Type.Field
tableDeleteFieldByPK accessMode tables table = do
  args <- tablePKArguments table
  pure $
    Type.field
      ( "delete_"
          <> encodeOutsideTableNames
            tables
            (doubleXEncodeGql table.name <> "_by_pk")
      )
      (Type.nonNull $ mutationByPkResponseType accessMode table)
      & Type.fieldWithDescription
        ("Delete row in table \"" <> table.name <> "\" by PK")
      & Type.withArguments args


directives :: [Type.Directive]
directives =
  [ Type.directive
      "skip"
      ["INLINE_FRAGMENT", "FRAGMENT_SPREAD", "FIELD"]
      [ Type.inputValue "if" (Type.nonNull Type.typeBool)
          & Type.inputValueWithDescription "Skipped when true."
      ]
      & Type.directiveWithDescription
        "Directs the executor to skip this field or fragment \
        \when the `if` argument is true."
  , Type.directive
      "include"
      ["INLINE_FRAGMENT", "FRAGMENT_SPREAD", "FIELD"]
      [ Type.inputValue "if" (Type.nonNull Type.typeBool)
          & Type.inputValueWithDescription "Included when true."
      ]
      & Type.directiveWithDescription
        "Directs the executor to include this field or fragment \
        \only when the `if` argument is true."
  ]


getSchema
  :: AccessMode
  -> [TableEntry]
  -> Type.Schema
getSchema accessMode tables = do
  let
    queryType = do
      P.guard $ canRead accessMode
      P.fold
        [ tables <&> tableQueryField
        , tables & P.mapMaybe (tableQueryByPKField tables)
        ]

    tablesWithoutViews =
      List.filter
        (\table -> table.object_type == Table)
        tables

    insertMutations = do
      P.guard $ canInsert accessMode
      P.fold
        [ tablesWithoutViews <&> tableInsertField accessMode
        ]

    writeMutations = do
      P.guard $ canWrite accessMode
      P.fold
        [ tablesWithoutViews <&> tableUpdateField accessMode
        , tablesWithoutViews <&> tableDeleteField accessMode
        , tablesWithoutViews
            & P.mapMaybe (tableUpdateFieldByPk accessMode tables)
        , tablesWithoutViews
            & P.mapMaybe (tableDeleteFieldByPK accessMode tables)
        ]

    mutationType = insertMutations <> writeMutations

  Type.collectSchemaTypes $
    Type.Schema
      Nothing
      []
      (Type.object "Query" queryType)
      (Just $ Type.object "Mutation" mutationType)
      directives


-- We make this toplevel, because putting it inside `getSchemaResolver`
-- means haskell will evaluate it each time, which leads to each execution
-- taking 2-3 additional seconds
schemaField :: Either Text (Out.Field IO)
schemaField = do
  let field = Type.field "__schema" $ Type.nonNull Type.typeSchema
  ty <- makeType field.type_
  pure $ Out.Field field.description ty mempty


getSchemaResolver
  :: AccessMode
  -> [TableEntry]
  -> IO (HashMap Text (Resolver IO))
getSchemaResolver accessMode tables = do
  case schemaField of
    Right field -> do
      let schema = getSchema accessMode tables
      let resolver = Out.ValueResolver field $ pure $ toGraphQL schema
      pure $ HashMap.singleton "__schema" resolver
    Left err -> fail $ T.unpack err
