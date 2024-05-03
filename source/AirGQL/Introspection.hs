module AirGQL.Introspection (
  getSchemaResolver,
  typeNameResolver,
  createType,
)
where

import Protolude (
  Applicative (pure),
  Bool (False, True),
  Eq ((/=)),
  Foldable (null),
  IO,
  Int,
  IsString,
  Maybe (Just, Nothing),
  MonadReader (ask),
  Monoid (mempty),
  Num ((+)),
  Ord ((<)),
  Semigroup ((<>)),
  Text,
  concat,
  filter,
  forM,
  fromMaybe,
  not,
  ($),
  (&),
  (<&>),
  (>>=),
 )

import Data.HashMap.Strict as HashMap (
  HashMap,
  empty,
  fromList,
  lookup,
  singleton,
 )
import Database.SQLite.Simple (Connection)
import Language.GraphQL.Type (
  Value (Boolean, List, Null, Object, String),
  boolean,
  string,
 )
import Language.GraphQL.Type.In as In (Type (NamedScalarType))
import Language.GraphQL.Type.Out as Out (
  Context (values),
  Field (Field),
  Resolver (ValueResolver),
  Type (
    ListType,
    NamedObjectType,
    NamedScalarType,
    NonNullListType,
    NonNullObjectType,
    NonNullScalarType
  ),
 )

import AirGQL.GQLWrapper (
  InArgument (InArgument, argDescMb, argType, valueMb),
  OutField (OutField, arguments, descriptionMb, fieldType),
  inArgumentToArgument,
  outFieldToField,
 )
import AirGQL.Lib (
  AccessMode (ReadAndWrite, ReadOnly, WriteOnly),
  ColumnEntry,
  GqlTypeName (full),
  TableEntryRaw (name),
  column_name_gql,
  datatype_gql,
  getColumns,
  notnull,
  select_options,
 )
import AirGQL.Types.OutObjectType (
  OutObjectType (OutObjectType, descriptionMb, fields, interfaceTypes, name),
  outObjectTypeToObjectType,
 )
import DoubleXEncoding (doubleXEncodeGql)


emptyType :: Value
emptyType =
  Object $ HashMap.singleton "kind" "OBJECT"


intType :: Value
intType =
  Object $
    HashMap.fromList
      [ ("__typename", "__Type")
      , ("kind", "SCALAR")
      , ("name", "Int")
      ,
        ( "description"
        , "The `Int` scalar type represents \
          \non-fractional signed whole numeric values. \
          \Int can represent values between -(2^31) and 2^31 - 1."
        )
      ]


floatType :: Value
floatType =
  Object $
    HashMap.fromList
      [ ("__typename", "__Type")
      , ("kind", "SCALAR")
      , ("name", "Float")
      ,
        ( "description"
        , "Signed double-precision floating-point value."
        )
      ]


stringType :: Value
stringType =
  Object $
    HashMap.fromList
      [ ("__typename", "__Type")
      , ("kind", "SCALAR")
      , ("name", "String")
      ,
        ( "description"
        , "The `String` scalar type represents textual data, \
          \represented as UTF-8 character sequences. \
          \The String type is most often used by GraphQL \
          \to represent free-form human-readable text."
        )
      ]


booleanType :: Value
booleanType =
  Object $
    HashMap.fromList
      [ ("__typename", "__Type")
      , ("kind", "SCALAR")
      , ("name", "Boolean")
      ,
        ( "description"
        , "The `Boolean` scalar type represents `true` or `false`."
        )
      ]


nonNullString :: Out.Field IO
nonNullString =
  outFieldToField $
    OutField
      { descriptionMb = Just "nonNullString description"
      , fieldType = Out.NonNullScalarType string
      , arguments = HashMap.empty
      }


nullableString :: Out.Field IO
nullableString =
  Out.Field
    (Just "nullableString")
    (Out.NamedScalarType string)
    HashMap.empty


nonNullBoolean :: Out.Field IO
nonNullBoolean =
  outFieldToField $
    OutField
      { descriptionMb = Just "nonNullBoolean description"
      , fieldType = Out.NonNullScalarType boolean
      , arguments = HashMap.empty
      }


getTypeTuple :: (IsString a) => Value -> Value -> (a, Value)
getTypeTuple theKind theType =
  ( "type"
  , Object $
      HashMap.fromList
        [ ("kind", theKind)
        , ("name", theType)
        ]
  )


nonNullType :: Value -> Value
nonNullType inner =
  Object $
    HashMap.fromList
      [ ("kind", "NON_NULL")
      , ("ofType", inner)
      ]


listType :: Value -> Value
listType inner =
  Object $
    HashMap.fromList
      [ ("kind", "LIST")
      , ("ofType", inner)
      ]


createType :: Text -> Text -> [Value] -> [Text] -> Text -> Value
createType rootName description args nestedTypes name =
  let
    createChildType :: [Text] -> Text -> Value
    createChildType nestedChildTypes childName =
      case nestedChildTypes of
        [] -> Null
        (childHeadKind : childRestKinds) ->
          if not $ null childRestKinds
            then
              Object $
                HashMap.fromList
                  [ ("kind", String childHeadKind)
                  , ("ofType", createChildType childRestKinds childName)
                  ]
            else
              Object $
                HashMap.fromList
                  [ ("kind", String childHeadKind)
                  , ("name", String name)
                  ]
  in
    case nestedTypes of
      [] -> Null
      kinds ->
        Object $
          HashMap.fromList
            ( [ ("name", String rootName)
              , ("description", String description)
              , ("type", createChildType kinds name)
              ]
                <> if null args then [] else [("args", List args)]
            )


createField :: Text -> Maybe Text -> Value -> Value
createField name descriptionMb type_ =
  Object $
    HashMap.fromList
      [ ("name", String name)
      , ("type", type_)
      ]
      <> case descriptionMb of
        Nothing -> mempty
        Just description ->
          HashMap.singleton
            "description"
            (String description)


nameField :: Value
nameField =
  Object $
    HashMap.fromList
      [ ("name", "name")
      ,
        ( "type"
        , nonNullType $
            Object $
              HashMap.fromList
                [ ("kind", "SCALAR")
                , ("name", "String")
                ]
        )
      ]


descriptionField :: Value
descriptionField =
  Object $
    HashMap.fromList
      [ ("name", "description")
      , getTypeTuple "SCALAR" "String"
      ]


argsFieldValue :: Value
argsFieldValue =
  Object $
    HashMap.fromList
      [ ("name", "args")
      ,
        ( "type"
        , nonNullType $
            listType $
              nonNullType $
                Object $
                  HashMap.fromList
                    [ ("kind", "OBJECT")
                    , ("name", "__InputValue")
                    ]
        )
      ]


locationsFieldValue :: Value
locationsFieldValue =
  Object $
    HashMap.fromList
      [ ("name", "locations")
      ,
        ( "type"
        , nonNullType $
            listType $
              nonNullType $
                Object $
                  HashMap.fromList
                    [ ("kind", "ENUM")
                    , ("name", "__DirectiveLocation")
                    ]
        )
      ]


typeFieldValue :: Value
typeFieldValue =
  Object $
    HashMap.fromList
      [ ("name", "type")
      ,
        ( "type"
        , nonNullType $
            Object $
              HashMap.fromList
                [ ("kind", "OBJECT")
                , ("name", "__Type")
                ]
        )
      ]


isDeprecatedFieldValue :: Value
isDeprecatedFieldValue =
  Object $
    HashMap.fromList
      [ ("name", "isDeprecated")
      ,
        ( "type"
        , nonNullType $
            Object $
              HashMap.fromList
                [ ("kind", "SCALAR")
                , ("name", "Boolean")
                ]
        )
      ]


typeType :: Int -> Out.Type IO
typeType level =
  Out.NamedObjectType $
    outObjectTypeToObjectType $
      OutObjectType
        { name = "__Type"
        , descriptionMb = Just "__Type description"
        , interfaceTypes = []
        , fields =
            HashMap.fromList $
              [
                ( "__typename"
                , ValueResolver nonNullString $ pure "__Type"
                )
              ,
                ( "kind"
                , ValueResolver nonNullString $ do
                    context <- ask
                    case context & Out.values of
                      Object obj ->
                        pure $
                          fromMaybe "ERROR: kind" $
                            HashMap.lookup "kind" obj
                      _ -> pure "ERROR: kind"
                )
              ,
                ( "name"
                , ValueResolver nullableString $ do
                    context <- ask
                    case context & Out.values of
                      Object obj ->
                        pure $
                          fromMaybe Null $
                            HashMap.lookup "name" obj
                      _ -> pure Null
                )
              ,
                ( "description"
                , ValueResolver nullableString $ do
                    context <- ask
                    case context & Out.values of
                      Object obj ->
                        pure $
                          fromMaybe Null $
                            HashMap.lookup "description" obj
                      _ -> pure Null
                )
              ,
                ( "fields"
                , ValueResolver fieldsField $ do
                    context <- ask
                    case context & Out.values of
                      Object obj ->
                        pure $
                          fromMaybe Null $
                            HashMap.lookup "fields" obj
                      _ -> pure Null
                )
              ,
                ( "possibleTypes"
                , ValueResolver typesField $ do
                    context <- ask
                    case context & Out.values of
                      Object obj ->
                        pure $
                          fromMaybe Null $
                            HashMap.lookup "possibleTypes" obj
                      _ -> pure Null
                )
              ,
                ( "interfaces"
                , ValueResolver typesField $ do
                    context <- ask
                    case context & Out.values of
                      Object obj ->
                        pure $
                          fromMaybe (List []) $
                            HashMap.lookup "interfaces" obj
                      _ -> pure $ List []
                )
              ,
                ( "inputFields"
                , ValueResolver inputsFieldsField $ do
                    context <- ask
                    case context & Out.values of
                      Object obj ->
                        pure $
                          fromMaybe Null $
                            HashMap.lookup "inputFields" obj
                      _ -> pure Null
                )
              ,
                ( "enumValues"
                , ValueResolver enumValuesField $ do
                    context <- ask
                    case context & Out.values of
                      Object obj ->
                        pure $
                          fromMaybe Null $
                            HashMap.lookup "enumValues" obj
                      _ -> pure Null
                )
              ]
                <> ( if level < 7
                      then
                        [
                          ( "ofType"
                          , ValueResolver (typeField $ level + 1) $ do
                              context <- ask
                              case context & Out.values of
                                Object obj ->
                                  pure $
                                    fromMaybe Null $
                                      HashMap.lookup "ofType" obj
                                _ -> pure Null
                          )
                        ]
                      else []
                   )
        }


typeField :: Int -> Field IO
typeField level =
  outFieldToField $
    OutField
      { descriptionMb = Just "typeField description"
      , fieldType = typeType level
      , arguments = HashMap.empty
      }


typesField :: Field IO
typesField =
  outFieldToField $
    OutField
      { descriptionMb = Just "typesField description"
      , fieldType = Out.ListType $ typeType 0
      , arguments = HashMap.empty
      }


inputValueType :: Out.Type IO
inputValueType =
  Out.NonNullObjectType $
    outObjectTypeToObjectType $
      OutObjectType
        { name = "__InputValue"
        , descriptionMb = Just "__InputValue description"
        , interfaceTypes = []
        , fields =
            HashMap.fromList
              [
                ( "__typename"
                , ValueResolver nonNullString $ pure "__InputValue"
                )
              ,
                ( "name"
                , ValueResolver nonNullString $ do
                    context <- ask
                    case context & Out.values of
                      Object obj ->
                        pure $
                          fromMaybe "ERROR: name" $
                            HashMap.lookup "name" obj
                      _ -> pure "ERROR: name"
                )
              ,
                ( "description"
                , ValueResolver nullableString $ do
                    context <- ask
                    case context & Out.values of
                      Object obj ->
                        pure $
                          fromMaybe Null $
                            HashMap.lookup "description" obj
                      _ -> pure Null
                )
              ,
                ( "defaultValue"
                , ValueResolver nullableString $ do
                    context <- ask
                    case context & Out.values of
                      Object obj ->
                        pure $
                          fromMaybe Null $
                            HashMap.lookup "defaultValue" obj
                      _ -> pure Null
                )
              ,
                ( "type"
                , ValueResolver (typeField 0) $ do
                    context <- ask
                    case context & Out.values of
                      Object obj ->
                        pure $
                          fromMaybe emptyType $
                            HashMap.lookup "type" obj
                      _ -> pure emptyType
                )
              ]
        }


argsField :: Field IO
argsField =
  outFieldToField $
    OutField
      { descriptionMb = Just "argsField description"
      , fieldType = Out.NonNullListType inputValueType
      , arguments = HashMap.empty
      }


inputsFieldsField :: Field IO
inputsFieldsField =
  outFieldToField $
    OutField
      { descriptionMb = Just "inputsFieldsField description"
      , fieldType = Out.ListType inputValueType
      , arguments = HashMap.empty
      }


enumValuesType :: Out.Type IO
enumValuesType =
  Out.ListType $
    Out.NonNullObjectType $
      outObjectTypeToObjectType $
        OutObjectType
          { name = "__EnumValue"
          , descriptionMb = Just "__EnumValue description"
          , interfaceTypes = []
          , fields =
              HashMap.fromList
                [
                  ( "__typename"
                  , ValueResolver nonNullString $ pure "__EnumValue"
                  )
                ,
                  ( "name"
                  , ValueResolver nonNullString $ do
                      context <- ask
                      case context & Out.values of
                        Object obj ->
                          pure $
                            fromMaybe "ERROR: name" $
                              HashMap.lookup "name" obj
                        _ -> pure "ERROR: name"
                  )
                ,
                  ( "description"
                  , ValueResolver nullableString $ do
                      context <- ask
                      case context & Out.values of
                        Object obj ->
                          pure $
                            fromMaybe Null $
                              HashMap.lookup "description" obj
                        _ -> pure Null
                  )
                ,
                  ( "isDeprecated"
                  , ValueResolver nonNullBoolean $ do
                      context <- ask
                      case context & Out.values of
                        Object obj ->
                          pure $
                            fromMaybe (Boolean False) $
                              HashMap.lookup "isDeprecated" obj
                        _ -> pure $ Boolean False
                  )
                ,
                  ( "deprecationReason"
                  , ValueResolver nullableString $ do
                      context <- ask
                      case context & Out.values of
                        Object obj ->
                          pure $
                            fromMaybe Null $
                              HashMap.lookup "deprecationReason" obj
                        _ -> pure Null
                  )
                ]
          }


enumValuesField :: Field IO
enumValuesField =
  outFieldToField $
    OutField
      { descriptionMb = Just "enumValuesField description"
      , fieldType = enumValuesType
      , arguments =
          HashMap.fromList
            [
              ( "includeDeprecated"
              , inArgumentToArgument $
                  InArgument
                    { argDescMb = Just "includeDeprecated description"
                    , argType = In.NamedScalarType boolean
                    , valueMb = Just $ Boolean True
                    }
              )
            ]
      }


queryTypeType :: Field IO
queryTypeType =
  outFieldToField $
    OutField
      { descriptionMb = Just "Provides the queryType"
      , fieldType = typeType 0
      , arguments = HashMap.empty
      }


mutationTypeType :: Field IO
mutationTypeType =
  outFieldToField $
    OutField
      { descriptionMb = Just "Provides the mutationType"
      , fieldType = typeType 0
      , arguments = HashMap.empty
      }


subscriptionTypeType :: Field IO
subscriptionTypeType =
  outFieldToField $
    OutField
      { descriptionMb = Just "Provides the subscriptionType"
      , fieldType = typeType 0
      , arguments = HashMap.empty
      }


fieldsTypeOutput :: Out.Type IO
fieldsTypeOutput =
  Out.ListType $
    Out.NonNullObjectType $
      outObjectTypeToObjectType $
        OutObjectType
          { name = "__Field"
          , descriptionMb = Just "__Field description"
          , interfaceTypes = []
          , fields =
              HashMap.fromList
                [
                  ( "__typename"
                  , ValueResolver nonNullString $ pure "__Field"
                  )
                ,
                  ( "name"
                  , ValueResolver nonNullString $ do
                      context <- ask
                      case context & Out.values of
                        Object obj ->
                          pure $
                            fromMaybe "ERROR: name" $
                              HashMap.lookup "name" obj
                        _ -> pure "ERROR: name"
                  )
                ,
                  ( "description"
                  , ValueResolver nullableString $ do
                      context <- ask
                      case context & Out.values of
                        Object obj ->
                          pure $
                            fromMaybe Null $
                              HashMap.lookup "description" obj
                        _ -> pure Null
                  )
                ,
                  ( "args"
                  , ValueResolver argsField $ do
                      context <- ask
                      case context & Out.values of
                        Object obj ->
                          pure $
                            fromMaybe (List []) $
                              HashMap.lookup "args" obj
                        _ -> pure $ List []
                  )
                ,
                  ( "type"
                  , ValueResolver (typeField 0) $ do
                      context <- ask
                      case context & Out.values of
                        Object obj ->
                          pure $
                            fromMaybe emptyType $
                              HashMap.lookup "type" obj
                        _ -> pure emptyType
                  )
                ,
                  ( "isDeprecated"
                  , ValueResolver nonNullBoolean $ do
                      context <- ask
                      case context & Out.values of
                        Object obj ->
                          pure $
                            fromMaybe (Boolean False) $
                              HashMap.lookup "isDeprecated" obj
                        _ -> pure $ Boolean False
                  )
                ,
                  ( "deprecationReason"
                  , ValueResolver nullableString $ do
                      context <- ask
                      case context & Out.values of
                        Object obj ->
                          pure $
                            fromMaybe Null $
                              HashMap.lookup "deprecationReason" obj
                        _ -> pure Null
                  )
                ]
          }


fieldsField :: Field IO
fieldsField =
  outFieldToField $
    OutField
      { descriptionMb = Just "The fields type"
      , fieldType = fieldsTypeOutput
      , arguments =
          HashMap.fromList
            [
              ( "includeDeprecated"
              , inArgumentToArgument $
                  InArgument
                    { argDescMb = Just "includeDeprecated description"
                    , argType = In.NamedScalarType boolean
                    , valueMb = Just $ Boolean True
                    }
              )
            ]
      }


directivesType :: Field IO
directivesType =
  let
    directivesTypeOutput :: Out.Type IO
    directivesTypeOutput =
      Out.ListType $
        Out.NonNullObjectType $
          outObjectTypeToObjectType $
            OutObjectType
              { name = "__Directive"
              , descriptionMb = Just "__Directive description"
              , interfaceTypes = []
              , fields =
                  HashMap.fromList
                    [
                      ( "__typename"
                      , ValueResolver nullableString $ pure "__Directive"
                      )
                    ,
                      ( "name"
                      , ValueResolver nonNullString $ do
                          context <- ask
                          case context & Out.values of
                            Object obj ->
                              pure $
                                fromMaybe "ERROR: name" $
                                  HashMap.lookup "name" obj
                            _ -> pure "ERROR: name"
                      )
                    ,
                      ( "description"
                      , ValueResolver nullableString $ do
                          context <- ask
                          case context & Out.values of
                            Object obj ->
                              pure $
                                fromMaybe Null $
                                  HashMap.lookup "description" obj
                            _ -> pure Null
                      )
                    ,
                      ( "locations"
                      , let
                          locationsTypeName :: Field m
                          locationsTypeName =
                            Out.Field
                              (Just "locationsTypeName name")
                              (Out.ListType $ Out.NonNullScalarType string)
                              HashMap.empty
                        in
                          ValueResolver locationsTypeName $ do
                            context <- ask
                            case context & Out.values of
                              Object obj ->
                                pure $
                                  fromMaybe (List []) $
                                    HashMap.lookup "locations" obj
                              _ -> pure $ List []
                      )
                    ,
                      ( "args"
                      , ValueResolver argsField $ do
                          context <- ask
                          case context & Out.values of
                            Object obj ->
                              pure $
                                fromMaybe (List []) $
                                  HashMap.lookup "args" obj
                            _ -> pure $ List []
                      )
                    ]
              }
  in
    outFieldToField $
      OutField
        { descriptionMb = Just "Provides the directivesType"
        , fieldType = directivesTypeOutput
        , arguments = HashMap.empty
        }


filterType :: Bool -> Text -> Value
filterType isRequired tableName =
  let
    filterObj =
      Object $
        HashMap.fromList
          [ ("kind", "INPUT_OBJECT")
          , ("name", String $ doubleXEncodeGql tableName <> "_filter")
          ,
            ( "description"
            , "Select rows matching the provided filter object"
            )
          ]
  in
    if isRequired
      then nonNullType filterObj
      else filterObj


getFieldsForQuery :: Text -> Value
getFieldsForQuery tableName =
  createType
    (doubleXEncodeGql tableName)
    ("Rows from the table \"" <> tableName <> "\"")
    [ Object $
        HashMap.fromList
          [ ("name", "filter")
          , ("description", "Filter to select specific rows")
          , ("type", filterType False tableName)
          ]
    , Object $
        HashMap.fromList
          [ ("name", "order_by")
          , ("description", "Columns used to sort the data")
          ,
            ( "type"
            , listType $
                Object $
                  HashMap.fromList
                    [ ("kind", "INPUT_OBJECT")
                    ,
                      ( "name"
                      , String $
                          doubleXEncodeGql tableName
                            <> "_order_by"
                      )
                    ]
            )
          ]
    , Object $
        HashMap.fromList
          [ ("name", "limit")
          , ("description", "Limit the number of returned rows")
          , ("type", intType)
          ]
    , Object $
        HashMap.fromList
          [ ("name", "offset")
          , ("description", "The index to start returning rows from")
          , ("type", intType)
          ]
    ]
    ["NON_NULL", "LIST", "NON_NULL", "OBJECT"]
    (doubleXEncodeGql tableName <> "_row")


getFieldsForMutation :: Text -> [Value]
getFieldsForMutation tableName =
  [ Object $
      HashMap.fromList
        [ ("name", String $ "insert_" <> doubleXEncodeGql tableName)
        ,
          ( "description"
          , String $
              "Insert new rows in table \"" <> tableName <> "\""
          )
        ,
          ( "args"
          , List
              [ createField
                  "objects"
                  (Just "Rows to be inserted")
                  $ nonNullType
                  $ listType
                  $ nonNullType
                  $ Object
                  $ HashMap.fromList
                    [ ("kind", "INPUT_OBJECT")
                    ,
                      ( "name"
                      , String $
                          doubleXEncodeGql
                            tableName
                            <> "_insert_input"
                      )
                    ]
              , createField
                  "on_conflict"
                  (Just "Specifies how to handle broken UNIQUE constraints")
                  $ listType
                  $ nonNullType
                  $ Object
                  $ HashMap.fromList
                    [ ("kind", "INPUT_OBJECT")
                    ,
                      ( "name"
                      , String $
                          doubleXEncodeGql
                            tableName
                            <> "_upsert_on_conflict"
                      )
                    ]
              ]
          )
        ,
          ( "type"
          , nonNullType $
              Object $
                HashMap.fromList
                  [ ("kind", "OBJECT")
                  ,
                    ( "name"
                    , String $
                        doubleXEncodeGql tableName
                          <> "_mutation_response"
                    )
                  ]
          )
        ]
  , Object $
      HashMap.fromList
        [ ("name", String $ "update_" <> doubleXEncodeGql tableName)
        ,
          ( "description"
          , String $
              "Update rows in table \"" <> tableName <> "\""
          )
        ,
          ( "args"
          , List
              [ Object $
                  HashMap.fromList
                    [ ("name", "filter")
                    , ("description", "Filter to select rows to be updated")
                    , ("type", filterType True tableName)
                    ]
              , Object $
                  HashMap.fromList
                    [ ("name", "set")
                    , ("description", "Fields to be updated")
                    ,
                      ( "type"
                      , nonNullType $
                          Object $
                            HashMap.fromList
                              [ ("kind", "INPUT_OBJECT")
                              ,
                                ( "name"
                                , String $
                                    doubleXEncodeGql tableName
                                      <> "_set_input"
                                )
                              ]
                      )
                    ]
              ]
          )
        ,
          ( "type"
          , nonNullType $
              Object $
                HashMap.fromList
                  [ ("kind", "OBJECT")
                  ,
                    ( "name"
                    , String $
                        doubleXEncodeGql tableName
                          <> "_mutation_response"
                    )
                  ]
          )
        ]
  , Object $
      HashMap.fromList
        [ ("name", String $ "delete_" <> doubleXEncodeGql tableName)
        ,
          ( "description"
          , String $ "Delete rows in table \"" <> tableName <> "\""
          )
        ,
          ( "args"
          , List
              [ Object $
                  HashMap.fromList
                    [ ("name", "filter")
                    , ("description", "Filter to select rows to be deleted")
                    , ("type", filterType True tableName)
                    ]
              ]
          )
        ,
          ( "type"
          , nonNullType $
              Object $
                HashMap.fromList
                  [ ("kind", "OBJECT")
                  ,
                    ( "name"
                    , String $
                        doubleXEncodeGql tableName
                          <> "_mutation_response"
                    )
                  ]
          )
        ]
  ]


makeComparisonType :: Text -> Text -> Value -> Value
makeComparisonType typeName description type_ =
  let field fieldName = createField fieldName Nothing type_
  in  Object $
        HashMap.fromList
          [ ("__typename", "__Type")
          , ("kind", "INPUT_OBJECT")
          , ("name", String typeName)
          ,
            ( "description"
            , String description
            )
          ,
            ( "inputFields"
            , List
                [ field "eq"
                , field "neq"
                , field "gt"
                , field "gte"
                , field "lt"
                , field "lte"
                , field "like"
                , field "ilike"
                , createField
                    "in"
                    Nothing
                    (listType type_)
                , createField
                    "nin"
                    Nothing
                    (listType type_)
                ]
            )
          ]


comparisonTypes :: AccessMode -> [Value]
comparisonTypes accessMode =
  case accessMode of
    ReadOnly -> []
    _ ->
      [ makeComparisonType "IntComparison" "Compare to an Int" intType
      , makeComparisonType "FloatComparison" "Compare to a Float" floatType
      , makeComparisonType "StringComparison" "Compare to a String" stringType
      , makeComparisonType "BooleanComparison" "Compare to a Boolean" booleanType
      ]


orderingTermType :: Value
orderingTermType =
  Object $
    HashMap.fromList
      [ ("__typename", "__Type")
      , ("kind", "ENUM")
      , ("name", String "OrderingTerm")
      ,
        ( "description"
        , String "Ordering options when ordering by a column"
        )
      ,
        ( "enumValues"
        , List
            [ Object $
                HashMap.fromList
                  [ ("name", "ASC")
                  , ("description", "In ascending order")
                  ]
            , Object $
                HashMap.fromList
                  [ ("name", "asc")
                  , ("description", "In ascending order")
                  , ("isDeprecated", Boolean True)
                  ,
                    ( "deprecationReason"
                    , String "GraphQL spec recommends all caps for enums"
                    )
                  ]
            , Object $
                HashMap.fromList
                  [ ("name", "DESC")
                  , ("description", "In descending order")
                  ]
            , Object $
                HashMap.fromList
                  [ ("name", "desc")
                  , ("description", "In descending order")
                  , ("isDeprecated", Boolean True)
                  ,
                    ( "deprecationReason"
                    , String "GraphQL spec recommends all caps for enums"
                    )
                  ]
            ]
        )
      ]


getFullDatatype :: ColumnEntry -> Text
getFullDatatype entry = case entry.datatype_gql of
  -- TODO: Should be "Any", but that's not a valid GraphQL type
  Nothing -> "String"
  Just type_ -> type_.full


getSchemaFieldOutput
  :: Text
  -> Connection
  -> AccessMode
  -> [TableEntryRaw]
  -> IO (Out.Type IO)
getSchemaFieldOutput dbId conn accessMode tables = do
  typesForTables <- forM tables $ \table -> do
    columns <- getColumns dbId conn table.name
    fields <- forM columns $ \columnEntry -> do
      let colName = columnEntry.column_name_gql
      pure $
        createType
          colName
          "" -- TODO: Reactivate description when user can specify it
          [] -- No arguments
          ( if columnEntry.notnull
              then ["NON_NULL", "SCALAR"]
              else ["SCALAR"]
          )
          (getFullDatatype columnEntry)

    fieldsNullable <- forM columns $ \columnEntry -> do
      let colName = columnEntry.column_name_gql
      pure $
        createType
          colName
          "" -- TODO: Reactivate description when user can specify it
          [] -- No arguments
          ["SCALAR"]
          (getFullDatatype columnEntry)

    fieldsWithComparisonExp <- forM columns $ \columnEntry -> do
      let colName = columnEntry.column_name_gql
      pure $
        createType
          colName
          "" -- TODO: Reactivate description when user can specify it
          [] -- No arguments
          ["INPUT_OBJECT"]
          (getFullDatatype columnEntry <> "Comparison")

    fieldsWithOrderingTerm <- forM columns $ \columnEntry -> do
      let colName = columnEntry.column_name_gql
      pure $
        createType
          colName
          "" -- TODO: Reactivate description when user can specify it
          [] -- No arguments
          ["INPUT_OBJECT"]
          "OrderingTerm"

    let
      customRowTypes =
        columns
          >>= \columnEntry ->
            case (columnEntry.select_options, columnEntry.datatype_gql) of
              (Just _, Just name) ->
                let
                  colName = columnEntry.column_name_gql
                  typeName = name.full
                  description = "Data type for column " <> colName
                  rowType =
                    Object $
                      HashMap.fromList
                        [ ("__typename", "__Type")
                        , ("kind", "SCALAR")
                        , ("name", String typeName)
                        , ("description", String description)
                        ]

                  comparisonType =
                    makeComparisonType
                      (typeName <> "Comparison")
                      ("Compare with values for column" <> colName)
                      ( Object $
                          HashMap.fromList
                            [ ("kind", "SCALAR")
                            , ("name", String typeName)
                            , ("description", String description)
                            ]
                      )
                in
                  [rowType, comparisonType]
              _ -> []

      fieldEnumVariants =
        columns
          <&> \columnEntry ->
            Object $
              HashMap.singleton "name" $
                String $
                  column_name_gql columnEntry

      fieldEnumDescription =
        "This enum contains a variant for each column in the table" :: Value

      fieldEnumType =
        Object $
          HashMap.fromList
            [ ("__typename", "__Type")
            , ("kind", "ENUM")
            , ("name", String $ doubleXEncodeGql table.name <> "_column")
            ,
              ( "description"
              , fieldEnumDescription
              )
            , ("enumValues", List fieldEnumVariants)
            ]

      fieldEnumTypeReference =
        Object $
          HashMap.fromList
            [ ("kind", "INPUT_OBJECT")
            , ("name", String $ doubleXEncodeGql table.name <> "_column")
            ,
              ( "description"
              , fieldEnumDescription
              )
            ]

      requiresWrite obj = case accessMode of
        ReadOnly -> Null
        WriteOnly -> obj
        ReadAndWrite -> obj
      requiresRead obj = case accessMode of
        ReadOnly -> Null
        WriteOnly -> obj
        ReadAndWrite -> obj

    pure $
      customRowTypes
        <> [ Object $
              HashMap.fromList
                [ ("__typename", "__Type")
                , ("kind", "OBJECT")
                , ("name", String $ doubleXEncodeGql table.name <> "_row")
                ,
                  ( "description"
                  , String $
                      "Available columns for table \""
                        <> table.name
                        <> "\""
                  )
                , ("fields", List fields)
                ]
           , requiresWrite $
              Object $
                HashMap.fromList
                  [ ("__typename", "__Type")
                  , ("kind", "OBJECT")
                  ,
                    ( "name"
                    , String $
                        doubleXEncodeGql table.name <> "_mutation_response"
                    )
                  ,
                    ( "description"
                    , String $ "Mutation response for " <> table.name
                    )
                  ,
                    ( "fields"
                    , List
                        [ Object $
                            HashMap.fromList
                              [ ("name", "affected_rows")
                              ,
                                ( "type"
                                , Object $
                                    HashMap.fromList
                                      [ ("kind", "NON_NULL")
                                      ,
                                        ( "ofType"
                                        , Object $
                                            HashMap.fromList
                                              [ ("kind", "SCALAR")
                                              , ("name", "Int")
                                              ]
                                        )
                                      ]
                                )
                              ]
                        , requiresRead $
                            Object $
                              HashMap.fromList
                                [ ("name", "returning")
                                ,
                                  ( "type"
                                  , Object $
                                      HashMap.fromList
                                        [ ("kind", "NON_NULL")
                                        ,
                                          ( "ofType"
                                          , Object $
                                              HashMap.fromList
                                                [ ("kind", "LIST")
                                                ,
                                                  ( "ofType"
                                                  , Object $
                                                      HashMap.fromList
                                                        [ ("kind", "NON_NULL")
                                                        ,
                                                          ( "ofType"
                                                          , Object $
                                                              HashMap.fromList
                                                                [ ("kind", "OBJECT")
                                                                ,
                                                                  ( "name"
                                                                  , String $ doubleXEncodeGql table.name <> "_row"
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                        ]
                    )
                  ]
           , requiresWrite $
              Object $
                HashMap.fromList
                  [ ("__typename", "__Type")
                  , ("kind", "INPUT_OBJECT")
                  ,
                    ( "name"
                    , String $ doubleXEncodeGql table.name <> "_insert_input"
                    )
                  ,
                    ( "description"
                    , String $ "Input object for " <> table.name
                    )
                  , ("inputFields", List fields)
                  ]
           , fieldEnumType
           , requiresWrite $
              Object $
                HashMap.fromList
                  [ ("__typename", "__Type")
                  , ("kind", "INPUT_OBJECT")
                  ,
                    ( "name"
                    , String $ doubleXEncodeGql table.name <> "_upsert_on_conflict"
                    )
                  ,
                    ( "description"
                    , String $ "Specifies how broken UNIQUE constraints for " <> table.name <> " should be handled"
                    )
                  ,
                    ( "inputFields"
                    , List
                        [ createField
                            "constraint"
                            (Just "columns to handle conflicts of")
                            $ nonNullType
                            $ listType
                            $ nonNullType fieldEnumTypeReference
                        , createField
                            "update_columns"
                            (Just "columns to override on conflict")
                            $ nonNullType
                            $ listType
                            $ nonNullType fieldEnumTypeReference
                        , createField
                            "where"
                            (Just "filter specifying which conflicting columns to update")
                            (filterType False table.name)
                        ]
                    )
                  ]
           , requiresWrite $
              Object $
                HashMap.fromList
                  [ ("__typename", "__Type")
                  , ("kind", "INPUT_OBJECT")
                  ,
                    ( "name"
                    , String $ doubleXEncodeGql table.name <> "_set_input"
                    )
                  ,
                    ( "description"
                    , String $ "Fields to set for " <> table.name
                    )
                  , ("inputFields", List fieldsNullable)
                  ]
           , requiresWrite $
              Object $
                HashMap.fromList
                  [ ("__typename", "__Type")
                  , ("kind", "INPUT_OBJECT")
                  ,
                    ( "name"
                    , String $ doubleXEncodeGql table.name <> "_filter"
                    )
                  ,
                    ( "description"
                    , String "Filter object to select rows"
                    )
                  , ("inputFields", List fieldsWithComparisonExp)
                  ]
           , requiresWrite $
              Object $
                HashMap.fromList
                  [ ("__typename", "__Type")
                  , ("kind", "INPUT_OBJECT")
                  ,
                    ( "name"
                    , String $ doubleXEncodeGql table.name <> "_order_by"
                    )
                  ,
                    ( "description"
                    , String $
                        "Ordering options when selecting data from \""
                          <> table.name
                          <> "\"."
                    )
                  , ("inputFields", List fieldsWithOrderingTerm)
                  ]
           ]

  let
    queryTypeObj =
      Object $
        HashMap.fromList
          [ ("__typename", "__Type")
          , ("kind", "OBJECT")
          , ("name", "Query")
          ,
            ( "fields"
            , List $
                tables
                  <&> AirGQL.Lib.name
                  <&> getFieldsForQuery
            )
          ]
    mutationTypeObj =
      Object $
        HashMap.fromList
          [ ("__typename", "__Type")
          , ("kind", "OBJECT")
          , ("name", "Mutation")
          ,
            ( "fields"
            , List $
                tables
                  <&> AirGQL.Lib.name
                  <&> getFieldsForMutation
                  & concat
            )
          ]

  pure $
    Out.NonNullObjectType $
      outObjectTypeToObjectType $
        OutObjectType
          { name = "__Schema"
          , descriptionMb = Just "__Schema description"
          , interfaceTypes = []
          , fields =
              HashMap.fromList
                [
                  ( "__typename"
                  , ValueResolver nonNullString $ pure "__Schema"
                  )
                ,
                  ( "queryType"
                  , ValueResolver queryTypeType $ pure queryTypeObj
                  )
                ,
                  ( "mutationType"
                  , case accessMode of
                      ReadOnly -> ValueResolver mutationTypeType $ pure Null
                      WriteOnly ->
                        ValueResolver mutationTypeType $ pure mutationTypeObj
                      ReadAndWrite ->
                        ValueResolver mutationTypeType $ pure mutationTypeObj
                  )
                ,
                  ( "subscriptionType"
                  , -- AirGQL doesn't support Subscriptions yet
                    ValueResolver subscriptionTypeType $ pure Null
                  )
                ,
                  ( "types"
                  , ValueResolver typesField $
                      pure $
                        List $
                          concat typesForTables
                            <> comparisonTypes accessMode
                            <> [orderingTermType]
                            <> [ queryTypeObj
                               , case accessMode of
                                  ReadOnly -> Null
                                  WriteOnly -> mutationTypeObj
                                  ReadAndWrite -> mutationTypeObj
                               , booleanType
                               , intType
                               , floatType
                               , stringType
                               , Object $
                                  HashMap.fromList
                                    [ ("__typename", "__Type")
                                    , ("kind", "SCALAR")
                                    , ("name", "ID")
                                    ,
                                      ( "description"
                                      , "The `ID` scalar type represents a unique identifier, \
                                        \often used to refetch an object or as key for a cache. \
                                        \The ID type appears in a JSON response as a String; \
                                        \however, it is not intended to be human-readable. \
                                        \When expected as an input type, any string \
                                        \(such as `\"4\"`) or integer (such as `4`) input value \
                                        \will be accepted as an ID."
                                      )
                                    ]
                               , Object $
                                  HashMap.fromList
                                    [ ("__typename", "__Type")
                                    , ("kind", "SCALAR")
                                    , ("name", "Upload")
                                    ,
                                      ( "description"
                                      , "The `Upload` scalar type represents a file upload."
                                      )
                                    ]
                               , Object $
                                  HashMap.fromList
                                    [ ("__typename", "__Type")
                                    , ("kind", "OBJECT")
                                    , ("name", "__Schema")
                                    ,
                                      ( "description"
                                      , "A GraphQL Schema defines the capabilities of a GraphQL server. \
                                        \It exposes all available types and directives on the server, \
                                        \as well as the entry points for \
                                        \query, mutation, and subscription operations."
                                      )
                                    ,
                                      ( "fields"
                                      , List
                                          [ Object $
                                              HashMap.fromList
                                                [ ("name", "types")
                                                ,
                                                  ( "description"
                                                  , "A list of all types supported by this server."
                                                  )
                                                ,
                                                  ( "type"
                                                  , Object $
                                                      HashMap.fromList
                                                        [ ("kind", "NON_NULL")
                                                        ,
                                                          ( "ofType"
                                                          , Object $
                                                              HashMap.fromList
                                                                [ ("kind", "LIST")
                                                                ,
                                                                  ( "ofType"
                                                                  , Object $
                                                                      HashMap.fromList
                                                                        [ ("kind", "NON_NULL")
                                                                        ,
                                                                          ( "ofType"
                                                                          , Object $
                                                                              HashMap.fromList
                                                                                [ ("kind", "OBJECT")
                                                                                , ("name", "__Type")
                                                                                ]
                                                                          )
                                                                        ]
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "queryType")
                                                ,
                                                  ( "description"
                                                  , "The type that query operations will be rooted at."
                                                  )
                                                ,
                                                  ( "type"
                                                  , Object $
                                                      HashMap.fromList
                                                        [ ("kind", "NON_NULL")
                                                        ,
                                                          ( "ofType"
                                                          , Object $
                                                              HashMap.fromList
                                                                [ ("kind", "OBJECT")
                                                                , ("name", "__Type")
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "mutationType")
                                                ,
                                                  ( "description"
                                                  , "If this server supports mutation, the type \
                                                    \that mutation operations will be rooted at."
                                                  )
                                                , getTypeTuple "OBJECT" "__Type"
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "subscriptionType")
                                                ,
                                                  ( "description"
                                                  , "If this server support subscription, the type \
                                                    \that subscription operations will be rooted at."
                                                  )
                                                , getTypeTuple "OBJECT" "__Type"
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "directives")
                                                ,
                                                  ( "description"
                                                  , "A list of all directives supported by this server."
                                                  )
                                                ,
                                                  ( "type"
                                                  , Object $
                                                      HashMap.fromList
                                                        [ ("kind", "NON_NULL")
                                                        ,
                                                          ( "ofType"
                                                          , Object $
                                                              HashMap.fromList
                                                                [ ("kind", "LIST")
                                                                ,
                                                                  ( "ofType"
                                                                  , Object $
                                                                      HashMap.fromList
                                                                        [ ("kind", "NON_NULL")
                                                                        ,
                                                                          ( "ofType"
                                                                          , Object $
                                                                              HashMap.fromList
                                                                                [ ("kind", "OBJECT")
                                                                                , ("name", "__Directive")
                                                                                ]
                                                                          )
                                                                        ]
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          ]
                                      )
                                    ]
                               , Object $
                                  HashMap.fromList
                                    [ ("__typename", "__Type")
                                    , ("kind", "OBJECT")
                                    , ("name", "__Type")
                                    ,
                                      ( "description"
                                      , "The fundamental unit of any GraphQL Schema is the type. \
                                        \There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.\n\n\
                                        \Depending on the kind of a type, certain fields describe information about that type. \
                                        \Scalar types provide no information beyond a name and description, while Enum types provide their values. \
                                        \Object and Interface types provide the fields they describe. \
                                        \Abstract types, Union and Interface, provide the Object types possible at runtime. \
                                        \List and NonNull types compose other types."
                                      )
                                    ,
                                      ( "fields"
                                      , List
                                          [ Object $
                                              HashMap.fromList
                                                [ ("name", "kind")
                                                ,
                                                  ( "type"
                                                  , Object $
                                                      HashMap.fromList
                                                        [ ("kind", "NON_NULL")
                                                        ,
                                                          ( "ofType"
                                                          , Object $
                                                              HashMap.fromList
                                                                [ ("kind", "ENUM")
                                                                , ("name", "__TypeKind")
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "name")
                                                , -- Don't know why not "NON_NULL"
                                                  getTypeTuple "SCALAR" "String"
                                                ]
                                          , descriptionField
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "fields")
                                                ,
                                                  ( "type"
                                                  , Object $
                                                      HashMap.fromList
                                                        [ ("kind", "LIST")
                                                        ,
                                                          ( "ofType"
                                                          , Object $
                                                              HashMap.fromList
                                                                [ ("kind", "NON_NULL")
                                                                ,
                                                                  ( "ofType"
                                                                  , Object $
                                                                      HashMap.fromList
                                                                        [ ("kind", "OBJECT")
                                                                        , ("name", "__Field")
                                                                        ]
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ,
                                                  ( "args"
                                                  , List
                                                      [ Object $
                                                          HashMap.fromList
                                                            [ ("name", "includeDeprecated")
                                                            , getTypeTuple "SCALAR" "Boolean"
                                                            , -- Don't know why this has to be a string
                                                              ("defaultValue", "false")
                                                            ]
                                                      ]
                                                  )
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "interfaces")
                                                ,
                                                  ( "type"
                                                  , Object $
                                                      HashMap.fromList
                                                        [ ("kind", "LIST")
                                                        ,
                                                          ( "ofType"
                                                          , Object $
                                                              HashMap.fromList
                                                                [ ("kind", "NON_NULL")
                                                                ,
                                                                  ( "ofType"
                                                                  , Object $
                                                                      HashMap.fromList
                                                                        [ ("kind", "OBJECT")
                                                                        , ("name", "__Type")
                                                                        ]
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "possibleTypes")
                                                ,
                                                  ( "type"
                                                  , Object $
                                                      HashMap.fromList
                                                        [ ("kind", "LIST")
                                                        ,
                                                          ( "ofType"
                                                          , Object $
                                                              HashMap.fromList
                                                                [ ("kind", "NON_NULL")
                                                                ,
                                                                  ( "ofType"
                                                                  , Object $
                                                                      HashMap.fromList
                                                                        [ ("kind", "OBJECT")
                                                                        , ("name", "__Type")
                                                                        ]
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "enumValues")
                                                ,
                                                  ( "type"
                                                  , Object $
                                                      HashMap.fromList
                                                        [ ("kind", "LIST")
                                                        ,
                                                          ( "ofType"
                                                          , Object $
                                                              HashMap.fromList
                                                                [ ("kind", "NON_NULL")
                                                                ,
                                                                  ( "ofType"
                                                                  , Object $
                                                                      HashMap.fromList
                                                                        [ ("kind", "OBJECT")
                                                                        , ("name", "__EnumValue")
                                                                        ]
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ,
                                                  ( "args"
                                                  , List
                                                      [ Object $
                                                          HashMap.fromList
                                                            [ ("name", "includeDeprecated")
                                                            , getTypeTuple "SCALAR" "Boolean"
                                                            , -- Don't know why this has to be a string
                                                              ("defaultValue", "false")
                                                            ]
                                                      ]
                                                  )
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "inputFields")
                                                ,
                                                  ( "type"
                                                  , Object $
                                                      HashMap.fromList
                                                        [ ("kind", "LIST")
                                                        ,
                                                          ( "ofType"
                                                          , Object $
                                                              HashMap.fromList
                                                                [ ("kind", "NON_NULL")
                                                                ,
                                                                  ( "ofType"
                                                                  , Object $
                                                                      HashMap.fromList
                                                                        [ ("kind", "OBJECT")
                                                                        , ("name", "__InputValue")
                                                                        ]
                                                                  )
                                                                ]
                                                          )
                                                        ]
                                                  )
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "ofType")
                                                , getTypeTuple "OBJECT" "__Type"
                                                ]
                                          ]
                                      )
                                    ]
                               , Object $
                                  HashMap.fromList
                                    [ ("__typename", "__Type")
                                    , ("kind", "ENUM")
                                    , ("name", "__TypeKind")
                                    ,
                                      ( "description"
                                      , "An enum describing what kind of type a given `__Type` is."
                                      )
                                    ,
                                      ( "enumValues"
                                      , List
                                          [ Object $
                                              HashMap.fromList
                                                [ ("name", "SCALAR")
                                                ,
                                                  ( "description"
                                                  , "Indicates this type is a scalar."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "OBJECT")
                                                ,
                                                  ( "description"
                                                  , "Indicates this type is an object. `fields` and `interfaces` are valid fields."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "INTERFACE")
                                                ,
                                                  ( "description"
                                                  , "Indicates this type is an interface. `fields` and `possibleTypes` are valid fields."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "UNION")
                                                ,
                                                  ( "description"
                                                  , "Indicates this type is a union. `possibleTypes` is a valid field."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "ENUM")
                                                ,
                                                  ( "description"
                                                  , "Indicates this type is an enum. `enumValues` is a valid field."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "INPUT_OBJECT")
                                                ,
                                                  ( "description"
                                                  , "Indicates this type is an input object. `inputFields` is a valid field."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "LIST")
                                                ,
                                                  ( "description"
                                                  , "Indicates this type is a list. `ofType` is a valid field."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "NON_NULL")
                                                ,
                                                  ( "description"
                                                  , "Indicates this type is a non-null. `ofType` is a valid field."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          ]
                                      )
                                    ]
                               , Object $
                                  HashMap.fromList
                                    [ ("__typename", "__Type")
                                    , ("kind", "OBJECT")
                                    , ("name", "__Field")
                                    ,
                                      ( "description"
                                      , "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type."
                                      )
                                    ,
                                      ( "fields"
                                      , List
                                          [ nameField
                                          , descriptionField
                                          , argsFieldValue
                                          , typeFieldValue
                                          , isDeprecatedFieldValue
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "deprecationReason")
                                                , getTypeTuple "SCALAR" "String"
                                                ]
                                          ]
                                      )
                                    ]
                               , Object $
                                  HashMap.fromList
                                    [ ("__typename", "__Type")
                                    , ("kind", "OBJECT")
                                    , ("name", "__InputValue")
                                    ,
                                      ( "description"
                                      , "Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value."
                                      )
                                    ,
                                      ( "fields"
                                      , List
                                          [ nameField
                                          , descriptionField
                                          , typeFieldValue
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "defaultValue")
                                                ,
                                                  ( "description"
                                                  , "A GraphQL-formatted string representing \
                                                    \the default value for this input value."
                                                  )
                                                , getTypeTuple "SCALAR" "String"
                                                ]
                                          ]
                                      )
                                    ]
                               , Object $
                                  HashMap.fromList
                                    [ ("__typename", "__Type")
                                    , ("kind", "OBJECT")
                                    , ("name", "__EnumValue")
                                    ,
                                      ( "description"
                                      , "One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string."
                                      )
                                    ,
                                      ( "fields"
                                      , List
                                          [ nameField
                                          , descriptionField
                                          , isDeprecatedFieldValue
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "deprecationReason")
                                                , getTypeTuple "SCALAR" "String"
                                                ]
                                          ]
                                      )
                                    ]
                               , Object $
                                  HashMap.fromList
                                    [ ("__typename", "__Type")
                                    , ("kind", "OBJECT")
                                    , ("name", "__Directive")
                                    ,
                                      ( "description"
                                      , "A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.\n\nIn some cases, you need to provide options to alter GraphQL's execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor."
                                      )
                                    ,
                                      ( "fields"
                                      , List
                                          [ nameField
                                          , descriptionField
                                          , locationsFieldValue
                                          , argsFieldValue
                                          ]
                                      )
                                    ]
                               , Object $
                                  HashMap.fromList
                                    [ ("__typename", "__Type")
                                    , ("kind", "ENUM")
                                    , ("name", "__DirectiveLocation")
                                    ,
                                      ( "description"
                                      , "A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies."
                                      )
                                    ,
                                      ( "enumValues"
                                      , List
                                          [ Object $
                                              HashMap.fromList
                                                [ ("name", "QUERY")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to a query operation."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "MUTATION")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to a mutation operation."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "SUBSCRIPTION")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to a subscription operation."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "FIELD")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to a field."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "FRAGMENT_DEFINITION")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to a fragment definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "FRAGMENT_SPREAD")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to a fragment spread."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "INLINE_FRAGMENT")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to an inline fragment."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "VARIABLE_DEFINITION")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to a variable definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "SCHEMA")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to a schema definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "SCALAR")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to a scalar definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "OBJECT")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to an object type definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "FIELD_DEFINITION")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to a field definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "ARGUMENT_DEFINITION")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to an argument definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "INTERFACE")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to an interface definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "UNION")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to a union definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "ENUM")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to an enum definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "ENUM_VALUE")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to an enum value definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "INPUT_OBJECT")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to an input object \
                                                    \type definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          , Object $
                                              HashMap.fromList
                                                [ ("name", "INPUT_FIELD_DEFINITION")
                                                ,
                                                  ( "description"
                                                  , "Location adjacent to an input object \
                                                    \field definition."
                                                  )
                                                , ("isDeprecated", Boolean False)
                                                , ("deprecationReason", Null)
                                                ]
                                          ]
                                      )
                                    ]
                               ]
                              & filter (/= Null)
                  )
                ,
                  ( "directives"
                  , ValueResolver directivesType $
                      pure $
                        List
                          [ Object $
                              HashMap.fromList
                                [ ("__typename", "__Directive")
                                , ("name", "skip")
                                ,
                                  ( "description"
                                  , "Directs the executor to skip this field or fragment \
                                    \when the `if` argument is true."
                                  )
                                ,
                                  ( "locations"
                                  , List ["INLINE_FRAGMENT", "FRAGMENT_SPREAD", "FIELD"]
                                  )
                                ,
                                  ( "args"
                                  , List
                                      [ Object $
                                          HashMap.fromList
                                            [ ("name", "if")
                                            , ("description", "Skipped when true.")
                                            , ("defaultValue", Null)
                                            ,
                                              ( "type"
                                              , Object $
                                                  HashMap.fromList
                                                    [ ("kind", "NON_NULL")
                                                    ,
                                                      ( "ofType"
                                                      , Object $
                                                          HashMap.fromList
                                                            [ ("kind", "SCALAR")
                                                            , ("name", "Boolean")
                                                            ]
                                                      )
                                                    ]
                                              )
                                            ]
                                      ]
                                  )
                                ]
                          , Object $
                              HashMap.fromList
                                [ ("__typename", "__Directive")
                                , ("name", "include")
                                ,
                                  ( "description"
                                  , "Directs the executor to include this field or fragment \
                                    \only when the `if` argument is true."
                                  )
                                ,
                                  ( "locations"
                                  , List ["INLINE_FRAGMENT", "FRAGMENT_SPREAD", "FIELD"]
                                  )
                                ,
                                  ( "args"
                                  , List
                                      [ Object $
                                          HashMap.fromList
                                            [ ("name", "if")
                                            , ("description", "Included when true.")
                                            , ("defaultValue", Null)
                                            ,
                                              ( "type"
                                              , Object $
                                                  HashMap.fromList
                                                    [ ("kind", "NON_NULL")
                                                    ,
                                                      ( "ofType"
                                                      , Object $
                                                          HashMap.fromList
                                                            [ ("kind", "SCALAR")
                                                            , ("name", "Boolean")
                                                            ]
                                                      )
                                                    ]
                                              )
                                            ]
                                      ]
                                  )
                                ]
                          , Object $
                              HashMap.fromList
                                [ ("__typename", "__Directive")
                                , ("name", "deprecated")
                                ,
                                  ( "description"
                                  , "Marks an element of a GraphQL schema \
                                    \as no longer supported."
                                  )
                                ,
                                  ( "locations"
                                  , List ["ENUM_VALUE", "FIELD_DEFINITION"]
                                  )
                                ,
                                  ( "args"
                                  , List
                                      [ Object $
                                          HashMap.fromList
                                            [ ("name", "reason")
                                            ,
                                              ( "description"
                                              , "Explains why this element was deprecated, \
                                                \usually also including a suggestion \
                                                \for how to access supported similar data. \
                                                \Formatted using the Markdown syntax \
                                                \(as specified by \
                                                \[CommonMark](https://commonmark.org/)."
                                              )
                                            , ("defaultValue", "\"No longer supported\"")
                                            , getTypeTuple "SCALAR" "String"
                                            ]
                                      ]
                                  )
                                ]
                          ]
                  )
                ]
          }


getSchemaField
  :: Text
  -> Connection
  -> AccessMode
  -> [TableEntryRaw]
  -> IO (Field IO)
getSchemaField dbId conn accessMode tables = do
  schemaFieldOutput <- getSchemaFieldOutput dbId conn accessMode tables

  pure $
    outFieldToField $
      OutField
        { descriptionMb = Just "The schema"
        , fieldType = schemaFieldOutput
        , arguments = HashMap.empty
        }


getSchemaResolver
  :: Text
  -> Connection
  -> AccessMode
  -> [TableEntryRaw]
  -> IO (HashMap Text (Resolver IO))
getSchemaResolver dbId conn accessMode tables = do
  schemaField <- getSchemaField dbId conn accessMode tables

  pure $
    HashMap.singleton
      "__schema"
      (ValueResolver schemaField (pure Null))


typeNameOutField :: Field m
typeNameOutField =
  outFieldToField $
    OutField
      { descriptionMb = Just "The type name"
      , fieldType = Out.NonNullScalarType string
      , arguments = HashMap.empty
      }


typeNameResolver :: HashMap Text (Resolver IO)
typeNameResolver =
  HashMap.singleton
    "__typename"
    (ValueResolver typeNameOutField $ pure "Query")
