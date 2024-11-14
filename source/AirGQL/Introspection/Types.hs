module AirGQL.Introspection.Types (
  Schema (..),
  Directive (..),
  Name,
  IntrospectionType (..),
  TypeKind (..),
  Field (..),
  InputValue (..),
  EnumValue (..),
  collectSchemaTypes,
  deprecatedEnumValue,
  directive,
  directiveWithDescription,
  enum,
  enumValue,
  enumValueWithDescription,
  field,
  fieldWithDescription,
  inputObject,
  inputValue,
  inputValueWithDescription,
  list,
  nonNull,
  object,
  repeatableDirective,
  scalar,
  typeBool,
  typeField,
  typeFloat,
  typeID,
  typeInt,
  typeIntrospectionType,
  typeSchema,
  typeString,
  withArguments,
  withDefaultValue,
  withDescription,
) where

import Protolude (
  Bool (False, True),
  Generic,
  Maybe (Just, Nothing),
  MonadState (get, put),
  Monoid (mempty),
  Show,
  State,
  Text,
  execState,
  for_,
  not,
  pure,
  show,
  when,
  ($),
  (&),
  (<$>),
  (<>),
 )

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Language.GraphQL.Class (ToGraphQL (toGraphQL))
import Language.GraphQL.Type qualified as Value


data Schema = Schema
  { description :: Maybe Text
  , types :: [IntrospectionType]
  , queryType :: IntrospectionType
  , mutationType :: Maybe IntrospectionType
  , directives :: [Directive]
  }
  deriving (Show, Generic)


instance ToGraphQL Schema where
  toGraphQL schema =
    Value.Object $
      HashMap.fromList
        [ ("description", toGraphQL schema.description)
        , ("types", toGraphQL schema.types)
        , ("queryType", toGraphQL schema.queryType)
        , ("mutationType", toGraphQL schema.mutationType)
        , ("directives", toGraphQL schema.directives)
        , ("subscriptionType", Value.Null)
        ]


-- | The name of a graphql type.
type Name = Text


data TypeKind
  = Scalar Name
  | Object Name [Field]
  | Enum Name [EnumValue]
  | InputObject Name [InputValue]
  | List IntrospectionType
  | NonNull IntrospectionType
  deriving (Show, Generic)


data IntrospectionType = IType
  { kind :: TypeKind
  , description :: Maybe Text
  }
  deriving (Show, Generic)


instance ToGraphQL IntrospectionType where
  toGraphQL ty = do
    Value.Object $
      HashMap.fromList
        [
          ( "kind"
          , case ty.kind of
              Scalar _ -> Value.Enum "SCALAR"
              Object _ _ -> Value.Enum "OBJECT"
              Enum _ _ -> Value.Enum "ENUM"
              InputObject _ _ -> Value.Enum "INPUT_OBJECT"
              List _ -> Value.Enum "LIST"
              NonNull _ -> Value.Enum "NON_NULL"
          )
        ,
          ( "name"
          , case ty.kind of
              Scalar name -> Value.String name
              Object name _ -> Value.String name
              Enum name _ -> Value.String name
              InputObject name _ -> Value.String name
              _ -> Value.Null
          )
        , ("description", toGraphQL ty.description)
        ,
          ( "interfaces"
          , case ty.kind of
              Object _ _ -> Value.List []
              _ -> Value.Null
          )
        , ("possibleTypes", Value.Null)
        ,
          ( "fields"
          , case ty.kind of
              Object _ fields -> toGraphQL fields
              _ -> Value.Null
          )
        ,
          ( "enumValues"
          , case ty.kind of
              Enum _ variants -> toGraphQL variants
              _ -> Value.Null
          )
        ,
          ( "inputFields"
          , case ty.kind of
              InputObject _ fields -> toGraphQL fields
              _ -> Value.Null
          )
        ,
          ( "ofType"
          , case ty.kind of
              NonNull inner -> toGraphQL inner
              List inner -> toGraphQL inner
              _ -> Value.Null
          )
        ]


mkType :: TypeKind -> IntrospectionType
mkType kind =
  IType
    { kind
    , description = Nothing
    }


nonNull :: IntrospectionType -> IntrospectionType
nonNull ty = mkType $ NonNull ty


list :: IntrospectionType -> IntrospectionType
list ty = mkType $ List ty


object :: Text -> [Field] -> IntrospectionType
object name fields = mkType $ Object name fields


inputObject :: Text -> [InputValue] -> IntrospectionType
inputObject name fields = mkType $ InputObject name fields


enum :: Text -> [EnumValue] -> IntrospectionType
enum name variants = mkType $ Enum name variants


withDescription :: Text -> IntrospectionType -> IntrospectionType
withDescription newDesc (IType{..}) =
  IType
    { description = Just newDesc
    , ..
    }


scalar :: Text -> IntrospectionType
scalar tyName = mkType $ Scalar tyName


data Field = Field
  { name :: Text
  , description :: Maybe Text
  , args :: [InputValue]
  , type_ :: IntrospectionType
  , isDeprecated :: Bool
  , deprecationReason :: Maybe Text
  }
  deriving (Show, Generic)


instance ToGraphQL Field where
  toGraphQL thisField =
    Value.Object $
      HashMap.fromList
        [ ("name", toGraphQL thisField.name)
        , ("description", toGraphQL thisField.description)
        , ("args", toGraphQL thisField.args)
        , ("type", toGraphQL thisField.type_)
        , ("isDeprecated", toGraphQL thisField.isDeprecated)
        , ("deprecationReason", toGraphQL thisField.deprecationReason)
        ]


fieldWithDescription :: Text -> Field -> Field
fieldWithDescription newDesc (Field{..}) =
  Field
    { description = Just newDesc
    , ..
    }


field :: Text -> IntrospectionType -> Field
field fieldName fieldType =
  Field
    { name = fieldName
    , description = Nothing
    , args = []
    , type_ = fieldType
    , isDeprecated = False
    , deprecationReason = Nothing
    }


withArguments :: [InputValue] -> Field -> Field
withArguments argList (Field{..}) = Field{args = args <> argList, ..}


data InputValue = InputValue
  { name :: Text
  , description :: Maybe Text
  , type_ :: IntrospectionType
  , defaultValue :: Maybe Value.Value
  }
  deriving (Show, Generic)


instance ToGraphQL InputValue where
  toGraphQL value =
    Value.Object $
      HashMap.fromList
        [ ("name", toGraphQL value.name)
        , ("description", toGraphQL value.description)
        , ("type", toGraphQL value.type_)
        , -- TODO: I'm not sure `show` is the correct function here

          ( "defaultValue"
          , case value.defaultValue of
              Nothing -> Value.Null
              Just s -> Value.String $ show s
          )
        ]


inputValue :: Text -> IntrospectionType -> InputValue
inputValue fieldName fieldType =
  InputValue
    { name = fieldName
    , description = Nothing
    , type_ = fieldType
    , defaultValue = Nothing
    }


inputValueWithDescription :: Text -> InputValue -> InputValue
inputValueWithDescription newDesc (InputValue{..}) =
  InputValue
    { description = Just newDesc
    , ..
    }


withDefaultValue :: Value.Value -> InputValue -> InputValue
withDefaultValue newValue (InputValue{..}) =
  InputValue
    { defaultValue = Just newValue
    , ..
    }


data EnumValue = EnumValue
  { name :: Text
  , description :: Maybe Text
  , isDeprecated :: Bool
  , deprecationReason :: Maybe Text
  }
  deriving (Show, Generic)


instance ToGraphQL EnumValue where
  toGraphQL value =
    Value.Object $
      HashMap.fromList
        [ ("name", toGraphQL value.name)
        , ("description", toGraphQL value.description)
        , ("isDeprecated", toGraphQL value.isDeprecated)
        , ("deprecationReason", toGraphQL value.deprecationReason)
        ]


enumValue :: Text -> EnumValue
enumValue name =
  EnumValue
    { name = name
    , description = Nothing
    , isDeprecated = False
    , deprecationReason = Nothing
    }


enumValueWithDescription :: Text -> EnumValue -> EnumValue
enumValueWithDescription newDesc (EnumValue{..}) =
  EnumValue{description = Just newDesc, ..}


deprecatedEnumValue :: Text -> EnumValue -> EnumValue
deprecatedEnumValue reason (EnumValue{..}) =
  EnumValue
    { isDeprecated = True
    , deprecationReason = Just reason
    , ..
    }


data Directive = Directive
  { name :: Text
  , description :: Maybe Text
  , locations :: [Text]
  , args :: [InputValue]
  , isRepeatable :: Bool
  }
  deriving (Generic, Show)


instance ToGraphQL Directive where
  toGraphQL value =
    Value.Object $
      HashMap.fromList
        [ ("name", toGraphQL value.name)
        , ("description", toGraphQL value.description)
        , ("isRepeatable", toGraphQL value.isRepeatable)
        , ("args", toGraphQL value.args)
        , ("locations", Value.List $ Value.Enum <$> value.locations)
        ]


directive :: Text -> [Text] -> [InputValue] -> Directive
directive name locations args =
  Directive
    { name = name
    , args = args
    , locations = locations
    , description = Nothing
    , isRepeatable = False
    }


directiveWithDescription :: Text -> Directive -> Directive
directiveWithDescription newDesc (Directive{..}) =
  Directive{description = Just newDesc, ..}


repeatableDirective :: Directive -> Directive
repeatableDirective (Directive{..}) =
  Directive{isRepeatable = True, ..}


{-| Updates the `types` property of a schema to reference
every type contained in other parts of the schema.
-}
collectSchemaTypes :: Schema -> Schema
collectSchemaTypes schema = do
  let basic = [typeInt, typeFloat, typeString, typeBool, typeID, typeSchema]
  let all = do
        collectTypes schema.queryType
        for_ schema.mutationType collectTypes
        for_ basic collectTypes
  schema
    { types = HashMap.elems $ execState all mempty
    }


-- | Collect a map of all the named types occurring inside a type
collectTypes :: IntrospectionType -> State (HashMap Text IntrospectionType) ()
collectTypes ty = do
  -- Gives a name to the current type, and saves it.
  --
  -- If the type hadn't been found already, runs a custom continuation.
  let insertType name continue = do
        current <- get
        when (not $ HashMap.member name current) $ do
          put $ HashMap.insert name ty current
          continue

  case ty.kind of
    NonNull inner -> collectTypes inner
    List inner -> collectTypes inner
    Enum name _ -> insertType name $ pure ()
    Scalar name -> insertType name $ pure ()
    Object name fields -> insertType name $ do
      for_ fields $ \thisField -> do
        collectTypes thisField.type_
        for_ thisField.args $ \arg ->
          collectTypes arg.type_
    InputObject name fields -> insertType name $ do
      for_ fields $ \thisField -> collectTypes thisField.type_


--------------------- Declarations for all the standard gql types
typeString :: IntrospectionType
typeString =
  scalar "String"
    & withDescription
      "The `String` scalar type represents textual data, \
      \represented as UTF-8 character sequences. \
      \The String type is most often used by GraphQL \
      \to represent free-form human-readable text."


typeInt :: IntrospectionType
typeInt =
  scalar "Int"
    & withDescription
      "The `Int` scalar type represents \
      \non-fractional signed whole numeric values. \
      \Int can represent values between -(2^31) and 2^31 - 1."


typeFloat :: IntrospectionType
typeFloat =
  scalar "Float"
    & withDescription "Signed double-precision floating-point value."


typeBool :: IntrospectionType
typeBool =
  scalar "Boolean"
    & withDescription "The `Boolean` scalar type represents `true` or `false`."


typeID :: IntrospectionType
typeID =
  scalar "ID"
    & withDescription
      "The `ID` scalar type represents a unique identifier, \
      \often used to refetch an object or as key for a cache. \
      \The ID type appears in a JSON response as a String; \
      \however, it is not intended to be human-readable. \
      \When expected as an input type, any string \
      \(such as `\"4\"`) or integer (such as `4`) input value \
      \will be accepted as an ID."


typeField :: IntrospectionType
typeField =
  object
    "__Field"
    [ field "name" $ nonNull typeString
    , field "description" typeString
    , field "args" $ nonNull $ list $ nonNull typeInputValue
    , field "type" $ nonNull typeIntrospectionType
    , field "isDeprecated" $ nonNull typeBool
    , field "deprecationReason" typeString
    ]
    & withDescription
      "Object and Interface types are described by a list of Fields, each of \
      \which has a name, potentially a list of arguments, and a return type."


typeInputValue :: IntrospectionType
typeInputValue =
  object
    "__InputValue"
    [ field "name" $ nonNull typeString
    , field "description" typeString
    , field "type" $ nonNull typeIntrospectionType
    , field "defaultValue" typeString
        & fieldWithDescription
          "A GraphQL-formatted string representing \
          \the default value for this input value."
    ]
    & withDescription
      "Arguments provided to Fields or Directives and the input \
      \fields of an InputObject are represented as Input Values \
      \which describe their type and optionally a default value."


typeEnumValue :: IntrospectionType
typeEnumValue =
  object
    "__EnumValue"
    [ field "name" $ nonNull typeString
    , field "description" typeString
    , field "isDeprecated" $ nonNull typeBool
    , field "deprecationReason" typeString
    ]
    & withDescription
      "One possible value for a given Enum. Enum values are unique values, \
      \not a placeholder for a string or numeric value. However an Enum value \
      \is returned in a JSON response as a string."


typeTypeKind :: IntrospectionType
typeTypeKind =
  enum
    "__TypeKind"
    [ enumValue "SCALAR"
        & enumValueWithDescription "Indicates this type is a scalar."
    , enumValue "OBJECT"
        & enumValueWithDescription
          "Indicates this type is an object. `fields` and \
          \`interfaces` are valid fields."
    , enumValue "INTERFACE"
        & enumValueWithDescription
          "Indicates this type is an interface. `fields` \
          \and `possibleTypes` are valid fields."
    , enumValue "UNION"
        & enumValueWithDescription
          "Indicates this type is a union. `possibleTypes` is a valid field."
    , enumValue "ENUM"
        & enumValueWithDescription
          "Indicates this type is an enum. `enumValues` is a valid field."
    , enumValue "INPUT_OBJECT"
        & enumValueWithDescription
          "Indicates this type is an input object. \
          \`inputFields` is a valid field."
    , enumValue "LIST"
        & enumValueWithDescription
          "Indicates this type is a list. `ofType` is a valid field."
    , enumValue "NON_NULL"
        & enumValueWithDescription
          "Indicates this type is a non-null. `ofType` is a valid field."
    ]
    & withDescription
      "An enum describing what kind of type a given `__Type` is."


typeIntrospectionType :: IntrospectionType
typeIntrospectionType =
  object
    "__Type"
    [ field "kind" $ nonNull typeTypeKind
    , field "name" typeString
    , field "description" typeString
    , field "interfaces" $ list $ nonNull typeIntrospectionType
    , field "possibleTypes" $ list $ nonNull typeIntrospectionType
    , field "fields" (list $ nonNull typeField)
        & withArguments
          [ inputValue "includeDeprecated" typeBool
              & withDefaultValue (toGraphQL False)
          ]
    , field "enumValues" (list $ nonNull typeEnumValue)
        & withArguments
          [ inputValue "includeDeprecated" typeBool
              & withDefaultValue (toGraphQL False)
          ]
    , field "inputFields" $ list $ nonNull typeInputValue
    , field "ofType" typeIntrospectionType
    ]
    & withDescription
      "The fundamental unit of any GraphQL Schema is the type. \
      \There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.\n\n\
      \Depending on the kind of a type, certain fields describe information about that type. \
      \Scalar types provide no information beyond a name and description, while Enum types provide their values. \
      \Object and Interface types provide the fields they describe. \
      \Abstract types, Union and Interface, provide the Object types possible at runtime. \
      \List and NonNull types compose other types."


typeSchema :: IntrospectionType
typeSchema =
  object
    "__Schema"
    [ field "description" typeString
    , fieldWithDescription "A list of all types supported by this server." $
        field "types" $
          nonNull $
            list $
              nonNull typeIntrospectionType
    , fieldWithDescription "The type that query operations will be rooted at." $
        field "queryType" $
          nonNull typeIntrospectionType
    , fieldWithDescription
        "If this server supports mutation, the type \
        \that mutation operations will be rooted at."
        $ field "mutationType" typeIntrospectionType
    , fieldWithDescription
        "If this server support subscription, the type \
        \that subscription operations will be rooted at."
        $ field "subscriptionType" typeIntrospectionType
    , fieldWithDescription "A list of all directives supported by this server." $
        field "directives" $
          nonNull $
            list $
              nonNull typeDirective
    ]
    & withDescription
      "A GraphQL Schema defines the capabilities of a GraphQL server. \
      \It exposes all available types and directives on the server, \
      \as well as the entry points for \
      \query, mutation, and subscription operations."


typeDirective :: IntrospectionType
typeDirective =
  object
    "__Directive"
    [ field "name" $ nonNull typeString
    , field "description" typeString
    , field "args" $ nonNull $ list $ nonNull typeInputValue
    , field "isRepeatable" $ nonNull typeBool
    , field "locations" $ nonNull $ list $ nonNull typeDirectiveLocation
    ]
    & withDescription
      "A Directive provides a way to describe alternate runtime execution \
      \and type validation behavior in a GraphQL document.\n\nIn some cases, \
      \you need to provide options to alter GraphQL's execution behavior in \
      \ways field arguments will not suffice, such as conditionally including \
      \or skipping a field. Directives provide this by describing additional \
      \information to the executor."


typeDirectiveLocation :: IntrospectionType
typeDirectiveLocation =
  enum
    "__DirectiveLocation"
    [ enumValue "QUERY"
        & enumValueWithDescription "Location adjacent to a query operation."
    , enumValue "MUTATION"
        & enumValueWithDescription "Location adjacent to a mutation operation."
    , enumValue "SUBSCRIPTION"
        & enumValueWithDescription
          "Location adjacent to a subscription operation."
    , enumValue "FIELD"
        & enumValueWithDescription "Location adjacent to a field."
    , enumValue "FRAGMENT_DEFINITION"
        & enumValueWithDescription "Location adjacent to a fragment definition."
    , enumValue "FRAGMENT_SPREAD"
        & enumValueWithDescription "Location adjacent to a fragment spread."
    , enumValue "INLINE_FRAGMENT"
        & enumValueWithDescription "Location adjacent to an inline fragment."
    , enumValue "VARIABLE_DEFINITION"
        & enumValueWithDescription "Location adjacent to a variable definition."
    , enumValue "SCHEMA"
        & enumValueWithDescription "Location adjacent to a schema definition."
    , enumValue "SCALAR"
        & enumValueWithDescription "Location adjacent to a scalar definition."
    , enumValue "OBJECT"
        & enumValueWithDescription
          "Location adjacent to an object type definition."
    , enumValue "FIELD_DEFINITION"
        & enumValueWithDescription "Location adjacent to a field definition."
    , enumValue "ARGUMENT_DEFINITION"
        & enumValueWithDescription
          "Location adjacent to an argument definition."
    , enumValue "INTERFACE"
        & enumValueWithDescription "Location adjacent to an interface definition."
    , enumValue "UNION"
        & enumValueWithDescription "Location adjacent to a union definition."
    , enumValue "ENUM"
        & enumValueWithDescription "Location adjacent to an enum definition."
    , enumValue "ENUM_VALUE"
        & enumValueWithDescription "Location adjacent to an enum value definition."
    , enumValue "INPUT_OBJECT"
        & enumValueWithDescription
          "Location adjacent to an input object type definition."
    , enumValue "INPUT_FIELD_DEFINITION"
        & enumValueWithDescription
          "Location adjacent to an input object field definition."
    ]
    & withDescription
      "A Directive can be adjacent to many parts of the GraphQL language, \
      \a __DirectiveLocation describes one such possible adjacencies."
