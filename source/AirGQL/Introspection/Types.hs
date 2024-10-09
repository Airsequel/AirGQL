module AirGQL.Introspection.Types (
  Schema (..),
  Name,
  IntrospectionType (..),
  TypeKind (..),
  Field (..),
  InputValue (..),
  EnumValue (..),
  list,
  nonNull,
  field,
  withArguments,
  inputValue,
  inputValueWithDescription,
  withDescription,
  fieldWithDescription,
  scalar,
  object,
  inputObject,
  typeSchema,
  typeIntrospectionType,
  typeField,
  typeString,
  typeInt,
  typeBool,
  collectSchemaTypes,
  enum,
  enumValue,
  enumValueWithDescription,
  deprecatedEnumValue,
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
        , ("subscriptionType", Value.Null)
        , ("directives", Value.List [])
        ]


typeSchema :: IntrospectionType
typeSchema =
  object
    "__Schema"
    [ field "description" typeString
    , field "types" $ nonNull $ list $ nonNull typeIntrospectionType
    , field "queryType" $ nonNull typeIntrospectionType
    , field "mutationType" typeIntrospectionType
    , field "subscriptionType" typeIntrospectionType
    , field "directives" $ nonNull $ list $ nonNull typeDirective
    ]


typeDirective :: IntrospectionType
typeDirective =
  object
    "__Directive"
    [ field "name" typeString
    , field "description" typeString
    , field "args" $ nonNull $ list $ nonNull typeInputValue
    , field "isRepeatable" $ nonNull typeBool
    , field "locations" $ nonNull $ list $ nonNull typeDirectiveLocation
    ]


typeDirectiveLocation :: IntrospectionType
typeDirectiveLocation =
  enum
    "__DirectiveLocation"
    [ enumValue "QUERY"
    , enumValue "MUTATION"
    , enumValue "SUBSCRIPTION"
    , enumValue "FIELD"
    , enumValue "FRAGMENT_DEFINITION"
    , enumValue "FRAGMENT_SPREAD"
    , enumValue "INLINE_FRAGMENT"
    , enumValue "VARIABLE_DEFINITION"
    , enumValue "SCHEMA"
    , enumValue "SCALAR"
    , enumValue "OBJECT"
    , enumValue "FIELD_DEFINITION"
    , enumValue "ARGUMENT_DEFINITION"
    , enumValue "INTERFACE"
    , enumValue "UNION"
    , enumValue "ENUM"
    , enumValue "ENUM_VALUE"
    , enumValue "INPUT_OBJECT"
    , enumValue "INPUT_FIELD_DEFINITION"
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


typeIntrospectionType :: IntrospectionType
typeIntrospectionType =
  object
    "__Type"
    [ field
        "kind"
        $ enum
          "__TypeKind"
          [ enumValue "SCALAR"
          , enumValue "OBJECT"
          , enumValue "ENUM"
          , enumValue "INPUT_OBJECT"
          , enumValue "LIST"
          , enumValue "NON_NULL"
          , enumValue "INTERFACE"
          ]
    , field "name" typeString
    , field "description" typeString
    , field "interfaces" $ list $ nonNull typeIntrospectionType
    , field "possibleTypes" $ list $ nonNull typeIntrospectionType
    , field "fields" (list $ nonNull typeField)
        & withArguments [inputValue "includeDeprecated" typeBool]
    , field "enumValues" (list $ nonNull typeEnumValue)
        & withArguments [inputValue "includeDeprecated" typeBool]
    , field "inputFields" $ list $ nonNull typeInputValue
    , field "ofType" typeIntrospectionType
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
        , -- TODO: I don't think show is the correct function here
          ("defaultValue", Value.String $ show value.defaultValue)
        ]


typeInputValue :: IntrospectionType
typeInputValue =
  object
    "__InputValue"
    [ field "name" $ nonNull typeString
    , field "description" typeString
    , field "type" $ nonNull typeIntrospectionType
    , field "defaultValue" typeString
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


typeEnumValue :: IntrospectionType
typeEnumValue =
  object
    "__EnumValue"
    [ field "name" $ nonNull typeString
    , field "description" typeString
    , field "isDeprecated" $ nonNull typeBool
    , field "deprecationReason" typeString
    ]


enumValue :: Text -> EnumValue
enumValue name = EnumValue{name = name, description = Nothing, isDeprecated = False, deprecationReason = Nothing}


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


typeString :: IntrospectionType
typeString = scalar "String"


typeInt :: IntrospectionType
typeInt = scalar "Int"


typeBool :: IntrospectionType
typeBool = scalar "Boolean"


{-| Updates the `types` property of a schema to reference
every type contained in other parts of the schema.
-}
collectSchemaTypes :: Schema -> Schema
collectSchemaTypes schema = do
  let basic = [typeInt, typeString, typeBool, typeSchema]
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
