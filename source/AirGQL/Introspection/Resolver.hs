module AirGQL.Introspection.Resolver (makeType, makeConstField, makeChildField) where

import Protolude (
  Either (Left),
  IO,
  Int,
  MonadReader (ask),
  Text,
  fromMaybe,
  mempty,
  note,
  pure,
  ($),
  (+),
  (<$>),
  (<&>),
  (<>),
  (>=),
 )
import Protolude qualified as P

import AirGQL.Introspection.Types qualified as IType
import Data.HashMap.Strict qualified as HashMap
import Language.GraphQL.Type qualified as Type
import Language.GraphQL.Type.In qualified as In
import Language.GraphQL.Type.Out qualified as Out


type Result = Either Text


maxDepth :: Int
maxDepth = 9


makeType :: IType.IntrospectionType -> Result (Out.Type IO)
makeType = makeTypeWithDepth 0


makeTypeWithDepth :: Int -> IType.IntrospectionType -> Result (Out.Type IO)
makeTypeWithDepth depth ty = do
  case ty.kind of
    IType.Scalar -> do
      name <- note "No `name` found for scalar" ty.name
      pure $ Out.NamedScalarType $ Type.ScalarType name ty.description
    IType.List -> do
      ofType <- note "No `ofType` found for list" ty.ofType
      Out.ListType <$> makeTypeWithDepth depth ofType
    IType.Enum -> do
      name <- note "No `name` found for enum" ty.name
      enumValues <- note "No `enumValues` found for enum" ty.enumValues
      let variants = enumValues <&> \variant -> (variant.name, Type.EnumValue variant.description)
      pure $
        Out.NamedEnumType $
          Type.EnumType name ty.description $
            HashMap.fromList variants
    IType.NonNull -> do
      ofType <- note "No `ofType` found for nonnull" ty.ofType
      inner <- makeTypeWithDepth depth ofType
      pure $ case inner of
        Out.EnumBaseType enumType -> Out.NonNullEnumType enumType
        Out.UnionBaseType unionType -> Out.NonNullUnionType unionType
        Out.ScalarBaseType scalarType -> Out.NonNullScalarType scalarType
        Out.ObjectBaseType objectType -> Out.NonNullObjectType objectType
        Out.ListBaseType listType -> Out.NonNullListType listType
        Out.InterfaceBaseType interfaceType -> Out.NonNullInterfaceType interfaceType
    IType.InputObject -> do
      Left "input object in out position"
    IType.Object -> do
      name <- note "No `name` found for object" ty.name
      fields <- note "No `fields` found for object" ty.fields
      resolvers <-
        if depth >= maxDepth
          then pure []
          else P.for fields $ \field -> do
            resolver <- makeChildFieldWithDepth (depth + 1) field
            pure (field.name, resolver)

      typenameResolver <-
        makeConstFieldWithDepth
          depth
          (IType.field "__typename" $ IType.nonNull IType.typeString)
          (Type.String name)

      pure
        $ Out.NamedObjectType
        $ Type.ObjectType
          name
          ty.description
          []
        $ HashMap.fromList
        $ ("__typename", typenameResolver) : resolvers


makeConstField :: IType.Field -> Type.Value -> Result (Out.Resolver IO)
makeConstField = makeConstFieldWithDepth 0


makeConstFieldWithDepth :: Int -> IType.Field -> Type.Value -> Result (Out.Resolver IO)
makeConstFieldWithDepth depth field value = do
  ty <- makeTypeWithDepth depth field.type_
  let gqlField = Out.Field field.description ty mempty
  pure $ Out.ValueResolver gqlField $ pure value


makeChildField :: IType.Field -> Result (Out.Resolver IO)
makeChildField = makeChildFieldWithDepth 0


makeChildFieldWithDepth :: Int -> IType.Field -> Result (Out.Resolver IO)
makeChildFieldWithDepth depth field = do
  args <- P.for field.args $ \arg -> do
    ty <- makeInTypeWithDepth depth arg.type_
    pure (arg.name, In.Argument arg.description ty arg.defaultValue)
  ty <- makeTypeWithDepth depth field.type_
  let gqlField = Out.Field field.description ty $ HashMap.fromList args
  pure $ Out.ValueResolver gqlField $ do
    context <- ask
    let defaultValue =
          if Out.isNonNullType ty
            then
              Type.String $
                "Error: field '"
                  <> field.name
                  <> "' not found "
            else Type.Null
    case context.values of
      Type.Object obj ->
        pure $
          fromMaybe defaultValue $
            HashMap.lookup field.name obj
      _ -> pure defaultValue


makeInTypeWithDepth :: Int -> IType.IntrospectionType -> Result In.Type
makeInTypeWithDepth depth ty = do
  case ty.kind of
    IType.Scalar -> do
      name <- note "No `name` found for scalar" ty.name
      pure $ In.NamedScalarType $ Type.ScalarType name ty.description
    IType.List -> do
      ofType <- note "No `ofType` found for list" ty.ofType
      In.ListType <$> makeInTypeWithDepth depth ofType
    IType.Enum -> do
      name <- note "No `name` found for enum" ty.name
      enumValues <- note "No `enumValues` found for enum" ty.enumValues
      let variants = enumValues <&> \variant -> (variant.name, Type.EnumValue variant.description)
      pure $
        In.NamedEnumType $
          Type.EnumType name ty.description $
            HashMap.fromList variants
    IType.NonNull -> do
      ofType <- note "No `ofType` found for nonnull" ty.ofType
      inner <- makeInTypeWithDepth depth ofType
      pure $ case inner of
        In.EnumBaseType enumType -> In.NonNullEnumType enumType
        In.ScalarBaseType scalarType -> In.NonNullScalarType scalarType
        In.InputObjectBaseType objectType -> In.NonNullInputObjectType objectType
        In.ListBaseType listType -> In.NonNullListType listType
    IType.Object -> do
      Left "out object in input position"
    IType.InputObject -> do
      name <- note "No `name` found for object" ty.name
      fields <- note "No `inputFields` found for object" ty.inputFields
      gqlFields <-
        if depth >= maxDepth
          then pure []
          else P.for fields $ \field -> do
            inner <- makeInTypeWithDepth (depth + 1) field.type_
            let inputField =
                  In.InputField
                    field.description
                    inner
                    field.defaultValue
            pure (field.name, inputField)

      pure
        $ In.NamedInputObjectType
        $ Type.InputObjectType
          name
          ty.description
        $ HashMap.fromList gqlFields
