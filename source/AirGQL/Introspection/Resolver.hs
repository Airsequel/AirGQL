module AirGQL.Introspection.Resolver (makeType, makeConstField) where

import Protolude (
  Either (Left),
  IO,
  Int,
  MonadReader (ask),
  Text,
  fromMaybe,
  mempty,
  pure,
  show,
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


makeType :: IType.IntrospectionType -> Result (Out.Type IO)
makeType =
  let
    -- This is the same as `makeTypeWithDepth`, except the outputs
    -- for `__Type` are memoized.
    --
    -- This turns the memory usage from O(C^n) to O(n), where n is the depth.
    -- This greatly speeds up introspection from the playground, which requires
    -- a depth of around 10 (from what I can recall).
    makeTypeWithDepthMemo :: Int -> IType.IntrospectionType -> Result (Out.Type IO)
    makeTypeWithDepthMemo depth ty = case ty.kind of
      IType.Object "__Type" _ ->
        P.join $ P.note "(impossible)" $ P.atMay typeCache depth
      _ -> makeTypeWithDepth depth ty

    -- The memoization is done using a haskell lazy array.
    typeCache = [makeTypeWithDepth i IType.typeIntrospectionType | i <- [0 ..]]

    makeTypeWithDepth :: Int -> IType.IntrospectionType -> Result (Out.Type IO)
    makeTypeWithDepth depth ty =
      case ty.kind of
        IType.Scalar name -> do
          pure $ Out.NamedScalarType $ Type.ScalarType name ty.description
        IType.List ofType -> do
          Out.ListType <$> makeTypeWithDepthMemo depth ofType
        IType.Enum name enumValues -> do
          let variants =
                enumValues
                  <&> \variant -> (variant.name, Type.EnumValue variant.description)
          pure $
            Out.NamedEnumType $
              Type.EnumType name ty.description $
                HashMap.fromList variants
        IType.NonNull ofType -> do
          inner <- makeTypeWithDepthMemo depth ofType
          pure $ case inner of
            Out.EnumBaseType enumType -> Out.NonNullEnumType enumType
            Out.UnionBaseType unionType -> Out.NonNullUnionType unionType
            Out.ScalarBaseType scalarType -> Out.NonNullScalarType scalarType
            Out.ObjectBaseType objectType -> Out.NonNullObjectType objectType
            Out.ListBaseType listType -> Out.NonNullListType listType
            Out.InterfaceBaseType interfaceType -> Out.NonNullInterfaceType interfaceType
        IType.Object name fields -> do
          resolvers <- P.for fields $ \field -> do
            resolver <-
              if depth >= 30
                then
                  makeConstField
                    (IType.field field.name IType.typeString)
                    (Type.String "Maximum depth exceeded")
                else makeChildField (depth + 1) field
            pure (field.name, resolver)

          typenameResolver <-
            makeConstField
              (IType.field "__typename" $ IType.nonNull IType.typeString)
              (Type.String name)

          pure
            $ Out.NamedObjectType
            $ Type.ObjectType
              name
              -- ty.description
              P.Nothing
              []
            $ HashMap.fromList
            $ ("__typename", typenameResolver) : resolvers
        _ -> do
          Left $ "invalid type in out position: " <> show ty.kind

    -- Creates a field which looks up it's value in the object returned by the
    -- parent resolver.
    makeChildField :: Int -> IType.Field -> Result (Out.Resolver IO)
    makeChildField depth field = do
      args <- P.for field.args $ \arg -> do
        ty <- makeInType arg.type_
        pure (arg.name, In.Argument arg.description ty arg.defaultValue)
      ty <- makeTypeWithDepthMemo depth field.type_
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
  in
    makeTypeWithDepth 0


makeConstField :: IType.Field -> Type.Value -> Result (Out.Resolver IO)
makeConstField field value = do
  ty <- makeType field.type_
  let gqlField = Out.Field field.description ty mempty
  pure $ Out.ValueResolver gqlField $ pure value


makeInType :: IType.IntrospectionType -> Result In.Type
makeInType ty = do
  case ty.kind of
    IType.Scalar name -> do
      pure $ In.NamedScalarType $ Type.ScalarType name ty.description
    IType.List ofType -> do
      In.ListType <$> makeInType ofType
    IType.Enum name enumValues -> do
      let variants = enumValues <&> \variant -> (variant.name, Type.EnumValue variant.description)
      pure $
        In.NamedEnumType $
          Type.EnumType name ty.description $
            HashMap.fromList variants
    IType.NonNull ofType -> do
      inner <- makeInType ofType
      pure $ case inner of
        In.EnumBaseType enumType -> In.NonNullEnumType enumType
        In.ScalarBaseType scalarType -> In.NonNullScalarType scalarType
        In.InputObjectBaseType objectType -> In.NonNullInputObjectType objectType
        In.ListBaseType listType -> In.NonNullListType listType
    IType.InputObject name fields -> do
      gqlFields <- P.for fields $ \field -> do
        inner <- makeInType field.type_
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
    _ -> do
      Left $ "invalid type in input position: " <> show ty.kind
