{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}
module AirGQL.Introspection.Resolver (
  makeType,
  makeConstField,
  makeField,
) where

import Protolude (
  Either (Left),
  IO,
  Int,
  Maybe (Just, Nothing),
  MonadReader (ask),
  Text,
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
import Control.Exception qualified as Exception
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import GHC.IO.Exception (userError)
import Language.GraphQL.Error (ResolverException (ResolverException))
import Language.GraphQL.Type qualified as Type
import Language.GraphQL.Type.In qualified as In
import Language.GraphQL.Type.Out qualified as Out


type Result = Either Text


throwResolverError :: Text -> m a
throwResolverError err =
  Exception.throw $
    ResolverException $
      userError $
        T.unpack err


{-| Turns a type descriptor into a graphql output type, erroring out on input
types. Child resolvers look up their respective fields in the value produced by
their parent.

Lookups for `__Type` objects are memoized, and a maximum depth of 30 is enforced.
-}
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
              ty.description
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
                  throwResolverError $
                    "Error: field '"
                      <> field.name
                      <> "' not found "
                else pure Type.Null

        case context.values of
          Type.Object obj -> do
            let errorValue = HashMap.lookup ("__error_" <> field.name) obj
            P.for_ errorValue $ \case
              Type.String err -> throwResolverError err
              _ -> pure ()

            case HashMap.lookup field.name obj of
              Just value -> pure value
              Nothing -> defaultValue
          _ -> defaultValue
  in
    makeTypeWithDepth 0


{-| Turns a field descriptor into a graphql field. See the documentation
for `makeType` for details about the behaviour of child resolvers.
-}
makeField :: IType.Field -> Result (Out.Field IO)
makeField field = do
  args <- P.for field.args $ \arg -> do
    ty <- makeInType arg.type_
    pure (arg.name, In.Argument arg.description ty arg.defaultValue)
  ty <- makeType field.type_
  pure $ Out.Field field.description ty $ HashMap.fromList args


-- | Create a resolver which always returns a constant value.
makeConstField :: IType.Field -> Type.Value -> Result (Out.Resolver IO)
makeConstField field value = do
  gqlField <- makeField field
  pure $ Out.ValueResolver gqlField $ pure value


{-| The input-type version of `makeOutType`. No maximum depth is enforced, nor
is any memoization used. This is the case because input types are usually pretty
shallow.
-}
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
