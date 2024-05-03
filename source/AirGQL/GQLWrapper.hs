{-|
Increase readability of code
by wrapping `graphql` library with descriptive wrappers
-}
module AirGQL.GQLWrapper (
  OutField (..),
  outFieldToField,
  InArgument (..),
  inArgumentToArgument,
)
where

import Protolude (Maybe, Text)

import Language.GraphQL.Type (Value)
import Language.GraphQL.Type.In qualified as In
import Language.GraphQL.Type.Out qualified as Out


data OutField m = OutField
  { descriptionMb :: Maybe Text
  , fieldType :: Out.Type m
  , arguments :: In.Arguments
  }


outFieldToField :: OutField m -> Out.Field m
outFieldToField outField =
  Out.Field
    outField.descriptionMb
    outField.fieldType
    outField.arguments


data InArgument = InArgument
  { argDescMb :: Maybe Text
  , argType :: In.Type
  , valueMb :: Maybe Value
  }


inArgumentToArgument :: InArgument -> In.Argument
inArgumentToArgument inArgument =
  In.Argument
    inArgument.argDescMb
    inArgument.argType
    inArgument.valueMb
