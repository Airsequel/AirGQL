module AirGQL.Types.OutObjectType (
  OutObjectType (OutObjectType, name, descriptionMb, interfaceTypes, fields),
  outObjectTypeToObjectType,
)
where

import Protolude (Maybe, Text, (&))

import Data.HashMap.Strict (HashMap)
import Language.GraphQL.Type (InterfaceType)
import Language.GraphQL.Type.Out qualified as Out


data OutObjectType m = OutObjectType
  { name :: Text
  , descriptionMb :: Maybe Text
  , interfaceTypes :: [InterfaceType m]
  , fields :: HashMap Text (Out.Resolver m)
  }


type role OutObjectType nominal


outObjectTypeToObjectType :: OutObjectType m -> Out.ObjectType m
outObjectTypeToObjectType objectType =
  Out.ObjectType
    (objectType & name)
    (objectType & (descriptionMb :: OutObjectType m -> Maybe Text))
    (objectType & interfaceTypes)
    (objectType & fields)
