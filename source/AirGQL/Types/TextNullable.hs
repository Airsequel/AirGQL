module AirGQL.Types.TextNullable (
  TextNullable (..),
)
where

import Protolude (
  Eq,
  Generic,
  Show,
  Text,
  pure,
  ($),
 )

import Data.Aeson (
  FromJSON,
  ToJSON,
  Value (Null, String),
  parseJSON,
 )


data TextNullable = TextUndefined | TextNull | TextValue Text
  deriving (Show, Eq, Generic)


instance FromJSON TextNullable where
  parseJSON (String str) = pure $ TextValue str
  parseJSON Null = pure TextNull
  parseJSON _ = pure TextUndefined
instance ToJSON TextNullable
