{-| Each table, say `foo`, generates a `foo` and a `foo_by_pk`. If a table
named `foo_by_pk` also exists, this would create a naming conflict. This
issue also occurs in a few other places.

To solve this, we implement the `encodeOutsideList` function, which encodes a name
such that it does not conflict with any other name from a given list. This
is done by repeatedly appending _ at the end, until the name does not reside in
the given list anymore.
-}
module AirGQL.Introspection.NamingConflict (
  encodeOutsideList,
  encodeOutsideTableNames,
  encodeOutsidePKNames,
) where

import Protolude (Text, fromMaybe, ($), (<$>), (<>))

import AirGQL.Lib (ColumnEntry (column_name_gql), TableEntry (name), getPKColumns)
import Data.List qualified as List
import DoubleXEncoding (doubleXEncodeGql)


encodeOutsideList :: [Text] -> Text -> Text
encodeOutsideList list name = do
  if name `List.elem` list
    then encodeOutsideList list (name <> "_")
    else name


-- | Encode a name so it does not conflict with any table name
encodeOutsideTableNames :: [TableEntry] -> Text -> Text
encodeOutsideTableNames tables =
  encodeOutsideList $ (\t -> doubleXEncodeGql t.name) <$> tables


{-| Encode a name so it does not conflict with any column that is part of a
PK constraint for a given table.
-}
encodeOutsidePKNames :: TableEntry -> Text -> Text
encodeOutsidePKNames table = do
  let cols = fromMaybe [] $ getPKColumns table
  encodeOutsideList $ column_name_gql <$> cols
