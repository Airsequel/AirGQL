module AirGQL.Types.Types (
  FileFormat (..),
  FilenameField (..),
  GQLPost (..),
  GQLResponse (..),
  gqlResponseToObject,
  MetadataPair (..),
  RawJsonMime,
  Database (..),
  UsageError (..),
)
where

import Protolude (
  Eq,
  Generic,
  Maybe (Nothing),
  Monoid (mempty),
  Show,
  Text,
 )
import Protolude qualified as P

import Data.Aeson (
  FromJSON,
  KeyValue ((.=)),
  Object,
  ToJSON (toJSON),
  Value (Object),
  object,
 )
import Database.SQLite.Simple qualified as SS
import Servant.Docs (ToSample (toSamples), singleSample)


-- Necessary to avoid JSON string quoting
data RawJsonMime


data FileFormat
  = SQLiteFile
  | CSVFile
  | PlainTextFile
  | DisallowedFile Text
  deriving (Show, Eq)


data GQLPost = GQLPost
  { query :: Text
  , operationName :: Maybe Text
  , variables :: Maybe Object
  }
  deriving (Eq, Show, Generic)


instance ToJSON GQLPost
instance FromJSON GQLPost


instance ToSample GQLPost where
  toSamples _ =
    singleSample
      GQLPost
        { query = "{ users { name, email } }"
        , variables = Nothing
        , operationName = Nothing
        }


data GQLResponse = GQLResponse
  { data_ :: Maybe Value
  , errors :: Maybe [Value]
  }
  deriving (Eq, Show, Generic)


instance ToJSON GQLResponse where
  toJSON GQLResponse{data_, errors} =
    object
      [ "data" .= data_
      , "errors" .= errors
      ]


-- emptyGQLResponse :: GQLResponse
-- emptyGQLResponse = GQLResponse
--   { data_ = Nothing
--   , errors = Nothing
--   }

newtype FilenameField = FilenameField Text
  deriving (Generic, Show)


instance SS.FromRow FilenameField


data MetadataPair = MetadataPair
  { attribute :: Text
  , value :: Text
  }
  deriving (Eq, Show, Generic)


instance SS.FromRow MetadataPair


gqlResponseToObject :: GQLResponse -> Object
gqlResponseToObject gqlRes =
  case toJSON gqlRes of
    Object obj -> obj
    _ -> mempty


data Database = Database
  { id :: Text
  , name :: Text
  , environment :: Maybe Text
  , ownership_utc :: Text
  }
  deriving (Generic, Show)


instance FromJSON Database
instance ToJSON Database
instance SS.FromRow Database


-- Errors

newtype UsageError = UsageError Text
  deriving (Eq, Show)


instance P.Exception UsageError
