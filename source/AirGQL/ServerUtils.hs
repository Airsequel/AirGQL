module AirGQL.ServerUtils (
  executeQuery,
) where

import Protolude (
  Applicative (pure),
  Either (Left, Right),
  FilePath,
  IO,
  Maybe (Just, Nothing),
  toList,
  ($),
  (&),
  (<&>),
  (||),
 )
import Protolude qualified as P

import Conduit (sourceToList)
import Control.Arrow ((>>>))
import Data.Aeson (Object, Value (String))
import Data.Text (Text)
import Language.GraphQL.Error (Error (Error), Response (Response))
import Language.GraphQL.JSON (graphql)

import AirGQL.GraphQL (getDerivedSchema)
import AirGQL.Lib (getEnrichedTables)
import AirGQL.Types.SchemaConf (SchemaConf)
import AirGQL.Types.Types (
  GQLResponse (GQLResponse, data_, errors),
  gqlResponseToObject,
 )
import AirGQL.Utils (withRetryConn)


executeQuery ::
  SchemaConf ->
  -- | Path to the SQLite database file to open
  FilePath ->
  -- | Database identifier (used for the schema name and generated file URLs)
  Text ->
  Text ->
  Object ->
  Maybe Text ->
  IO Object
executeQuery schemaConf dbFilePath dbId query vars opNameMb = do
  withRetryConn dbFilePath $ \theConn -> do
    tablesEither <- getEnrichedTables theConn
    case tablesEither of
      Left err ->
        pure $
          gqlResponseToObject $
            GQLResponse
              { data_ = Nothing
              , errors = Just [String err]
              }
      Right tables -> do
        schema <- getDerivedSchema schemaConf theConn dbId tables
        result <- graphql schema opNameMb vars query

        case result of
          Left errMsg -> do
            errors <- sourceToList errMsg
            pure $
              gqlResponseToObject $
                GQLResponse
                  { data_ = Nothing
                  , errors =
                      Just $
                        errors
                          <&> ((\(Response _ errs) -> errs) >>> toList)
                          & P.concat
                            <&> (\(Error msg _ _) -> String msg)
                  }
          Right response -> pure response
