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
import Data.Text qualified as T
import Database.SQLite.Simple qualified as SS
import Language.GraphQL.Error (Error (Error), Response (Response))
import Language.GraphQL.JSON (graphql)
import System.FilePath (pathSeparator, (</>))

import AirGQL.GraphQL (getDerivedSchema)
import AirGQL.Lib (getEnrichedTables)
import AirGQL.Types.SchemaConf (SchemaConf)
import AirGQL.Types.Types (
  GQLResponse (GQLResponse, data_, errors),
  gqlResponseToObject,
 )


executeQuery ::
  SchemaConf ->
  Text ->
  FilePath ->
  Text ->
  Object ->
  Maybe Text ->
  IO Object
executeQuery schemaConf dbIdOrPath reqDir query vars opNameMb = do
  let dbFilePath =
        if pathSeparator `T.elem` dbIdOrPath || '.' `T.elem` dbIdOrPath
          then T.unpack dbIdOrPath
          else reqDir </> "main.sqlite"

  theConn <- SS.open dbFilePath
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
      schema <- getDerivedSchema schemaConf theConn dbIdOrPath tables
      result <- graphql schema opNameMb vars query
      SS.close theConn

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
