module AirGQL.Servant.GraphQL (
  gqlQueryGetHandler,
  gqlQueryPostHandler,
  playgroundDefaultQueryHandler,
) where

import Protolude (
  Applicative (pure),
  FilePath,
  MonadIO (liftIO),
  Monoid (mempty),
  Semigroup ((<>)),
  ($),
  (&),
 )
import Protolude qualified as P

import Control.Monad.Catch (catchAll)
import Data.Aeson (Object)
import Data.Text (Text)
import Data.Text qualified as T
import DoubleXEncoding (doubleXEncodeGql)
import Servant (NoContent, err303, errHeaders)
import Servant.Server qualified as Servant

import AirGQL.Lib (
  column_name,
  getColumns,
  getTableNames,
 )
import AirGQL.ServerUtils (executeQuery)
import AirGQL.Types.SchemaConf (SchemaConf)
import AirGQL.Types.Types (GQLPost (operationName, query, variables))
import AirGQL.Utils (
  throwErr400WithMsg,
  throwErr404WithMsg,
  withRetryConn,
 )


gqlQueryGetHandler :: Text -> Servant.Handler NoContent
gqlQueryGetHandler dbId =
  P.throwError
    err303
      { errHeaders =
          [("Location", P.encodeUtf8 $ "/dbs/" <> dbId <> "/graphiql")]
      }


gqlQueryPostHandler ::
  SchemaConf ->
  -- | Path to the SQLite database file to open
  FilePath ->
  -- | Database identifier (used for the schema name and generated file URLs)
  Text ->
  GQLPost ->
  Servant.Handler Object
gqlQueryPostHandler schemaConf dbFilePath dbId gqlPost = do
  let
    handleNoDbError :: P.SomeException -> Servant.Handler a
    handleNoDbError excpetion = do
      let errMsg = P.show excpetion

      if "unable to open database file" `T.isInfixOf` errMsg
        then
          throwErr404WithMsg $
            "Database \"" <> dbId <> "\" does not exist"
        else do
          P.putErrLn $
            "Error during execution of GraphQL query: " <> errMsg
          throwErr400WithMsg errMsg

  catchAll
    ( liftIO $
        executeQuery
          schemaConf
          dbFilePath
          dbId
          gqlPost.query
          (gqlPost.variables & P.fromMaybe mempty)
          gqlPost.operationName
    )
    handleNoDbError


playgroundDefaultQueryHandler ::
  -- | Path to the SQLite database file to open
  FilePath ->
  -- | Database identifier (used for column lookups)
  Text ->
  Servant.Handler Text
playgroundDefaultQueryHandler dbFilePath dbId = do
  liftIO $ withRetryConn dbFilePath $ \mainConn -> do
    tableEntries <- getTableNames mainConn

    case tableEntries of
      (headTable : _) -> do
        cols <- getColumns dbId mainConn headTable
        pure $
          P.fold
            [ "query "
            , doubleXEncodeGql headTable
            , "Query {\n"
            , "  "
            , doubleXEncodeGql headTable
            , "( limit: 100 ) {\n"
            , cols
                & P.foldMap
                  ( \col ->
                      "    " <> doubleXEncodeGql col.column_name <> "\n"
                  )
            , "  }\n"
            , "}"
            ]
      _ -> pure ""
