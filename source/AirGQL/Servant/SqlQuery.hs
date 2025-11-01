module AirGQL.Servant.SqlQuery (
  getAffectedTables,
  sqlQueryPostHandler,
)
where

import Protolude (
  Applicative (pure),
  Either (Left, Right),
  Maybe (Just, Nothing),
  MonadIO (liftIO),
  Semigroup ((<>)),
  otherwise,
  show,
  when,
  ($),
  (&),
  (*),
  (-),
  (/=),
  (<&>),
  (>),
 )
import Protolude qualified as P

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Database.SQLite.Simple qualified as SS
import Language.SQL.SimpleSQL.Parse (prettyError)
import Language.SQL.SimpleSQL.Syntax (Statement (CreateTable))
import Servant.Server qualified as Servant
import System.Timeout (timeout)

import AirGQL.Config (defaultConfig, sqlTimeoutTime)
import AirGQL.Lib (
  SQLPost (query),
  TableEntryRaw (sql, tbl_name),
  getTables,
  lintTableCreationCode,
  parseSql,
  sqlDataToAesonValue,
  sqliteErrorToText,
 )
import AirGQL.Types.PragmaConf (PragmaConf, getSQLitePragmas)
import AirGQL.Types.SqlQueryPostResult (
  SqlQueryPostResult (
    SqlQueryPostResult,
    affectedTables,
    columns,
    errors,
    rows,
    runtimeSeconds
  ),
  resultWithErrors,
 )
import AirGQL.Utils (
  getMainDbPath,
  throwErr400WithMsg,
  withRetryConn,
 )


getAffectedTables :: [TableEntryRaw] -> [TableEntryRaw] -> [Text]
getAffectedTables pre post =
  let
    loop left right = do
      case (left, right) of
        ([], _) -> right <&> tbl_name
        (_, []) -> left <&> tbl_name
        (headLeft : tailLeft, headRight : tailRight) ->
          case P.compare headLeft.tbl_name headRight.tbl_name of
            P.LT -> headLeft.tbl_name : loop tailLeft right
            P.GT -> headRight.tbl_name : loop left tailRight
            P.EQ
              | headLeft.sql /= headRight.sql ->
                  headLeft.tbl_name : loop tailLeft tailRight
              | otherwise ->
                  loop tailLeft tailRight
  in
    loop
      (P.sortOn tbl_name pre)
      (P.sortOn tbl_name post)


sqlQueryPostHandler ::
  PragmaConf ->
  Text ->
  SQLPost ->
  Servant.Handler SqlQueryPostResult
sqlQueryPostHandler pragmaConf dbId sqlPost = do
  let maxSqlQueryLength :: P.Int = 100_000

  when (T.length sqlPost.query > maxSqlQueryLength) $ do
    throwErr400WithMsg $
      "SQL query is too long ("
        <> show (T.length sqlPost.query)
        <> " characters, maximum is "
        <> show maxSqlQueryLength
        <> ")"

  validationErrors <- liftIO $ case parseSql sqlPost.query of
    Left error -> pure [prettyError error]
    Right statement@(CreateTable{}) ->
      withRetryConn (getMainDbPath dbId) $ \conn ->
        lintTableCreationCode (Just conn) statement
    _ -> pure []

  case validationErrors of
    [] -> do
      let
        dbFilePath = getMainDbPath dbId
        microsecondsPerSecond = 1000000 :: P.Int

        timeoutTimeMicroseconds =
          defaultConfig.sqlTimeoutTime
            * microsecondsPerSecond

      let sqlitePragmas = getSQLitePragmas pragmaConf

      let
        performSqlOperations =
          withRetryConn dbFilePath $ \conn -> do
            preTables <- getTables conn

            P.for_ sqlitePragmas $ SS.execute_ conn
            SS.execute_ conn "PRAGMA foreign_keys = True"

            let query = SS.Query sqlPost.query

            columnNames <- SS.withStatement conn query $ \statement -> do
              numCols <- SS.columnCount statement
              P.for [0 .. (numCols - 1)] $ SS.columnName statement

            tableRowsMb :: Maybe [[SS.SQLData]] <-
              timeout timeoutTimeMicroseconds $ SS.query_ conn query
            changes <- SS.changes conn

            postTables <- getTables conn

            pure $ case tableRowsMb of
              Just tableRows ->
                Right (columnNames, tableRows, changes, preTables, postTables)
              Nothing -> Left "Sql query execution timed out"

      startTime <- liftIO getCurrentTime
      sqlResults <-
        liftIO $
          P.catches
            performSqlOperations
            [ P.Handler $
                \(error :: SS.SQLError) -> pure $ Left $ sqliteErrorToText error
            , P.Handler $
                \(error :: SS.ResultError) -> pure $ Left $ show error
            , P.Handler $
                \(error :: SS.FormatError) -> pure $ Left $ show error
            ]
      endTime <- liftIO getCurrentTime

      let measuredTime =
            nominalDiffTimeToSeconds
              (diffUTCTime endTime startTime)

      case sqlResults of
        Left error ->
          pure $ resultWithErrors measuredTime [error]
        Right (columnNames, tableRows, changes, preTables, postTables) -> do
          -- TODO: Use GQL error format {"message": "…", "code": …, …} instead
          let
            keys = columnNames <&> Key.fromText

            rowList =
              tableRows
                <&> \row ->
                  row
                    <&> sqlDataToAesonValue ""
                    & P.zip keys
                    & KeyMap.fromList

            affectedTables =
              if changes > 0
                then postTables <&> tbl_name
                else getAffectedTables preTables postTables

          pure $
            SqlQueryPostResult
              { rows = rowList
              , columns = columnNames
              , runtimeSeconds = measuredTime
              , affectedTables = affectedTables
              , errors = []
              }
    _ ->
      pure $
        resultWithErrors
          0
          validationErrors
