module AirGQL.Servant.SqlQuery (
  getAffectedTables,
  sqlQueryPostHandler,
)
where

import Protolude (
  Applicative (pure),
  Either (Left, Right),
  FilePath,
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
  (<$>),
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
  splitSqlStatements,
  sqlDataToAesonValue,
  sqliteErrorToText,
  validateStatementSafety,
 )
import AirGQL.Types.PragmaConf (PragmaConf, getSQLitePragmas)
import AirGQL.Types.SqlQueryPostResult (
  SqlQueryPostResult (
    SqlQueryPostResult,
    affectedTables,
    columns,
    errors,
    numChanges,
    rows,
    runtimeSeconds
  ),
  resultWithErrors,
 )
import AirGQL.Utils (
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
  -- | Path to the SQLite database file to open
  FilePath ->
  SQLPost ->
  Servant.Handler SqlQueryPostResult
sqlQueryPostHandler pragmaConf dbFilePath sqlPost = do
  let maxSqlQueryLength :: P.Int = 100_000

  when (T.length sqlPost.query > maxSqlQueryLength) $ do
    throwErr400WithMsg $
      "SQL query is too long ("
        <> show (T.length sqlPost.query)
        <> " characters, maximum is "
        <> show maxSqlQueryLength
        <> ")"

  statements <- liftIO $ splitSqlStatements sqlPost.query

  -- Each statement is validated on its own, as they are also
  -- executed one at a time.
  validationErrors <-
    liftIO $
      if P.null statements
        then pure ["No SQL statements were provided"]
        else withRetryConn dbFilePath $ \conn ->
          statements
            & P.traverse
              ( \statementTxt -> case validateStatementSafety statementTxt of
                  errors@(_ : _) -> pure errors
                  [] -> case parseSql statementTxt of
                    Left error -> pure [prettyError error]
                    Right statement@(CreateTable{}) ->
                      lintTableCreationCode (Just conn) statement
                    _ -> pure []
              )
              <&> P.concat

  case validationErrors of
    [] -> do
      let
        microsecondsPerSecond = 1000000 :: P.Int

        timeoutTimeMicroseconds =
          defaultConfig.sqlTimeoutTime
            * microsecondsPerSecond

      let sqlitePragmas = getSQLitePragmas pragmaConf

      let
        -- Execute a single statement and
        -- return its column names and result rows
        runStatement conn statementTxt =
          SS.withStatement conn (SS.Query statementTxt) $ \statement -> do
            numCols <- SS.columnCount statement
            columnNames <-
              P.for [0 .. (numCols - 1)] $ SS.columnName statement
            let
              collectRows = do
                rowMb :: Maybe [SS.SQLData] <- SS.nextRow statement
                case rowMb of
                  Just row -> (row :) <$> collectRows
                  Nothing -> pure []
            rows <- collectRows
            pure (columnNames, rows)

        performSqlOperations =
          withRetryConn dbFilePath $ \conn -> do
            preTables <- getTables conn

            P.for_ sqlitePragmas $ SS.execute_ conn
            SS.execute_ conn "PRAGMA foreign_keys = True"

            changesBefore <- SS.totalChanges conn

            -- Statements are executed in order and the rows of the
            -- last statement are returned (like in the `sqlite3` CLI)
            resultsMb <-
              timeout timeoutTimeMicroseconds $
                P.for statements $
                  runStatement conn

            changesAfter <- SS.totalChanges conn
            postTables <- getTables conn

            pure $ case resultsMb of
              Just results ->
                let
                  (columnNames, tableRows) =
                    results & P.lastMay & P.fromMaybe ([], [])
                in
                  Right
                    ( columnNames
                    , tableRows
                    , changesAfter - changesBefore
                    , preTables
                    , postTables
                    )
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
              , numChanges = changes
              , runtimeSeconds = measuredTime
              , affectedTables = affectedTables
              , errors = []
              }
    _ ->
      pure $
        resultWithErrors
          0
          validationErrors
