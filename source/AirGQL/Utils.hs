-- For embedded SQL queries
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}
-- HLint can't figure out where TemplateHaskell is used,
-- even though it throws an error without the pragma.
{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module AirGQL.Utils (
  collectAllErrorsAsText,
  collectErrorList,
  colToFileUrl,
  escDoubleQuotes,
  escSingleQuotes,
  getDbDir,
  getGraphiQLVersion,
  getMainDbPath,
  getOrderOfLinkedList,
  getReadOnlyFilePath,
  getDbIdFromReadOnlyId,
  getSqliteBinaryVersion,
  getSqliteEmbeddedVersion,
  headerJsonContent,
  quoteKeyword,
  quoteText,
  removeIfExists,
  runSqliteCommand,
  throwErr400WithMsg,
  throwErr404WithMsg,
  throwErr500WithMsg,
  withRetryConn,
  DiffKind (..),
) where

import Protolude (
  Applicative (pure),
  ExitCode (ExitFailure, ExitSuccess),
  FilePath,
  IO,
  Maybe (Just, Nothing),
  Monoid (mempty),
  Semigroup ((<>)),
  Text,
  catch,
  liftIO,
  not,
  show,
  throwError,
  throwIO,
  when,
  ($),
  (&),
  (.),
  (/=),
  (<&>),
 )
import Protolude qualified as P

import Control.Monad.Catch (catchAll)
import Data.Aeson (KeyValue ((.=)), Value (String), encode, object)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Either.Extra (mapLeft)
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as SS
import Network.HTTP.Types (HeaderName, encodePathSegments)
import Servant.Server (
  ServerError (errBody, errHeaders),
  err400,
  err404,
  err500,
 )
import Servant.Server qualified as Servant
import System.Directory (removeFile)
import System.FilePath (takeFileName, (</>))
import System.IO.Error (IOError, isDoesNotExistError)
import System.Posix.Files (readSymbolicLink)
import System.Process (readProcess)
import System.Process.Typed (
  byteStringInput,
  createPipe,
  proc,
  readProcessInterleaved,
  setStderr,
  setStdin,
  setStdout,
 )

import AirGQL.ExternalAppContext (ExternalAppContext (sqlite))


getDbDir :: Text -> FilePath
getDbDir dbId =
  "data"
    </> "databases"
    </> T.unpack dbId


getMainDbPath :: Text -> FilePath
getMainDbPath dbId =
  getDbDir dbId
    </> "main.sqlite"


getReadOnlyFilePath :: Text -> FilePath
getReadOnlyFilePath readonlyId =
  "data" </> "readonly" </> T.unpack readonlyId


getSqliteEmbeddedVersion :: Connection -> IO Text
getSqliteEmbeddedVersion conn = do
  sqliteEmbeddedVersion <-
    SS.query_
      conn
      "select sqlite_version()"
      :: IO [[SS.SQLData]]
  case sqliteEmbeddedVersion of
    [[SS.SQLText verTxt]] -> pure verTxt
    _ -> pure mempty


getSqliteBinaryVersion :: ExternalAppContext -> IO Text
getSqliteBinaryVersion ctx = do
  P.fmap (T.strip . T.pack) $
    readProcess
      ctx.sqlite
      ["--safe", ":memory:"]
      (T.unpack "select sqlite_version()")


getGraphiQLVersion :: IO Text
getGraphiQLVersion = do
  -- let packageJson :: BL.ByteString =
  --       $( "package.json"
  --           & makeRelativeToProject
  --           P.>>= embedStringFile
  --        )
  --
  -- pure $
  --   (Aeson.decode packageJson :: Maybe Object)
  --     P.>>= KeyMap.lookup "dependencies"
  --     P.>>= ( \case
  --               Aeson.Object o -> KeyMap.lookup "graphiql" o
  --               _ -> Nothing
  --           )
  --     P.>>= ( \case
  --               Aeson.String s -> Just s
  --               _ -> Nothing
  --           )
  --     & fromMaybe ""
  pure "TODO"


-- | Escape double quotes in SQL strings
escDoubleQuotes :: Text -> Text
escDoubleQuotes =
  T.replace "\"" "\"\""


-- | Quote a keyword in an SQL query
quoteKeyword :: Text -> Text
quoteKeyword keyword =
  keyword
    & escDoubleQuotes
    & (\word -> "\"" <> word <> "\"")


-- | Escape single quotes in SQL strings
escSingleQuotes :: Text -> Text
escSingleQuotes =
  T.replace "'" "''"


-- | Quote literal text in an SQL query
quoteText :: Text -> Text
quoteText keyword =
  keyword
    & escSingleQuotes
    & (\word -> "'" <> word <> "'")


headerJsonContent :: [(HeaderName, BS.ByteString)]
headerJsonContent =
  [("Content-Type", "application/json;charset=utf-8")]


-- | Throw the specified server error with a message
throwServerErrorWithMsg :: ServerError -> Text -> Servant.Handler a
throwServerErrorWithMsg serverError errorMsg =
  throwError $
    serverError
      { errHeaders = headerJsonContent
      , errBody =
          encode $
            object
              ["errors" .= [String errorMsg]]
      }


-- | Throw an "400 Bad Request" error with a message
throwErr400WithMsg :: Text -> Servant.Handler a
throwErr400WithMsg = throwServerErrorWithMsg err400


-- | Throw an "404 Not Found" error with a message
throwErr404WithMsg :: Text -> Servant.Handler a
throwErr404WithMsg = throwServerErrorWithMsg err404


-- | Throw an "500 Internal Server Error" error with a message
throwErr500WithMsg :: Text -> Servant.Handler a
throwErr500WithMsg = throwServerErrorWithMsg err500


{-| Get the order of a linked list.
 | Each tuple is `(name, previous name in list)`.
 | The first's element previous name is `Nothing`.
 | Tries to find the longest chain of elements if no start element is found.
 | It's quite complicated to also handle incomplete orderings correctly.
-}
getOrderOfLinkedList :: [(Text, Maybe Text)] -> [Text]
getOrderOfLinkedList tables =
  let
    findAfter :: [(Text, Maybe Text)] -> (Text, Maybe Text) -> [Text]
    findAfter remaining (tableName, previousTableMb) =
      P.maybeToList previousTableMb
        <> case P.find ((P.== Just tableName) P.. P.snd) remaining of
          Just found@(name, _) ->
            let remaining' = List.filter (/= found) remaining
            in  tableName : findAfter remaining' (name, Nothing)
          Nothing -> [tableName]
  in
    if P.null tables
      then []
      else
        let
          sortByLength :: [[Text]] -> [[Text]]
          sortByLength =
            P.sortBy (\x y -> P.compare (P.length y) (P.length x))

          chainsByLength =
            tables
              <&> findAfter tables
              & sortByLength

          -- First table ist always the (x, Nothing) table entry
          firstElement =
            case P.find ((P.== Nothing) P.. P.snd) tables of
              Just tableEntry -> [P.fst tableEntry]
              Nothing -> []
        in
          -- Sort them by length, combine them, and remove duplicates
          ([firstElement] <> chainsByLength)
            & P.concat
            & List.nub


getDbIdFromReadOnlyId :: Text -> IO (Maybe Text)
getDbIdFromReadOnlyId readOnlyId = do
  catchAll
    ( do
        dbId <- liftIO $ readSymbolicLink $ getReadOnlyFilePath readOnlyId
        pure $ Just $ T.pack $ takeFileName dbId
    )
    ( \err -> do
        when (not $ "does not exist" `P.isInfixOf` show err) $ do
          P.putErrText $ "Error while reading readonly symlink:\n" <> show err
        pure Nothing
    )


colToFileUrl :: Text -> Text -> Text -> Text -> Text
colToFileUrl readonlyId tableName colName rowid =
  T.decodeUtf8 $
    BL.toStrict $
      toLazyByteString $
        encodePathSegments
          [ "readonly"
          , readonlyId
          , "tables"
          , tableName
          , "columns"
          , colName
          , "files"
          , "rowid"
          , rowid
          ]


removeIfExists :: FilePath -> IO ()
removeIfExists fileName =
  let
    handleExists :: IOError -> IO ()
    handleExists e
      | isDoesNotExistError e = pure ()
      | P.otherwise = throwIO e
  in
    removeFile fileName `catch` handleExists


runSqliteCommand :: ExternalAppContext -> FilePath -> BL.ByteString -> Servant.Handler Text
runSqliteCommand ctx dbPath command = do
  let
    processConfig =
      setStdin
        (byteStringInput command)
        $ setStdout createPipe
        $ setStderr createPipe
        $ proc ctx.sqlite [dbPath]

  (exitCode, output) <- readProcessInterleaved processConfig

  let outputText = P.decodeUtf8 $ BS.toStrict output

  case exitCode of
    ExitSuccess ->
      pure outputText
    ExitFailure _ ->
      throwErr500WithMsg outputText


-- | Similar to `sequence`, except it doesn't stop on the first error.
collectErrorList :: [P.Either e b] -> P.Either [e] [b]
collectErrorList results =
  case P.lefts results of
    [] -> P.Right (P.rights results)
    lefts -> P.Left lefts


{-|
Similar to `sequence`, except it doesn't stop on the first error.
What differentiates this from `collectErrorList` is
that it also merges the errors into a single error message.
-}
collectAllErrorsAsText :: [P.Either Text b] -> P.Either Text [b]
collectAllErrorsAsText results =
  collectErrorList results
    & mapLeft
      ( \lefts ->
          "Multiple errors occurred:\n" <> P.unlines lefts
      )


data DiffKind = Added | Removed | Kept
  deriving (P.Eq, P.Ord, P.Show)


{-| Run an action with a connection, retrying if the database is busy.
| Necessary because of WAL mode:
| https://sqlite.org/wal.html#sometimes_queries_return_sqlite_busy_in_wal_mode
-}
withRetryConn :: FilePath -> (Connection -> IO a) -> IO a
withRetryConn filePath action = do
  SS.withConnection filePath $ \conn -> do
    SS.execute_ conn "PRAGMA busy_timeout = 5000;" -- 5 seconds
    action conn
