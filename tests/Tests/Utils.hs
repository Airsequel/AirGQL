module Tests.Utils (
  testRoot,
  withDataDbConn,
  withTestDbConn,
) where

import Protolude (
  Bool (True),
  FilePath,
  IO,
  ($),
  (<>),
 )

import Database.SQLite.Simple qualified as SS
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))

import AirGQL.Utils (removeIfExists, withRetryConn)


testRoot :: FilePath
testRoot = "../../airgql/tests"


-- | Get a connection to a database in the test database directory
withTestDbConn :: Bool -> FilePath -> (SS.Connection -> IO a) -> IO a
withTestDbConn shouldSaveDbs testDbPath callback = do
  removeIfExists $ testRoot </> testDbPath
  withRetryConn
    (if shouldSaveDbs then testRoot </> testDbPath else ":memory:")
    callback


-- | Get a connection to a test database in the main data directory
withDataDbConn :: FilePath -> (SS.Connection -> IO a) -> IO a
withDataDbConn testDbDir callback = do
  let fullPath = "data" </> "databases" </> "_TEST_" <> testDbDir
  removePathForcibly fullPath
  createDirectoryIfMissing True fullPath
  withRetryConn (fullPath </> "main.sqlite") callback
