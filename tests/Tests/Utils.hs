{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

module Tests.Utils (
  testRoot,
  withDataDbConn,
  withTestDbConn,
  rmSpaces,
  dbPath,
  fixtureDbId,
  shouldSaveDbs,
) where

import Protolude (
  Bool (True),
  FilePath,
  IO,
  Maybe (Just, Nothing),
  Text,
  encodeUtf8,
  pure,
  ($),
  (&),
  (<>),
 )

import Data.Aeson qualified as Ae
import Data.ByteString.Lazy qualified as BL
import Database.SQLite.Simple qualified as SS
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))

import AirGQL.Utils (removeIfExists, withRetryConn)


testRoot :: FilePath
testRoot = "../../airgql/tests"


dbPath :: FilePath
dbPath = testRoot </> "fixture.db"


-- Although airsequel tries its best to be separate from Airsequel proper,
-- it looks like a bunch of functions still take database ids as arguments,
-- even though this concept doesn't exist in airgql. Example usages include:
-- - including the ID in error messages
-- - generating an url where the user can access a file when it's cell needs
--   to be converted to graphql
--
-- I don't think any of the usages matter when testing,
-- so we use a dummy id instead.
fixtureDbId :: Text
fixtureDbId = "fixtures-db"


-- | Save test databases after running tests for later inspection
shouldSaveDbs :: Bool
shouldSaveDbs = True


-- | Get a connection to a database in the test database directory
withTestDbConn :: FilePath -> (SS.Connection -> IO a) -> IO a
withTestDbConn testDbPath callback = do
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


rmSpaces :: Text -> BL.ByteString
rmSpaces text =
  let
    value :: Maybe Ae.Value =
      text
        & encodeUtf8
        & pure
        & BL.fromChunks
        & Ae.decode
  in
    case value of
      Just val -> Ae.encode val
      Nothing -> "ERROR: Failed to decode JSON"
