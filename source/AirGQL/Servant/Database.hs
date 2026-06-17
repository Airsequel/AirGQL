{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with maybe" #-}

module AirGQL.Servant.Database (
  apiDatabaseSchemaGetHandler,
  apiDatabaseVacuumPostHandler,
) where

import Protolude (
  Applicative (pure),
  FilePath,
  MonadIO (liftIO),
  Monoid (mempty),
  Text,
  ($),
 )

import Data.Aeson (Object)
import Database.SQLite.Simple qualified as SS
import Servant.Server qualified as Servant

import AirGQL.ExternalAppContext (ExternalAppContext)
import AirGQL.Utils (
  runSqliteCommand,
  withRetryConn,
 )


apiDatabaseSchemaGetHandler ::
  ExternalAppContext ->
  -- | Path to the SQLite database file to open
  FilePath ->
  Servant.Handler Text
apiDatabaseSchemaGetHandler ctx dbFilePath = do
  runSqliteCommand ctx dbFilePath ".schema"


apiDatabaseVacuumPostHandler ::
  -- | Path to the SQLite database file to open
  FilePath ->
  Servant.Handler Object
apiDatabaseVacuumPostHandler dbFilePath = do
  liftIO $ withRetryConn dbFilePath $ \conn ->
    SS.execute_ conn "VACUUM"
  pure mempty
