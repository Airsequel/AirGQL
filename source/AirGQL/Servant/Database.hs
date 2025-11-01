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
  MonadIO (liftIO),
  Monoid (mempty),
  ($),
 )

import Data.Aeson (Object)
import Data.Text (Text)
import Database.SQLite.Simple qualified as SS
import Servant.Server qualified as Servant

import AirGQL.ExternalAppContext (ExternalAppContext)
import AirGQL.Utils (
  getMainDbPath,
  runSqliteCommand,
  withRetryConn,
 )


apiDatabaseSchemaGetHandler ::
  ExternalAppContext ->
  Text ->
  Servant.Handler Text
apiDatabaseSchemaGetHandler ctx dbId = do
  runSqliteCommand ctx (getMainDbPath dbId) ".schema"


apiDatabaseVacuumPostHandler ::
  Text ->
  Servant.Handler Object
apiDatabaseVacuumPostHandler dbId = do
  liftIO $ withRetryConn (getMainDbPath dbId) $ \conn ->
    SS.execute_ conn "VACUUM"
  pure mempty
