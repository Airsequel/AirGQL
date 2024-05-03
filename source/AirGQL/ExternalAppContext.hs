{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}
module AirGQL.ExternalAppContext (
  SandboxingConfig (..),
  ExternalAppContext (..),
  getExternalAppContext,
) where

import Protolude (
  FilePath,
  IO,
  Maybe (Just, Nothing),
  Show,
  Text,
  not,
  pure,
  ($),
  (&&),
  (/=),
  (<|>),
  (==),
 )
import Protolude qualified as P

import Data.ByteString qualified as BS
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.Info (os)
import System.Process.Typed (ExitCode (ExitSuccess), proc, readProcessStdout)


lookupBinaryPath :: Text -> IO (Maybe FilePath)
lookupBinaryPath name = do
  (code, resultBS) <- readProcessStdout $ proc "which" [T.unpack name]
  let result = T.strip $ P.decodeUtf8 $ BS.toStrict resultBS
  pure $
    if code == ExitSuccess
      && result /= ""
      && not ("which: no" `T.isInfixOf` result)
      then Just $ T.unpack result
      else Nothing


data SandboxingConfig = SandboxingConfig
  { firejail :: FilePath
  , extraBinds :: [FilePath]
  }
  deriving (Show)


data ExternalAppContext = ExternalAppContext
  { sqlite :: FilePath
  , sqliteLib :: Maybe FilePath
  , baseUrl :: Text
  }
  deriving (Show)


getExternalAppContext :: Text -> IO ExternalAppContext
getExternalAppContext baseUrl = do
  sqlite <- lookupBinaryPath "sqlite3"
  sqliteEnv <- lookupEnv "AIRGQL_SQLITE_BIN"
  sqliteLib <- lookupEnv "AIRGQL_SQLITE_LIB"

  pure $
    ExternalAppContext
      { baseUrl = baseUrl
      , sqlite = P.fromMaybe "/usr/bin/sqlite3" $ sqliteEnv <|> sqlite
      , sqliteLib =
          sqliteLib
            <|> if os == "darwin"
              then Just "/usr/local/opt/sqlite/lib/libsqlite3.dylib"
              else Nothing
      }
