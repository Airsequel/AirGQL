{-# LANGUAGE DeriveDataTypeable #-}
-- To look up git hash
{-# LANGUAGE TemplateHaskell #-}

-- Necessary for cmdArgs
{-# OPTIONS -Wno-partial-fields #-}

module Main (main) where

import Protolude (
  Applicative (pure),
  Eq ((==)),
  FilePath,
  IO,
  Int,
  Maybe (Just),
  Semigroup ((<>)),
  Show,
  Text,
  const,
  putText,
  repeat,
  show,
  ($),
  (&),
  (||),
 )
import Protolude qualified as P

import Data.Data (Data)
import Data.Text qualified as T
import GitHash (giDirty, giTag, tGitInfoCwd)
import Network.HTTP.Client.MultipartFormData ()
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setOnException,
  setPort,
 )
import Network.Wai.Middleware.Cors (
  cors,
  corsMethods,
  corsRequestHeaders,
  simpleCorsResourcePolicy,
  simpleMethods,
 )
import System.Console.CmdArgs as CmdArgs (
  Default (def),
  args,
  auto,
  cmdArgs,
  help,
  modes,
  program,
  summary,
  typ,
  (&=),
 )

import AirGQL.ExternalAppContext (getExternalAppContext)
import AirGQL.Utils (
  getGraphiQLVersion,
  getSqliteBinaryVersion,
  getSqliteEmbeddedVersion,
  withRetryConn,
 )
import Server.Server (platformApp)


data Cli
  = Help
  | Version
  | -- Start the AirGQL server
    -- serving the GraphQL endpoint for the specified SQLite database
    Serve
      { dbFilePath :: FilePath
      -- TODO: , readOnly :: Bool
      }
  deriving (Show, Data)


cliHelp :: Cli
cliHelp =
  Help
    &= auto


cliVersion :: Cli
cliVersion = Version


cliServe :: Cli
cliServe =
  Serve
    { dbFilePath = def &= typ "Path to database file" &= args
    -- TODO: , readOnly = def
    }
    &= help "Serve database via GraphQL"


corsMiddleware :: Middleware
corsMiddleware =
  let
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type", "Authorization"]
        , corsMethods = "PUT" : simpleMethods
        }
  in
    cors (const $ Just policy)


-- | Imitates output from `git describe --always --dirty`
versionSlug :: Text
versionSlug =
  T.pack $
    giTag $$tGitInfoCwd
      <> (if giDirty $$tGitInfoCwd then "-dirty" else "")


main :: IO ()
main = do
  let
    port :: Int = 4189

    separatorLine = "\n" <> T.concat (P.take 80 $ repeat "=")
    separatorLineThin = "\n" <> T.concat (P.take 80 $ repeat "-")

    runWarp =
      runSettings $
        defaultSettings
          & setPort port
          & setOnException
            ( \_ exception -> do
                let exceptionText :: Text = show exception
                if (exceptionText == "Thread killed by timeout manager")
                  || ( exceptionText
                        == "Warp: Client closed connection prematurely"
                     )
                  then pure ()
                  else do
                    putText exceptionText
            )

    buildBanner ::
      Text ->
      Text ->
      Text ->
      Text ->
      Text
    buildBanner
      sqliteEmbeddedVersion
      sqliteBinaryVersion
      graphiQLVersion
      baseUrl =
        separatorLine
          <> "\n\n"
          <> "AirGQL Server\n"
          <> separatorLineThin
          <> "\n\n"
          <> "Version:\t\t  "
          <> versionSlug
          <> "\n\
             \GraphQL URL:\t\t  "
          <> baseUrl
          <> "/graphql"
          <> "\n\
             \\n\
             \SQLite Embedded version:  "
          <> sqliteEmbeddedVersion
          <> "\n\
             \SQLite Binary version:\t  "
          <> sqliteBinaryVersion
          <> "\n\
             \GraphiQL version:\t  "
          <> graphiQLVersion
          <> "\n"
          <> separatorLine
          <> "\n"

  providedArgs <-
    cmdArgs $
      modes
        [ cliHelp
        , cliVersion
        , cliServe
        ]
        &= program "airgql"
        &= summary (T.unpack versionSlug)
        &= help "Automatic GraphQL API generation for SQLite databases"

  case providedArgs of
    Help ->
      putText "Run `airgql --help` for detailed usage instructions"
    ----------
    Version ->
      putText versionSlug
    ----------
    Serve{dbFilePath} -> do
      withRetryConn dbFilePath $ \conn -> do
        P.when (dbFilePath == "") $
          P.die "ERROR: No database file path was specified"

        let baseUrl :: Text = "http://localhost:" <> show port
        ctx <- getExternalAppContext baseUrl
        sqliteEmbeddedVersion <- getSqliteEmbeddedVersion conn
        sqliteBinaryVersion <- getSqliteBinaryVersion ctx
        graphiQLVersion <- getGraphiQLVersion

        putText $
          buildBanner
            sqliteEmbeddedVersion
            sqliteBinaryVersion
            graphiQLVersion
            baseUrl

        runWarp $ corsMiddleware $ platformApp ctx dbFilePath
