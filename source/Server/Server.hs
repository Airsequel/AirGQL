-- Necessary for servant-docs instances
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Server.Server (platformAPI, platformApp)
where

import Protolude (
  Int,
  Monoid (mempty),
  Proxy (Proxy),
  ($),
 )
import Protolude qualified as P

import Data.Aeson (Object, Value, object)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Network.Wai (Application)
import Network.Wai.Parse (
  defaultParseRequestBodyOptions,
  setMaxRequestFilesSize,
  setMaxRequestNumFiles,
 )
import Servant (Context (EmptyContext, (:.)), NoContent)
import Servant.API (
  Capture,
  Get,
  JSON,
  PlainText,
  Post,
  ReqBody,
  (:<|>) ((:<|>)),
  (:>),
 )
import Servant.Docs (
  DocCapture (DocCapture),
  ToCapture (toCapture),
  ToSample,
  singleSample,
  toSamples,
 )
import Servant.HTML.Blaze (HTML)
import Servant.Multipart (
  MultipartData (MultipartData),
  MultipartOptions (generalOptions),
  Tmp,
  ToMultipartSample (toMultipartSamples),
  defaultMultipartOptions,
 )
import Servant.Server (Server)
import Servant.Server qualified as Servant
import Text.Blaze.Internal (MarkupM)

import AirGQL.Config (Config (maxDbSize), defaultConfig)
import AirGQL.ExternalAppContext (ExternalAppContext)
import AirGQL.Lib (SQLPost)
import AirGQL.Servant.Database (
  apiDatabaseSchemaGetHandler,
  apiDatabaseVacuumPostHandler,
 )
import AirGQL.Servant.GraphQL (
  gqlQueryGetHandler,
  gqlQueryPostHandler,
  playgroundDefaultQueryHandler,
  readOnlyGqlPostHandler,
  writeOnlyGqlPostHandler,
 )
import AirGQL.Servant.SqlQuery (sqlQueryPostHandler)
import AirGQL.Types.SchemaConf (SchemaConf (pragmaConf), defaultSchemaConf)
import AirGQL.Types.SqlQueryPostResult (SqlQueryPostResult)
import AirGQL.Types.Types (GQLPost)


{- FOURMOLU_DISABLE -}
-- ATTENTION: Order of handlers matters!
type PlatformAPI =
  -- gqlQueryGetHandler
  -- Redirect to GraphiQL playground
  "graphql" :> Get '[HTML] NoContent

  -- gqlQueryPostHandler
  :<|> "graphql"
      :> ReqBody '[JSON] GQLPost
      :> Post '[JSON] Object

  -- writeOnlyGqlPostHandler
  :<|> "readonly" :> "graphql"
          :> ReqBody '[JSON] GQLPost
          :> Post '[JSON] Object

  -- writeOnlyGqlPostHandler
  :<|> "writeonly" :> "graphql"
          :> ReqBody '[JSON] GQLPost
          :> Post '[JSON] Object

  -- playgroundDefaultQueryHandler
  :<|> "playground" :> "default-query"
          :> Get '[PlainText] Text

  -- apiDatabaseSchemaGetHandler
  :<|> "schema" :> Get '[PlainText] Text

  -- apiDatabaseVacuumPostHandler
  :<|> "vacuum" :> Post '[JSON] Object

  -- sqlQueryPostHandler
  :<|> "sql"
          :> ReqBody '[JSON] SQLPost
          :> Post '[JSON] SqlQueryPostResult

{- FOURMOLU_ENABLE -}


-- | Instances for automatic documentation generation via servant-docs
instance ToSample (MultipartData Tmp) where
  toSamples _ = singleSample $ MultipartData mempty mempty


instance ToMultipartSample Tmp (MultipartData Tmp) where
  toMultipartSamples _ = []


instance ToSample Value where
  toSamples _ = singleSample $ object []


instance ToSample (KeyMap.KeyMap Value) where
  toSamples _ = singleSample $ KeyMap.fromList []


instance ToSample (MarkupM ()) where
  toSamples _ = singleSample mempty


instance ToSample BL.ByteString where
  toSamples _ = singleSample mempty


instance ToSample Text where
  toSamples _ = singleSample mempty


instance ToSample P.ByteString where
  toSamples _ = singleSample mempty


instance ToCapture (Capture "readonlyId" Text) where
  toCapture _ = DocCapture "readonlyId" "Read-only ID of the database"


instance ToCapture (Capture "dbId" Text) where
  toCapture _ = DocCapture "dbId" "ID of the database to be served"


platformAPI :: Proxy PlatformAPI
platformAPI = Proxy


platformServer :: ExternalAppContext -> P.FilePath -> Server PlatformAPI
platformServer ctx filePath = do
  let dbPath = T.pack filePath
  gqlQueryGetHandler dbPath
    :<|> gqlQueryPostHandler defaultSchemaConf dbPath
    :<|> readOnlyGqlPostHandler dbPath
    :<|> writeOnlyGqlPostHandler dbPath
    :<|> playgroundDefaultQueryHandler dbPath
    :<|> apiDatabaseSchemaGetHandler ctx dbPath
    :<|> apiDatabaseVacuumPostHandler dbPath
    :<|> sqlQueryPostHandler defaultSchemaConf.pragmaConf dbPath


platformApp :: ExternalAppContext -> P.FilePath -> Application
platformApp ctx filePath = do
  let
    maxFileSizeInByte :: Int = defaultConfig.maxDbSize

    multipartOpts :: MultipartOptions Tmp
    multipartOpts =
      (defaultMultipartOptions (Proxy :: Proxy Tmp))
        { generalOptions =
            setMaxRequestNumFiles 1 $
              setMaxRequestFilesSize
                (P.fromIntegral maxFileSizeInByte)
                defaultParseRequestBodyOptions
        }

    context :: Context '[MultipartOptions Tmp]
    context =
      multipartOpts :. EmptyContext

  Servant.serveWithContext platformAPI context $ platformServer ctx filePath
