-- Unit and integration tests
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

import Protolude (
  Applicative (pure),
  Bool (False, True),
  Either (Right),
  FilePath,
  IO,
  Maybe (Just, Nothing),
  Monoid (mempty),
  Text,
  fromMaybe,
  readFile,
  show,
  ($),
  (&),
  (.),
  (<$),
  (<>),
 )
import Protolude qualified as P

import Control.Monad.Catch (catchAll)
import Data.Aeson (Value (Number))
import Data.Aeson qualified as Ae
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Object)
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Internal.Strict qualified as HashMap
import Data.List qualified as List
import Data.Text (pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding as T (encodeUtf8)
import Database.SQLite.Simple (
  Query,
  SQLData (SQLFloat, SQLInteger, SQLNull, SQLText),
  execute_,
  open,
  query_,
 )
import Database.SQLite.Simple qualified as SS
import Database.SQLite.Simple.QQ (sql)
import Language.GraphQL.JSON (graphql)
import Language.GraphQL.TH (gql)
import Language.GraphQL.Type as GQL (Value (Object, String))
import Servant.Server (runHandler)
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.FilePath ((</>))
import System.Posix (changeWorkingDirectory, getWorkingDirectory)
import Test.Hspec (
  SpecWith,
  before_,
  describe,
  it,
  pendingWith,
  shouldBe,
  shouldContain,
  shouldMatchList,
 )
import Test.Hspec qualified as Hspec

import AirGQL.GraphQL (getDerivedSchema)
import AirGQL.Introspection (createType)
import AirGQL.Lib (
  AccessMode (WriteOnly),
  ColumnEntry (
    ColumnEntry,
    column_name,
    column_name_gql,
    datatype,
    datatype_gql,
    dflt_value,
    isGenerated,
    isOmittable,
    isUnique,
    notnull,
    primary_key,
    select_options
  ),
  GqlTypeName (GqlTypeName, full, root),
  ObjectType (Table),
  SQLPost (SQLPost),
  TableEntryRaw (
    TableEntryRaw,
    name,
    object_type,
    rootpage,
    tbl_name
  ),
  getColumns,
  getTables,
  replaceCaseInsensitive,
  stringToGqlTypeName,
 )
import AirGQL.Lib qualified
import AirGQL.Raw (raw)
import AirGQL.Servant.SqlQuery (sqlQueryPostHandler)
import AirGQL.Types.PragmaConf qualified as PragmaConf
import AirGQL.Types.SchemaConf (SchemaConf (accessMode), defaultSchemaConf)
import AirGQL.Types.SqlQueryPostResult (
  SqlQueryPostResult (
    affectedTables,
    errors,
    rows
  ),
 )
import AirGQL.Types.Utils (encodeToText)
import AirGQL.Utils (
  getOrderOfLinkedList,
  removeIfExists,
  withRetryConn,
 )
import Tests.Utils (testRoot, withDataDbConn, withTestDbConn)


-- | Save test databases after running tests for later inspection
shouldSaveDbs :: Bool
shouldSaveDbs = True


dbPath :: Text
dbPath = T.pack (testRoot </> "fixture.db")


rmSpaces :: Text -> BL.ByteString
rmSpaces text =
  let
    value :: Maybe Ae.Value =
      text
        & T.encodeUtf8
        & pure
        & BL.fromChunks
        & Ae.decode
  in
    case value of
      Just val -> Ae.encode val
      Nothing -> "ERROR: Failed to decode JSON"


createUsersTableQuery :: Query
createUsersTableQuery =
  [sql|
    CREATE TABLE IF NOT EXISTS "users" (
      "name"              TEXT,
      "email"             TEXT NOT NULL UNIQUE,
      "created_utc"       TEXT NOT NULL,
      "number_of_logins"  INTEGER,
      "progress"          REAL,
      PRIMARY KEY("email")
    );
  |]


createUsersTable :: SS.Connection -> IO ()
createUsersTable conn =
  execute_ conn createUsersTableQuery


createSongsTableQuery :: Query
createSongsTableQuery =
  [sql|
    CREATE TABLE IF NOT EXISTS "songs" (
      "name"              TEXT NOT NULL,
      "duration_seconds"  INTEGER NOT NULL
    );
  |]


createSongsTable :: SS.Connection -> IO ()
createSongsTable conn =
  execute_ conn createSongsTableQuery


-- prettyPrintSchema :: Schema m -> Text
-- prettyPrintSchema schema =
--   let
--     ObjectType name queryDesc interfaceTypes resolvers = query schema
--   in ""
--   <> "Description:\n" <> show (description schema) <> "\n\n"
--   <> "Query:\n\n"
--   <> "  Name:\n" <> name <> "\n\n"
--   <> "  Description:\n" <> show queryDesc <> "\n\n"
--   <> "  InterfaceTypes:\n" <> show interfaceTypes <> "\n\n"
--   <> "  Resolvers:\n" <> show resolvers <> "\n\n"
--   <> "Mutation:\n\n" <> show (mutation schema) <> "\n\n"
--   <> "Subscriptions:\n" <> show (subscription schema) <> "\n\n"
--   -- "Directives:\n" <> show (directives schema) <> "\n\n" <>
--   -- "Types:\n" <> show (types schema) <> "\n\n" <>
--   -- "Interface Implementations:\n" <> show (implementations schema) <> "\n\n"
--   <> ""

testSuite :: SpecWith ()
testSuite = do
  describe "Utils" $ do
    -- Hspec.fit "pretty prints the schema" $ do
    --   conn <- open $ unpack dbPath
    --   tables <- getTables conn
    --   schema <-
    --     getDerivedSchema defaultSchemaConf conn dbPath tables

    --   putText $ prettyPrintSchema schema

    --   True `shouldBe` True

    it "replaces strings case insensitively" $ do
      let
        results =
          [ replaceCaseInsensitive "hello" "hi" "hello world"
          , replaceCaseInsensitive "hello" "hi" "Hello World"
          , replaceCaseInsensitive "l" "L" "Hello World"
          ]

      results `shouldBe` ["hi world", "hi World", "HeLLo WorLd"]

    it "loads all tables from database" $ do
      tables <- do
        conn <- open $ unpack dbPath
        getTables conn

      shouldBe
        tables
        [ TableEntryRaw
            { name = "users"
            , tbl_name = "users"
            , object_type = Table
            , rootpage = 2
            , AirGQL.Lib.sql =
                "\
                \CREATE TABLE \"users\" (\n\
                \      \"name\"              TEXT,\n\
                \      \"email\"             TEXT NOT NULL UNIQUE,\n\
                \      \"created_utc\"       TEXT NOT NULL,\n\
                \      \"number_of_logins\"  INTEGER,\n\
                \      \"progress\"          REAL,\n\
                \      PRIMARY KEY(\"email\")\n\
                \    )"
            }
        , TableEntryRaw
            { name = "songs"
            , tbl_name = "songs"
            , object_type = Table
            , rootpage = 4
            , AirGQL.Lib.sql =
                "\
                \CREATE TABLE \"songs\" (\n\
                \      \"name\"              TEXT NOT NULL,\n\
                \      \"duration_seconds\"  INTEGER NOT NULL\n\
                \    )"
            }
        ]

    describe "getColumns" $ do
      it "loads all columns from users table" $ do
        tableColumns <- do
          conn <- open $ unpack dbPath
          getColumns dbPath conn "users"

        let
          columnsExpected =
            [ ColumnEntry
                { column_name = "rowid"
                , column_name_gql = "rowid"
                , datatype = "INTEGER"
                , datatype_gql = Just $ stringToGqlTypeName "Int"
                , select_options = Nothing
                , notnull = True
                , isUnique = True
                , isOmittable = True
                , isGenerated = False
                , dflt_value = Nothing
                , primary_key = True
                }
            , ColumnEntry
                { column_name = "name"
                , column_name_gql = "name"
                , datatype = "TEXT"
                , datatype_gql = Just $ stringToGqlTypeName "String"
                , select_options = Nothing
                , notnull = False
                , isUnique = False
                , isOmittable = True
                , isGenerated = False
                , dflt_value = Nothing
                , primary_key = False
                }
            , ColumnEntry
                { column_name = "email"
                , column_name_gql = "email"
                , datatype = "TEXT"
                , datatype_gql = Just $ stringToGqlTypeName "String"
                , select_options = Nothing
                , notnull = True
                , isUnique = True
                , isOmittable = False
                , isGenerated = False
                , dflt_value = Nothing
                , primary_key = True
                }
            , ColumnEntry
                { column_name = "created_utc"
                , column_name_gql = "created_utc"
                , datatype = "TEXT"
                , datatype_gql = Just $ stringToGqlTypeName "String"
                , select_options = Nothing
                , notnull = True
                , isUnique = False
                , isOmittable = False
                , isGenerated = False
                , dflt_value = Nothing
                , primary_key = False
                }
            , ColumnEntry
                { column_name = "number_of_logins"
                , column_name_gql = "number_of_logins"
                , datatype = "INTEGER"
                , datatype_gql = Just $ stringToGqlTypeName "Int"
                , select_options = Nothing
                , notnull = False
                , isUnique = False
                , isOmittable = True
                , isGenerated = False
                , dflt_value = Nothing
                , primary_key = False
                }
            , ColumnEntry
                { column_name = "progress"
                , column_name_gql = "progress"
                , datatype = "REAL"
                , datatype_gql = Just $ stringToGqlTypeName "Float"
                , select_options = Nothing
                , notnull = False
                , isUnique = False
                , isOmittable = True
                , isGenerated = False
                , dflt_value = Nothing
                , primary_key = False
                }
            ]

        tableColumns `shouldBe` columnsExpected

      it "loads a nullable single-select column" $ do
        let dbName = "creates_nullable_single-select.db"
        withTestDbConn shouldSaveDbs dbName $ \conn -> do
          execute_
            conn
            [sql|
              CREATE TABLE checks (
                color TEXT CHECK (color IN ('red', 'green', 'blue'))
              )
            |]

          tableColumns <- getColumns dbName conn "checks"

          let
            columnExpected =
              ColumnEntry
                { column_name = "color"
                , column_name_gql = "color"
                , datatype = "TEXT"
                , datatype_gql =
                    Just $
                      GqlTypeName
                        { full = "checks_color_String"
                        , root = "String"
                        }
                , select_options = Just ["red", "green", "blue"]
                , notnull = False
                , isUnique = False
                , isOmittable = True
                , isGenerated = False
                , dflt_value = Nothing
                , primary_key = False
                }

          P.lastMay tableColumns `shouldBe` Just columnExpected

      it "loads a non-null single-select column" $ do
        let dbName = "creates_non-null_single-select.db"
        withTestDbConn shouldSaveDbs dbName $ \conn -> do
          execute_
            conn
            [sql|
              CREATE TABLE checks (
                color TEXT CHECK (color IN ('red', 'green', 'blue')) NOT NULL
              )
            |]

          tableColumns <- getColumns dbName conn "checks"

          let
            columnExpected =
              ColumnEntry
                { column_name = "color"
                , column_name_gql = "color"
                , datatype = "TEXT"
                , datatype_gql =
                    Just $
                      GqlTypeName
                        { full = "checks_color_String"
                        , root = "String"
                        }
                , select_options = Just ["red", "green", "blue"]
                , notnull = True
                , isUnique = False
                , isOmittable = False
                , isGenerated = False
                , dflt_value = Nothing
                , primary_key = False
                }

          P.lastMay tableColumns `shouldBe` Just columnExpected

      it "coerces a multi-type single-select column to text" $ do
        let dbName = "multi-type_single-select.db"
        withTestDbConn shouldSaveDbs dbName $ \conn -> do
          execute_
            conn
            [sql|
              CREATE TABLE checks (
                value TEXT CHECK (value IN (1, 2.2, 'three'))
              )
            |]

          tableColumns <- getColumns dbName conn "checks"

          let
            columnExpected =
              ColumnEntry
                { column_name = "value"
                , column_name_gql = "value"
                , datatype = "TEXT"
                , datatype_gql =
                    Just $
                      GqlTypeName
                        { full = "checks_value_String"
                        , root = "String"
                        }
                , select_options = Just ["1", "2.2", "three"]
                , notnull = False
                , isUnique = False
                , isOmittable = True
                , isGenerated = False
                , dflt_value = Nothing
                , primary_key = False
                }

          P.lastMay tableColumns `shouldBe` Just columnExpected

      it "marks integer primary keys as omittable" $ do
        let dbName = "integer-omittable-primary-key.db"
        withTestDbConn shouldSaveDbs dbName $ \conn -> do
          execute_
            conn
            [sql|
              CREATE TABLE items (
                id INTEGER PRIMARY KEY
              )
            |]

          tableColumns <- getColumns dbName conn "items"

          let
            columnExpected =
              ColumnEntry
                { column_name = "id"
                , column_name_gql = "id"
                , datatype = "INTEGER"
                , datatype_gql = Just $ stringToGqlTypeName "Int"
                , select_options = Nothing
                , notnull = True
                , isUnique = True
                , isOmittable = True
                , isGenerated = False
                , dflt_value = Nothing
                , primary_key = True
                }

          P.lastMay tableColumns `shouldBe` Just columnExpected

      it "correctly parses generated columns" $ do
        let dbName = "generated-columns.db"
        withTestDbConn shouldSaveDbs dbName $ \conn -> do
          execute_
            conn
            [sql|
              CREATE TABLE "generated_columns" (
                "start" REAL NOT NULL,
                "end" REAL NOT NULL,
                "distance_km" REAL GENERATED ALWAYS AS ("end" - "start") STORED,
                "distance_miles" REAL GENERATED ALWAYS AS (distance_km * 1.6) VIRTUAL
              )
            |]

          tableColumns <- getColumns dbName conn "generated_columns"

          let (generated, non_generated) =
                -- => [ColumnDef]
                tableColumns
                  -- => ([ColumnDef], [ColumnDef])
                  & List.partition (\c -> c.isGenerated)
                  -- => (Text, Text)
                  & P.bimap
                    (P.fmap column_name)
                    (P.fmap column_name)

          -- We use `shouldMatchList` because the order does not matter
          non_generated `shouldMatchList` ["start", "end", "rowid"]
          generated `shouldMatchList` ["distance_km", "distance_miles"]

    it "sorts the tables from the metadata table" $ do
      getOrderOfLinkedList [("a", Nothing), ("b", Just "a"), ("c", Just "b")]
        `shouldBe` ["a", "b", "c"]

      getOrderOfLinkedList [("a", Nothing), ("c", Just "b"), ("b", Just "a")]
        `shouldBe` ["a", "b", "c"]

      getOrderOfLinkedList [("b", Just "a"), ("c", Just "b"), ("a", Nothing)]
        `shouldBe` ["a", "b", "c"]

      getOrderOfLinkedList [("a", Just "x"), ("c", Just "b"), ("b", Just "a")]
        `shouldBe` ["x", "a", "b", "c"]

      getOrderOfLinkedList [("c", Just "b"), ("b", Just "a"), ("a", Just "x")]
        `shouldBe` ["x", "a", "b", "c"]

      getOrderOfLinkedList [("a", Nothing), ("b", Just "a"), ("c", Nothing)]
        `shouldBe` ["a", "b", "c"]

      getOrderOfLinkedList [("a", Nothing), ("b", Nothing), ("c", Nothing)]
        `shouldBe` ["a", "b", "c"]

      getOrderOfLinkedList [("a", Nothing), ("b", Just "a"), ("d", Just "c")]
        `shouldBe` ["a", "b", "c", "d"]

      -- Nothing must always be first
      getOrderOfLinkedList [("c", Just "b"), ("d", Just "c"), ("a", Nothing)]
        `shouldBe` ["a", "b", "c", "d"]

      -- Should *not* error out on cycles
      getOrderOfLinkedList [("c", Just "a"), ("b", Just "a"), ("a", Just "b")]
        `shouldBe` ["a", "b", "c"]

  describe "REST API" $ do
    describe "SQL" $ do
      it "supports retrieving the schema" $ do
        -- let query = "CREATE TABLE users (name TEXT, email TEXT);"
        -- let dbPost = DatabasePost "test-db" (Just "test-team") "" (Just query)

        -- Right schemaResult <-
        --   runHandler $
        --     apiDatabaseSchemaGetHandler ctx "TODO"

        -- T.strip schemaResult `shouldBe` query
        pendingWith "Create database first"

      it "supports executing an SQL query" $ do
        let dbId = "api-sql-query"
        withDataDbConn dbId $ \_ -> do
          Right result <-
            runHandler $
              sqlQueryPostHandler
                PragmaConf.defaultConf
                ("_TEST_" <> dbId)
                SQLPost{query = "SELECT TRUE, 123, 'test'"}

          result.rows
            `shouldBe` [ KeyMap.fromList
                          [ ("TRUE", Number 1)
                          , ("123", Number 123)
                          , ("'test'", "test")
                          ]
                       ]

          result.affectedTables `shouldMatchList` []

      it "return columns in the requested order" $ do
        -- Even though JSON objects are unordered by definition,
        -- the fields (columns) must be returned in the requested order
        -- as Elm relies on it for decoding.

        let dbId = "api-sql-col-order"
        withDataDbConn dbId $ \_ -> do
          Right result <-
            runHandler $
              sqlQueryPostHandler
                PragmaConf.defaultConf
                ("_TEST_" <> dbId)
                SQLPost{query = "SELECT 0 AS b, 0 AS a"}

          (result & encodeToText & T.unpack)
            `shouldContain` "[{\"b\":0,\"a\":0}]"

          result.affectedTables `shouldMatchList` []

      it "supports using math functions" $ do
        let dbId = "api-sql-math-query"
        withDataDbConn dbId $ \_ -> do
          Right result <-
            runHandler $
              sqlQueryPostHandler
                PragmaConf.defaultConf
                ("_TEST_" <> dbId)
                SQLPost{query = "SELECT abs(floor(cos(2 * pi() / 3)))"}

          result.rows
            `shouldBe` [ KeyMap.singleton
                          "abs(floor(cos(2 * pi() / 3)))"
                          (Number 1.0)
                       ]

          result.affectedTables `shouldMatchList` []

      -- The following few tests verify that different operations return
      -- the proper set of affected tables. This query creates a completely
      -- unrelated table just to ensure it is not getting returned by the
      -- methods tested below
      let createUnrelatedTable = do
            [sql|
              CREATE TABLE IF NOT EXISTS todos (
                title TEXT,
                content TEXT,
                done INTEGER
              )
            |]

      -- This query creates a table commonly used by the following few tests.
      let createNotesTable = do
            [sql|
              CREATE TABLE IF NOT EXISTS notes (
                content TEXT
              )
            |]

      it "should not allow rowid references" $ do
        let dbId = "api-sql-rowid-references"
        let query = "CREATE TABLE foo ( bar TEXT references goo(rowid) )"
        withDataDbConn dbId $ \_ -> do
          Right result <-
            runHandler $
              sqlQueryPostHandler
                PragmaConf.defaultConf
                ("_TEST_" <> dbId)
                SQLPost{query = query}

          let
            expectedMessage =
              "Column 'bar' references the rowid column of table 'goo'.\n"
                <> "This is not supported by SQLite:\n"
                <> "https://www.sqlite.org/foreignkeys.html"

          result.errors `shouldBe` [expectedMessage]

      it "should error out on `without rowid` table creation" $ do
        let dbId = "api-sql-without-rowid"
        let query = "CREATE TABLE foo (bar INTEGER PRIMARY KEY) WITHOUT ROWID"
        withDataDbConn dbId $ \_ -> do
          Right result <-
            runHandler $
              sqlQueryPostHandler
                PragmaConf.defaultConf
                ("_TEST_" <> dbId)
                SQLPost{query = query}

          let
            expectedMessage =
              "Table 'foo' does not have a rowid column. "
                <> "Such tables are not currently supported by Airsequel."

          result.errors `shouldBe` [expectedMessage]

      it "should return no affected tables on a simple select" $ do
        let dbId = "api-sql-simple-select"
        withDataDbConn dbId $ \conn -> do
          execute_ conn createUnrelatedTable
          execute_ conn createNotesTable

          execute_
            conn
            [sql|
              INSERT INTO notes VALUES ("hello")
            |]

          Right result <-
            runHandler $
              sqlQueryPostHandler
                PragmaConf.defaultConf
                ("_TEST_" <> dbId)
                SQLPost{query = "SELECT * from notes"}

          result.rows
            `shouldBe` [KeyMap.singleton "content" "hello"]

          result.affectedTables `shouldMatchList` []

      it "should return the correct affected tables when creating tables" $ do
        let dbId = "api-sql-simple-create"
        withDataDbConn dbId $ \conn -> do
          execute_ conn createUnrelatedTable
          Right result <-
            runHandler $
              sqlQueryPostHandler
                PragmaConf.defaultConf
                ("_TEST_" <> dbId)
                SQLPost
                  { query = SS.fromQuery createNotesTable
                  }

          result.affectedTables
            `shouldMatchList` ["notes"]

      it "should return the correct affected tables when dropping tables" $ do
        let dbId = "api-sql-simple-drop"
        withDataDbConn dbId $ \conn -> do
          execute_ conn createUnrelatedTable
          execute_ conn createNotesTable

          Right result <-
            runHandler $
              sqlQueryPostHandler
                PragmaConf.defaultConf
                ("_TEST_" <> dbId)
                SQLPost{query = "DROP TABLE notes"}

          result.affectedTables `shouldMatchList` ["notes"]

      it "should return the correct affected tables when editing tables" $ do
        let dbId = "api-sql-add-column"
        withDataDbConn dbId $ \conn -> do
          execute_ conn createUnrelatedTable
          execute_ conn createNotesTable

          Right result <-
            runHandler $
              sqlQueryPostHandler
                PragmaConf.defaultConf
                ("_TEST_" <> dbId)
                SQLPost
                  { query =
                      "ALTER TABLE notes ADD COLUMN title TEXT"
                  }

          result.affectedTables `shouldMatchList` ["notes"]

      it "should mark every table as affected when changes are detected" $ do
        let dbId = "api-sql-insert-values"
        withDataDbConn dbId $ \conn -> do
          execute_ conn createUnrelatedTable
          execute_ conn createNotesTable

          Right result <-
            runHandler $
              sqlQueryPostHandler
                PragmaConf.defaultConf
                ("_TEST_" <> dbId)
                SQLPost
                  { query =
                      [raw| INSERT INTO "notes" VALUES ("hello") |]
                  }

          result.affectedTables `shouldMatchList` ["notes", "todos"]

  describe "Queries" $ do
    it "supports retrieving data" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values ('Adrian', 'adrian@example.com', '2021-01-01T00:00Z')
        |]

      tables <- getTables conn
      schema <-
        getDerivedSchema
          defaultSchemaConf
          conn
          dbPath
          tables

      Right result <- graphql schema Nothing mempty "{ users { name } }"
      Ae.encode result
        `shouldBe` [gql|
        {"data":{"users":[{"name":"Adrian"}]}}
      |]

    it "supports retrieving data from tables with special names" $ do
      let testDbPath = testRoot </> "special_table_name.db"
      removeIfExists testDbPath
      let dbPathNorm = if shouldSaveDbs then testDbPath else ":memory:"

      withRetryConn dbPathNorm $ \conn -> do
        execute_
          conn
          [sql|
            CREATE TABLE "name with-special$chars" (
              name TEXT
            )
          |]
        execute_
          conn
          [sql|
            INSERT INTO "name with-special$chars" (name)
            VALUES ('John')
          |]

        tables <- getTables conn
        schema <-
          getDerivedSchema
            defaultSchemaConf
            conn
            dbPath
            tables

        Right result <-
          graphql
            schema
            Nothing
            mempty
            "{ nameXX0withXXDspecialXX4chars { name } }"
        Ae.encode result
          `shouldBe` [gql|
          {"data":{"nameXX0withXXDspecialXX4chars":[{"name":"John"}]}}
        |]

    describe "column names with special characters" $ do
      let
        dbPathSpaces :: FilePath = testRoot </> "spaces-test.db"
        setupDatabaseSpaces = do
          removeIfExists dbPathSpaces
          conn <- open dbPathSpaces
          execute_
            conn
            [sql|
              CREATE TABLE IF NOT EXISTS test_entries (
                id INTEGER PRIMARY KEY,
                `column with spaces` TEXT
              )

            |]
          execute_
            conn
            [sql|
            INSERT INTO test_entries (id, `column with spaces`)
            VALUES (0, 'Just a test')
          |]

      before_ setupDatabaseSpaces $ it "supports column names with spaces" $ do
        conn <- open dbPathSpaces
        tables <- getTables conn
        schema <-
          getDerivedSchema
            defaultSchemaConf
            conn
            (pack dbPathSpaces)
            tables
        Right result <-
          graphql
            schema
            Nothing
            mempty
            "{ test_entries { columnXX0withXX0spaces } }"
        Ae.encode result
          `shouldBe` rmSpaces
            [raw|
          { "data": {
              "test_entries": [
                { "columnXX0withXX0spaces": "Just a test" }
              ]
          }}
        |]

      before_ setupDatabaseSpaces $
        it "generates introspection schema for column names with spaces" $
          do
            conn <- open dbPathSpaces
            tables <- getTables conn
            schema <-
              getDerivedSchema
                defaultSchemaConf
                conn
                (pack dbPathSpaces)
                tables
            Right result <-
              graphql
                schema
                Nothing
                mempty
                "{ __schema{ types { name fields { name } } } } }"
            show (Ae.encode result) `shouldContain` "columnXX0withXX0spaces"

    it "avoids column name remapping collisions" $ do
      let dbPathSpaces = testRoot </> "spaces-collision-test.db"
      removeIfExists dbPathSpaces
      conn <- open dbPathSpaces
      execute_
        conn
        [sql|
          CREATE TABLE IF NOT EXISTS test_entries (
            id INTEGER PRIMARY KEY,
            `the column` TEXT,
            `the_column` TEXT,
            `the_column_1` TEXT,
            `the_column_2` TEXT
          )
        |]
      execute_
        conn
        [sql|
        INSERT INTO test_entries
          (id, `the column`, the_column, the_column_1, the_column_2)
        VALUES
          (0, 'with spaces', 'no spaces', 'no spaces 1', 'no spaces 2')
      |]

      tables <- getTables conn
      schema <-
        getDerivedSchema
          defaultSchemaConf
          conn
          (pack dbPathSpaces)
          tables

      Right result <-
        graphql
          schema
          Nothing
          mempty
          [gql|
          { test_entries {
              theXX0column
              the_column
              the_column_1
              the_column_2
            }
          }
        |]
      Ae.encode result
        `shouldBe` rmSpaces
          [raw|
        { "data": {
            "test_entries": [
              { "theXX0column": "with spaces",
                "the_column":   "no spaces",
                "the_column_1": "no spaces 1",
                "the_column_2": "no spaces 2"
              }
            ]
        }}
      |]

    it "includes rowid and sorts by rowid" $ do
      let dbPathSpaces = testRoot </> "rowid_test.db"
      removeIfExists dbPathSpaces
      conn <- open dbPathSpaces

      execute_
        conn
        [sql|
          CREATE TABLE IF NOT EXISTS "users" (
            "email"             TEXT NOT NULL UNIQUE PRIMARY KEY,
            "number_of_logins"  INTEGER
          );
        |]
      execute_
        conn
        [sql|
          INSERT INTO users (email, number_of_logins)
          VALUES ('john@example.com', 0), ('eve@example.com', 4);
        |]

      tables <- getTables conn
      schema <-
        getDerivedSchema
          defaultSchemaConf
          conn
          (pack dbPathSpaces)
          tables

      Right result <-
        graphql
          schema
          Nothing
          mempty
          [gql|
          {
            users {
              rowid
            }
          }
        |]
      Ae.encode result
        `shouldBe` rmSpaces
          [raw|
        {
          "data": {
            "users": [
              { "rowid": 1 },
              { "rowid": 2 }
            ]
          }
        }
      |]

    it "supports aliases" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
        insert into users (name, email, created_utc)
        values ('Adrian', 'adrian@example.com', '2021-01-01T00:00Z')
      |]

      let
        query =
          [gql|
            {
              userA: users { name }
              userB: users { email }
            }
          |]
        expected =
          rmSpaces
            [raw|
          {"data":{
            "userB":[{"email":"adrian@example.com"}],
            "userA":[{"name":"Adrian"}]
          }}
        |]

      tables <- getTables conn
      schema <-
        getDerivedSchema
          defaultSchemaConf
          conn
          dbPath
          tables

      Right result <- graphql schema Nothing mempty query
      Ae.encode result `shouldBe` expected

    it "supports fragments" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
        insert into users (name, email, created_utc)
        values ('Adrian', 'adrian@example.com', '2021-01-01T00:00Z')
      |]

      let
        query =
          [gql|
            {
              userA: users { ...basicFields }
              userB: users { ...basicFields }
            }

            fragment basicFields on users {
              name
              email
            }
          |]
        expected =
          rmSpaces
            [raw|
            { "data": {
              "userB":[{"email":"adrian@example.com","name":"Adrian"}],
              "userA":[{"email":"adrian@example.com","name":"Adrian"}]
            }}
          |]

      tables <- getTables conn
      schema <-
        getDerivedSchema
          defaultSchemaConf
          conn
          dbPath
          tables

      Right result <- graphql schema Nothing mempty query
      Ae.encode result `shouldBe` expected

    it "supports directives" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values
          ('John', 'john@example.com', '2021-01-01T00:00Z'),
          ('Eve', 'eve@example.com', '2019-01-01T00:00Z')
        |]

      let
        query =
          [gql|
            query DirectiveTest ($withName: Boolean!) {
              users {
                name @include(if: $withName)
              }
            }
          |]

        variables :: Object
        variables =
          fromMaybe mempty $ Ae.decode "{ \"withName\": true }"

        expected =
          rmSpaces
            [raw|
            { "data": {
              "users": [
                { "name": "John" },
                { "name": "Eve" }
              ]
            }}
          |]
      tables <- getTables conn
      schema <-
        getDerivedSchema
          defaultSchemaConf
          conn
          dbPath
          tables

      Right result <- graphql schema Nothing variables query

      Ae.encode result `shouldBe` expected

    it "supports retrieving records with a filter" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values
          ('John', 'john@example.com', '2021-01-01T00:00Z'),
          ('Eve', 'eve@example.com', '2019-01-01T00:00Z')
        |]

      tables <- getTables conn
      schema <-
        getDerivedSchema
          defaultSchemaConf
          conn
          dbPath
          tables

      Right result <-
        graphql
          schema
          Nothing
          mempty
          [gql|
          {
            users (filter: {email: {eq: "eve@example.com"}}) {
              name
            }
          }
        |]

      Ae.encode result
        `shouldBe` "{\"data\":{\"users\":[{\"name\":\"Eve\"}]}}"

    it "supports retrieving records with a filter over int and float" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
          insert into users (name, email, created_utc, progress)
          values
          ('John', 'john@example.com', '2021-01-01T00:00Z', 0.7),
          ('Eve', 'eve@example.com', '2019-01-01T00:00Z', 0.4)
        |]

      tables <- getTables conn
      schema <-
        getDerivedSchema
          defaultSchemaConf
          conn
          dbPath
          tables

      Right result1 <-
        graphql
          schema
          Nothing
          mempty
          [gql|{
          users (filter: {rowid: {eq: 2}}) {
            name
          }
        }|]

      Ae.encode result1
        `shouldBe` "{\"data\":{\"users\":[{\"name\":\"Eve\"}]}}"

      Right result2 <-
        graphql
          schema
          Nothing
          mempty
          [gql|{
          users (filter: {progress: {eq: 0.4}}) {
            name
          }
        }|]

      Ae.encode result2
        `shouldBe` "{\"data\":{\"users\":[{\"name\":\"Eve\"}]}}"

    it "supports retrieving records with a filter over boolean and null" $ do
      let dbPathFilter = testRoot </> "filter_eq_boolean.db"
      removeIfExists dbPathFilter
      conn <- open dbPathFilter
      execute_
        conn
        [sql|
          CREATE TABLE IF NOT EXISTS "users" (
            "name"              TEXT,
            "is_admin"          BOOLEAN
          );
        |]
      execute_
        conn
        [sql|
          INSERT INTO USERS (name, is_admin)
          VALUES
          ('John', TRUE),
          ('Eve', FALSE),
          ('Anna', NULL)
        |]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables

      Right result1 <-
        graphql
          schema
          Nothing
          mempty
          [gql|{
          users (filter: {is_admin: {eq: false}}) {
            name
          }
        }|]

      Ae.encode result1
        `shouldBe` "{\"data\":{\"users\":[{\"name\":\"Eve\"}]}}"

      Right result2 <-
        graphql
          schema
          Nothing
          mempty
          [gql|{
          users (filter: {is_admin: {eq: null}}) {
            name
          }
        }|]

      Ae.encode result2
        `shouldBe` "{\"data\":{\"users\":[{\"name\":\"Anna\"}]}}"

    it "supports retrieving records with like and ilike filter" $ do
      withRetryConn (unpack dbPath) $ \conn -> do
        execute_
          conn
          [sql|
            insert into users (name, email, created_utc)
            values
            ('John', 'john@example.com', '2021-01-01T00:00Z'),
            ('Anna', 'anna@EXAMPLE.COM', '2019-01-01T00:00Z'),
            ('Eve', 'eve@evil.com', '2019-01-01T00:00Z')
          |]

        tables <- getTables conn
        schema <-
          getDerivedSchema
            defaultSchemaConf
            conn
            dbPath
            tables

        Right likeResult <-
          graphql
            schema
            Nothing
            mempty
            [gql|
              {
                users (filter: {email: {like: "%example%"}}) {
                  name
                }
              }
            |]

        Ae.encode likeResult
          `shouldBe` "{\"data\":{\"users\":[{\"name\":\"John\"}]}}"

        Right ilikeResult <-
          graphql
            schema
            Nothing
            mempty
            [gql|
              {
                users (filter: {email: {ilike: "%example%"}}) {
                  name
                }
              }
            |]

        Ae.encode ilikeResult
          `shouldBe` "{\"data\":{\"users\":\
                     \[{\"name\":\"John\"},{\"name\":\"Anna\"}]}}"

    it "supports retrieving records with several filters" $ do
      withRetryConn (unpack dbPath) $ \conn -> do
        execute_
          conn
          [sql|
            insert into users (name, email, created_utc)
            values
            ('John', 'john@example.com', '2021-01-01T00:00Z'),
            ('Anna', 'anna@example.com', '2019-01-01T00:00Z'),
            ('Eve', 'eve@evil.com', '2019-01-01T00:00Z')
          |]

        tables <- getTables conn
        schema <-
          getDerivedSchema
            defaultSchemaConf
            conn
            dbPath
            tables

        Right likeResult <-
          graphql
            schema
            Nothing
            mempty
            [gql|
              {
                users (
                  filter: {
                    email: { like: "%example%" },
                    name: { gt: "B" }
                  }
                ) {
                  name
                }
              }
            |]

        Ae.encode likeResult
          `shouldBe` "{\"data\":{\"users\":[{\"name\":\"John\"}]}}"

    it "supports mutating records with several filters" $ do
      withRetryConn (unpack dbPath) $ \conn -> do
        execute_
          conn
          [sql|
            insert into users (name, email, created_utc)
            values
            ('John', 'john@example.com', '2021-01-01T00:00Z'),
            ('Anna', 'anna@example.com', '2019-01-01T00:00Z'),
            ('Eve', 'eve@evil.com', '2019-01-01T00:00Z')
          |]

        tables <- getTables conn
        schema <-
          getDerivedSchema
            defaultSchemaConf
            conn
            dbPath
            tables

        Right likeResult <-
          graphql
            schema
            Nothing
            mempty
            [gql|
              mutation UpdateEmailAddress {
                update_users (
                  filter: {
                    email: { like: "%example%" }
                    name: { gt: "B" }
                  }
                  set: { email: "john@new.com" }
                ) {
                  affected_rows
                  returning {
                    email
                  }
                }
              }
            |]

        Ae.encode likeResult
          `shouldBe` "{\"data\":{\"update_users\":\
                     \{\"affected_rows\":1,\
                     \\"returning\":[{\"email\":\"john@new.com\"}]}}}"

    it "supports retrieving multi-type columns" $ do
      withTestDbConn shouldSaveDbs "multi-type_columns.db" $ \conn -> do
        execute_
          conn
          [sql|
            CREATE VIEW "multi_type_column" AS
            SELECT 1 AS col UNION
            SELECT 2.2 AS col UNION
            SELECT 'three' AS col UNION
            SELECT NULL AS col
          |]

        tables <- getTables conn
        schema <-
          getDerivedSchema defaultSchemaConf conn dbPath tables

        Right result1 <-
          graphql
            schema
            Nothing
            mempty
            [gql|{
          multi_type_column { col }
        }|]

        let expected =
              rmSpaces
                [raw|
              {
                "data": {
                  "multi_type_column": [
                    { "col": null },
                    { "col": "1" },
                    { "col": "2.2" },
                    { "col": "three" }
                  ]
                }
              }
            |]

        Ae.encode result1 `shouldBe` expected

    it "supports querying a single entry" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values
          ('John', 'john@example.com', '2021-01-01T00:00Z'),
          ('Eve', 'eve@example.com', '2019-01-01T00:00Z')
        |]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables

      Right result <-
        graphql
          schema
          Nothing
          mempty
          [gql|
          {
            users_by_pk (email: "eve@example.com") {
              name
            }
          }
        |]

      pendingWith "Not implemented yet"

      Ae.encode result
        `shouldBe` "{\"data\":{\"users\":[{\"name\":\"Eve\"}]}}"

    it "errors out on integer overflows" $ do
      withTestDbConn shouldSaveDbs "integer-overflows.db" $ \conn -> do
        execute_
          conn
          [sql|
            CREATE TABLE test (
              big INTEGER,
              alsobig INTEGER
            )
          |]

        execute_
          conn
          [sql|
            INSERT INTO test(big, alsobig)
            VALUES (8000000000, 9000000000)
          |]

        tables <- getTables conn
        schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
        Right result <-
          graphql
            schema
            Nothing
            mempty
            [gql|
              {
                test {
                  big,
                  alsobig
                }
              }
            |]

        let expected =
              rmSpaces
                [raw|
                  {
                    "data": {
                      "test": null
                    },
                    "errors": [{
                      "locations": [{
                        "column": 3,
                        "line": 2
                      }],
                      "message":
                        "user error (Multiple errors occurred:\nOn column \"big\": Integer 8000000000 would overflow. This happens because SQLite uses 64-bit ints, but GraphQL uses 32-bit ints. Use a Number (64-bit float) or Text column instead.\nOn column \"alsobig\": Integer 9000000000 would overflow. This happens because SQLite uses 64-bit ints, but GraphQL uses 32-bit ints. Use a Number (64-bit float) or Text column instead.\n)",
                      "path": ["test"]
                    }]
                  }
                |]

        Ae.encode result `shouldBe` expected

  describe "Mutations" $ do
    it "supports inserting data" $ do
      let
        query =
          [gql|
            mutation InsertUser {
              insert_users(objects: [
                {
                  name: "John",
                  email: "john@example.com",
                  created_utc: "2021-11-21T09:51Z"
                },
                {
                  email: "eve@example.com",
                  created_utc: "2021-11-21T09:51Z"
                }
              ]) {
                affected_rows
              }
            }
          |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "insert_users": {
                  "affected_rows": 2
                }
              }
            }
          |]

      conn <- open $ unpack dbPath
      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

      allUsers <- query_ conn "select * from users"

      allUsers
        `shouldBe` [
                     [ SQLText "John"
                     , SQLText "john@example.com"
                     , SQLText "2021-11-21T09:51Z"
                     , SQLNull
                     , SQLNull
                     ]
                   ,
                     [ SQLNull
                     , SQLText "eve@example.com"
                     , SQLText "2021-11-21T09:51Z"
                     , SQLNull
                     , SQLNull
                     ]
                   ]

    it "supports inserting and retrieving booleans" $ do
      let testDbPath = testRoot </> "boolean-test.db"
      conn <- open testDbPath
      execute_
        conn
        [sql|
          CREATE TABLE IF NOT EXISTS checks (
            id INTEGER PRIMARY KEY,
            completed BOOLEAN DEFAULT (FALSE) NOT NULL
          )
        |]
      execute_ conn "DELETE FROM checks"
      execute_
        conn
        [sql|
          INSERT INTO checks (id, completed)
          VALUES (1, 0), (2, 1), (3, FALSE), (4, TRUE)
        |]

      tables <- getTables conn
      schema <-
        getDerivedSchema
          defaultSchemaConf
          conn
          (T.pack testDbPath)
          tables

      let
        mutation =
          [gql|
            mutation InsertChecks {
              insert_checks(objects: [
                { id: 5, completed: true },
              ]) {
                affected_rows
              }
            }
          |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "insert_checks": {
                  "affected_rows": 1
                }
              }
            }
          |]

      Right result <- graphql schema Nothing mempty mutation

      Ae.encode result `shouldBe` expected

      allUsers <- query_ conn "select * from checks"

      allUsers
        `shouldBe` [ [SQLInteger 1, SQLInteger 0]
                   , [SQLInteger 2, SQLInteger 1]
                   , [SQLInteger 3, SQLInteger 0]
                   , [SQLInteger 4, SQLInteger 1]
                   , [SQLInteger 5, SQLInteger 1]
                   ]

      let
        query =
          [gql|
            query GetChecks {
              checks { id, completed }
            }
          |]
        expectedResult =
          rmSpaces
            [raw|
            {"data":{"checks":[
              {"id":1,"completed":false},
              {"id":2,"completed":true},
              {"id":3,"completed":false},
              {"id":4,"completed":true},
              {"id":5,"completed":true}
            ]}}
          |]

      Right queryResult <- graphql schema Nothing mempty query

      Ae.encode queryResult `shouldBe` expectedResult

    it "supports inserting empty records" $ do
      let testDbPath = testRoot </> "empty-record-insertion.db"
      SS.withConnection testDbPath $ \conn -> do
        execute_
          conn
          [sql|
            CREATE TABLE IF NOT EXISTS items (
              id INTEGER PRIMARY KEY NOT NULL
            )
          |]
        execute_ conn "DELETE FROM items"

        tables <- getTables conn
        schema <-
          getDerivedSchema
            defaultSchemaConf
            conn
            (T.pack testDbPath)
            tables

        let
          mutation =
            [gql|
              mutation InsertItems {
                insert_items(objects: [{}]) {
                  affected_rows
                }
              }
            |]
          expected =
            rmSpaces
              [raw|
              {
                "data": {
                  "insert_items": {
                    "affected_rows": 1
                  }
                }
              }
            |]

        Right result <- graphql schema Nothing mempty mutation

        Ae.encode result `shouldBe` expected

        allItems <- query_ conn "select * from items"
        allItems `shouldBe` [[SQLInteger 1]]

    it "treats NULL as a value when using 'neq' filters" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values
            (NULL, 'john@example.com', '2021-01-01T00:00Z'),
            ('Eve', 'eve@example.com', '2021-01-02T00:00Z')
        |]

      let
        query =
          [gql|
            query {
              users(filter: { name: { neq: "Eve" }}) {
                name, email
              }
            }
          |]

        expected =
          rmSpaces
            [raw|{
              "data": {
                "users": [{
                  "name": null,
                  "email": "john@example.com"
                }]
              }
            }|]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

    it "treats NULL as a value when using 'nin' filters" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values
            ('Eve', 'eve@example.com', '2021-01-01T00:00Z'),
            ('Jon', 'jon@example.com', '2021-01-02T00:00Z'),
            ('Arbuckle', 'arbuckle@example.com', '2021-01-03T00:00Z'),
            (NULL, 'adam@example.com', '2021-01-04T00:00Z')
        |]

      let
        query =
          [gql|
            query {
              users(filter: { name: { nin: ["Eve", "Arbuckle"]}}) {
                name, email
              }
            }
          |]

        expected =
          rmSpaces
            [raw|{
              "data": {
                "users": [{
                  "name": "Jon",
                  "email": "jon@example.com"
                }, {
                  "name": null,
                  "email": "adam@example.com"
                }]
              }
            }|]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

    it "supports 'in' filters" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values
            ('Eve', 'eve@example.com', '2021-01-01T00:00Z'),
            ('Jon', 'jon@example.com', '2021-01-02T00:00Z'),
            ('Arbuckle', 'arbuckle@example.com', '2021-01-03T00:00Z'),
            (NULL, 'adam@example.com', '2021-01-04T00:00Z')
        |]

      let
        query =
          [gql|
            query {
              users(filter: { name: { in: ["Eve", "Arbuckle"]}}) {
                name, email
              }
            }
          |]

        expected =
          rmSpaces
            [raw|{
              "data": {
                "users": [{
                  "name": "Eve",
                  "email": "eve@example.com"
                }, {
                  "name": "Arbuckle",
                  "email": "arbuckle@example.com"
                }]
              }
            }|]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

    it "supports inserting data and returning the created data" $ do
      let
        query =
          [gql|
            mutation InsertUsers {
              insert_users(objects: [
                {
                  name: "John",
                  email: "john@example.com",
                  created_utc: "2021-11-21T09:51Z"
                }
              ]) {
                affected_rows
                returning {
                  rowid,
                  name
                }
              }
            }
          |]
        expected =
          rmSpaces
            [raw|{
              "data": {
                "insert_users": {
                  "affected_rows": 1,
                  "returning": [
                    {
                      "rowid": 1,
                      "name": "John"
                    }
                  ]
                }
              }
            }|]

      conn <- open $ unpack dbPath
      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

    it "supports updating data and returning the updated data" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
        insert into users (name, email, created_utc)
        values
          ('John', 'john@update-test.com', '2021-01-01T00:00Z'),
          ('Eve', 'eve@update-test.com', '2021-01-02T00:00Z')
      |]

      let
        query =
          [gql|
            mutation UpdateUsers {
              update_users(
                filter: { email: { eq: "eve@update-test.com" } },
                set: { name: "New Name" }
              ) {
                affected_rows
                returning {
                  rowid
                  name
                }
              }
            }
          |]
        expected =
          rmSpaces
            [raw|{
              "data": {
                "update_users": {
                  "affected_rows": 1,
                  "returning": [
                    {
                      "rowid": 2,
                      "name": "New Name"
                    }
                  ]
                }
              }
            }|]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

    it "supports upserting data" $ do
      let
        firstQuery =
          [gql|
            mutation {
              insert_users(objects: [
                {
                  email: "eve@example.com",
                  name: "Eve",
                  created_utc: "2021-11-21T09:51Z"
                }
              ]) {
                affected_rows
              }
            }
          |]

        -- the `where` clause is not required here, but is there
        -- to make sure filters work properly.
        secondQuery =
          [gql|
            mutation {
              insert_users(
                objects: {
                  email: "eve@example.com",
                  name: "Eveline",
                  created_utc: "2022-11-21T09:51Z"
                },
                on_conflict: {
                  constraint: email,
                  update_columns: [name, created_utc],
                  where: { created_utc: { eq: "2021-11-21T09:51Z" }}
                }) {
                  returning { name }
                }
            }
          |]

        expected =
          rmSpaces
            [raw|
              {
                "data": {
                  "insert_users": {
                    "returning": [{
                      "name": "Eveline"
                    }]
                  }
                }
              }
            |]

      conn <- open $ unpack dbPath
      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
      Right _ <- graphql schema Nothing mempty firstQuery
      Right result <- graphql schema Nothing mempty secondQuery

      Ae.encode result `shouldBe` expected

      allUsers <- query_ conn "select * from users"

      let user =
            [ SQLText "Eveline"
            , SQLText "eve@example.com"
            , SQLText "2022-11-21T09:51Z"
            , SQLNull
            , SQLNull
            ]

      allUsers `shouldBe` [user]

    it "fails on invalid upserts" $ do
      let
        firstQuery =
          [gql|
            mutation {
              insert_users(objects: [
                {
                  email: "eve@example.com",
                  name: "Eve",
                  created_utc: "2021-11-21T09:51Z"
                }
              ]) {
                affected_rows
              }
            }
          |]

        -- We specify the `progress` column on `update_columns`,
        -- but do not provide an explicit value.
        secondQuery =
          [gql|
            mutation {
              insert_users(
                objects: {
                  email: "eve@example.com",
                  name: "Eveline",
                  created_utc: "2022-11-21T09:51Z"
                },
                on_conflict: {
                  constraint: email,
                  update_columns: [name, created_utc, progress]
                }) {
                  returning { name }
                }
            }
          |]

        expected :: Text
        expected =
          "user error (Column progress cannot be set on conflicts without being explicitly provided)"

      conn <- open $ unpack dbPath
      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
      Right _ <- graphql schema Nothing mempty firstQuery
      Just err <-
        catchAll
          (Nothing <$ graphql schema Nothing mempty secondQuery)
          (pure . Just . show)

      err `shouldBe` expected

    it "supports deleting data and returning the deleted data" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
        insert into users (name, email, created_utc)
        values
          ('John', 'john@del-test.com', '2021-01-01T00:00Z'),
          ('Eve', 'eve@del-test.com', '2021-01-02T00:00Z')
      |]

      let
        query =
          [gql|
            mutation DeleteUsers {
              delete_users(
                filter: { email: { eq: "eve@del-test.com" } }
              ) {
                affected_rows
                returning {
                  rowid
                  name
                }
              }
            }
          |]
        expected =
          rmSpaces
            [raw|{
              "data": {
                "delete_users": {
                  "affected_rows": 1,
                  "returning": [
                    {
                      "rowid": 2,
                      "name": "Eve"
                    }
                  ]
                }
              }
            }|]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

    it "supports inserting and retrieving single select fields" $ do
      let testDbPath = testRoot </> "single-select-test.db"
      conn <- open testDbPath
      let
        sqlQuery =
          [raw|
          CREATE TABLE IF NOT EXISTS checks (
            color TEXT CHECK ( color IN ('red', 'green', 'blue') )
          )
        |]
      execute_ conn $ SS.Query sqlQuery
      execute_ conn "DELETE FROM checks"
      execute_
        conn
        [sql|
          INSERT INTO checks (color)
          VALUES ('red')
        |]

      tables <- getTables conn
      schema <-
        getDerivedSchema
          defaultSchemaConf
          conn
          (T.pack testDbPath)
          tables

      -- tableColumns <- getColumns testDbPath conn "checks"

      let
        mutation =
          [gql|
            mutation {
              insert_checks(objects: [
                { color: "green" },
              ]) {
                affected_rows
              }
            }
          |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "insert_checks": {
                  "affected_rows": 1
                }
              }
            }
          |]

      Right result <- graphql schema Nothing mempty mutation

      Ae.encode result `shouldBe` expected

      allColors <- query_ conn "select * from checks"

      allColors `shouldBe` [[SQLText "red"], [SQLText "green"]]

      let
        query =
          [gql|
            query {
              checks { color }
            }
          |]
        expectedResult =
          rmSpaces
            [raw|
            { "data": {
              "checks": [
                {"color": "red"},
                {"color": "green"}
              ]
            } }
          |]

      Right queryResult <- graphql schema Nothing mempty query

      Ae.encode queryResult `shouldBe` expectedResult

    it "supports simultaneous inserts" $ do
      conn <- open $ unpack dbPath

      let
        query =
          [gql|
            mutation InsertObjects {
              insert_users(objects: [
                {
                  email: "john@example.com",
                  created_utc: "2021-11-21T09:51Z",
                }
              ]) {
                affected_rows
              }
              insert_songs(objects: [
                {
                  name: "Best Song",
                  duration_seconds: 125,
                }
              ]) {
                affected_rows
              }
            }
          |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "insert_users": {
                  "affected_rows": 1
                },
                "insert_songs": {
                  "affected_rows": 1
                }
              }
            }
          |]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables

      Right result <- graphql schema Nothing mempty query
      Ae.encode result `shouldBe` expected

      allUsers <- query_ conn "select * from songs"

      allUsers `shouldBe` [[SQLText "Best Song", SQLInteger 125]]

    it "supports simultaneous updates" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
        insert into users (name, email, created_utc)
        values
          ('John', 'john@update-test.com', '2021-01-01T00:00Z'),
          ('Eve', 'eve@update-test.com', '2021-01-02T00:00Z')
      |]

      let
        query =
          [gql|
            mutation UpdateUsers {
              john_update: update_users(
                filter: { email: { eq: "john@update-test.com" } }
                set: { name: "John New" }
              ) {
                affected_rows
              }
              eve_update: update_users(
                filter: { email: { eq: "eve@update-test.com" } }
                set: { name: "Eve New" }
              ) {
                affected_rows
              }
            }
          |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "john_update": {
                  "affected_rows": 1
                },
                "eve_update": {
                  "affected_rows": 1
                }
              }
            }
          |]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables

      Right result <- graphql schema Nothing mempty query
      Ae.encode result `shouldBe` expected

      allUsers <- query_ conn "select name from users"

      allUsers
        `shouldBe` [ [SQLText "John New"]
                   , [SQLText "Eve New"]
                   ]

    it "supports variables" $ do
      let
        query =
          [gql|
            mutation InsertUsers ($objects: [users_insert_input]) {
              insert_users(objects: $objects) {
                affected_rows
              }
            }
          |]
        variables :: Object
        variables =
          fromMaybe mempty $
            Ae.decode
              [raw|
            {
              "objects": [
                {
                  "email": "new.user@example.org",
                  "created_utc" : "2022-01-29T14:22Z"
                }
              ]
            }
          |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "insert_users": {
                  "affected_rows": 1
                }
              }
            }
          |]

      conn <- open $ unpack dbPath
      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables

      Right result <- graphql schema Nothing variables query

      Ae.encode result `shouldBe` expected

    it "correctly converts between GraphQL and SQLite floats" $ do
      conn <- open (testRoot </> "float-test.db")
      execute_
        conn
        [sql|
          CREATE TABLE IF NOT EXISTS loaders (
            id INTEGER PRIMARY KEY,
            progress REAL NOT NULL
          )
        |]
      execute_ conn "DELETE FROM loaders"
      execute_
        conn
        [sql|
          INSERT INTO loaders (id, progress)
          VALUES (1, 0), (2, 0.1), (3, -0.1), (4, 123.456)
        |]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables

      let
        mutation =
          [gql|
            mutation InsertLoaders {
              insert_loaders(objects: [
                { id: 5, progress: 1.23 },
              ]) {
                affected_rows
              }
            }
          |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "insert_loaders": {
                  "affected_rows": 1
                }
              }
            }
          |]

      Right result <- graphql schema Nothing mempty mutation

      Ae.encode result `shouldBe` expected

      allUsers <- query_ conn "select * from loaders"

      allUsers
        `shouldBe` [ [SQLInteger 1, SQLFloat 0.0]
                   , [SQLInteger 2, SQLFloat 0.1]
                   , [SQLInteger 3, SQLFloat (-0.1)]
                   , [SQLInteger 4, SQLFloat 123.456]
                   , [SQLInteger 5, SQLFloat 1.23]
                   ]

      let
        query =
          [gql|
            query GetLoaders {
              loaders { id, progress }
            }
          |]
        expectedResult :: Object
        expectedResult =
          fromMaybe mempty $
            Ae.decode
              [raw|
                { "data": { "loaders": [
                  { "id": 1, "progress": 0 },
                  { "id": 2, "progress": 0.1 },
                  { "id": 3, "progress": -0.1 },
                  { "id": 4, "progress": 123.456 },
                  { "id": 5, "progress": 1.23 }
                ]}}
              |]

      Right queryResult <- graphql schema Nothing mempty query

      queryResult `shouldBe` expectedResult

    -- This particular case used to fail in a previous ticket
    it "correctly roundtrips floats inserted using graphql and retrieved using REST" $ do
      let dbId = "float-roundtrip-test"
      withDataDbConn dbId $ \conn -> do
        SS.execute_
          conn
          [sql|
            CREATE TABLE IF NOT EXISTS loaders (
              progress REAL NOT NULL
            )
          |]

        tables <- getTables conn
        schema <-
          getDerivedSchema
            defaultSchemaConf
            conn
            ("_TEST_" <> dbId)
            tables

        let
          mutation =
            [gql|
              mutation InsertLoaders {
                insert_loaders(objects: [
                  { progress: 1.23000 },
                ]) {
                  affected_rows
                }
              }
            |]
          expected =
            rmSpaces
              [raw|
              {
                "data": {
                  "insert_loaders": {
                    "affected_rows": 1
                  }
                }
              }
            |]

        Right result <- graphql schema Nothing mempty mutation
        Ae.encode result `shouldBe` expected

        Right restResult <-
          runHandler $
            sqlQueryPostHandler
              PragmaConf.defaultConf
              ("_TEST_" <> dbId)
              SQLPost{query = "SELECT * from loaders"}

        restResult.rows
          `shouldBe` [ KeyMap.singleton
                        "progress"
                        (Number 1.23)
                     ]

    it "supports updating data" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values
          ('John', 'john@example.com', '2021-01-01T00:00Z'),
          ('Eve', 'eve@example.com', '2018-01-01T00:00Z')
        |]

      let
        query =
          [gql|
            mutation UpdateUser {
              update_users(
                filter: {email: {eq: "eve@example.com"}},
                set: {
                  name: "Liz"
                }
              ) {
                affected_rows
              }
            }
          |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "update_users": {
                  "affected_rows": 1
                }
              }
            }
          |]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

      remainingUsers <-
        query_
          conn
          "select * from users where email = 'eve@example.com'"

      remainingUsers
        `shouldBe` [
                     [ SQLText "Liz"
                     , SQLText "eve@example.com"
                     , SQLText "2018-01-01T00:00Z"
                     , SQLNull
                     , SQLNull
                     ]
                   ]

    it "supports deleting data by text id" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values
          ('John', 'john@example.com', '2021-01-01T00:00Z'),
          ('Eve', 'eve@example.com', '2019-01-01T00:00Z')
        |]

      let
        query =
          [gql|
            mutation DeleteUsers {
              delete_users(filter: {email: {eq: "eve@example.com"}}) {
                affected_rows
              }
            }
          |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "delete_users": {
                  "affected_rows": 1
                }
              }
            }
          |]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

      remainingUsers <- query_ conn "select * from users"

      remainingUsers
        `shouldBe` [
                     [ SQLText "John"
                     , SQLText "john@example.com"
                     , SQLText "2021-01-01T00:00Z"
                     , SQLNull
                     , SQLNull
                     ]
                   ]

    it "supports deleting data by integer id" $ do
      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values
          ('John', 'john@example.com', '2021-01-01T00:00Z'),
          ('Eve', 'eve@example.com', '2019-01-01T00:00Z')
        |]

      let
        query =
          [gql|
            mutation DeleteUsers {
              delete_users(filter: {rowid: {eq: 2}}) {
                affected_rows
              }
            }
          |]
        expected =
          rmSpaces
            [raw|{
            "data": {
              "delete_users": {
                "affected_rows": 1
              }
            }
          }|]

      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

      remainingUsers <- query_ conn "select * from users"

      remainingUsers
        `shouldBe` [
                     [ SQLText "John"
                     , SQLText "john@example.com"
                     , SQLText "2021-01-01T00:00Z"
                     , SQLNull
                     , SQLNull
                     ]
                   ]

    it "returns error on foreign key constraint violation" $ do
      withTestDbConn shouldSaveDbs (testRoot </> "foreign_key_constraint.db") $
        \conn -> do
          execute_
            conn
            [sql|
              CREATE TABLE artist(
                id    INTEGER PRIMARY KEY,
                name  TEXT
              )
            |]
          execute_
            conn
            [sql|
              INSERT INTO artist (id, name)
              VALUES (1, 'Artist 1')
            |]
          execute_
            conn
            [sql|
              CREATE TABLE track(
                id       INTEGER,
                name     TEXT,
                artistId  INTEGER,  -- Must map to an artist.id
                FOREIGN KEY(artistId) REFERENCES artist(id)
              )
            |]

          tables <- getTables conn
          schema <-
            getDerivedSchema defaultSchemaConf conn dbPath tables

          let invalidQuery =
                [gql|
                mutation InsertTrack {
                  insert_track(objects: [
                    {
                      id: 1,
                      name: "Track 1",
                      artistId: 2
                    }
                  ]) {
                    affected_rows
                  }
                }
              |]

          Right result <- graphql schema Nothing mempty invalidQuery

          let expected =
                rmSpaces
                  [raw|{
                  "data": { "insert_track": null },
                  "errors": [
                    {
                      "locations": [ { "column": 3, "line": 2 } ],
                      "message":
                        "SQLite3 returned ErrorConstraint while attempting to perform step: FOREIGN KEY constraint failed",
                      "path": [ "insert_track" ]
                    }
                  ]
                }|]

          Ae.encode result `shouldBe` expected

  describe "Introspection" $ do
    it "creates JSON object for GQL types" $ do
      let
        tableName = "example_table"

        actual =
          createType
            tableName
            "Description of field"
            [] -- No arguments
            ["NON_NULL", "LIST", "NON_NULL", "OBJECT"]
            tableName

        expected =
          GQL.Object $
            HashMap.fromList
              [ ("name", GQL.String tableName)
              , ("description", "Description of field")
              ,
                ( "type"
                , GQL.Object $
                    HashMap.fromList
                      [ ("kind", "NON_NULL")
                      ,
                        ( "ofType"
                        , GQL.Object $
                            HashMap.fromList
                              [ ("kind", "LIST")
                              ,
                                ( "ofType"
                                , GQL.Object $
                                    HashMap.fromList
                                      [ ("kind", "NON_NULL")
                                      ,
                                        ( "ofType"
                                        , GQL.Object $
                                            HashMap.fromList
                                              [ ("kind", "OBJECT")
                                              , ("name", GQL.String tableName)
                                              ]
                                        )
                                      ]
                                )
                              ]
                        )
                      ]
                )
              ]

      actual `shouldBe` expected

    describe "Query" $ do
      it "supports a minimal introspection query" $ do
        let
          introspectionQuery :: Text
          introspectionQuery =
            [gql|
              query IntrospectionQuery {
                __schema {
                  queryType { name }
                }
              }
            |]
          expected =
            rmSpaces
              [raw|
                {
                  "data": {
                    "__schema": {
                      "queryType": {
                        "name": "Query"
                      }
                    }
                  }
                }
              |]

        conn <- open $ unpack dbPath
        tables <- getTables conn
        schema <-
          getDerivedSchema defaultSchemaConf conn dbPath tables

        Right result <- graphql schema Nothing mempty introspectionQuery

        Ae.encode result `shouldBe` expected

      it "supports an optional filter argument" $ do
        let
          introspectionQuery :: Text
          introspectionQuery =
            [gql|
            query IntrospectionQuery {
              __schema {
                queryType {
                  name
                  fields {
                    name
                    args {
                      name
                      type {
                        name
                      }
                    }
                  }
                }
              }
            }
          |]
          expected =
            rmSpaces
              [raw|{
            "data": {
              "__schema": {
                "queryType": {
                  "name": "Query",
                  "fields": [
                    {
                      "name": "users",
                      "args": [
                        { "name": "filter",
                          "type": { "name": "users_filter" }
                        },
                        { "name": "order_by",
                          "type": { "name": null }
                        },
                        { "name": "limit",
                          "type": { "name": "Int" }
                        },
                        { "name": "offset",
                          "type": { "name": "Int" }
                        }
                      ]
                    },
                    {
                      "name": "songs",
                      "args": [
                        { "name": "filter",
                          "type": { "name": "songs_filter" }
                        },
                        { "name": "order_by",
                          "type": { "name": null }
                        },
                        { "name": "limit",
                          "type": { "name": "Int" }
                        },
                        { "name": "offset",
                          "type": { "name": "Int" }
                        }
                      ]
                    }
                  ]
                }
              }
            }
          }|]

        conn <- open $ unpack dbPath
        tables <- getTables conn
        schema <-
          getDerivedSchema defaultSchemaConf conn dbPath tables

        Right result <- graphql schema Nothing mempty introspectionQuery

        Ae.encode result `shouldBe` expected

      it "doesn't allow writeonly tokens to read data" $ do
        let
          introspectionQuery :: Text
          introspectionQuery =
            [gql|
              query IntrospectionQuery {
                __schema {
                  queryType { name }
                }
              }
            |]

          expected =
            rmSpaces
              [raw|
                {
                  "data": null,
                  "errors": [{
                    "locations": [{ "column":3, "line":2 }],
                    "message": "user error (Cannot read field using writeonly access code)",
                    "path": ["__schema"]
                  }]
                }
              |]

        schema <- withRetryConn (unpack dbPath) $ \conn -> do
          tables <- getTables conn
          getDerivedSchema
            defaultSchemaConf{accessMode = WriteOnly}
            conn
            dbPath
            tables

        Right response <-
          graphql schema Nothing mempty introspectionQuery

        Ae.encode response `shouldBe` expected

    describe "Mutation" $ do
      it "supports introspection queries" $ do
        let
          introspectionQuery :: Text
          introspectionQuery =
            [gql|
              query IntrospectionQuery {
                __schema {
                  mutationType {
                    name
                    fields {
                      name
                      args {
                        name
                      }
                    }
                  }
                }
              }
            |]
          expected =
            rmSpaces
              [raw|
            {
              "data": {
                "__schema": {
                  "mutationType": {
                    "name": "Mutation",
                    "fields": [
                      {
                        "name": "insert_users",
                        "args": [ { "name": "objects" }, { "name": "on_conflict" } ]
                      },
                      {
                        "name": "update_users",
                        "args": [
                          { "name": "filter" },
                          { "name": "set" }
                        ]
                      },
                      {
                        "name": "delete_users",
                        "args": [ { "name": "filter" } ]
                      },
                      {
                        "name": "insert_songs",
                        "args": [ { "name": "objects" }, { "name": "on_conflict" } ]
                      },
                      {
                        "name": "update_songs",
                        "args": [
                          { "name": "filter" },
                          { "name": "set" }
                        ]
                      },
                      {
                        "name": "delete_songs",
                        "args": [ { "name": "filter" } ]
                      }
                    ]
                  }
                }
              }
            }
          |]

        conn <- open $ unpack dbPath
        tables <- getTables conn
        schema <-
          getDerivedSchema defaultSchemaConf conn dbPath tables

        Right result <- graphql schema Nothing mempty introspectionQuery

        Ae.encode result `shouldBe` expected

    it "supports __typename on root query" $ do
      let
        introspectionQuery =
          [gql|
          query TypeName {
            __typename
          }
        |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "__typename" : "Query"
              }
            }
          |]

      conn <- open $ unpack dbPath
      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables

      Right result <- graphql schema Nothing mempty introspectionQuery

      Ae.encode result `shouldBe` expected

    it "returns fields for {query,mutation,subscription}Type" $ do
      let
        introspectionQuery =
          [gql|
            query {
              __schema {
                queryType { fields { name } }
                mutationType { fields { name } }
                subscriptionType { fields { name } }
              }
            }
          |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "__schema": {
                  "queryType": {
                    "fields": [
                      { "name": "users" },
                      { "name": "songs" }
                    ]
                  },
                  "subscriptionType": null,
                  "mutationType": {
                    "fields": [
                      { "name": "insert_users" },
                      { "name": "update_users" },
                      { "name": "delete_users" },
                      { "name": "insert_songs" },
                      { "name": "update_songs" },
                      { "name": "delete_songs" }
                    ]
                  }
                }
              }
            }
          |]

      conn <- open $ unpack dbPath
      tables <- getTables conn
      schema <-
        getDerivedSchema defaultSchemaConf conn dbPath tables

      Right result <- graphql schema Nothing mempty introspectionQuery

      Ae.encode result `shouldBe` expected

    it "supports __typename fields" $ do
      let
        introspectionQuery =
          [gql|
          query UsersTypeName {
            users {
              __typename
            }
          }
        |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "users": [
                  { "__typename" : "users_row" }
                ]
              }
            }
          |]

      conn <- open $ unpack dbPath
      execute_
        conn
        [sql|
        insert into users (name, email, created_utc)
        values ('John', 'john@example.com', '2022-01-01T00:00Z')
      |]
      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables

      Right result <- graphql schema Nothing mempty introspectionQuery

      Ae.encode result `shouldBe` expected

    it "returns types" $ do
      let
        introspectionQuery =
          [gql|
          query {
            __schema {
              types {
                kind
                name
              }
            }
          }
        |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "__schema": {
                  "types": [
                    { "kind": "OBJECT", "name": "users_row" },
                    { "kind": "OBJECT", "name": "users_mutation_response" },
                    { "kind": "INPUT_OBJECT", "name": "users_insert_input" },
                    { "kind": "ENUM", "name": "users_column" },
                    { "kind": "INPUT_OBJECT", "name": "users_upsert_on_conflict" },
                    { "kind": "INPUT_OBJECT", "name": "users_set_input" },
                    { "kind": "INPUT_OBJECT", "name": "users_filter" },
                    { "kind": "INPUT_OBJECT", "name": "users_order_by" },

                    { "kind": "OBJECT", "name": "songs_row" },
                    { "kind": "OBJECT", "name": "songs_mutation_response" },
                    { "kind": "INPUT_OBJECT", "name": "songs_insert_input" },
                    { "kind": "ENUM", "name": "songs_column" },
                    { "kind": "INPUT_OBJECT", "name": "songs_upsert_on_conflict" },
                    { "kind": "INPUT_OBJECT", "name": "songs_set_input" },
                    { "kind": "INPUT_OBJECT", "name": "songs_filter" },
                    { "kind": "INPUT_OBJECT", "name": "songs_order_by" },

                    { "kind": "INPUT_OBJECT", "name": "IntComparison" },
                    { "kind": "INPUT_OBJECT", "name": "FloatComparison" },
                    { "kind": "INPUT_OBJECT", "name": "StringComparison" },
                    { "kind": "INPUT_OBJECT", "name": "BooleanComparison" },

                    { "kind": "ENUM", "name": "OrderingTerm" },

                    { "kind": "OBJECT", "name": "Query" },
                    { "kind": "OBJECT", "name": "Mutation" },
                    { "kind": "SCALAR", "name": "Boolean" },
                    { "kind": "SCALAR", "name": "Int" },
                    { "kind": "SCALAR", "name": "Float" },
                    { "kind": "SCALAR", "name": "String" },
                    { "kind": "SCALAR", "name": "ID" },
                    { "kind": "SCALAR", "name": "Upload" },
                    { "kind": "OBJECT", "name": "__Schema" },
                    { "kind": "OBJECT", "name": "__Type" },
                    { "kind": "ENUM",   "name": "__TypeKind" },
                    { "kind": "OBJECT", "name": "__Field" },
                    { "kind": "OBJECT", "name": "__InputValue" },
                    { "kind": "OBJECT", "name": "__EnumValue" },
                    { "kind": "OBJECT", "name": "__Directive" },
                    { "kind": "ENUM",   "name": "__DirectiveLocation" }
                  ]
                }
              }
            }
          |]

      conn <- open $ unpack dbPath
      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables

      Right result <- graphql schema Nothing mempty introspectionQuery

      Ae.encode result `shouldBe` expected

    it "returns directives on __schema" $ do
      let
        introspectionQuery :: Text =
          [gql|
            query UsersTypeName {
              __schema {
                directives {
                  name
                  description
                  locations
                  args {
                    name
                    description
                    defaultValue
                    type { ...TypeRef }
                  }
                }
              }
            }

            fragment TypeRef on __Type {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                  ofType {
                    kind
                    name
                    ofType {
                      kind
                      name
                      ofType {
                        kind
                        name
                        ofType {
                          kind
                          name
                          ofType {
                            kind
                            name
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          |]
        expected =
          rmSpaces
            [raw|
            {
              "data": {
                "__schema": {
                  "directives": [
                    {
                      "name": "skip",
                      "args": [
                        {
                          "name": "if",
                          "defaultValue": null,
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind":"SCALAR",
                              "name":"Boolean",
                              "ofType":null
                            }
                          },
                          "description": "Skipped when true."
                        }
                      ],
                      "locations": [
                        "INLINE_FRAGMENT",
                        "FRAGMENT_SPREAD",
                        "FIELD"
                      ],
                      "description": "Directs the executor to skip this field or fragment when the `if` argument is true."
                    },
                    {
                      "name": "include",
                      "args": [
                        {
                          "name": "if",
                          "defaultValue": null,
                          "type": {
                            "kind": "NON_NULL",
                            "name": null,
                            "ofType": {
                              "kind":"SCALAR",
                              "name":"Boolean",
                              "ofType":null
                            }
                          },
                          "description": "Included when true."
                        }
                      ],
                      "locations": [
                        "INLINE_FRAGMENT",
                        "FRAGMENT_SPREAD",
                        "FIELD"
                      ],
                      "description":
                        "Directs the executor to include this field or fragment only when the `if` argument is true."
                    },
                    {
                      "name": "deprecated",
                      "args": [
                        {
                          "name": "reason",
                          "defaultValue": "\"No longer supported\"",
                          "type": {
                            "kind": "SCALAR",
                            "name": "String",
                            "ofType": null
                          },
                          "description":
                            "Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formatted using the Markdown syntax (as specified by [CommonMark](https://commonmark.org/)."
                        }
                      ],
                      "locations": [
                        "ENUM_VALUE",
                        "FIELD_DEFINITION"
                      ],
                      "description":
                        "Marks an element of a GraphQL schema as no longer supported."
                    }
                  ]
                }
              }
            }
          |]

      conn <- open $ unpack dbPath
      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables

      Right result <- graphql schema Nothing mempty introspectionQuery

      Ae.encode result `shouldBe` expected

    it "supports a full introspection query" $ do
      gqlFile <- makeAbsolute $ testRoot </> "introspection_query.gql"
      introspectionQuery <- readFile gqlFile

      jsonFile <- makeAbsolute $ testRoot </> "introspection_result.json"
      expected <- readFile jsonFile

      conn <- open $ unpack dbPath
      tables <- getTables conn
      schema <- getDerivedSchema defaultSchemaConf conn dbPath tables

      Right result <- graphql schema Nothing mempty introspectionQuery

      Ae.encode result `shouldBe` rmSpaces expected

    it "doesn't allow writeonly tokens to return data" $ do
      let dbName = "no-writeonly-return.db"
      withTestDbConn shouldSaveDbs dbName $ \conn -> do
        execute_
          conn
          [sql|
            CREATE TABLE items (
              id INTEGER PRIMARY KEY
            )
          |]

      let
        query :: Text
        query =
          [gql|
              mutation items {
                update_items(filter: { id: { eq: 0 }}, set: { id: 0 }) {
                  returning { id }
                }
              }
            |]

        expected =
          rmSpaces
            [raw|
              {
                "data": null,
                "errors": [{
                  "locations": [{ "column":3, "line":2 }],
                  "message": "Cannot query field \"update_items\" on type \"Mutation\"."
                }]
              }
            |]

      schema <- withRetryConn (unpack dbPath) $ \conn -> do
        tables <- getTables conn
        getDerivedSchema
          defaultSchemaConf{accessMode = WriteOnly}
          conn
          dbPath
          tables

      Right response <-
        graphql schema Nothing mempty query

      Ae.encode response `shouldBe` expected


deleteDbEntries :: Text -> IO ()
deleteDbEntries databasePath = do
  conn <- open $ unpack databasePath
  execute_ conn "delete from users"
  execute_ conn "delete from songs"


main :: IO ()
main = do
  cwd <- getWorkingDirectory
  createDirectoryIfMissing True (".." </> "data" </> "TEST" </> "data")
  changeWorkingDirectory (cwd </> ".." </> "data" </> "TEST")

  removeIfExists $ unpack dbPath

  withRetryConn (unpack dbPath) $ \conn -> do
    createUsersTable conn
    createSongsTable conn

  Hspec.hspec $ before_ (deleteDbEntries dbPath) testSuite
