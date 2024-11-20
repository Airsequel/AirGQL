-- Unit and integration tests
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

import Protolude (
  Bool (False, True),
  Either (Right),
  FilePath,
  IO,
  Maybe (Just, Nothing),
  ($),
  (&),
  (<>),
 )
import Protolude qualified as P

import Data.Aeson (Value (Number))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List qualified as List
import Data.Text qualified as T
import Database.SQLite.Simple (
  Query,
  execute_,
  open,
 )
import Database.SQLite.Simple qualified as SS
import Database.SQLite.Simple.QQ (sql)
import Servant.Server (runHandler)
import System.Directory (createDirectoryIfMissing)
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

import AirGQL.Lib (
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
import Tests.IntrospectionSpec qualified
import Tests.MutationSpec qualified
import Tests.QuerySpec qualified
import Tests.Utils (
  dbPath,
  fixtureDbId,
  withDataDbConn,
  withTestDbConn,
 )


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
      tables <- SS.withConnection dbPath $ \conn ->
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
        tableColumns <- SS.withConnection dbPath $ \conn ->
          getColumns fixtureDbId conn "users"

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
                , isReference = False
                , dflt_value = Nothing
                , primary_key = True
                , isRowid = True
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
                , isReference = False
                , dflt_value = Nothing
                , primary_key = False
                , isRowid = False
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
                , isReference = False
                , dflt_value = Nothing
                , primary_key = True
                , isRowid = False
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
                , isReference = False
                , dflt_value = Nothing
                , primary_key = False
                , isRowid = False
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
                , isReference = False
                , dflt_value = Nothing
                , primary_key = False
                , isRowid = False
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
                , isReference = False
                , dflt_value = Nothing
                , primary_key = False
                , isRowid = False
                }
            ]

        tableColumns `shouldBe` columnsExpected

      it "loads a nullable single-select column" $ do
        let dbName = "creates_nullable_single-select.db"
        withTestDbConn dbName $ \conn -> do
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
                , isReference = False
                , dflt_value = Nothing
                , primary_key = False
                , isRowid = False
                }

          P.lastMay tableColumns `shouldBe` Just columnExpected

      it "loads a non-null single-select column" $ do
        let dbName = "creates_non-null_single-select.db"
        withTestDbConn dbName $ \conn -> do
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
                , isReference = False
                , dflt_value = Nothing
                , primary_key = False
                , isRowid = False
                }

          P.lastMay tableColumns `shouldBe` Just columnExpected

      it "coerces a multi-type single-select column to text" $ do
        let dbName = "multi-type_single-select.db"
        withTestDbConn dbName $ \conn -> do
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
                , isReference = False
                , dflt_value = Nothing
                , primary_key = False
                , isRowid = False
                }

          P.lastMay tableColumns `shouldBe` Just columnExpected

      it "marks integer primary keys as omittable" $ do
        let dbName = "integer-omittable-primary-key.db"
        withTestDbConn dbName $ \conn -> do
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
                , isReference = False
                , dflt_value = Nothing
                , primary_key = True
                , isRowid = False
                }

          P.lastMay tableColumns `shouldBe` Just columnExpected

      it "correctly parses generated columns" $ do
        let dbName = "generated-columns.db"
        withTestDbConn dbName $ \conn -> do
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

  describe "Query" Tests.QuerySpec.main
  describe "Mutation" Tests.MutationSpec.main
  describe "Introspection" Tests.IntrospectionSpec.main


deleteDbEntries :: FilePath -> IO ()
deleteDbEntries databasePath = do
  conn <- open databasePath
  execute_ conn "delete from users"
  execute_ conn "delete from songs"


main :: IO ()
main = do
  cwd <- getWorkingDirectory
  createDirectoryIfMissing True (".." </> "data" </> "TEST" </> "data")
  changeWorkingDirectory (cwd </> ".." </> "data" </> "TEST")

  removeIfExists dbPath

  withRetryConn dbPath $ \conn -> do
    createUsersTable conn
    createSongsTable conn

  Hspec.hspec $ before_ (deleteDbEntries dbPath) testSuite
