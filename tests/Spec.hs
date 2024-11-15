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
  Monoid (mempty),
  fromMaybe,
  ($),
  (&),
  (<>),
 )
import Protolude qualified as P

import Data.Aeson (Value (Number))
import Data.Aeson qualified as Ae
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Object)
import Data.List qualified as List
import Data.Text qualified as T
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

import AirGQL.GraphQL (getDerivedSchema)
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
  getEnrichedTables,
  getTables,
  replaceCaseInsensitive,
  stringToGqlTypeName,
 )
import AirGQL.Lib qualified
import AirGQL.Raw (raw)
import AirGQL.Servant.SqlQuery (sqlQueryPostHandler)
import AirGQL.Types.PragmaConf qualified as PragmaConf
import AirGQL.Types.SchemaConf (defaultSchemaConf)
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
import Tests.QuerySpec qualified
import Tests.Utils (
  dbPath,
  fixtureDbId,
  rmSpaces,
  testRoot,
  unorderedShouldBe,
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

      it "should not allow '_by_pk' in table names" $ do
        let dbId = "api-sql-by-pk-name"
        let query = "CREATE TABLE foo_by_pk ( bar TEXT )"
        withDataDbConn dbId $ \_ -> do
          Right result <-
            runHandler $
              sqlQueryPostHandler
                PragmaConf.defaultConf
                ("_TEST_" <> dbId)
                SQLPost{query = query}

          let expectedMessage =
                [raw|Table names shouldn't contain "_by_pk", yet "foo_by_pk" does|]

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

      conn <- SS.open dbPath
      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
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

      Right tables <- getEnrichedTables conn
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

        Right tables <- getEnrichedTables conn
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
      conn <- SS.open dbPath
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

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

    it "treats NULL as a value when using 'nin' filters" $ do
      conn <- SS.open dbPath
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

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

    it "supports 'in' filters" $ do
      conn <- SS.open dbPath
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

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
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

      conn <- SS.open dbPath
      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

    it "supports updating data and returning the updated data" $ do
      conn <- SS.open dbPath
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

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
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

      conn <- SS.open dbPath
      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
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

        expected =
          [raw|
            {
              "data": null,
              "errors": [{
                "locations": [{ "column": 3, "line": 2 }],
                "path": ["insert_users"],
                "message": "user error (Column progress cannot be set on conflicts without being explicitly provided)"
              }]
            }
          |]

      conn <- SS.open dbPath
      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
      Right _ <- graphql schema Nothing mempty firstQuery
      Right err <- graphql schema Nothing mempty secondQuery

      err `unorderedShouldBe` expected

    it "supports deleting data and returning the deleted data" $ do
      conn <- SS.open dbPath
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
            [raw|
              {
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
              }
            |]

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

    it "supports inserting and retrieving single select fields" $ do
      let testDbPath = testRoot </> "single-select-test.db"
      conn <- open testDbPath
      let
        sqlQuery =
          [sql|
            CREATE TABLE IF NOT EXISTS checks (
              color TEXT CHECK ( color IN ('red', 'green', 'blue') )
            )
          |]
      execute_ conn sqlQuery
      execute_ conn "DELETE FROM checks"
      execute_
        conn
        [sql|
          INSERT INTO checks (color)
          VALUES ('red')
        |]

      Right tables <- getEnrichedTables conn
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
      conn <- SS.open dbPath

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

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

      Right result <- graphql schema Nothing mempty query
      Ae.encode result `shouldBe` expected

      allUsers <- query_ conn "select * from songs"

      allUsers `shouldBe` [[SQLText "Best Song", SQLInteger 125]]

    it "supports simultaneous updates" $ do
      conn <- SS.open dbPath
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

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
            mutation InsertUsers ($objects: [users_insert_input!]!) {
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

      conn <- SS.open dbPath
      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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

        Right tables <- getEnrichedTables conn
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
      conn <- SS.open dbPath
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

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
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
      conn <- SS.open dbPath
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

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
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
      conn <- SS.open dbPath
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

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
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
      withTestDbConn (testRoot </> "foreign_key_constraint.db") $
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

          Right tables <- getEnrichedTables conn
          schema <-
            getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
                    "data": null,
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

  describe "Query" Tests.QuerySpec.main
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
