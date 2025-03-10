{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Tests.QuerySpec (main) where

import Protolude (
  Either (Right),
  FilePath,
  Maybe (Nothing),
  Monoid (mempty),
  bracket_,
  fromMaybe,
  show,
  void,
  ($),
 )

import Data.Aeson qualified as Ae
import Database.SQLite.Simple qualified as SS
import Database.SQLite.Simple.QQ (sql)
import Language.GraphQL.JSON (graphql)
import Language.GraphQL.TH (gql)
import System.FilePath ((</>))
import Test.Hspec (Spec, before_, describe, it, shouldBe, shouldContain)

import AirGQL.GraphQL (getDerivedSchema)
import AirGQL.Lib (getEnrichedTables)
import AirGQL.Raw (raw)
import AirGQL.Types.SchemaConf (defaultSchemaConf)
import AirGQL.Utils (removeIfExists, withRetryConn)
import Data.Text qualified as T
import Tests.Utils (dbPath, fixtureDbId, rmSpaces, shouldSaveDbs, testRoot, unorderedShouldBe, withTestDbConn)


main :: Spec
main = void $ do
  it "supports retrieving data" $ do
    conn <- SS.open dbPath
    SS.execute_
      conn
      [sql|
        insert into users (name, email, created_utc)
        values ('Adrian', 'adrian@example.com', '2021-01-01T00:00Z')
      |]

    Right tables <- getEnrichedTables conn
    schema <-
      getDerivedSchema
        defaultSchemaConf
        conn
        fixtureDbId
        tables

    Right result <- graphql schema Nothing mempty "{ users { name } }"
    Ae.encode result
      `shouldBe` [gql|
        {"data":{"users":[{"name":"Adrian"}]}}
      |]

  it "supports retrieving data from tables with special names" $ do
    let testDbPath = testRoot </> "special-table-name.db"
    removeIfExists testDbPath
    let dbPathNorm = if shouldSaveDbs then testDbPath else ":memory:"

    withRetryConn dbPathNorm $ \conn -> do
      SS.execute_
        conn
        [sql|
          CREATE TABLE "name with-special$chars" (
            name TEXT
          )
        |]
      SS.execute_
        conn
        [sql|
          INSERT INTO "name with-special$chars" (name)
          VALUES ('John')
        |]

      Right tables <- getEnrichedTables conn
      schema <-
        getDerivedSchema
          defaultSchemaConf
          conn
          fixtureDbId
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
        conn <- SS.open dbPathSpaces
        SS.execute_
          conn
          [sql|
            CREATE TABLE IF NOT EXISTS test_entries (
              id INTEGER PRIMARY KEY,
              `column with spaces` TEXT
            )
          |]
        SS.execute_
          conn
          [sql|
            INSERT INTO test_entries (id, `column with spaces`)
            VALUES (0, 'Just a test')
          |]

    before_ setupDatabaseSpaces $ it "supports column names with spaces" $ do
      conn <- SS.open dbPathSpaces
      Right tables <- getEnrichedTables conn
      schema <-
        getDerivedSchema
          defaultSchemaConf
          conn
          (T.pack dbPathSpaces)
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
      it "generates introspection schema for column names with spaces" $ do
        conn <- SS.open dbPathSpaces
        Right tables <- getEnrichedTables conn
        schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
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
    conn <- SS.open dbPathSpaces
    SS.execute_
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
    SS.execute_
      conn
      [sql|
        INSERT INTO test_entries
          (id, `the column`, the_column, the_column_1, the_column_2)
        VALUES
          (0, 'with spaces', 'no spaces', 'no spaces 1', 'no spaces 2')
      |]

    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
    conn <- SS.open dbPathSpaces

    SS.execute_
      conn
      [sql|
        CREATE TABLE IF NOT EXISTS "users" (
          "email"             TEXT NOT NULL UNIQUE PRIMARY KEY,
          "number_of_logins"  INTEGER
        );
      |]
    SS.execute_
      conn
      [sql|
        INSERT INTO users (email, number_of_logins)
        VALUES ('john@example.com', 0), ('eve@example.com', 4);
      |]

    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
    conn <- SS.open dbPath
    SS.execute_
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

    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

    Right result <- graphql schema Nothing mempty query
    Ae.encode result `shouldBe` expected

  it "supports fragments" $ do
    conn <- SS.open dbPath
    SS.execute_
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

            fragment basicFields on users_row {
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

    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

    Right result <- graphql schema Nothing mempty query
    Ae.encode result `shouldBe` expected

  it "supports directives" $ do
    conn <- SS.open dbPath
    SS.execute_
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

      variables :: Ae.Object
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

    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

    Right result <- graphql schema Nothing variables query

    Ae.encode result `shouldBe` expected

  it "supports retrieving records with a filter" $ do
    conn <- SS.open dbPath
    SS.execute_
      conn
      [sql|
        insert into users (name, email, created_utc)
        values
        ('John', 'john@example.com', '2021-01-01T00:00Z'),
        ('Eve', 'eve@example.com', '2019-01-01T00:00Z')
      |]

    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
    conn <- SS.open dbPath
    SS.execute_
      conn
      [sql|
        insert into users (name, email, created_utc, progress)
        values
        ('John', 'john@example.com', '2021-01-01T00:00Z', 0.7),
        ('Eve', 'eve@example.com', '2019-01-01T00:00Z', 0.4)
      |]

    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
    conn <- SS.open dbPathFilter
    SS.execute_
      conn
      [sql|
        CREATE TABLE IF NOT EXISTS "users" (
          "name"              TEXT,
          "is_admin"          BOOLEAN
        );
      |]
    SS.execute_
      conn
      [sql|
        INSERT INTO USERS (name, is_admin)
        VALUES
        ('John', TRUE),
        ('Eve', FALSE),
        ('Anna', NULL)
      |]

    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
    withRetryConn dbPath $ \conn -> do
      SS.execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values
          ('John', 'john@example.com', '2021-01-01T00:00Z'),
          ('Anna', 'anna@EXAMPLE.COM', '2019-01-01T00:00Z'),
          ('Eve', 'eve@evil.com', '2019-01-01T00:00Z')
        |]

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
    withRetryConn dbPath $ \conn -> do
      SS.execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values
          ('John', 'john@example.com', '2021-01-01T00:00Z'),
          ('Anna', 'anna@example.com', '2019-01-01T00:00Z'),
          ('Eve', 'eve@evil.com', '2019-01-01T00:00Z')
        |]

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
    withRetryConn dbPath $ \conn -> do
      SS.execute_
        conn
        [sql|
          insert into users (name, email, created_utc)
          values
          ('John', 'john@example.com', '2021-01-01T00:00Z'),
          ('Anna', 'anna@example.com', '2019-01-01T00:00Z'),
          ('Eve', 'eve@evil.com', '2019-01-01T00:00Z')
        |]

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
    withTestDbConn "multi-type_columns.db" $ \conn -> do
      SS.execute_
        conn
        [sql|
          CREATE VIEW "multi_type_column" AS
          SELECT 1 AS col UNION
          SELECT 2.2 AS col UNION
          SELECT 'three' AS col UNION
          SELECT NULL AS col
        |]

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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

  it "supports querying a single entry by inferred pk" $ do
    conn <- SS.open dbPath
    SS.execute_
      conn
      [sql|
        insert into users (name, email, created_utc)
        values
        ('John', 'john@example.com', '2021-01-01T00:00Z'),
        ('Eve', 'eve@example.com', '2019-01-01T00:00Z')
      |]

    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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

    let expected =
          [raw|
            {
              "data": {
                "users_by_pk": {
                  "name":"Eve"
                }
              }
            }
          |]

    result `unorderedShouldBe` expected

  it "supports querying a single entry by composite pk" $ do
    conn <- SS.open dbPath
    -- The changes we make here influence other tests. At some point we
    -- should probably come up with a better system, but for now I'm just
    -- rollbacking so the creation of the table does not influence
    -- introspection tests and whatnot.
    bracket_
      (SS.execute_ conn [sql| begin transaction |])
      (SS.execute_ conn [sql| rollback |])
      $ do
        SS.execute_
          conn
          [sql|
            create table users_liked_songs (
              email TEXT REFERENCES users,
              song_id INT REFERENCES songs,
              rating INT NOT NULL,
              PRIMARY KEY (email, song_id)
            )
          |]

        SS.execute_
          conn
          [sql|
            insert into users (name, email, created_utc)
            values
            ('John', 'john@example.com', '2021-01-01T00:00Z'),
            ('Eve', 'eve@example.com', '2019-01-01T00:00Z')
          |]

        SS.execute_
          conn
          [sql|
            insert into songs (name, duration_seconds)
            values
              ('Never Gonna Give You Up', 215),
              ('Beethoven — Symphony No. 9', 3600);
          |]

        SS.execute_
          conn
          [sql|
            insert into users_liked_songs (email, song_id, rating)
            values
              ('eve@example.com', 1, 8),
              ('john@example.com', 2, 9),
              ('eve@example.com', 2, 7);
          |]

        Right tables <- getEnrichedTables conn
        schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

        Right result <-
          graphql
            schema
            Nothing
            mempty
            [gql|
              {
                users_liked_songs_by_pk (email: "eve@example.com", song_id: 2) {
                  rating
                }
              }
            |]

        let expected =
              [raw|
                {
                  "data": {
                    "users_liked_songs_by_pk": {
                      "rating": 7
                    }
                  }
                }
              |]

        result `unorderedShouldBe` expected

  it "errors out on integer overflows" $ do
    withTestDbConn "integer-overflows.db" $ \conn -> do
      SS.execute_
        conn
        [sql|
            CREATE TABLE test (
              big INTEGER,
              alsobig INTEGER
            )
          |]

      SS.execute_
        conn
        [sql|
          INSERT INTO test(big, alsobig)
          VALUES (8000000000, 9000000000)
        |]

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
                      "test": [{
                        "alsobig": null,
                        "big": null
                      }]
                    },
                    "errors": [{
                      "locations": [{
                        "column": 5,
                        "line": 3
                      }],
                      "message":
                        "user error (Integer 8000000000 would overflow. This happens because SQLite uses 64-bit ints, but GraphQL uses 32-bit ints. Use a Number (64-bit float) or Text column instead.)",
                      "path": ["test", 0, "big"]
                    }, {
                      "locations": [{
                        "column": 5,
                        "line": 4
                      }],
                      "message":
                        "user error (Integer 9000000000 would overflow. This happens because SQLite uses 64-bit ints, but GraphQL uses 32-bit ints. Use a Number (64-bit float) or Text column instead.)",
                      "path": ["test", 0, "alsobig"]
                    }]
                  }
                |]

      Ae.encode result `shouldBe` expected

  it "treats NULL as a value when using 'neq' filters" $ do
    conn <- SS.open dbPath
    SS.execute_
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
    SS.execute_
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
    SS.execute_
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
