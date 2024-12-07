{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Tests.MutationSpec (main) where

import Protolude (
  Either (Right),
  Maybe (Nothing),
  Monoid (mempty),
  fromMaybe,
  void,
  ($),
  (<>),
 )

import Data.Aeson qualified as Ae
import Data.Aeson.KeyMap qualified as KeyMap
import Database.SQLite.Simple qualified as SS
import Database.SQLite.Simple.QQ (sql)
import Language.GraphQL.JSON (graphql)
import Language.GraphQL.TH (gql)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe)

import AirGQL.GraphQL (getDerivedSchema)
import AirGQL.Lib (SQLPost (SQLPost, query), getEnrichedTables, insertOnly)
import AirGQL.Raw (raw)
import AirGQL.Servant.SqlQuery (sqlQueryPostHandler)
import AirGQL.Types.PragmaConf qualified as PragmaConf
import AirGQL.Types.SchemaConf (SchemaConf (accessMode), defaultSchemaConf)
import AirGQL.Types.SqlQueryPostResult (
  SqlQueryPostResult (rows),
 )
import Data.Text qualified as T
import Database.SQLite.Simple (SQLData (SQLFloat, SQLInteger, SQLNull, SQLText))
import Servant (runHandler)
import Tests.Utils (dbPath, fixtureDbId, rmSpaces, testRoot, unorderedShouldBe, withDataDbConn, withTestDbConn)


main :: Spec
main = void $ do
  describe "insert" $ do
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
      schema <-
        getDerivedSchema
          defaultSchemaConf{accessMode = insertOnly}
          conn
          fixtureDbId
          tables
      Right result <- graphql schema Nothing mempty query

      Ae.encode result `shouldBe` expected

      allUsers <- SS.query_ conn "select * from users"

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
      conn <- SS.open testDbPath
      SS.execute_
        conn
        [sql|
            CREATE TABLE IF NOT EXISTS checks (
              id INTEGER PRIMARY KEY,
              completed BOOLEAN DEFAULT (FALSE) NOT NULL
            )
          |]
      SS.execute_ conn "DELETE FROM checks"
      SS.execute_
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

      allUsers <- SS.query_ conn "select * from checks"

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
        SS.execute_
          conn
          [sql|
            CREATE TABLE IF NOT EXISTS items (
              id INTEGER PRIMARY KEY NOT NULL
            )
          |]
        SS.execute_ conn "DELETE FROM items"

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

        allItems <- SS.query_ conn "select * from items"
        allItems `shouldBe` [[SQLInteger 1]]

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

    it "supports inserting and retrieving single select fields" $ do
      let testDbPath = testRoot </> "single-select-test.db"
      conn <- SS.open testDbPath
      let
        sqlQuery =
          [sql|
            CREATE TABLE IF NOT EXISTS checks (
              color TEXT CHECK ( color IN ('red', 'green', 'blue') )
            )
          |]
      SS.execute_ conn sqlQuery
      SS.execute_ conn "DELETE FROM checks"
      SS.execute_
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

      allColors <- SS.query_ conn "select * from checks"

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

      allUsers <- SS.query_ conn "select * from songs"

      allUsers `shouldBe` [[SQLText "Best Song", SQLInteger 125]]

    it "returns error on foreign key constraint violation" $ do
      withTestDbConn "foreign-key-constraint.db" $
        \conn -> do
          SS.execute_
            conn
            [sql|
              CREATE TABLE artist(
                id    INTEGER PRIMARY KEY,
                name  TEXT
              )
            |]
          SS.execute_
            conn
            [sql|
              INSERT INTO artist (id, name)
              VALUES (1, 'Artist 1')
            |]
          SS.execute_
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

    it "correctly converts between GraphQL and SQLite floats" $ do
      conn <- SS.open $ testRoot </> "float-test.db"
      SS.execute_
        conn
        [sql|
          CREATE TABLE IF NOT EXISTS loaders (
            id INTEGER PRIMARY KEY,
            progress REAL NOT NULL
          )
        |]
      SS.execute_ conn "DELETE FROM loaders"
      SS.execute_
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

      allUsers <- SS.query_ conn "select * from loaders"

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

        expectedResult :: Ae.Object
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
                        (Ae.Number 1.23)
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

        variables :: Ae.Object
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

  describe "update" $ do
    it "supports updating data and returning the updated data" $ do
      conn <- SS.open dbPath
      SS.execute_
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

      allUsers <- SS.query_ conn "select * from users"

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

    it "supports simultaneous updates" $ do
      conn <- SS.open dbPath
      SS.execute_
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

      allUsers <- SS.query_ conn "select name from users"

      allUsers
        `shouldBe` [ [SQLText "John New"]
                   , [SQLText "Eve New"]
                   ]

    it "supports updating data" $ do
      conn <- SS.open dbPath
      SS.execute_
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
        SS.query_
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

    it "supports updating data by pk" $ do
      conn <- SS.open dbPath
      SS.execute_
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
              update_users_by_pk(
                email: "eve@update-test.com",
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
          [raw|{
            "data": {
              "update_users_by_pk": {
                "affected_rows": 1,
                "returning": {
                  "rowid": 2,
                  "name": "New Name"
                }
              }
            }
          }|]

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
      Right result <- graphql schema Nothing mempty query

      result `unorderedShouldBe` expected

  describe "delete" $ do
    it "supports deleting data and returning the deleted data" $ do
      conn <- SS.open dbPath
      SS.execute_
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

    it "supports deleting data by text id" $ do
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

      remainingUsers <- SS.query_ conn "select * from users"

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

      remainingUsers <- SS.query_ conn "select * from users"

      remainingUsers
        `shouldBe` [
                     [ SQLText "John"
                     , SQLText "john@example.com"
                     , SQLText "2021-01-01T00:00Z"
                     , SQLNull
                     , SQLNull
                     ]
                   ]

    it "supports deleting data by pk" $ do
      conn <- SS.open dbPath
      SS.execute_
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
              delete_users_by_pk(
                email: "eve@del-test.com"
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
          [raw|
            {
              "data": {
                "delete_users_by_pk": {
                  "affected_rows": 1,
                  "returning": {
                    "rowid": 2,
                    "name": "Eve"
                  }
                }
              }
            }
          |]

      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables
      Right result <- graphql schema Nothing mempty query

      result `unorderedShouldBe` expected
