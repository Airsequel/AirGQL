{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Tests.IntrospectionSpec (main) where

import Protolude (
  Either (Right),
  Maybe (Nothing),
  Monoid (mempty),
  Text,
  readFile,
  void,
  ($),
 )

import Data.Aeson qualified as Ae
import Database.SQLite.Simple qualified as SS
import Database.SQLite.Simple.QQ (sql)
import Language.GraphQL.JSON (graphql)
import Language.GraphQL.TH (gql)
import System.Directory (makeAbsolute)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe)

import AirGQL.GraphQL (getDerivedSchema)
import AirGQL.Lib (AccessMode (WriteOnly), getEnrichedTables)
import AirGQL.Raw (raw)
import AirGQL.Types.SchemaConf (SchemaConf (accessMode), defaultSchemaConf)
import AirGQL.Utils (withRetryConn)
import Tests.Utils (dbPath, fixtureDbId, rmSpaces, testRoot, unorderedShouldBe, withTestDbConn)


main :: Spec
main = void $ do
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

      conn <- SS.open dbPath
      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
                      },
                      {
                        "name": "users_by_pk",
                        "args": [
                          { "name": "email",
                            "type": { "name": null }
                          }
                        ]
                      },
                      {
                        "name": "songs_by_pk",
                        "args": [
                          { "name": "rowid",
                            "type": { "name": null }
                          }
                        ]
                      }
                    ]
                  }
                }
              }
            }|]

      conn <- SS.open dbPath
      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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

      schema <- withRetryConn dbPath $ \conn -> do
        Right tables <- getEnrichedTables conn
        getDerivedSchema
          defaultSchemaConf{accessMode = WriteOnly}
          conn
          fixtureDbId
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

      conn <- SS.open dbPath
      Right tables <- getEnrichedTables conn
      schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

      Right result <- graphql schema Nothing mempty introspectionQuery

      result `unorderedShouldBe` expected

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

    conn <- SS.open dbPath
    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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
        [raw|
          {
            "data": {
              "__schema": {
                "queryType": {
                  "fields": [
                    { "name": "users" },
                    { "name": "songs" },
                    { "name": "users_by_pk" },
                    { "name": "songs_by_pk" }
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

    conn <- SS.open dbPath
    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

    Right result <- graphql schema Nothing mempty introspectionQuery

    result `unorderedShouldBe` expected

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

    conn <- SS.open dbPath
    SS.execute_
      conn
      [sql|
          insert into users (name, email, created_utc)
          values ('John', 'john@example.com', '2022-01-01T00:00Z')
        |]
    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

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

                  { "kind": "ENUM", "name": "OrderingTerm" },

                  { "kind": "OBJECT", "name": "Query" },
                  { "kind": "OBJECT", "name": "Mutation" },
                  { "kind": "SCALAR", "name": "Boolean" },
                  { "kind": "SCALAR", "name": "Int" },
                  { "kind": "SCALAR", "name": "Float" },
                  { "kind": "SCALAR", "name": "String" },
                  { "kind": "SCALAR", "name": "ID" },
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

    conn <- SS.open dbPath
    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

    Right result <- graphql schema Nothing mempty introspectionQuery

    result `unorderedShouldBe` expected

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
                    }
                  ]
                }
              }
            }
          |]

    conn <- SS.open dbPath
    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

    Right result <- graphql schema Nothing mempty introspectionQuery

    Ae.encode result `shouldBe` expected

  it "supports a full introspection query" $ do
    gqlFile <- makeAbsolute $ testRoot </> "introspection_query.gql"
    introspectionQuery <- readFile gqlFile

    jsonFile <- makeAbsolute $ testRoot </> "introspection_result.json"
    expected <- readFile jsonFile

    conn <- SS.open dbPath
    Right tables <- getEnrichedTables conn
    schema <- getDerivedSchema defaultSchemaConf conn fixtureDbId tables

    Right result <- graphql schema Nothing mempty introspectionQuery
    result `unorderedShouldBe` expected

  it "doesn't allow writeonly tokens to return data" $ do
    let dbName = "no-writeonly-return.db"
    withTestDbConn dbName $ \conn -> do
      SS.execute_
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

    schema <- withRetryConn dbPath $ \conn -> do
      Right tables <- getEnrichedTables conn
      getDerivedSchema
        defaultSchemaConf{accessMode = WriteOnly}
        conn
        fixtureDbId
        tables

    Right response <-
      graphql schema Nothing mempty query

    Ae.encode response `shouldBe` expected